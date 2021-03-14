{-# LANGUAGE TypeInType, GADTs, ScopedTypeVariables, FlexibleInstances,
             TypeOperators, ConstraintKinds, TypeFamilies, PartialTypeSignatures,
             UndecidableInstances, ViewPatterns, RankNTypes, TypeApplications,
             MultiParamTypeClasses, UndecidableSuperClasses, TemplateHaskell,
             StandaloneDeriving, DerivingStrategies, GeneralizedNewtypeDeriving #-}

{-|

Monadic capabilities are additional methods for a base monad. For instance, when
our base monad is 'IO', our capabilities might include logging, networking,
database access, and so on.

This framework allows mutually recursive late-bound capabilities with runtime
dispatch and a type-safe interface.

A capability is defined as a record type with methods parametrized over a base
monad:

@
data Logging m =
  Logging
    { _logError :: String -> m (),
      _logDebug :: String -> m ()
    }
@

We can define implementations as values of this record type:

@
loggingDummy :: Monad m => CapImpl Logging '[] m
loggingDummy = CapImpl $ Logging (\\_ -> return ()) (\\_ -> return ())

loggingIO :: MonadIO m => CapImpl Logging '[] m
loggingIO = CapImpl $
  Logging
    { _logError = \\msg -> liftIO . putStrLn $ "[Error] " ++ msg
      _logDebug = \\msg -> liftIO . putStrLn $ "[Debug] " ++ msg
    }
@

The dictionary is wrapped in 'CapImpl' to guarantee that it is sufficiently
polymorphic (this is required to support simultaneous use of monadic actions in
negative position and capability extension).

Then we want to use this capability in the 'CapsT' monad (which is nothing more
but a synonym for 'ReaderT' of 'Capabilities'), and for this we define a helper
per method:

@
logError :: HasCap Logging caps => String -> CapsT caps m ()
logError message = withCap $ \\cap -> _logError cap message

logDebug :: HasCap Logging caps => String -> CapsT caps m ()
logDebug message = withCap $ \\cap -> _logDebug cap message
@

We can define other capabilities in a similar manner:

@
data Networking m =
  Networking
    { _sendRequest :: ByteString -> m ByteString }

data FileStorage m =
  FileStorage
    { _readFile :: FilePath -> m ByteString,
      _writeFile :: FilePath -> ByteString -> m ()
    }
@

Implementations of capabilities may depend on other capabilities, which are
listed in their signature. For instance, this is how we can define the
'FileStorage' capability using the 'Logging' capability:

@
fileStorageIO :: MonadIO m => CapImpl FileStorage '[Logging] m
fileStorageIO = CapImpl $
  FileStorage
    { _readFile = \\path -> do
        logDebug $ "readFile " ++ path
        lift $ ByteString.readFile path
      _writeFile = \\path content -> do
        logDebug $
          "writeFile " ++ path ++
          " (" ++ show (ByteString.length content) ++
          " bytes)"
        lift $ ByteString.writeFile path content
    }
@

Here the @fileStorageIO@ implementation requires a logging capability,
but it's not specified which one.

When we decided what set of capabilities our application needs, we can put them
together in a 'Capabilities' map and run the application with this map in a
'ReaderT' context:

@
caps = buildCaps $
  AddCap loggingIO $
  AddCap fileStorageIO $
  BaseCaps emptyCaps

flip runReaderT caps $ do
  config <- readFile "config.yaml"
  ...
@

Capabilities passed to 'buildCaps' can depend on each other. The order does not
matter (although it is reflected in the types), and duplicate capabilities are
disallowed.

We can override a capability locally:

@
do
  config <- readFile "config.yaml"
  withReaderT (overrideCap loggingDummy) $ do
    -- logging is disabled here
    writeFile "config-backup.yaml" config
    ...
@

or we can add more capabilities:

@
do
  config <- readFile "config.yaml"
  networkingImpl <- parseNetworkingConfig config
  withReaderT (addCap networkingImpl) $ do
    -- networking capability added
    resp <- sendRequest req
    ...
@

-}

module Monad.Capabilities
  (
    -- * Capabilities
    Capabilities(),
    CapsT,
    emptyCaps,
    buildCaps,
    CapabilitiesBuilder(..),
    CapImpl(..),
    getCap,
    overrideCap,
    addCap,
    insertCap,
    withCap,
    checkCap,
    adjustCap,

    -- * Default capabilities
    Context(..),
    HasContext,
    newContext,
    askContext,
    localContext,

    -- * Type-level checks
    type HasCap,
    type HasCaps,
    type HasNoCap,
    HasCapDecision(..),

    -- * Utils
    makeCap

  ) where

import Control.Monad.Trans.Reader
import Data.Kind (Type, Constraint)
import Data.Traversable
import Data.Proxy
import Data.Type.Equality
import Data.List (foldl1')
import GHC.TypeLits (TypeError, ErrorMessage(..))
import Type.Reflection (Typeable)
import Data.Coerce (coerce)
import Unsafe.Coerce (unsafeCoerce)

import qualified Data.TypeRepMap as TypeRepMap
import Data.TypeRepMap (TypeRepMap)

import qualified Language.Haskell.TH as TH

type MonadK = Type -> Type

type CapK = MonadK -> Type

-- | @'Capabilities' caps m@ is a map of capabilities @caps@ over a base monad
-- @m@. Consider the following capabilities:
--
-- @
-- data X m = X (String -> m String)
-- data Y m = Y (Int -> m Bool)
-- @
--
-- We can construct a map of capabilities with the following type:
--
-- @
-- capsXY :: Capabilities '[X, Y] IO
-- @
--
-- In this case, @capsXY@ would be a map with two elements, one at key @X@ and
-- one at key @Y@. The types of capabilities themselves serve as keys.
--
-- 'Capabilities' is a heterogeneous collection, meaning that its values have
-- different types. The type of a value is determined by the key:
--
-- @
--
--  X:   X (\\_ -> return "hi") :: X (CapsT '[X, Y] IO)
--  Y:   Y (\\_ -> return True) :: Y (CapsT '[X, Y] IO)
-- ----  ---------------------    --------------------
-- keys         values              types of values
-- @
--
-- Notice that stored dictionaries are parametrized not just by the base monad
-- @IO@, but with the 'CapsT' transformer on top. This means that each
-- capability has access to all other capabilities and itself.
--
newtype Capabilities (caps :: [CapK]) (m :: MonadK) =
  Capabilities (TypeRepMap (CapElem m))

emptyCaps :: Capabilities '[] m
emptyCaps = Capabilities TypeRepMap.empty

deriving newtype instance Show (Capabilities caps m)

-- | The 'CapsT' transformer adds access to capabilities. This is a convenience
-- synonym for 'ReaderT' of 'Capabilities', and all 'ReaderT' functions
-- ('runReaderT', 'withReaderT') can be used with it.
type CapsT caps m = ReaderT (Capabilities caps m) m

-- | The 'CapImpl' newtype guarantees that the wrapped capability implementation
-- is sufficiently polymorphic so that required subtyping properties hold in
-- methods that take monadic actions as input (negative position).
--
-- This rules out using 'addCap', 'insertCap', and 'buildCaps' inside capability
-- implementations in an unsafe manner.
data CapImpl cap icaps m where
  CapImpl ::
    WithSpine icaps =>
    { getCapImpl :: forall caps. HasCaps icaps caps => cap (CapsT caps m)
    } ->
    CapImpl cap icaps m

newtype CapElem m cap =
  CapElem { getCapElem :: forall caps. cap (CapsT caps m) }

overCapElem ::
  (forall caps. cap (CapsT caps m) -> cap' (CapsT caps m')) ->
  CapElem m cap ->
  CapElem m' cap'
overCapElem f (CapElem cap) = CapElem (f cap)

-- Continuation-passing encoding of a list spine:
--
-- data Spine xs where
--   Cons :: Spine xs -> Spine (x : xs)
--   Nil :: Spine '[]
--
class WithSpine xs where
  onSpine ::
    forall r.
    Proxy xs ->
    ((xs ~ '[]) => r) ->
    (forall y ys.
      (xs ~ (y : ys)) =>
      WithSpine ys =>
      Proxy y ->
      Proxy ys ->
      r) ->
    r

instance WithSpine '[] where
  onSpine _ onNil _ = onNil

instance WithSpine xs => WithSpine (x : xs) where
  onSpine _ _ onCons = onCons Proxy Proxy

toCapElem ::
  forall cap icaps m.
  CapImpl cap icaps m ->
  CapElem m cap
toCapElem (CapImpl cap) = CapElem
  (fiatHasElems (Proxy @icaps) (Proxy @caps) cap :: forall caps. cap (CapsT caps m))

fiatHasElems ::
  forall icaps caps.
  WithSpine icaps =>
  Proxy icaps ->
  Proxy caps ->
  forall r. (HasCaps icaps caps => r) -> r
fiatHasElems Proxy Proxy r =
  onSpine (Proxy @icaps)
    -- nil
    r
    -- cons
    (\(Proxy :: Proxy cap) (Proxy :: Proxy icaps') ->
       case unsafeUnitConstr @(HasCap cap caps) of
         Refl -> fiatHasElems (Proxy @icaps') (Proxy @caps) r)

{-

Since 'caps' is phantom, we can reorder capabilities, remove non-unique
capabilities, or extend them.

The tricky case is extension. Assume @caps'@ subsumes @caps@, and consider each
@cap n@ where @n ~ CapsT caps m@ individually. When we cast this to use @caps'@,
we must know that @cap@ will continue to work correctly.

1. Assume @cap@ uses @n@ in positive position exclusively. This means that the
   capability defines methods that take @Capabilities caps m@ as input, and
   it's okay if we pass @Capabilities caps' m@ instead, as we will simply have
   some unnecessary input.

2. Assume @cap@ uses @n@ in a negative poistion as well. This means that the
   capability defines method that will be passing @Capabilities caps m@ to
   other monadic actions. But when we cast to @caps'@, these monadic actions
   require @Capabilities caps' m@, where @caps'@ subsumes @caps@, so at runtime
   it's possible that we don't pass all needed capabilities for them.

In order for (2) to be safe, we need to place an additional requirement on
capabilities which use the provided @Capabilities caps m@ in a negative position:

  The positive occurence of @Capabilities caps m@ must come from a value
  provided by an occurence of @Capabilities caps m@ in a negative position,
  unmodified, rather than be constructed.

Essentially, we want capabilities to do only two things with @Capabilities@:

* extract parts of it with 'getCap'
* pass it along

In this case, even when on types we put @Capabilities caps m@ in a positive
position (where @caps@ might be insufficient), at runtime we know that these
capabilities actually contain @caps'@.

We guarantee this property by the 'CapImpl' newtype.

-}

-- | 'CapabilitiesBuilder' is a type to extend capabilities.
--
-- The @allCaps@ parameter is a list of capabilities that will be provided to
-- 'buildCaps' eventually, when the building process is done. The @caps@
-- parameter is the part of capabilities that was constructed so far. The
-- builder is considered complete when @allCaps ~ caps@, only then it can be
-- passed to 'buildCaps'.
data CapabilitiesBuilder (allCaps :: [CapK]) (caps :: [CapK]) (m :: MonadK) where
  AddCap ::
    (Typeable cap, HasCaps icaps allCaps, HasNoCap cap caps) =>
    CapImpl cap icaps m ->
    CapabilitiesBuilder allCaps caps m ->
    CapabilitiesBuilder allCaps (cap : caps) m
  BaseCaps ::
    Capabilities caps m ->
    CapabilitiesBuilder allCaps caps m

-- | Build a map of capabilities from individual implementations:
--
-- @
-- capsXY :: Capabilities '[X, Y] IO
-- capsXY = buildCaps $
--     AddCap xImpl $
--     AddCap yImpl $
--     BaseCaps emptyCaps
-- @
buildCaps :: forall caps m. CapabilitiesBuilder caps caps m -> Capabilities caps m
buildCaps = Capabilities . go
  where
    go ::
      CapabilitiesBuilder caps caps' m ->
      TypeRepMap (CapElem m)
    go (BaseCaps (Capabilities caps)) = caps
    go (AddCap capImpl otherCaps) =
      TypeRepMap.insert (toCapElem capImpl) (go otherCaps)

-- | Ensure that the @caps@ list has an element @cap@.
type family HasCap cap caps :: Constraint where
  HasCap cap (cap  : _) = ()
  HasCap cap (cap' : caps) = HasCap cap caps
  HasCap cap '[] =
    TypeError
      (Text "Capability " :<>:
       ShowType cap :<>:
       Text " must be available")

-- | Ensure that the @caps@ list subsumes @icaps@. It is equivalent
-- to a @HasCap icap caps@ constraint for each @icap@ in @icaps@.
type family HasCaps icaps caps :: Constraint where
  HasCaps '[] _ = ()
  HasCaps (icap : icaps) caps = (HasCap icap caps, HasCaps icaps caps)

-- | Ensure that the @caps@ list does not have an element @cap@.
type family HasNoCap cap caps :: Constraint where
  HasNoCap cap (cap : _) =
    TypeError
      (Text "Capability " :<>:
       ShowType cap :<>:
       Text " is already present")
  HasNoCap cap (cap' : caps) = HasNoCap cap caps
  HasNoCap cap '[] = ()

-- | Lookup a capability in a 'Capabilities' map. The 'HasCap' constraint
-- guarantees that the lookup does not fail.
getCap :: forall cap m caps. (Typeable cap, HasCap cap caps) => Capabilities caps m -> cap (CapsT caps m)
getCap (Capabilities m) =
  case TypeRepMap.lookup m of
    Nothing -> error "getCap: impossible"
    Just e -> getCapElem e

-- An internal function that adds capabilities.
unsafeInsertCap ::
  (Typeable cap, HasCaps icaps caps') =>
  CapImpl cap icaps m ->
  Capabilities caps m ->
  Capabilities caps' m
unsafeInsertCap capImpl (Capabilities caps) =
  Capabilities (TypeRepMap.insert (toCapElem capImpl) caps)

-- | Extend the set of capabilities. In case the capability is already present,
-- it will be overriden (as with 'overrideCap'), but occur twice in the type.
insertCap ::
  (Typeable cap, HasCaps icaps (cap : caps)) =>
  CapImpl cap icaps m ->
  Capabilities caps m ->
  Capabilities (cap : caps) m
insertCap = unsafeInsertCap

-- | Extend the set of capabilities. In case the capability is already present,
-- a type error occurs.
addCap ::
  (Typeable cap, HasNoCap cap caps, HasCaps icaps (cap : caps)) =>
  CapImpl cap icaps m ->
  Capabilities caps m ->
  Capabilities (cap : caps) m
addCap capImpl caps = buildCaps (AddCap capImpl $ BaseCaps caps)

-- | Override the implementation of an existing capability.
overrideCap ::
  (Typeable cap, HasCap cap caps, HasCaps icaps caps) =>
  CapImpl cap icaps m ->
  Capabilities caps m ->
  Capabilities caps m
overrideCap = unsafeInsertCap

-- | Override the implementation of an existing capability using the previous
-- implementation. This is a more efficient equivalent to extracting a
-- capability with 'getCap', adjusting it with a function, and putting it back
-- with 'overrideCap'.
adjustCap ::
  forall cap caps m.
  (Typeable cap, HasCap cap caps) =>
  (forall caps'. cap (CapsT caps' m) -> cap (CapsT caps' m)) ->
  Capabilities caps m ->
  Capabilities caps m
adjustCap f (Capabilities caps) =
  Capabilities (TypeRepMap.adjust (overCapElem f) caps)

-- | Extract a capability from 'CapsT' and provide it to a continuation.
withCap :: (Typeable cap, HasCap cap caps) => (cap (CapsT caps m) -> CapsT caps m a) -> CapsT caps m a
withCap cont = ReaderT $ \caps -> runReaderT (cont (getCap caps)) caps

-- | Evidence that @cap@ is present or absent in @caps@.
data HasCapDecision cap caps where
  HasNoCap :: HasNoCap cap caps => HasCapDecision cap caps
  HasCap :: HasCap cap caps => HasCapDecision cap caps

instance Show (HasCapDecision cap caps) where
  show HasNoCap = "HasNoCap"
  show HasCap = "HasCap"

-- | Determine at runtime whether 'HasCap cap caps' or 'HasNoCap cap caps' holds.
checkCap :: forall cap caps m. Typeable cap => Capabilities caps m -> HasCapDecision cap caps
checkCap (Capabilities m) =
  if TypeRepMap.member @cap m
  then case unsafeUnitConstr @(HasCap cap caps) of Refl -> HasCap
  else case unsafeUnitConstr @(HasNoCap cap caps) of Refl -> HasNoCap

-- Use to construct 'HasCap' or 'HasNoCap'.
unsafeUnitConstr :: c :~: (() :: Constraint)
unsafeUnitConstr = unsafeCoerce Refl

-- | The 'Context' capability is used to model the @Reader@ effect within the
-- capabilities framework.
newtype Context x (m :: MonadK) = Context x

-- | The 'HasContext' constraint is a shorthand for 'HasCap' of 'Context'.
class (Typeable x, HasCap (Context x) caps) => HasContext x caps
instance (Typeable x, HasCap (Context x) caps) => HasContext x caps

-- | Initialize a 'Context' capability.
newContext :: forall x m. x -> CapImpl (Context x) '[] m
newContext x = CapImpl (Context x)

-- | Retrieve the context value. Moral equivalent of 'ask'.
askContext :: (HasContext x caps, Applicative m) => CapsT caps m x
askContext = withCap (\(Context x) -> pure x)

-- | Execute a computation with a modified context value. Moral equivalent of 'local'.
localContext :: forall x caps m a. (HasContext x caps) => (x -> x) -> CapsT caps m a -> CapsT caps m a
localContext f = local (adjustCap @(Context x) (coerce f))

makeCap :: TH.Name -> TH.DecsQ
makeCap capName = do
  let className = TH.mkName ("Monad" ++ TH.nameBase capName)
  info <- TH.reify capName
  (vbts, tyVars) <-
    case info of
      TH.TyConI (TH.DataD    _ _ tyVars _ [TH.RecC _ vbts] _) -> return (vbts, tyVars)
      TH.TyConI (TH.NewtypeD _ _ tyVars _ (TH.RecC _ vbts) _) -> return (vbts, tyVars)
      _ -> fail "Capabilities must be single-constructor record types"
  (mVar, extraTyVars) <-
    case reverse tyVars of
      (tv:tvs) -> return (tv, reverse tvs)
      _ -> fail "Capability must have a monadic parameter"
  let
    parametrize name = foldl1' TH.appT (TH.conT name : map tyVarBndrT extraTyVars)
    capType = parametrize capName
    classType = parametrize className
  methodSpecs <- for vbts $ \(fieldName, _, ty) -> do
    methodName <-
      case TH.nameBase fieldName of
        ('_':methodName) -> return $ TH.mkName methodName
        _ -> fail "Capability method names must start with underscores"
    tyArgList <-
      let
        toArgList (TH.ArrowT `TH.AppT` a `TH.AppT` b) = a:toArgList b
        toArgList (TH.ForallT _ _ a) = toArgList a
        toArgList _ = []
      in
        return $ toArgList ty
    return (methodName, fieldName, ty, tyArgList)
  class_decs <- (:[]) <$>
    TH.classD
      (TH.cxt [])
      className
      tyVars
      []
      [ TH.sigD methodName (return ty)
      | (methodName, _, ty, _) <- methodSpecs
      ]
  let
    methodDec methodName fieldName tyArgList = do
      TH.funD methodName
        [do
          argNames <- do
            for (zip [0..] tyArgList) $ \(i, _tyArg) ->
              TH.newName ("arg" ++ show (i::Int))
          let
            pats = map TH.varP argNames
            args = map TH.varE argNames
            body = TH.normalB $ do
              lamName <- TH.newName "cap"
              TH.appE (TH.appTypeE [e|withCap|] capType) $
                TH.lam1E (TH.varP lamName) $
                  foldl1' TH.appE (TH.varE fieldName : TH.varE lamName : args)
          TH.clause pats body []
        ]
  instance_decs <- (:[]) <$> do
    rVar <- TH.newName "r"
    capsVar <- TH.newName "caps"
    let typeableConstraints = [ [t|Typeable $(tyVarBndrT v)|] | v <- extraTyVars ]
    TH.instanceD
      (TH.cxt $
        [ [t|HasCap $capType $(TH.varT capsVar)|],
          [t| $(TH.varT rVar) ~ Capabilities $(TH.varT capsVar) $(tyVarBndrT' mVar) |]
        ] ++ typeableConstraints)
      [t| $classType (ReaderT $(TH.varT rVar) $(tyVarBndrT' mVar)) |]
      [ methodDec methodName fieldName tyArgList
      | (methodName, fieldName, _, tyArgList) <- methodSpecs
      ]
  return (class_decs ++ instance_decs)
  where
    tyVarBndrT (TH.PlainTV name) = TH.varT name
    tyVarBndrT (TH.KindedTV name k) = TH.sigT (TH.varT name) k

    tyVarBndrT' (TH.PlainTV name) = TH.varT name
    tyVarBndrT' (TH.KindedTV name _) = TH.varT name
