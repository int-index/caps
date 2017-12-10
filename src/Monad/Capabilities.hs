{-# LANGUAGE TypeInType, GADTs, ScopedTypeVariables, FlexibleInstances,
             TypeOperators, ConstraintKinds, TypeFamilies, PartialTypeSignatures,
             UndecidableInstances, ViewPatterns, RankNTypes #-}

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

instance Coercible1 Logging where
  coerce1 = Coercion
@

The 'Coercible1' instance is a safeguard to ensure that the 'm' parameter has
either a phantom or representatinal role. You could not define this instance
for a GADT or a data family (this is intentional).

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

Then we want to use this capability in the 'CapsT' monad, and for this
we define a helper per method:

@
logError :: (Monad m, HasCap Logging caps) => String -> CapsT caps m ()
logError message = withCap $ \\cap -> _logError cap message

logDebug :: (Monad m, HasCap Logging caps) => String -> CapsT caps m ()
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
listed in its signature. For instance, this is how we can define the
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
caps = initCaps $
  AddCap loggingIO $
  AddCap fileStorageIO $
  NoCaps

flip runReaderT caps $ do
  config <- readFile "config.yaml"
  ...
@

We can override a capability locally:

@
do
  config <- readFile "config.yaml"
  withReaderT (overrideCap loggingDummy) $ do
    -- logging is disabled here
    writeFile "config-backup.yaml" config
    resp <- sendRequest req
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
    initCaps,
    CapabilitiesBuilder(..),
    CapImpl(..),
    Cap,
    getCap,
    overrideCap,
    addCap,
    insertCap,
    withCap,

    -- * Type-level checks
    type HasCap,
    type HasCaps,
    type HasNoCap,

    -- * Utils
    Coercible1(..),
    Coercion(..)

  ) where

import Data.Kind (Type, Constraint)
import Data.Typeable (Typeable, TypeRep, typeRep)
import GHC.Exts (Any)
import GHC.TypeLits (TypeError, ErrorMessage(..))
import Data.Proxy
import Data.Coerce
import Data.Type.Coercion
import Control.Monad.Reader
import Unsafe.Coerce (unsafeCoerce)

import qualified Data.Map.Lazy as M

type MonadK = Type -> Type

type CapK = MonadK -> Type

newtype AnyCap (m :: MonadK) = AnyCap (Any m)

-- We need the 'Coercible1' constraint to guarantee
-- that 'cap' treats the monad representationally,
-- so that 'unsafeCastCapabilities' does not cause a segfault.
toAnyCap :: Coercible1 cap => cap m -> AnyCap m
toAnyCap a = AnyCap (unsafeCoerce a)

fromAnyCap :: AnyCap m -> cap m
fromAnyCap (AnyCap a) = unsafeCoerce a

class Coercible1 t where
  coerce1 :: Coercible a b => Coercion (t a) (t b)

-- | The 'Cap' constraint captures two facts about capabilities that
-- we care about:
--
-- * a capability must be 'Typeable', so we can identify it at runtime
--   by its type
--
-- * a capability must be 'Coercible1', so we can coerce the base monad
--   safely
class (Typeable cap, Coercible1 cap) => Cap cap
instance (Typeable cap, Coercible1 cap) => Cap cap

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
  Capabilities (M.Map TypeRep (AnyCap (CapsT caps m)))

-- | The 'CapsT' transformer adds access to capabilities. This is a convenience
-- synonym for 'ReaderT' of 'Capabilities', and all 'ReaderT' functions
-- ('runReaderT', 'withReaderT') can be used with it.
type CapsT caps m = ReaderT (Capabilities caps m) m

-- | The 'CapImpl' newtype guarantees that the wrapped capability implementation
-- is sufficiently polymorphic so that required subtyping properties hold in
-- methods that take monadic actions as input (negative position).
--
-- This rules out using 'addCap', 'insertCap', and 'initCaps' inside capability
-- implementations in an unsafe manner.
newtype CapImpl cap icaps m =
  CapImpl { getCapImpl :: forall caps. HasCaps icaps caps => cap (CapsT caps m) }

{-

'unsafeCastCapabilities' can be used to reorder capabilities, remove non-unique
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
unsafeCastCapabilities :: Capabilities caps m -> Capabilities caps' m
unsafeCastCapabilities = unsafeCoerce

-- | 'CapabilitiesBuilder' is a helper type that serves as input to 'initCaps'.
-- It is similar to a list, where 'AddCap' is cons and 'NoCaps' is nil, but does
-- additional bookkeeping at type-level.
--
-- The @allCaps@ parameter is a list of capabilities that will be provided to
-- 'initCaps' eventually, when the building process is done. The @caps@
-- parameter is the part of capabilities that was constructed so far. The
-- builder is considered complete when @allCaps ~ caps@, only then it can be
-- passed to 'initCaps'.
data CapabilitiesBuilder (allCaps :: [CapK]) (caps :: [CapK]) (m :: MonadK) where
  AddCap ::
    (Cap cap, HasCaps icaps allCaps) =>
    CapImpl cap icaps m ->
    CapabilitiesBuilder allCaps caps m ->
    CapabilitiesBuilder allCaps (cap ': caps) m
  NoCaps :: CapabilitiesBuilder allCaps '[] m

-- | Build a map of capabilities from individual implementations:
--
-- @
-- capsXY :: Capabilities '[X, Y] IO
-- capsXY = initCaps $
--     AddCap xImpl $
--     AddCap yImpl $
--     NoCaps
-- @
initCaps :: forall caps m. CapabilitiesBuilder caps caps m -> Capabilities caps m
initCaps = Capabilities . M.fromList . go
  where
    go ::
      CapabilitiesBuilder caps caps' m ->
      [(TypeRep, AnyCap (CapsT caps m))]
    go NoCaps = []
    go (AddCap (CapImpl cap :: CapImpl cap _ _) otherCaps) =
      let
        key = typeRep (Proxy :: Proxy cap)
      in
        (key, toAnyCap cap) : go otherCaps

-- | Ensure that the @caps@ list has an element @cap@.
type family HasCap cap caps :: Constraint where
  HasCap cap (cap  ': _) = ()
  HasCap cap (cap' ': caps) = HasCap cap caps
  HasCap cap '[] =
    TypeError
      (Text "Capability " :<>:
       ShowType cap :<>:
       Text " must be available")

-- | Ensure that the @caps@ list subsumes @icaps@. It is equivalent
-- to a @HasCap icap caps@ constraint for each @icap@ in @icaps@.
type family HasCaps icaps caps :: Constraint where
  HasCaps '[] _ = ()
  HasCaps (icap ': icaps) caps = (HasCap icap caps, HasCaps icaps caps)

-- | Ensure that the @caps@ list does not have an element @cap@.
type family HasNoCap cap caps :: Constraint where
  HasNoCap cap (cap  ': _) =
    TypeError
      (Text "Capability " :<>:
       ShowType cap :<>:
       Text " is already present")
  HasNoCap cap (cap' ': caps) = HasNoCap cap caps
  HasNoCap cap '[] = ()

-- | Lookup a capability in a 'Capabilities' map. The 'HasCap' constraint
-- guarantees that the lookup does not fail.
getCap :: forall cap m caps. (Cap cap, HasCap cap caps) => Capabilities caps m -> cap (CapsT caps m)
getCap (Capabilities m) = fromAnyCap (m M.! typeRep (Proxy :: Proxy cap))

-- An internal function that adds capabilities.
unsafeInsertCap ::
  (Cap cap, HasCaps icaps caps') =>
  CapImpl cap icaps m ->
  Capabilities caps m ->
  Capabilities caps' m
unsafeInsertCap (CapImpl cap :: CapImpl cap _ _) (unsafeCastCapabilities -> Capabilities caps) =
  let
    key = typeRep (Proxy :: Proxy cap)
  in
    Capabilities (M.insert key (toAnyCap cap) caps)

-- | Extend the set of capabilities. In case the capability is already present,
-- it will be overriden (as with 'overrideCap'), but occur twice in the type.
insertCap ::
  (Cap cap, HasCaps icaps (cap ': caps)) =>
  CapImpl cap icaps m ->
  Capabilities caps m ->
  Capabilities (cap ': caps) m
insertCap = unsafeInsertCap

-- | Extend the set of capabilities. In case the capability is already present,
-- a type error occurs.
addCap ::
  (Cap cap, HasNoCap cap caps, HasCaps icaps (cap ': caps)) =>
  CapImpl cap icaps m ->
  Capabilities caps m ->
  Capabilities (cap ': caps) m
addCap = insertCap

-- | Override the implementation of an existing capability.
overrideCap ::
  (Cap cap, HasCap cap caps, HasCaps icaps caps) =>
  CapImpl cap icaps m ->
  Capabilities caps m ->
  Capabilities caps m
overrideCap = unsafeInsertCap

-- | Extract a capability from 'CapsT' and provide it to a continuation.
withCap :: (Monad m, Cap cap, HasCap cap caps) => (cap (CapsT caps m) -> CapsT caps m a) -> CapsT caps m a
withCap cont = do
  cap <- asks getCap
  cont cap
