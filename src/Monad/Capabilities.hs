{-# LANGUAGE TypeInType, GADTs, ScopedTypeVariables, FlexibleInstances,
             TypeOperators, ConstraintKinds, TypeFamilies,
             UndecidableInstances, ViewPatterns, RankNTypes #-}

module Monad.Capabilities
  ( Capabilities(),
    Coercible1(..),
    Coercion(..),
    CapsT,
    CapabilitiesBuilder(..),
    initCaps,
    getCap,
    type HasCap,
    insertCap,
    overrideCap,
    withCap
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

class (Typeable cap, Coercible1 cap) => Cap cap
instance (Typeable cap, Coercible1 cap) => Cap cap

type CapsT caps m = ReaderT (Capabilities caps m) m

newtype Capabilities (caps :: [CapK]) (m :: MonadK) =
  Capabilities (M.Map TypeRep (AnyCap (CapsT caps m)))

unsafeCastCapabilities :: Capabilities caps m -> Capabilities caps' m
unsafeCastCapabilities = unsafeCoerce

data CapabilitiesBuilder (allCaps :: [CapK]) (caps :: [CapK]) (m :: MonadK) where
  AddCap ::
    Cap cap =>
    (cap (CapsT allCaps m)) ->
    CapabilitiesBuilder allCaps caps m ->
    CapabilitiesBuilder allCaps (cap ': caps) m
  NoCaps :: CapabilitiesBuilder allCaps '[] m

initCaps :: forall allCaps m. CapabilitiesBuilder allCaps allCaps m -> Capabilities allCaps m
initCaps = Capabilities . M.fromList . go
  where
    go ::
      forall caps.
      CapabilitiesBuilder allCaps caps m ->
      [(TypeRep, AnyCap (CapsT allCaps m))]
    go NoCaps = []
    go (AddCap (cap :: cap (CapsT allCaps m)) otherCaps) =
      let
        key = typeRep (Proxy :: Proxy cap)
      in
        (key, toAnyCap cap) : go otherCaps

type family HasCap cap caps :: Constraint where
  HasCap cap (cap  ': _) = ()
  HasCap cap (cap' ': caps) = HasCap cap caps
  HasCap cap '[] =
    TypeError
      (Text "Capability " :<>:
       ShowType cap :<>:
       Text " must be available")

getCap :: forall cap m caps. (Cap cap, HasCap cap caps) => Capabilities caps m -> cap (CapsT caps m)
getCap (Capabilities m) = fromAnyCap (m M.! typeRep (Proxy :: Proxy cap))

unsafeInsertCap ::
  forall cap m caps caps'.
  Cap cap =>
  cap (CapsT caps' m) ->
  Capabilities caps m ->
  Capabilities caps' m
unsafeInsertCap cap (unsafeCastCapabilities -> Capabilities caps) =
  let
    key = typeRep (Proxy :: Proxy cap)
  in
    Capabilities (M.insert key (toAnyCap cap) caps)

insertCap ::
  forall cap m caps.
  Cap cap =>
  cap (CapsT (cap ': caps) m) ->
  Capabilities caps m ->
  Capabilities (cap ': caps) m
insertCap = unsafeInsertCap

overrideCap ::
  forall cap m caps.
  (Cap cap, HasCap cap caps) =>
  cap (CapsT caps m) ->
  Capabilities caps m ->
  Capabilities caps m
overrideCap = unsafeInsertCap

withCap :: (Monad m, Cap cap, HasCap cap caps) => (cap (CapsT caps m) -> CapsT caps m a) -> CapsT caps m a
withCap cont = do
  cap <- asks getCap
  cont cap
