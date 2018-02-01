{-# LANGUAGE TypeInType, GADTs, ScopedTypeVariables, FlexibleInstances,
             TypeOperators, ConstraintKinds, TypeFamilies, PartialTypeSignatures,
             UndecidableInstances, ViewPatterns, RankNTypes, TypeApplications,
             MultiParamTypeClasses, UndecidableSuperClasses, TemplateHaskell #-}

{-# OPTIONS -fno-warn-unused-top-binds #-}

-- | An inefficient implementation of the same interface that does not
-- use 'unsafeCoerce'.

module Monad.Capabilities.RefImpl
  (
    -- * Capabilities
    Capabilities(),
    CapsT(..),
    initCaps,
    CapabilitiesBuilder(..),
    CapImpl(..),
  --   Cap,
  --   getCap
  --   overrideCap,
  --   addCap,
  --   insertCap,
  --   withCap,
  --   checkCap,
  --   adjustCap,

  --   -- * Default capabilities
  --   Context(..),
  --   HasContext,
  --   newContext,
  --   askContext,
  --   localContext,

  --   -- * Type-level checks
  --   type HasCap,
  --   type HasCaps,
  --   type HasNoCap,
  --   HasCapDecision(..),

  --   -- * Utils
  --   Coercible1(..),
  --   Coercion(..),
  --   makeCap

  ) where

import Prelude ()
import Data.Void (Void)
import Data.Kind (Type)

type MonadK = Type -> Type

type CapK = MonadK -> Type

type Not a = a -> Void

data Elem x xs where
  ElemHere :: Elem x (x ': xs)
  ElemThere :: Elem x xs -> Elem x (y ': xs)

data Elems ys xs where
  ElemsNil :: Elems '[] xs
  ElemsCons :: Elem y xs -> Elems ys xs -> Elems (y ': ys) xs

newtype CapImpl (cap :: CapK) (icaps :: [CapK]) (m :: MonadK) =
  CapImpl
    { getCapImpl ::
        forall caps.
        Elems icaps caps ->
        cap (CapsT caps m)
    }

newtype CapsT caps m a = CapsT { runCapsT :: Capabilities caps m -> a }

newtype Capabilities caps m = Capabilities (CapabilitiesBuilder caps caps m)

data CapabilitiesBuilder :: [CapK] -> [CapK] -> MonadK -> Type where
   NoCaps :: CapabilitiesBuilder allCaps '[] m
   AddCap ::
     Elems icaps allCaps ->
     Not (Elem cap caps) ->
     CapImpl cap icaps m ->
     CapabilitiesBuilder allCaps caps m ->
     CapabilitiesBuilder allCaps (cap ': caps) m

initCaps :: CapabilitiesBuilder caps caps m -> Capabilities caps m
initCaps = Capabilities

liftElems :: Elems allCaps caps -> Elems allCaps (cap ': caps)
liftElems ElemsNil = ElemsNil
liftElems (ElemsCons elem elems) = ElemsCons (ElemThere elem) (liftElems elems)

elemsSelf :: CapabilitiesBuilder allCaps caps m -> Elems caps caps
elemsSelf NoCaps = ElemsNil
elemsSelf (AddCap _ _ _ caps') =
  ElemsCons ElemHere (liftElems (elemsSelf caps'))

getCap ::
  forall cap caps m.
  Elem cap caps ->
  Capabilities caps m ->
  cap (CapsT caps m)
getCap el (Capabilities caps) = getCapImpl (getCap' el caps) (elemsSelf caps)
  where
    getCap' ::
      forall caps'.
      Elem cap caps' ->
      CapabilitiesBuilder caps caps' m ->
      CapImpl cap caps m
    getCap' = getCap' -- TODO
