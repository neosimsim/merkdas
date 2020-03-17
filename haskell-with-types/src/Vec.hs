{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax       #-}

module Vec
  ( module Nats
  , Vec(..)
  , lengthV
  , lengthV'
  , replicateV
  , replicateV'
  ) where

import           GHC.Types
import           Nats

data Vec a (n :: ℕ) where
  Empty :: Vec a 'Zero
  Cons :: a -> Vec a n -> Vec a ('Suc n)

-- | Singleton classes correspond to implicit arguments in Agda.
--
-- @
-- lengthV : {A : Set}{n : ℕ} → Vec A n → n
-- lengthV : {_} {n} _ = n
-- @
lengthV ::
     forall (a :: Type) (n :: ℕ). (Singℕ n)
  => Vec a n
  -> Sℕ n
lengthV _ = singℕ

-- | Singleton types correspong to explicit values as types in Agda.
--
-- @
-- lengthV' : {A : Set} → (n : ℕ) → Vec A n → n
-- lengthV' : {_} n _ = n
-- @
lengthV' :: forall (a :: Type) (n :: ℕ). Sℕ n -> Vec a n -> Sℕ n
lengthV' n _ = n

-- | Again: Singleton classes correspond to implicit arguments in Agda.
--
-- @
-- replicateV : {A : Set}{n : ℕ} → A → Vec A n
-- replicateV : {_} {0} _ = Empty
-- replicateV : {_} {S n} x = Cons x (replicateV x)
-- @
replicateV ::
     forall (a :: Type) (n :: ℕ). Singℕ n
  => a
  -> Vec a n
replicateV a =
  case (singℕ :: Sℕ n) of
    SZero  -> Empty
    SSuc _ -> Cons a (replicateV a)

replicateV' :: a -> Sℕ n -> Vec a n
replicateV' _ SZero    = Empty
replicateV' x (SSuc n) = Cons x (replicateV' x n)
