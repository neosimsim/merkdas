{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax        #-}

module Nats
  ( ℕ(..)
  , Sℕ(..)
  , Singℕ(..)
  ) where

data ℕ where
  Zero :: ℕ
  Suc :: ℕ -> ℕ

data Sℕ (n :: ℕ) where
  SZero :: Sℕ 'Zero
  SSuc :: (SingRep n) => Sℕ n -> Sℕ ('Suc n)

instance Show (Sℕ 'Zero) where
  show _ = "0"

instance forall (n :: ℕ). (Singℕ n, Show (Sℕ n)) => Show (Sℕ ('Suc n)) where
  show _ = "Suc " ++ show @(Sℕ n) singℕ

class Singℕ (n :: ℕ) where
  singℕ :: Sℕ n

instance Singℕ 'Zero where
  singℕ = SZero

instance SingRep n => Singℕ ('Suc n) where
  singℕ = SSuc singℕ

class SingE (n :: ℕ) where
  fromSing :: Sℕ n -> ℕ

instance SingE (n :: ℕ) where
  fromSing SZero    = Zero
  fromSing (SSuc n) = Suc (fromSing n)

class (SingE a, Singℕ a) =>
      SingRep a


instance (SingE a, Singℕ a) => SingRep a
