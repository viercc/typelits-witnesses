{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module GHC.TypeNats.Compat(
    -- * Nat Kind
    Natural -- declared in GHC.Num.Natural in package ghc-bignum
  , Nat

    -- * Linking type and value level
  , KnownNat(), natSing, natVal, natVal'
  , SomeNat(..)
  , someNatVal
  , sameNat

    -- ** Singleton values
  , SNat
  , pattern SNat
  , fromSNat
  , withSomeSNat
  , withKnownNat

    -- * Functions on type literals
  , type (<=), type (<=?), type (<), type (<?)
  , type (+), type (*), type (^), type (-)
  , CmpNat
  , cmpNat
  , sCmpNat
  , Div, Mod, Log2
) where

import GHC.TypeNats
import Data.Type.Ord
import Data.GADT.Show()
import Data.GADT.Compare()

sCmpNat :: SNat n -> SNat m -> OrderingI n m
sCmpNat n m = withKnownNat n (withKnownNat m (cmpNat n m))
