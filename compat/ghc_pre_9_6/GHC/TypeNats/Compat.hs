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

import GHC.TypeNats (
    Natural
  , Nat
  , KnownNat(), natVal, natVal'
  , SomeNat(..)
  , someNatVal
  , sameNat
  , type (+), type (*), type (^), type (-)
  , CmpNat
  , cmpNat
  , Div, Mod, Log2
  )

import Data.Type.Equality
import Data.Type.Ord
import Data.GADT.Show
import Data.GADT.Compare

#if !MIN_VERSION_base(4,18,0)

import GHC.Exts (TYPE)

-- | An @'SNat' n@ is a witness for @'KnownNat' n@.
--
-- This means that if you pattern match on the 'SNat' constructor, in that
-- branch you will have a @'KnownNat' n@ constraint.
--
-- @
-- myFunc :: SNat n -> Bool
-- myFunc SNat = ...  -- in this body, we have `KnownNat n`
-- @
--
-- This is essentially a singleton for 'Nat', and stands in for the
-- /singletons/ 'SNat' and 'Data.Singleton.Sing' types.
data SNat n = KnownNat n => SNat

deriving instance Eq (SNat n)
deriving instance Ord (SNat n)

instance Show (SNat n) where
    showsPrec d x@SNat = showParen (d > 10) $
      showString "SNat @" . showsPrec 11 (fromSNat x)

instance TestEquality SNat where
    testEquality n@SNat m@SNat = sameNat n m

natSing :: KnownNat n => SNat n
natSing = SNat

sNatOf :: KnownNat n => proxy n -> SNat n
sNatOf _ = SNat

fromSNat :: SNat n -> Natural
fromSNat n@SNat = natVal n

withSomeSNat :: forall rep (r :: TYPE rep).
                Natural -> (forall n. SNat n -> r) -> r
withSomeSNat n body = case someNatVal n of
  SomeNat proxy -> body (sNatOf proxy)

withKnownNat :: SNat n -> (KnownNat n => r) -> r
withKnownNat SNat r = r

instance GShow SNat where
    gshowsPrec = showsPrec

instance GEq SNat where
    geq = testEquality

instance GCompare SNat where
    gcompare x y = case sCmpNat x y of
      LTI -> GLT
      EQI -> GEQ
      GTI -> GGT

#else

import GHC.TypeNats(SNat, pattern SNat, natSing, fromSNat, withSomeSNat, withKnownNat)

#endif

sCmpNat :: SNat n -> SNat m -> OrderingI n m
sCmpNat n m = withKnownNat n (withKnownNat m (cmpNat n m))
