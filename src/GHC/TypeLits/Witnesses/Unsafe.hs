module GHC.TypeLits.Witnesses.Unsafe (
    unsafeLiftNatOp1, unsafeLiftNatOp2
  ) where

import Numeric.Natural
import GHC.TypeNats.Compat

import Unsafe.Coerce (unsafeCoerce)

-- | Lift a unary operation to act on an @'SNat' n@ that returns an @'SNat'
-- m@.  The function given must properly describe the relationship between
-- @n@ and @m@.
--
-- For example:
--
-- @
-- double :: SNat n -> SNat (n * 2)
-- double = unsafeLiftNatOp1 (*2)
-- @
--
-- The correctness of the relationship is not checked, so be aware that
-- this can cause programs to break.
unsafeLiftNatOp1
    :: (Natural -> Natural)
    -> SNat n
    -> SNat m
unsafeLiftNatOp1 f x = withSomeSNat (f (fromSNat x)) unsafeCoerce

-- | Lift a binary operation to act on an @'SNat' n@ and @'SNat' m@ that
-- returns an @'SNat' o@.  The function given must properly describe the
-- relationship between @n@, @m@, and @o@.
--
-- For example:
--
-- @
-- multiply :: SNat n -> SNat m -> SNat (n * m)
-- multiply = unsafeLiftNatOp2 (*)
-- @
--
-- The correctness of the relationship is not checked, so be aware that
-- this can cause programs to break.
unsafeLiftNatOp2
    :: (Natural -> Natural -> Natural)
    -> SNat n
    -> SNat m
    -> SNat o
unsafeLiftNatOp2 f x y = withSomeSNat (f (fromSNat x) (fromSNat y)) unsafeCoerce
