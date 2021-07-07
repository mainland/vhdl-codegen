{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :  Language.VHDL.Codegen.VExp
-- Copyright   :  (c) 2020-2021 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Language.VHDL.Codegen.VExp where

import Data.Bits
import Data.Fixed.Q ( Q, UQ )
import Data.Loc ( (<-->), Loc(NoLoc), Located(locOf) )
import Data.Proxy ( Proxy(..) )
import GHC.TypeLits ( KnownNat, natVal )
import Language.VHDL.Quote
import qualified Language.VHDL.Syntax as V
import Text.PrettyPrint.Mainland.Class ( Pretty(pprPrec) )

import Language.VHDL.Codegen.Instances ()
import Language.VHDL.Codegen.Lift

data VExp a where
    -- | Constant
    VConst :: ToExp a => a -> VExp a

    -- | Conditional expression
    VIfte :: VExp Bool -> VExp a -> VExp a -> VExp a

    -- | VHDL expression
    VExp :: V.Exp -> VExp a

deriving instance Eq a => Eq (VExp a)

deriving instance Show a => Show (VExp a)

instance Pretty a => Pretty (VExp a) where
    pprPrec p (VConst x)    = pprPrec p x
    pprPrec p (VIfte c t e) = pprPrec p [vexp|ifte($c, $t, $e)|]
    pprPrec p (VExp ve)     = pprPrec p ve

instance Located (VExp l) where
    locOf VConst{}      = NoLoc
    locOf (VIfte c t e) = locOf c <--> locOf t <--> locOf e
    locOf (VExp ve)     = locOf ve

instance ToExp (VExp a) where
    toExp (VConst x)     l = toExp x l
    toExp (VIfte c t e)  _ = [vexp|ifte($c, $t, $e)|]
    toExp (VExp ve)      _ = ve

ifte :: Eq a => VExp Bool -> VExp a -> VExp a -> VExp a
ifte (VConst True)  t _ = t
ifte (VConst False) _ e = e
ifte c t e
    | t == e           = t
    | otherwise        = VIfte c t e

instance Eq a => IfThenElse (VExp Bool) (VExp a) where
    ifThenElse (VConst True)  t _ = t
    ifThenElse (VConst False) _ e = e
    ifThenElse c              t e = ifte c t e

instance (Eq a, Eq b,
          IfThenElse (VExp Bool) (VExp a),
          IfThenElse (VExp Bool) (VExp b)) => IfThenElse (VExp Bool) (VExp a, VExp b) where
    ifThenElse (VConst True)  t _ = t
    ifThenElse (VConst False) _ e = e

    ifThenElse c (t1,t2) (e1,e2) =
        (ifte c t1 e1, ifte c t2 e2)

instance (Eq a, Eq b, Eq c,
          IfThenElse (VExp Bool) (VExp a),
          IfThenElse (VExp Bool) (VExp b),
          IfThenElse (VExp Bool) (VExp c)) => IfThenElse (VExp Bool) (VExp a, VExp b, VExp c) where
    ifThenElse (VConst True)  t _ = t
    ifThenElse (VConst False) _ e = e

    ifThenElse c (t1,t2,t3) (e1,e2,e3) =
        (ifte c t1 e1, ifte c t2 e2, ifte c t3 e3)

instance (Eq s, Eq t, Eq u, Eq v,
          IfThenElse (VExp Bool) (VExp s),
          IfThenElse (VExp Bool) (VExp t),
          IfThenElse (VExp Bool) (VExp u),
          IfThenElse (VExp Bool) (VExp v))
       => IfThenElse (VExp Bool) (VExp s, VExp t, VExp u, VExp v) where
    ifThenElse (VConst True)  t _ = t
    ifThenElse (VConst False) _ e = e

    ifThenElse c (t1,t2,t3,t4) (e1,e2,e3,e4) =
        (ifte c t1 e1, ifte c t2 e2, ifte c t3 e3, ifte c t4 e4)

instance (Eq s, Eq t, Eq u, Eq v, Eq w,
          IfThenElse (VExp Bool) (VExp s),
          IfThenElse (VExp Bool) (VExp t),
          IfThenElse (VExp Bool) (VExp u),
          IfThenElse (VExp Bool) (VExp v),
          IfThenElse (VExp Bool) (VExp w))
       => IfThenElse (VExp Bool) (VExp s, VExp t, VExp u, VExp v, VExp w) where
    ifThenElse (VConst True)  t _ = t
    ifThenElse (VConst False) _ e = e

    ifThenElse c (t1,t2,t3,t4,t5) (e1,e2,e3,e4,e5) =
        (ifte c t1 e1, ifte c t2 e2, ifte c t3 e3, ifte c t4 e4, ifte c t5 e5)

instance LiftEq VExp where
    VConst x  .==. VConst y = VConst (x == y)

    x .==. y | x == y    = VConst True
             | otherwise = VExp [vexp|$x ?= $y|]

    VConst x  ./=. VConst y = VConst (x /= y)

    x ./=. y | x == y    = VConst False
             | otherwise = VExp [vexp|$x ?/= $y|]

instance LiftBool VExp where
    VConst True .&&. x           = x
    x           .&&. VConst True = x
    x           .&&. y           = VExp [vexp|$x and $y|]

    VConst True .||. _           = VConst True
    _           .||. VConst True = VConst True
    x           .||. y           = VExp [vexp|$x or $y|]

instance LiftOrd VExp where
    VConst x .<. VConst y = VConst (x < y)
    x        .<. y        = VExp [vexp|$x ?< $y|]

    VConst x .<=. VConst y = VConst (x <= y)
    x        .<=. y        = VExp [vexp|$x ?<= $y|]

    VConst x .>=. VConst y = VConst (x >= y)
    x        .>=. y        = VExp [vexp|$x ?>= $y|]

    VConst x .>. VConst y = VConst (x > y)
    x        .>. y        = VExp [vexp|$x ?> $y|]

-- | Num operations on deep values of a type. This class allows us to separate
-- out operations on deep values from operations on mixed shallow/staged values.
class DeepNum a where
    add_ :: VExp a -> VExp a -> VExp a
    x `add_` y  = VExp [vexp|$x + $y|]

    sub_ :: VExp a -> VExp a -> VExp a
    x `sub_` y = VExp [vexp|$x - $y|]

    mul_ :: VExp a -> VExp a -> VExp a
    x `mul_` y = VExp [vexp|$x * $y|]

    negate_ :: VExp a -> VExp a
    negate_ x = VExp [vexp|-$x|]

    abs_ :: VExp a -> VExp a
    abs_ x = VExp [vexp|abs($x)|]

    signum_ :: VExp a -> VExp a
    signum_ _ = error "VExp: signum not implemented"

instance DeepNum Int where

instance (KnownNat m, KnownNat f) => DeepNum (UQ m f) where
    x `add_` y = resize [vexp|$x + $y|]
    x `sub_` y = resize [vexp|$x - $y|]
    x `mul_` y = resize [vexp|$x * $y|]

instance (KnownNat m, KnownNat f) => DeepNum (Q m f) where
    x `add_` y = resize [vexp|$x + $y|]
    x `sub_` y = resize [vexp|$x - $y|]
    x `mul_` y = resize [vexp|$x * $y|]

instance (Eq a, Num a, DeepNum a, ToExp a) => Num (VExp a) where
    VConst 0 + y        = y
    x        + VConst 0 = x
    VConst x + VConst y = VConst (x + y)
    x        + y        = x `add_` y

    VConst 0 - y        = -y
    x        - VConst 0 = x
    VConst x - VConst y = VConst (x - y)
    x        - y        = x `sub_` y

    VConst 0 * _        = 0
    _        * VConst 0 = 0
    VConst 1 * y        = y
    x        * VConst 1 = x
    VConst x * VConst y = VConst (x * y)
    x        * y        = x `mul_` y

    negate (VConst x) = VConst (-x)
    negate x          = negate_ x

    abs (VConst x) = VConst (abs x)
    abs x          = abs_ x

    fromInteger i = VConst (fromInteger i)

    signum (VConst x) = VConst (signum x)
    signum x          = signum_ x

-- | Generate a std_logic_vector constant of length n with bit i set
slvBit :: Int
       -> VExp Int
       -> V.Exp
slvBit n (VConst i) = [vexp|std_logic_vector'($bs)|]
  where
    bs :: String
    bs = replicate (n-i-1) '0' ++ "1" ++ replicate i '0'

slvBit n i = [vexp|std_logic_vector'($bs sll $i)|]
  where
    bs :: String
    bs = replicate (n-1) '0' ++ "1"

infixl 8 `sla`, `sra`

-- | Arithmetic shift left of VHDL expression
sla :: (Bits a, LiftBits VExp a) => VExp a -> VExp Int -> VExp a
sla = shiftL'

-- | Arithmetic shift right of VHDL expression
sra :: (Bits a, LiftBits VExp a) => VExp a -> VExp Int -> VExp a
sra = shiftR'

-- | Bits operations on deep values of a type. This class allows us to separate
-- out operations on deep values from operations on mixed shallow/staged values.
class FiniteBits a => DeepBits a where
    -- | Convert a std_logic_vector (of the appropriate size) to a value of
    -- type `VExp a`.
    fromSLV :: V.Exp -> VExp a

    -- | Convert a value of type `VExp a` to a std_logic_vector.
    toSLV :: VExp a -> V.Name

    -- | Logical and of deep values
    and_ :: VExp a -> VExp a -> VExp a
    x `and_` y = VExp [vexp|$x and $y|]

    -- | Logical or of deep values
    or_ :: VExp a -> VExp a -> VExp a
    x `or_` y = VExp [vexp|$x or $y|]

    -- | Logical xor of deep values
    xor_ :: VExp a -> VExp a -> VExp a
    x `xor_` y = VExp [vexp|$x xor $y|]

    -- | Deep value with bit @i@ set.
    bit_ :: VExp Int -> VExp a
    bit_ i = fromSLV (slvBit n i)
      where
        n :: Int
        n = finiteBitSize (undefined :: a)

    -- | Test bit @i@ of deep value @x@.
    testBit_ :: VExp a -> VExp Int -> VExp Bool
    testBit_ x i = VExp [vexp|funname $name:(toSLV x)($i)|]

    -- | Set bit @i@ of deep value @x@.
    setBit_ :: VExp a -> VExp Int -> VExp a
    setBit_ x i = fromSLV [vexp|$(toSLV x) or $(slvBit n i)|]
      where
        n :: Int
        n = finiteBitSize (undefined :: a)

    -- | Complement deep value.
    complement_ :: VExp a -> VExp a
    complement_ x = VExp [vexp|not $x|]

    -- | Arithmetic shift left deep value @x@ by @i@ bits.
    shiftL_ :: VExp a -> VExp Int -> VExp a
    x `shiftL_` i = VExp [vexp|$x sla $i|]

    -- | Arithmetic shift right deep value @x@ by @i@ bits.
    shiftR_ :: VExp a -> VExp Int -> VExp a
    x `shiftR_` i = VExp [vexp|$x sra $i|]

instance (KnownNat m, KnownNat f) => DeepBits (UQ m f) where
    fromSLV e = VExp [vexp|to_ufixed($e, $int:(m-1), $int:(-f))|]
      where
        m, f :: Integer
        m = natVal (Proxy :: Proxy m)
        f = natVal (Proxy :: Proxy f)

    toSLV e = [vname|funname to_slv($e)|]

instance (DeepBits a, ToExp a) => LiftBits VExp a where
    finiteBitSize' _ = finiteBitSize (undefined ::a)

    VConst x ..&.. VConst y = VConst (x .&. y)
    e1       ..&.. e2       = e1 `and_` e2

    VConst x ..|.. VConst y = VConst (x .|. y)
    e1       ..|.. e2       = e1 `or_` e2

    VConst x `xor'` VConst y = VConst (x `xor` y)
    e1       `xor'` e2       = e1 `xor_` e2

    bit' (VConst i) = VConst (bit i)
    bit' i          = bit_ i

    testBit' x i = testBit_ x i

    setBit' x i = setBit_ x i

    complement' x = complement_ x

    shiftL' x 0 = x
    shiftL' x i = x `shiftL_` i

    shiftR' x 0 = x
    shiftR' x i = x `shiftR_` i

-- | A fixed-point VHDL type.
class Fixed a where
    resize :: ToExp e => e -> a

instance (KnownNat m, KnownNat f) => Fixed (VExp (UQ m f)) where
    resize e = VExp [vexp|resize($e, $int:(m-1), $int:(-f), round_style => fixed_truncate, overflow_style => fixed_wrap)|]
      where
        m, f :: Integer
        m = natVal (Proxy :: Proxy m)
        f = natVal (Proxy :: Proxy f)

instance (KnownNat m, KnownNat f) => Fixed (VExp (Q m f)) where
    resize e = VExp [vexp|resize($e, $int:m, $int:(-f), round_style => fixed_truncate, overflow_style => fixed_wrap)|]
      where
        m, f :: Integer
        m = natVal (Proxy :: Proxy m)
        f = natVal (Proxy :: Proxy f)

-- | A Lift instance can have any of its values turned into an embedded VExp.
class Lift t where
    type Lifted t

    lift :: t -> Lifted t

instance Lift Bool where
    type Lifted Bool = VExp Bool

    lift = VConst

instance (KnownNat m, KnownNat f) => Lift (Q m f) where
    type Lifted (Q m f) = VExp (Q m f)

    lift = VConst

instance (KnownNat m, KnownNat f) => Lift (UQ m f) where
    type Lifted (UQ m f) = VExp (UQ m f)

    lift = VConst

instance (Lift a, Lift b) => Lift (a, b)  where
    type Lifted (a, b) = (Lifted a, Lifted b)

    lift (x, y) = (lift x, lift y)

instance (Lift a, Lift b, Lift c) => Lift (a, b, c)  where
    type Lifted (a, b, c) = (Lifted a, Lifted b, Lifted c)

    lift (x, y, z) = (lift x, lift y, lift z)
