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

import Data.Bits ( FiniteBits(finiteBitSize) )
import Data.Fixed.Q ( Q, UQ )
import Data.Loc ( (<-->), noLoc, Loc(NoLoc), Located(locOf) )
import Data.Proxy ( Proxy(..) )
import GHC.TypeLits ( KnownNat, type (+), natVal )
import Language.VHDL.Quote
import qualified Language.VHDL.Syntax as V
import Text.PrettyPrint.Mainland.Class ( Pretty(pprPrec) )

import Language.VHDL.Codegen.Instances ()
import Language.VHDL.Codegen.Lift

data VExp a where
    -- | Boolean constant
    VBool :: Bool -> VExp Bool

    -- | Integer constant
    VInt :: Num a => Integer -> VExp a

    -- | Q constant
    VQ :: (KnownNat m, KnownNat f, KnownNat (m + f)) => Q m f -> VExp (Q m f)

    -- | UQ constant
    VUQ :: (KnownNat m, KnownNat f, KnownNat (m + f)) => UQ m f -> VExp (UQ m f)

    -- | Conditional expression
    VIfte :: VExp Bool -> VExp a -> VExp a -> VExp a

    -- | VHDL expression
    VExp :: V.Exp -> VExp a

ifte :: VExp Bool -> VExp a -> VExp a -> VExp a
ifte (VBool True)  t _ = t
ifte (VBool False) _ e = e
ifte c t e
    | t == e           = t
    | otherwise        = VIfte c t e

deriving instance Eq (VExp a)

deriving instance Show (VExp a)

instance Pretty (VExp a) where
    pprPrec p (VBool x)     = pprPrec p x
    pprPrec p (VInt x)      = pprPrec p x
    pprPrec p (VQ x)        = pprPrec p x
    pprPrec p (VUQ x)       = pprPrec p x
    pprPrec p (VIfte c t e) = pprPrec p [vexp|ifte($c, $t, $e)|]
    pprPrec p (VExp ve)     = pprPrec p ve

instance Located (VExp l) where
    locOf VBool{}       = NoLoc
    locOf VInt{}        = NoLoc
    locOf VQ{}          = NoLoc
    locOf VUQ{}         = NoLoc
    locOf (VIfte c t e) = locOf c <--> locOf t <--> locOf e
    locOf (VExp ve)     = locOf ve

instance ToExp (VExp a) where
    toExp (VBool True)  l = V.LitE (V.BitStringLit "'1'" l) l
    toExp (VBool False) l = V.LitE (V.BitStringLit "'0'" l) l
    toExp (VInt n)      l = V.LitE (V.IntLit (show n) n l) l
    toExp (VQ q)        l = toExp q l
    toExp (VUQ uq)      l = toExp uq l
    toExp (VIfte c t e) _ = [vexp|ifte($c, $t, $e)|]
    toExp (VExp ve)     _ = ve

instance IfThenElse (VExp Bool) (VExp a) where
    ifThenElse (VBool True)  t _ = t
    ifThenElse (VBool False) _ e = e
    ifThenElse c             t e = ifte c t e

instance (IfThenElse (VExp Bool) (VExp a),
          IfThenElse (VExp Bool) (VExp b)) => IfThenElse (VExp Bool) (VExp a, VExp b) where
    ifThenElse (VBool True)  t _ = t
    ifThenElse (VBool False) _ e = e

    ifThenElse c (t1,t2) (e1,e2) =
        (ifte c t1 e1, ifte c t2 e2)

instance (IfThenElse (VExp Bool) (VExp a),
          IfThenElse (VExp Bool) (VExp b),
          IfThenElse (VExp Bool) (VExp c)) => IfThenElse (VExp Bool) (VExp a, VExp b, VExp c) where
    ifThenElse (VBool True)  t _ = t
    ifThenElse (VBool False) _ e = e

    ifThenElse c (t1,t2,t3) (e1,e2,e3) =
        (ifte c t1 e1, ifte c t2 e2, ifte c t3 e3)

instance (IfThenElse (VExp Bool) (VExp s),
          IfThenElse (VExp Bool) (VExp t),
          IfThenElse (VExp Bool) (VExp u),
          IfThenElse (VExp Bool) (VExp v))
       => IfThenElse (VExp Bool) (VExp s, VExp t, VExp u, VExp v) where
    ifThenElse (VBool True)  t _ = t
    ifThenElse (VBool False) _ e = e

    ifThenElse c (t1,t2,t3,t4) (e1,e2,e3,e4) =
        (ifte c t1 e1, ifte c t2 e2, ifte c t3 e3, ifte c t4 e4)

instance (IfThenElse (VExp Bool) (VExp s),
          IfThenElse (VExp Bool) (VExp t),
          IfThenElse (VExp Bool) (VExp u),
          IfThenElse (VExp Bool) (VExp v),
          IfThenElse (VExp Bool) (VExp w))
       => IfThenElse (VExp Bool) (VExp s, VExp t, VExp u, VExp v, VExp w) where
    ifThenElse (VBool True)  t _ = t
    ifThenElse (VBool False) _ e = e

    ifThenElse c (t1,t2,t3,t4,t5) (e1,e2,e3,e4,e5) =
        (ifte c t1 e1, ifte c t2 e2, ifte c t3 e3, ifte c t4 e4, ifte c t5 e5)

instance LiftEq VExp where
    VBool x  .==. VBool y = VBool (x == y)
    VInt x   .==. VInt y  = VBool (x == y)
    VQ x     .==. VQ y    = VBool (x == y)
    VUQ x    .==. VUQ y   = VBool (x == y)

    x .==. y | x == y    = VBool True
             | otherwise = VExp [vexp|$x ?= $y|]

    VBool x  ./=. VBool y = VBool (x /= y)
    VInt x   ./=. VInt y  = VBool (x /= y)
    VQ x     ./=. VQ y    = VBool (x /= y)
    VUQ x    ./=. VUQ y   = VBool (x /= y)

    x ./=. y | x == y    = VBool False
             | otherwise = VExp [vexp|$x ?/= $y|]

instance LiftBool VExp where
    VBool True .&&. x          = x
    x          .&&. VBool True = x
    x          .&&. y          = VExp [vexp|$x and $y|]

    VBool True .||. _          = VBool True
    _          .||. VBool True = VBool True
    x          .||. y          = VExp [vexp|$x or $y|]

instance LiftOrd VExp where
    VBool x .<. VBool y = VBool (x < y)
    VInt x  .<. VInt y  = VBool (x < y)
    VQ x    .<. VQ y    = VBool (x < y)
    VUQ x   .<. VUQ y   = VBool (x < y)
    x       .<. y       = VExp [vexp|$x ?< $y|]

    VBool x .<=. VBool y = VBool (x <= y)
    VInt x  .<=. VInt y  = VBool (x <= y)
    VQ x    .<=. VQ y    = VBool (x <= y)
    VUQ x   .<=. VUQ y   = VBool (x <= y)
    x       .<=. y       = VExp [vexp|$x ?<= $y|]

    VBool x .>=. VBool y = VBool (x >= y)
    VInt x  .>=. VInt y  = VBool (x >= y)
    VQ x    .>=. VQ y    = VBool (x >= y)
    VUQ x   .>=. VUQ y   = VBool (x >= y)
    x       .>=. y       = VExp [vexp|$x ?>= $y|]

    VBool x .>. VBool y = VBool (x > y)
    VInt x  .>. VInt y  = VBool (x > y)
    VQ x    .>. VQ y    = VBool (x > y)
    VUQ x   .>. VUQ y   = VBool (x > y)
    x       .>. y       = VExp [vexp|$x ?> $y|]

instance Num a => Num (VExp a) where
    VQ 0 + y    = y
    x    + VQ 0 = x

    VQ x + VQ y = VQ (x + y)

    VUQ 0 + y     = y
    x     + VUQ 0 = x

    VUQ x + VUQ y = VUQ (x + y)

    x + y = VExp [vexp|$x + $y|]

    x - y = VExp [vexp|$x - $y|]

    VInt 1 * x = x

    x * y = VExp [vexp|$x * $y|]

    negate (VInt x) = VInt (-x)

    negate x = VExp [vexp|-$x|]

    abs x = VExp [vexp|abs($x)|]

    fromInteger i = VInt i

    signum _ = error "VExp: signum not implemented"

instance {-# OVERLAPS #-} (KnownNat m, KnownNat f, KnownNat (m + f)) => Num (VExp (UQ m f)) where
    VUQ 0 + y     = y
    x     + VUQ 0 = x

    VUQ x + VUQ y = VUQ (x + y)

    x + y = resize [vexp|$x + $y|]

    x - y = resize [vexp|$x - $y|]

    VInt 1 * x = x

    x * y = resize [vexp|$x * $y|]

    negate (VInt x) = VInt (-x)

    negate x = VExp [vexp|-$x|]

    abs x = VExp [vexp|abs($x)|]

    fromInteger i = VUQ (fromInteger i)

    signum _ = error "VExp: signum not implemented"

bits :: String -> V.Exp
bits s = V.LitE (V.BitStringLit ('"' : s ++ "\"") noLoc) noLoc

-- | Generate a std_logic_vector constant of length n with bit i set
slvBit :: Integer
       -> VExp Int
       -> V.Exp
slvBit n (VInt i) = [vexp|std_logic_vector'($bs)|]
  where
    bs :: String
    bs = replicate (fromIntegral (n-i-1)) '0' ++ "1" ++ replicate (fromIntegral i) '0'

slvBit n i = [vexp|std_logic_vector'($bs sll $i)|]
  where
    bs :: String
    bs = replicate (fromIntegral (n-1)) '0' ++ "1"

instance (KnownNat m, KnownNat f) => LiftBits VExp (UQ m f) where
    finiteBitSize' _ = finiteBitSize (undefined :: UQ m f)

    e1 ..&.. e2 = VExp [vexp|$e1 and $e2|]

    e1 ..|.. e2 = VExp [vexp|$e1 or $e2|]

    bit' i = VExp [vexp|to_ufixed($(slvBit n i), $int:(m-1), $int:(-f))|]
      where
        m, f, n :: Integer
        m = natVal (Proxy :: Proxy m)
        f = natVal (Proxy :: Proxy f)
        n = m+f

    testBit' x i = VExp [vexp|arrname to_slv($x)($i)|]

    setBit' x i = VExp [vexp|to_ufixed(to_slv($x) or $(slvBit n i), $int:(m-1), $int:(-f))|]
      where
        m, f, n:: Integer
        m = natVal (Proxy :: Proxy m)
        f = natVal (Proxy :: Proxy f)
        n = m+f

    complement' x = VExp [vexp|not $x|]

    sla x 0 = x
    sla x i = VExp [vexp|$x sla $i|]

    sra x 0 = x
    sra x i = VExp [vexp|$x sra $i|]

-- | A fixed-point VHDL type.
class Fixed a where
    resize :: ToExp e => e -> a

instance (KnownNat m, KnownNat f) => Fixed (VExp (UQ m f)) where
    resize e = VExp [vexp|resize($e, $int:(m-1), $int:(-f), round_style => fixed_truncate, overflow_style => fixed_wrap)|]
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

    lift = VBool

instance (KnownNat m, KnownNat f, KnownNat (m + f)) => Lift (Q m f) where
    type Lifted (Q m f) = VExp (Q m f)

    lift = VQ

instance (KnownNat m, KnownNat f, KnownNat (m + f)) => Lift (UQ m f) where
    type Lifted (UQ m f) = VExp (UQ m f)

    lift = VUQ

instance (Lift a, Lift b) => Lift (a, b)  where
    type Lifted (a, b) = (Lifted a, Lifted b)

    lift (x, y) = (lift x, lift y)

instance (Lift a, Lift b, Lift c) => Lift (a, b, c)  where
    type Lifted (a, b, c) = (Lifted a, Lifted b, Lifted c)

    lift (x, y, z) = (lift x, lift y, lift z)
