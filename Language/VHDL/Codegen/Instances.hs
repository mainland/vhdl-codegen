{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Module      :  Language.VHDL.Codegen.Instances
-- Copyright   :  (c) 2020-2021 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Language.VHDL.Codegen.Instances () where

import Data.Bit
import Data.Bits
import Data.Finite
import Data.Fixed.Q ( Q, UQ )
import Data.Proxy ( Proxy(..) )
import Data.Type.Equality ( type (:~:)(..) )
import qualified Data.Vector.Sized as S
import GHC.Real
import GHC.TypeLits
import Language.VHDL.Quote
import qualified Language.VHDL.Syntax as V
import Text.PrettyPrint.Mainland
import Text.PrettyPrint.Mainland.Class

overflowFail :: Maybe a -> a
overflowFail = maybe overflowError id

instance KnownNat n => Bits (S.Vector n Bit) where
    (.&.) = S.zipWith (.&.)
    (.|.) = S.zipWith (.|.)
    xor   = S.zipWith xor

    complement = S.map complement

    zeroBits = S.replicate 0

    bit i = overflowFail $ do
        SomeNat (_ :: Proxy i)    <- someNatVal (fromIntegral i - 1)
        SomeNat (_ :: Proxy m)    <- someNatVal (n - fromIntegral i - 1)
        Refl :: (i + m + 1 :~: n) <- sameNat (Proxy :: Proxy (i+m+1)) (Proxy :: Proxy n)
        return $ S.replicate' (Proxy :: Proxy i) 0 S.++ S.cons 1 (S.replicate' (Proxy :: Proxy m) 0)
      where
        n = natVal (Proxy :: Proxy n)

    setBit x i = x S.// [(finite (fromIntegral i), 1)]

    clearBit x i = x S.// [(finite (fromIntegral i), 0)]

    testBit x i = S.index x (finite (fromIntegral i)) == 1

    bitSizeMaybe _ = return (fromIntegral n)
      where
        n = natVal (Proxy :: Proxy n)

    bitSize _ = fromIntegral n
      where
        n = natVal (Proxy :: Proxy n)

    isSigned _ = False

    shiftL x i = overflowFail $ do
        SomeNat (_ :: Proxy i) <- someNatVal (fromIntegral i)
        SomeNat (_ :: Proxy m) <- someNatVal (n - fromIntegral i)
        Refl :: (i + m :~: n)  <- sameNat (Proxy :: Proxy (i+m)) (Proxy :: Proxy n)
        return $ S.replicate' (Proxy :: Proxy i) 0 S.++ S.take' (Proxy :: Proxy m) x
      where
        n = natVal (Proxy :: Proxy n)

    shiftR x i = overflowFail $ do
        SomeNat (_ :: Proxy i) <- someNatVal (fromIntegral i)
        SomeNat (_ :: Proxy m) <- someNatVal (n - fromIntegral i)
        Refl :: (i + m :~: n)  <- sameNat (Proxy :: Proxy (i+m)) (Proxy :: Proxy n)
        return $ S.drop' (Proxy :: Proxy i) x S.++ S.replicate' (Proxy :: Proxy i) 0
      where
        n = natVal (Proxy :: Proxy n)

    rotateL x i = overflowFail $ do
        SomeNat (_ :: Proxy i) <- someNatVal (fromIntegral i)
        SomeNat (_ :: Proxy m) <- someNatVal (n - fromIntegral i)
        Refl :: (i + m :~: n)  <- sameNat (Proxy :: Proxy (i+m)) (Proxy :: Proxy n)
        let (u :: S.Vector m Bit, v :: S.Vector i Bit) = S.splitAt x
        return $ v S.++ u
      where
        n = natVal (Proxy :: Proxy n)

    rotateR x i = overflowFail $ do
        SomeNat (_ :: Proxy i) <- someNatVal (fromIntegral i)
        SomeNat (_ :: Proxy m) <- someNatVal (n - fromIntegral i)
        Refl :: (i + m :~: n)  <- sameNat (Proxy :: Proxy (i+m)) (Proxy :: Proxy n)
        let (u :: S.Vector i Bit, v :: S.Vector m Bit) = S.splitAt x
        return $ v S.++ u
      where
        n = natVal (Proxy :: Proxy n)

    popCount x = S.foldl' (\c b -> c + if b == 1 then 1 else 0) 0 x

instance KnownNat n => FiniteBits (S.Vector n Bit) where
    finiteBitSize _ = fromIntegral n
      where
        n = natVal (Proxy :: Proxy n)

instance Pretty Bit where
    ppr = text . show

instance (KnownNat m, KnownNat f, KnownNat (m + f)) => Pretty (UQ m f) where
    ppr = text . show

instance (KnownNat m, KnownNat f, KnownNat (m + f)) => Pretty (Q m f) where
    ppr = text . show

instance ToType (Proxy Bool) where
    toType _ _ = [vtype|boolean|]

instance ToType (Proxy Bit) where
    toType _ _ = [vtype|std_logic|]

instance KnownNat n => ToType (Proxy (S.Vector n Bit)) where
    toType _ _ = [vtype|std_logic_vector(0 to $int:(n-1))|]
      where
        n = natVal (Proxy :: Proxy n)

instance (KnownNat m, KnownNat f) => ToType (Proxy (UQ m f)) where
    toType _ _ = [vtype|ufixed($int:(m-1) downto $int:(-f))|]
      where
        m = natVal (Proxy :: Proxy m)
        f = natVal (Proxy :: Proxy f)

instance (KnownNat m, KnownNat f) => ToType (Proxy (Q m f)) where
    toType _ _ = [vtype|sfixed($int:m downto $int:(-f))|]
      where
        m = natVal (Proxy :: Proxy m)
        f = natVal (Proxy :: Proxy f)

instance ToLit Bit where
    toLit x loc = V.BitStringLit (if x == 0 then "'0'" else "'1'") loc

instance KnownNat n => ToLit (S.Vector n Bit) where
    toLit x loc = bitsL x loc

instance (KnownNat m, KnownNat f) => ToLit (Q m f) where
    toLit x loc = bitsL x loc

instance (KnownNat m, KnownNat f) => ToLit (UQ m f) where
    toLit x loc = bitsL x loc

instance ToExp Bit where
    toExp b l = toExp (toLit b l) l

instance KnownNat n => ToExp (S.Vector n Bit) where
    toExp n _ = [vexp|std_logic_vector'($lit:n)|]

instance (KnownNat m, KnownNat f) => ToExp (Q m f) where
    toExp n _ = [vexp|to_sfixed(std_logic_vector'($lit:n), $int:(m), $int:(-f))|]
      where
        m, f :: Integer
        m = natVal (Proxy :: Proxy m)
        f = natVal (Proxy :: Proxy f)

instance (KnownNat m, KnownNat f) => ToExp (UQ m f) where
    toExp n _ = [vexp|to_ufixed(std_logic_vector'($lit:n), $int:(m-1), $int:(-f))|]
      where
        m, f :: Integer
        m = natVal (Proxy :: Proxy m)
        f = natVal (Proxy :: Proxy f)
