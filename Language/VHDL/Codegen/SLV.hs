{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :  Language.VHDL.Codegen.SLV
-- Copyright   :  (c) 2020-2021 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Language.VHDL.Codegen.SLV (
    SLV,
    showSLV,
    readSLV,
    SLVBits(..),
    LiftSLVBits(..)
  ) where

import Control.Monad.Fail ( MonadFail )
import Data.Bit ( Bit )
import Data.Bits ( Bits(testBit, zeroBits, setBit), FiniteBits )
import Data.Finite ( getFinite )
import Data.Fixed.Q ( Q, UQ )
import Data.Maybe ( fromJust )
import Data.Proxy ( Proxy(..) )
import qualified Data.Vector.Sized as S
import GHC.TypeLits ( KnownNat, Nat, type (+), natVal )

import Language.VHDL.Codegen.Instances ()

-- | Haskell version of std_logic_vector
type SLV n = S.Vector n Bit

showSLV :: S.Vector n Bit -> String
showSLV = reverse . S.foldr (\b s -> (if b == 1 then '1' else '0') : s) ""

readSLV :: KnownNat n => String -> S.Vector n Bit
readSLV s = case parse of
              Just (slv, _) -> slv
              _             -> fail "no parse"
  where
    parse = do
      bs <- mapM toBit s
      case S.fromList (reverse bs) of
          Nothing  -> fail "no parse"
          Just slv -> return (slv, "")
      where
        toBit :: MonadFail m => Char -> m Bit
        toBit '0' = return 0
        toBit '1' = return 1
        toBit _   = fail "no parse"

-- | A type that can be converted to/from a value of type @'SLV' a@.
class (FiniteBits a, KnownNat (BitSize a)) => SLVBits a where
    type BitSize a :: Nat

    fromSLV :: SLV (BitSize a) -> a
    fromSLV = S.ifoldl' (\x i b -> if b == 1 then setBit x (fromIntegral (getFinite i)) else x) zeroBits

    toSLV :: a -> SLV (BitSize a)
    toSLV x = fromJust $ S.fromList [if testBit x i then 1 else 0 | i <- [0..n-1]]
      where
        n :: Int
        n = fromIntegral $ natVal (Proxy :: Proxy (BitSize a))

instance KnownNat n => SLVBits (SLV n) where
    type BitSize (SLV n) = n

    fromSLV = id

    toSLV = id

instance (KnownNat m, KnownNat f) => SLVBits (UQ m f) where
    type BitSize (UQ m f) = m + f

instance (KnownNat m, KnownNat f) => SLVBits (Q m f) where
    type BitSize (Q m f) = m + f + 1

-- | 'SLVBits' operations lifted to a type indexed type.
class LiftSLVBits f a where
    fromSLV' :: SLVBits a => f (SLV (BitSize a)) -> f a

    toSLV' :: SLVBits a => f a -> f (SLV (BitSize a))
