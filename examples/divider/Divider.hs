{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Divider where

import Data.Bits
import Data.Proxy (Proxy(..))
import Data.Fixed.Q
import GHC.TypeLits

class Cast a b where
    cast :: a -> b

instance (KnownNat f, KnownNat f') => Cast (UQ m f) (UQ m' f') where
    cast (UQ x) = UQ (x `shift` fromIntegral (natVal (Proxy :: Proxy f') - natVal (Proxy :: Proxy f)))

underflow :: FiniteBits a => a -> Bool
underflow bits = testBit bits (finiteBitSize bits - 1)

-- | Restoring division.
class Restoring n d q r where
    restoring :: (n, d) -> (q, r)

-- | Non-restoring division.
class Nonrestoring n d q r where
    nonrestoring :: (n, d) -> (q, r)

instance (KnownNat m, KnownNat f, m' ~ (1+m+m), KnownNat m') => Restoring (UQ m f) (UQ m f) (UQ m f) (UQ m' f) where
    restoring (x0, d0) = post (foldl step (pre (x0, d0)) [0..n-1])
        where
          n = finiteBitSize x0
          m = intBitSize x0

          pre :: (UQ m f, UQ m f) -> (UQ m f, UQ m' f, UQ m' f)
          pre (x0, d0) = (0, cast x0, cast d0)

          post :: (UQ m f, UQ m' f, UQ m' f) -> (UQ m f, UQ m' f)
          post (q, r, _) = (q, r)

          step :: (UQ m f, UQ m' f, UQ m' f) -> Int -> (UQ m f, UQ m' f, UQ m' f)
          step (q, r, d) i =
              if underflow r'
                then (q,            r' + d `shiftL` m, d)
                else (q `setBit` b, r',                d)
            where
              r' = r `shiftL` 1 - d `shiftL` m
              b = n - i - 1

instance (KnownNat m, KnownNat f, m' ~ (1+m+m), KnownNat m') => Nonrestoring (UQ m f) (UQ m f) (UQ m f) (UQ m' f) where
    nonrestoring (x0, d0) = post (foldl step (pre (x0, d0)) [0..n-1])
        where
          n = finiteBitSize x0
          m = intBitSize x0

          pre :: (UQ m f, UQ m f) -> (UQ m f, UQ m' f, UQ m' f)
          pre (x0, d0) = (0, cast x0, cast d0)

          post :: (UQ m f, UQ m' f, UQ m' f) -> (UQ m f, UQ m' f)
          post (q, r, d) =
              if underflow r
                then (q' - ulp, r + d `shiftL` m)
                else (q', r)
            where
              q' = q - complement q

          step :: (UQ m f, UQ m' f, UQ m' f) -> Int -> (UQ m f, UQ m' f, UQ m' f)
          step (q, r, d) i =
              if underflow r
                then (q,            r `shiftL` 1 + d `shiftL` m, d)
                else (q `setBit` b, r `shiftL` 1 - d `shiftL` m, d)
            where
              b = n - i - 1
