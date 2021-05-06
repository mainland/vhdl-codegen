{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Divider where

import Data.Bits
import Data.Proxy (Proxy(..))
import Data.Fixed.Q
import GHC.TypeLits
import GHC.TypeLits.Extra (Max)

cast :: forall m m' f f' . (KnownNat f, KnownNat f') => UQ m f -> UQ m' f'
cast (UQ x) = UQ (x `shift` fromIntegral (natVal (Proxy :: Proxy f') - natVal (Proxy :: Proxy f)))

underflow :: FiniteBits a => a -> Bool
underflow bits = testBit bits (finiteBitSize bits - 1)

restoring :: forall m f m'. (KnownNat m, KnownNat f, m' ~ (Max (1+m) (m+m)), KnownNat m')
          => (UQ m f, UQ m f)
          -> (UQ m f, UQ m' f)
restoring (x0, d0) = foldl step (0, cast x) [n-1,n-2..0]
    where
      n = finiteBitSize x0
      m = intBitSize x0

      x, d :: UQ m' f
      x = cast x0
      d = cast d0

      step :: (UQ m f, UQ m' f) -> Int -> (UQ m f, UQ m' f)
      step (q, r) i
          | underflow r' = (q, r' + d `shiftL` m)
          | otherwise    = (q `setBit` i, r')
        where
          r' = r `shiftL` 1 - d `shiftL` m

nonrestoring :: forall m f m' . (KnownNat m, KnownNat f, m' ~ (Max (1+m) (m+m)), KnownNat m')
             => (UQ m f, UQ m f)
             -> (UQ m f, UQ m' f)
nonrestoring (x0, d0) = post (foldl step (pre (x0, d0)) [0..n-1])
    where
      n = finiteBitSize x0
      m = intBitSize x0

      pre :: (UQ m f, UQ m f) -> (UQ m f, UQ m' f, UQ m' f)
      pre (x0, d0) = (0, cast x0, cast d0)

      post :: (UQ m f, UQ m' f, UQ m' f) -> (UQ m f, UQ m' f)
      post (q, r, d) =
          if underflow r
          then (q' - bit 0, r + d `shiftL` m)
          else (q', r)
        where
          q' = q - complement q

      step :: (UQ m f, UQ m' f, UQ m' f) -> Int -> (UQ m f, UQ m' f, UQ m' f)
      step (q, r, d) i =
          if underflow r
          then (q,                      r `shiftL` 1 + d `shiftL` m, d)
          else (q `setBit` (n - i - 1), r `shiftL` 1 - d `shiftL` m, d)
