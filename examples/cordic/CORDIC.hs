{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :  CORDIC
-- Copyright   :  (c) 2020 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu
--
-- See Chapter 9 of Muller's /Elementary Functions/.

module CORDIC (
    GenShift(..),

    Mode(..),
    Coordinates(..),

    cordic,
    sigma,
    hyshift,
    zks,

    lim,
    k
  ) where

import Data.Bits ( Bits(shift) )
import Data.Fixed.Q ( Q )
import GHC.TypeLits ( KnownNat, type (+) )

-- | A type that supports a generalized bit shift operation.
class Floating a => GenShift a where
    genshift :: a -> Int -> a
    genshift x i = x * 2 ^^ i

instance GenShift Float where

instance GenShift Double where

instance (KnownNat m, KnownNat f, KnownNat (m+f), KnownNat (1+m+f), f ~ (1+f')) => GenShift (Q m f) where
    genshift = shift

data Mode = Rotation | Vectoring
  deriving (Eq, Ord, Enum, Show)

data Coordinates a = Circular
                   | Linear
                   | Hyperbolic a
  deriving (Eq, Ord, Show)

cordic :: forall a . (Ord a, GenShift a)
       => Coordinates a
       -> Mode
       -> Int
       -> Int
       -> (a, a, a)
       -> (a, a, a)
cordic coord mode i0 n xyz =
    foldl step xyz ([i0..n] `zip` zks coord i0)
  where
    step :: (a, a, a)
         -> (Int, a)
         -> (a, a, a)
    step (x, y, z) (i, w) =
        ( x - m*d*coordshift y i
        , y + d*coordshift x i
        , z - d*w
        )
      where
        m :: a
        m = case coord of
              Circular     -> 1
              Linear       -> 0
              Hyperbolic{} -> -1

        d :: a
        d = case mode of
              Rotation  -> case coord of
                             Hyperbolic lnb | lnb < 0 -> -(if z >= 0 then 1 else -1)
                             _                        -> if z >= 0 then 1 else -1
              Vectoring -> if y >= 0 then -1 else 1

        coordshift :: a -> Int -> a
        coordshift t j =
          case coord of
            Circular   -> t `genshift` (-sigma coord j)
            Linear     -> t `genshift` (-j)
            Hyperbolic{}
              | i > 0     -> t `genshift` (-sigma coord j)
              | otherwise -> t * (1 - 1 `genshift` (hyshift j))

-- | Index mapping function for CORDIC. Repeats terms $3^(j+1) - 1)/2$ twice for
-- hyperbolic CORDIC.
sigma :: Coordinates a -> Int -> Int
sigma Circular i
  | i < 0            = 0
  | otherwise        = i
sigma Linear i       = i
sigma Hyperbolic{} n = n - nrepeat n
  where
    -- Number of repeated indices up to but /not/ including i
    nrepeat :: Int -> Int
    nrepeat i
      | i > 1     = floor (logBase 3 (fromIntegral (2*i - 1) :: Double)) - 1
      | otherwise = 0

-- | Shift factor for hyperbolic CORDIC as a function of index. Valid only when
-- @i <= 0@.
hyshift :: Int -> Int
hyshift i = -2^(-i + 1)

-- | Coefficients for angular (z) terms.
zks :: GenShift a
    => Coordinates a -- ^ CORDIC coordinate scheme
    -> Int           -- ^ Index of initial term in series
    -> [a]
zks Circular               i0 = [atan (1 `genshift` (-sigma Circular i)) | i <- [i0..]]
zks Linear                 i0 = [1 `genshift` (-i) | i <- [i0..]]
zks coord@(Hyperbolic lnb) i0 = [atanh (1 - 1 `genshift` (hyshift i)) / lnb | i <- [i0..0]] ++
                                [atanh (1 `genshift` (-sigma coord i)) / lnb | i <- [max 1 i0..]]

-- | Normalization coefficients.
ks :: GenShift a
   => Coordinates a -- ^ CORDIC coordinate scheme
   -> Int           -- ^ Index of initial term in series
   -> [a]
ks coord@Hyperbolic{} i0 = [sqrt(1 - (1 - 1 `genshift` (hyshift i))^(2::Int)) | i <- [i0..0]] ++
                           [sqrt(1 - 1 `genshift` (-2*sigma coord i)) | i <- [max 1 i0..]]
ks _                  i0 = [sqrt(1 + 1 `genshift` (-2*sigma Circular i)) | i <- [i0..]]

-- | Compute normalization constant for CORDIC with n stages.
k :: GenShift a => Coordinates a -> Int -> Int -> a
k coord i0 n = product (take n (ks coord i0))

-- | Compute n-term approximation of upper bound on value of theta for CORDIC.
lim :: GenShift a => Coordinates a -> Int -> Int -> a
lim coord i0 n = sum (take n (zks coord i0))
