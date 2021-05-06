{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      :  Language.VHDL.Codegen.Lift
-- Copyright   :  (c) 2015-2020 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Language.VHDL.Codegen.Lift (
  IfThenElse(..),
  When(..),

  LiftEq(..),
  LiftOrd(..),
  LiftBool(..),
  LiftBits(..)
  ) where

import qualified Control.Monad
import Data.Bits ( Bits )

class IfThenElse a b where
    ifThenElse :: a -> b -> b -> b

instance IfThenElse Bool a where
    ifThenElse c t e = if c then t else e

class Functor f => When a f where
    when :: a -> f () -> f ()

instance Applicative f => When Bool f where
    when = Control.Monad.when

infix 4 .==., ./=.

class LiftEq f where
    (.==.) :: Eq a => f a -> f a -> f Bool
    (./=.) :: Eq a => f a -> f a -> f Bool

infix 4 .<., .<=., .>=., .>.

class LiftEq f => LiftOrd f where
    (.<.)  :: Ord a => f a -> f a -> f Bool
    (.<=.) :: Ord a => f a -> f a -> f Bool
    (.>=.) :: Ord a => f a -> f a -> f Bool
    (.>.)  :: Ord a => f a -> f a -> f Bool

infixr 3 .&&.
infixr 2 .||.

class LiftEq f => LiftBool f where
    (.&&.) :: f Bool -> f Bool -> f Bool
    (.||.) :: f Bool -> f Bool -> f Bool

infixl 5 ..|..
infixl 7 ..&..
infixl 8 `sla`, `sra`

class LiftBits f a where
    -- | Bitwise \"and\"
    (..&..) :: Bits a => f a -> f a -> f a

    -- | Bitwise \"or\"
    (..|..) :: Bits a => f a -> f a -> f a

    -- | Value with bit
    bit' :: f Int -> f a

    -- | Test a bit
    testBit' :: f a -> f Int -> f Bool

    -- | Test a bit
    setBit' :: f a -> f Int -> f a

    -- | Reverse all the bits in the argument
    complement' :: Bits a => f a -> f a

    -- | Arithmetic shift left
    sla :: Bits a => f a -> f Int -> f a

    -- | Arithmetic shift left
    sra :: Bits a => f a -> f Int -> f a
