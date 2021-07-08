{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      :  Language.VHDL.Codegen.Pipeline.Shallow
-- Copyright   :  (c) 2021 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Language.VHDL.Codegen.Pipeline.Shallow (
  Pipeline(..),
  runOneP,
  reifyP,
  reifyOneP
) where

import qualified Control.Category as C
import Language.VHDL.Quote ( ToExp )

import qualified Language.VHDL.Codegen.Pipeline as P
import Language.VHDL.Codegen.VExp ( Lift(lift), VExp(VConst) )

-- | A shallowly embedded pipeline
newtype Pipeline a b = P { runP :: [a] -> [b] }

-- | Run a pipeline for a single input
runOneP :: Pipeline a b -> a -> b
runOneP p x =
  case runP p [x] of
    [y] -> y
    _   -> error "runOneP: single input did not produce single output"

-- | Reify a pipeline operating on 'VExp' values as an un-embedded Haskell
-- function.
reifyP :: ToExp a => Pipeline (VExp a) (VExp b) -> [a] -> [b]
reifyP p xs = map unConst $ runP p (map VConst xs)

-- | Reify a pipeline operating on 'VExp' values as an un-embedded Haskell
-- function for a single input/output pair.
reifyOneP :: ToExp a => Pipeline (VExp a) (VExp b) -> a -> b
reifyOneP p x =
  case reifyP p [x] of
    [y] -> y
    _   -> error "reifyOneP: single input did not produce single output"

unConst :: VExp a -> a
unConst (VConst x) = x
unConst _          = error "unConst: not a constant"

instance C.Category Pipeline where
  id = P id

  f . g = P (runP f . runP g)

instance P.Pipeline Pipeline where
  f `seq` g = P (runP g . runP f)

  arr f = P $ map f

  siter f g h n = P $ map (h . go 0 . f)
    where
      go i x | i == n    = x
             | otherwise = go (i+1) (g x (lift i))

  piter = P.siter

  moore (step :: s -> i -> s) (out :: s -> o) state0 = P $ go state0
    where
      go :: s -> [i] -> [o]
      go _ []     = []
      go s (x:xs) = y : go s' xs
        where
          s' :: s
          s' = step s x

          y :: o
          y = out s'

  mealy (step :: s -> i -> (s, o)) state0 = P $ go state0
    where
      go :: s -> [i] -> [o]
      go _ []     = []
      go s (x:xs) = y : go s' xs
        where
          s' :: s
          y :: o
          (s', y) = step s x
