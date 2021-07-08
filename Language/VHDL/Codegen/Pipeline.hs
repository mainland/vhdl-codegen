-- |
-- Module      :  Language.VHDL.Codegen.Pipeline
-- Copyright   :  (c) 2021 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Language.VHDL.Codegen.Pipeline where

import Control.Category ( Category(..) )
import Language.VHDL.Codegen.Pack ( Pack )
import Language.VHDL.Codegen.VExp ( VExp )

class Category p => Pipeline p where
    -- | Sequencing pipelines
    seq :: p a b -> p b c -> p a c

    -- | "Arrow" pipeline
    arr :: (Pack a, Pack b) => (a -> b) -> p a b

    -- | Serial iteration pipeline
    siter :: (Pack a, Pack b, Pack c)
          => (a -> b)             -- ^ Pre-processing function
          -> (b -> VExp Int -> b) -- ^ Function to iterate
          -> (b -> c)             -- ^ Post-processing function
          -> Int                  -- ^ Number of iterations to perform
          -> p a c

    -- | Parallel iteration pipeline
    piter :: (Pack a, Pack b, Pack c)
          => (a -> b)             -- ^ Pre-processing function
          -> (b -> VExp Int -> b) -- ^ Function to iterate
          -> (b -> c)             -- ^ Post-processing function
          -> Int                  -- ^ Number of iterations to perform
          -> p a c

    -- | Moore machine pipeline
    moore :: (Pack s, Pack i, Pack o)
          => (s -> i -> s) -- ^ Moore machine state transfer function
          -> (s -> o)      -- ^ Moore machine output function
          -> s             -- ^ Initial state
          -> p i o

    -- | Mealy machine pipeline
    mealy :: (Pack s, Pack i, Pack o)
          => (s -> i -> (s, o)) -- ^ Mealy machine state transfer function
          -> s                  -- ^ Initial state
          -> p i o
