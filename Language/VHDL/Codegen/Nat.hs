{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :  Language.VHDL.Codegen.Nat
-- Copyright   :  (c) 2020-2021 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu
-- Stability   :  experimental
-- Portability :  portable
--
-- This module defines signed and unsigned Q format fixed-point numbers.

module Language.VHDL.Codegen.Nat (
    isZeroNat,
    isLteNat
  ) where


import Data.Proxy ( Proxy(..) )
import Data.Type.Equality ( type (:~:)(..) )
import GHC.TypeLits ( KnownNat, type (<=?), natVal )
import Unsafe.Coerce ( unsafeCoerce )

-- | Return a type-level proof that 'n ~ 0'
isZeroNat :: forall n. KnownNat n => Maybe (n :~: 0)
isZeroNat | n == 0    = Just (unsafeCoerce Refl)
          | otherwise = Nothing
  where
    n = natVal (Proxy :: Proxy n)

-- | Return a type-level proof that 'm <=? n'
isLteNat :: forall m n. (KnownNat m, KnownNat n) => Maybe ((m <=? n) :~: 'True)
isLteNat | m <= n    = Just (unsafeCoerce Refl)
         | otherwise = Nothing
  where
    m = natVal (Proxy :: Proxy m)
    n = natVal (Proxy :: Proxy n)
