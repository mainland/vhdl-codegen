{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}

import Data.Fixed.Q
import Data.Proxy (Proxy(..))
import GHC.TypeLits
import GHC.TypeLits.Extra (Max)
import Test.Hspec

import Test.QuickCheck
import Divider

prop_divide :: forall m f m' . (KnownNat m, KnownNat f, KnownNat (m+f), m' ~ (Max (1+m) (m+m)), KnownNat m', KnownNat (m'+f))
            => ((UQ m f, UQ m f) -> (UQ m f, UQ m' f))
            -> UQ m f
            -> UQ m f
            -> Property
prop_divide alg x d =
    toRational x < 2^m * toRational d ==>
    counterexample (show (q, r)) $
    toRational q*toRational d + toRational r * 2^^(-n) === toRational x
  where
    (q, r) = alg (x, d)

    m, f :: Integer
    m = fromIntegral $ natVal (Proxy :: Proxy m)
    f = fromIntegral $ natVal (Proxy :: Proxy f)
    n = f + m

main :: IO ()
main = hspec $ do
    restoringSpec
    nonrestoringSpec

restoringSpec :: Spec
restoringSpec = do
    describe "Restoring division" $ do
      it "UQ 8 0" $
        property (prop_divide restoring :: UQ 8 0 -> UQ 8 0 -> Property)
      it "UQ 0 8" $
        property (prop_divide restoring :: UQ 0 8 -> UQ 0 8 -> Property)
      it "UQ 0 2" $
        property (prop_divide restoring :: UQ 0 2 -> UQ 0 2 -> Property)
      it "UQ 8 8" $
        property (prop_divide restoring :: UQ 8 8 -> UQ 8 8 -> Property)

nonrestoringSpec :: Spec
nonrestoringSpec = do
    describe "Non-restoring division" $ do
      it "UQ 8 0" $
        property (prop_divide nonrestoring :: UQ 8 0 -> UQ 8 0 -> Property)
      it "UQ 0 8" $
        property (prop_divide nonrestoring :: UQ 0 8 -> UQ 0 8 -> Property)
      it "UQ 0 2" $
        property (prop_divide nonrestoring :: UQ 0 2 -> UQ 0 2 -> Property)
      it "UQ 8 8" $
        property (prop_divide nonrestoring :: UQ 8 8 -> UQ 8 8 -> Property)
