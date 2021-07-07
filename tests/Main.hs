{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Module      :  Main
-- Copyright   :  (c) 2020 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Main (
    main
  ) where

import Data.Bits
import Data.Fixed.Q
import Data.Int
import qualified Data.Vector.Sized as S
import Data.Word
import Test.HUnit (Assertion, (@?=))
import Test.Hspec
import Test.QuickCheck (Property,
                        (===),
                        property)

import Language.VHDL.Codegen.Instances ()
import Language.VHDL.Codegen.SLV

main :: IO ()
main = do
  hspec bitsTests

bitsTests :: Spec
bitsTests = describe "Bits" $ do
    it "toSLV" $
      showSLV (toSLV (2 :: UQ 4 0)) @?= "0010"
    it "fromSLV" $
      fromSLV (readSLV "0010" :: SLV 4) @?= (2 :: UQ 4 0)
    it "0010 .&. 0011" $
      readSLV "0010" .&. readSLV "0011" @?= (readSLV "0010" :: SLV 4)
    it "0010 .|. 0011" $
      readSLV "0010" .|. readSLV "0011" @?= (readSLV "0011" :: SLV 4)
    it "0010 `xor` 0011" $
      readSLV "0010" `xor` readSLV "0011" @?= (readSLV "0001" :: SLV 4)
    it "shift 0010 1" $
      shift (readSLV "0010") 1 @?= (readSLV "0100" :: SLV 4)
    it "shift 0010 (-1)" $
      shift (readSLV "0010") (-1) @?= (readSLV "0001" :: SLV 4)
    it "rotate 101001 1" $
      rotate (readSLV "101001") 1 @?= (readSLV "010011" :: SLV 6)
    it "rotate 101001 (-1)" $
      rotate (readSLV "101001") (-1) @?= (readSLV "110100" :: SLV 6)
