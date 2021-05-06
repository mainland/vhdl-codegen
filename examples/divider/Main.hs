{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Main (
    main
  ) where

import Prelude hiding ( id )

import Control.Category ( (>>>), id )
import Control.Monad ( replicateM )
import Control.Monad.IO.Class ( liftIO )
import Data.Bits
import Data.Foldable ( toList )
import Data.Fixed.Q
import Data.List ( zip4 )
import Data.Loc ( noLoc )
import Data.Maybe ( fromMaybe )
import Data.Proxy
import Data.Sequence ( Seq )
import Data.String ( fromString )
import GHC.TypeLits
import Language.VHDL.Quote
import qualified Language.VHDL.Syntax as V
import System.Environment
import System.IO
import Text.PrettyPrint.Mainland
import Text.PrettyPrint.Mainland.Class

import Language.VHDL.Codegen.Gensym
import Language.VHDL.Codegen.Lift
import Language.VHDL.Codegen.Monad
import Language.VHDL.Codegen.Pipeline
import Language.VHDL.Codegen.Testbench
import Language.VHDL.Codegen.VExp

import Opt

type N = UQ 8 8

main :: IO ()
main = do
    (conf, args) <- getArgs >>= compilerOpts
    when (not (help conf)) $ do
      (unit, p :: Pipeline (VExp N, VExp N) (VExp N, VExp (UQ 17 8))) <- evalCg $ nonrestoring conf
      case output conf of
        Nothing   -> return ()
        Just path -> liftIO $ withFile path WriteMode $ \h -> hPutDoc h (ppr (toList unit))
      case tb_output conf of
        Nothing   -> return ()
        Just path -> do unit <- evalCg $ vunitTestbench True "tb_divider" p
                        withFile path WriteMode $ \h -> hPutDoc h (ppr unit)

underflow :: forall m f . (KnownNat m,KnownNat f) => VExp (UQ m f) -> VExp Bool
underflow e = testBit' e (fromIntegral (finiteBitSize (undefined :: UQ m f) - 1))

cast :: forall m m' f f' . (KnownNat m', KnownNat f') => VExp (UQ m f) -> VExp (UQ m' f')
cast e = resize [vexp|$e|]

nonrestoring :: forall i i' f m . (KnownNat i, KnownNat f, KnownNat (i + f), KnownNat (i' + f), i' ~ (1+i+i), KnownNat i', MonadCg m)
             => Config
             -> m (Seq V.DesignUnit, Pipeline (VExp (UQ i f), VExp (UQ i f)) (VExp (UQ i f), VExp (UQ i' f)))
nonrestoring conf = withDesignUnit $ do
    p <- iter "divider"
              pre
              step
              post
              ["n", "d"]
              ["q", "r", "d"]
              (finiteBitSize (undefined :: UQ i f))
    wrapP "main" p
  where
    iter = case variant conf of
             Serial -> siter
             Parallel -> piter

    n, m :: VExp Int
    n = fromIntegral $ finiteBitSize (undefined :: UQ i f)
    m = fromIntegral $ intBitSize (undefined :: UQ i f)

    pre :: (VExp (UQ i f), VExp (UQ i f)) -> (VExp (UQ i f), VExp (UQ i' f), VExp (UQ i' f))
    pre (x0, d0) = (0, cast x0, cast d0)

    post :: (VExp (UQ i f), VExp (UQ i' f), VExp (UQ i' f)) -> (VExp (UQ i f), VExp (UQ i' f))
    post (q, r, d) =
        if underflow r
        then (q' - bit' 0, r + d `sla` m)
        else (q', r)
      where
        q' = q - complement' q

    step :: (VExp (UQ i f), VExp (UQ i' f), VExp (UQ i' f))
         -> VExp Int
         -> m (VExp (UQ i f), VExp (UQ i' f), VExp (UQ i' f))
    step (q, r, d) i =
        return $ if underflow r
        then (q,                       r `sla` 1 + d `sla` m, d)
        else (q `setBit'` (n - i - 1), r `sla` 1 - d `sla` m, d)
