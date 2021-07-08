{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Main (
    main
  ) where

import Data.Fixed.Q
import Data.Proxy ( Proxy )
import Data.Sequence ( Seq )
import Language.VHDL.Quote (ToType)
import qualified Language.VHDL.Syntax as V

import Language.VHDL.Codegen.Pipeline.VHDL
import Language.VHDL.Codegen.Pipeline.VHDL.Testbench
import Language.VHDL.Codegen.Monad
import Language.VHDL.Codegen.VExp

main :: IO ()
main = do
    (unit, p) <- evalCg $ mkIncrementer @(UQ 16 0)
    writeDesignUnit "rtl/incrementer.vhd" unit
    writeTestBench tbconf "tb_incrementer.vhd" p
  where
    tbconf = defaultTestBenchConfig { tb_entity   = "tb_incrementer"
                                    , tb_watchdog = Just 50000
                                    , tb_compare  = True
                                    }

mkIncrementer :: forall a m . (ToType (Proxy a), Num (VExp a), MonadCg m)
              => m (Seq V.DesignUnit, Pipeline (VExp a) (VExp a))
mkIncrementer = withDesignUnit $
    mealy "incrementer" (\x i -> (x + i, x + i)) 0
