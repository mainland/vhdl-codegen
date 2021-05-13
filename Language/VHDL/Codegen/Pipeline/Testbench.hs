{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module      :  Language.VHDL.Codegen.Testbench
-- Copyright   :  (c) 2021 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Language.VHDL.Codegen.Pipeline.Testbench (
  TestBenchConfig(..),
  defaultTestBenchConfig,

  vunitTestBench,
) where

import Prelude hiding ( id )

import Control.Monad.Fail (MonadFail)
import Data.List ( zip4 )
import Data.Proxy ( Proxy(..) )
import Data.String ( fromString )
import Language.VHDL.Quote
import qualified Language.VHDL.Syntax as V
import Text.PrettyPrint.Mainland ( prettyCompact )
import Text.PrettyPrint.Mainland.Class ( Pretty(ppr) )

import Language.VHDL.Codegen.Lift
import Language.VHDL.Codegen.Monad ( MonadCg )
import Language.VHDL.Codegen.Pipeline
import Language.VHDL.Codegen.Testbench

idName :: V.Id -> String
idName = prettyCompact . ppr

read_, write_ :: V.Id -> V.Id
read_ = mapId ("read_" ++)
write_ = mapId ("write_" ++)

data TestBenchConfig = TestBenchConfig
  { tb_watchdog :: Maybe Int -- ^ Watchdog timeout (in clock periods)
  , tb_compare  :: Bool      -- ^ True if test bench should compare to output,
                             -- False to write output
  }

defaultTestBenchConfig :: TestBenchConfig
defaultTestBenchConfig = TestBenchConfig
  { tb_watchdog = Nothing
  , tb_compare  = True
  }

-- | Create a VUnit testbench module for a pipeline.
vunitTestBench :: forall a b m . (TextIO a, TextIO b, MonadCg m)
               => TestBenchConfig
               -> V.Id
               -> Pipeline a b
               -> m [V.DesignUnit]
vunitTestBench conf entity p = do
  stiumuli_proc <- genStimuliProc
  compare_proc  <- genCompareProc
  write_proc    <- genWriteProc
  return [vfile|
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.fixed_pkg.all;

use std.textio.all;

library vunit_lib;
context vunit_lib.vunit_context;

library osvvm;
use osvvm.RandomPkg.all;

entity $id:entity is
  generic (
    runner_cfg : string;
    tb_path    : string;
    csv_i      : string := "data/in.csv";
    csv_o      : string := "data/out.csv"
  );
end entity $id:entity;

architecture test of $id:entity is
  constant clk_period : time := 10 ns;

  signal clk : std_logic := '1';
  signal rst : std_logic := '0';
  signal start, done, compared : boolean := false;

  signal in_ready : std_logic;
  signal in_valid : std_logic;
  $decls:in_signals

  signal out_ready : std_logic;
  signal out_valid : std_logic;
  $decls:out_signals
begin
  clk <= not clk after clk_period/2;

  test_main: process
  begin
    test_runner_setup(runner, runner_cfg);
    while test_suite loop
      if run("test") then
        rst <= '1';
        wait for 15*clk_period;
        rst <= '0';

        info("Init test");
        wait until rising_edge(clk);
        start <= true;
        wait until rising_edge(clk);
        start <= false;

        wait until (done and compared and rising_edge(clk));

        info("Test done");
      end if;
    end loop;
    test_runner_cleanup(runner);
  end process;

  $cstms:watchdog

  $cstm:stiumuli_proc

  $cstm:(if tb_compare conf then compare_proc else write_proc)

  uut: entity work.$id:(pipe_entity p) port map
    ( clk => clk,
      rst => rst,
      in_ready => in_ready,
      in_valid => in_valid,
      $assocs:in_assocs,
      out_ready => out_ready,
      out_valid => out_valid,
      $assocs:out_assocs
    );
end architecture test;
|]
  where
    watchdog :: [V.CStm]
    watchdog | Just n <- tb_watchdog conf = [vcstms|test_runner_watchdog(runner, $n*clk_period);|]
             | otherwise                  = []

    in_signals, out_signals :: [V.Decl]
    in_signals  = [[vdecl|signal $id:v : $ty:tau;|] | (v, tau) <- pipe_in p]
    out_signals = [[vdecl|signal $id:v : $ty:tau;|] | (v, tau) <- pipe_out p]

    in_assocs, out_assocs :: [V.AssocElem]
    in_assocs  = [[vassoc|$id:v => $id:v|] | (v, _) <- pipe_in p]
    out_assocs = [[vassoc|$id:v => $id:v|] | (v, _) <- pipe_out p]

    genStimuliProc :: m V.CStm
    genStimuliProc = do
      read_stms <- genReadStms
      return [vcstm|
      stimuli: process
        variable rnd : typename RandomPType;

        file fread       : text;
        variable l       : line;
        variable ctmp    : character;
        variable is_good : boolean;

        $decls:read_decls
      begin
        rnd.InitSeed("common_seed");

        wait until start and rising_edge(clk);
        done <= false;
        wait until rising_edge(clk);

        file_open(fread, tb_path & csv_i, read_mode);

        while not endfile(fread) loop
          wait for rnd.RandInt(0, 5) * clk_period;

          readline(fread, l);

          $stms:read_stms

          in_valid <= '1';
          wait until rising_edge(clk) and in_ready = '1';
          in_valid <= '0';
        end loop;

        file_close(fread);

        wait until rising_edge(clk);
        done <= true;

        wait;
      end process;
    |]
      where
        in_sigs :: [V.Id]
        in_sigs = map fst (pipe_in p)

        read_taus :: [V.Subtype]
        read_taus = ioType (Proxy :: Proxy a)

        read_vs :: [V.Id]
        read_vs = map read_ in_sigs

        read_decls :: [V.Decl]
        read_decls = [[vdecl|variable $id:v : $ty:tau;|] | (v, tau) <- read_vs `zip` read_taus]

        genReadStms :: MonadFail m => m [V.Stm]
        genReadStms = do
          read_vals <- fromTextIO (Proxy :: Proxy a) read_vs
          return $ go $ zip3 in_sigs read_vs read_vals
          where
            go :: [(V.Id, V.Id, V.Exp)] -> [V.Stm]
            go [] = []

            go ((sig, v, e) : binds) =
                [vstms|read(l, $v, is_good);
                       check_equal(is_good, true, "Reading " & $lbl);

                       $id:sig <= $e;
                       info($lbl & " = " & to_string($v) & " (" & to_string($e) & ")");|]
                ++
                if null binds
                  then []
                  else [vstms|read(l, ctmp, is_good);
                              check_equal(is_good, true, "Reading separator");|]
                ++
                go binds
              where
                lbl = idName sig

    genCompareProc :: m V.CStm
    genCompareProc = do
      read_stms <- genReadStms
      return [vcstm|
      compare: process
        variable rnd : typename RandomPType;

        file fread       : text;
        variable l       : line;
        variable ctmp    : character;
        variable is_good : boolean;

        $decls:read_decls
      begin
        out_ready <= '0';
        compared <= false;

        wait until start and rising_edge(clk);

        file_open(fread, tb_path & csv_o, read_mode);

        while not endfile(fread) loop
          wait for rnd.RandInt(0, 5) * clk_period;

          out_ready <= '1';
          wait until rising_edge(clk) and out_valid = '1';
          out_ready <= '0';

          readline(fread, l);
          $stms:read_stms
        end loop;

        file_close(fread);

        wait until rising_edge(clk);
        compared <= true;

        wait;
      end process;
    |]
      where
        out_sigs :: [V.Id]
        out_sigs = map fst (pipe_out p)

        read_taus :: [V.Subtype]
        read_taus = ioType (Proxy :: Proxy b)

        read_vs :: [V.Id]
        read_vs = map read_ out_sigs

        read_decls :: [V.Decl]
        read_decls = [[vdecl|variable $id:v : $ty:tau;|] | (v, tau) <- read_vs `zip` read_taus]

        genReadStms :: m [V.Stm]
        genReadStms = do
            read_vals     <- fromTextIO (Proxy :: Proxy b) read_vs
            expected_vals <- toTextIO (Proxy :: Proxy b) out_sigs
            return $ go $ zip4 out_sigs read_vs read_vals expected_vals
          where
            go :: [(V.Id, V.Id, V.Exp, V.Exp)] -> [V.Stm]
            go [] = []

            go ((sig, v, e, e_exp) : binds) =
                [vstms|read(l, $v, is_good);
                       check_equal(is_good, true, "Reading " & $lbl);

                       info($lbl & " = " & to_string($e_exp) & " (" & to_string($sig) & ")");
                       info($lbl & " ?= " & to_string($v) & " (" & to_string($e) & ")");
                       check_equal(to_slv($sig), to_slv($e), "Comparing output with reference");|]
                ++
                if null binds
                  then []
                  else [vstms|read(l, ctmp, is_good);
                              check_equal(is_good, true, "Reading separator");|]
                ++
                go binds
              where
                lbl = idName sig

    genWriteProc :: m V.CStm
    genWriteProc = do
      write_stms <- genWriteStms
      return [vcstm|
      compare: process
        variable rnd : typename RandomPType;

        variable count : integer := 0;

        file fwrite : text;
        variable l  : line;

        $decls:write_decls
      begin
        compared <= false;

        wait until start and rising_edge(clk);

        file_open(fwrite, tb_path & csv_o, write_mode);

        outer: while true loop
          out_ready <= '0';
          wait for rnd.RandInt(0, 5)*clk_period;
          out_ready <= '1';

          while true loop
            wait until rising_edge(clk);
            count := count + 1;

            if out_valid = '1' then
              count := 0;

              $stms:write_stms
              writeline(fwrite, l);

              exit;
            elsif count > 1000 then
              exit outer;
            end if;
          end loop;
        end loop;

        file_close(fwrite);

        wait until rising_edge(clk);
        compared <= true;

        wait;
      end process;
    |]
      where
        out_sigs :: [V.Id]
        out_sigs = map fst (pipe_out p)

        write_taus :: [V.Subtype]
        write_taus = ioType (Proxy :: Proxy b)

        write_vs :: [V.Id]
        write_vs = map write_ out_sigs

        write_decls :: [V.Decl]
        write_decls = [[vdecl|variable $id:v : $ty:tau;|] | (v, tau) <- write_vs `zip` write_taus]

        genWriteStms :: m [V.Stm]
        genWriteStms = do
            write_es <-  toTextIO (Proxy :: Proxy b) out_sigs
            return $ go $ zip3 out_sigs write_vs write_es
          where
            go :: [(V.Id, V.Id, V.Exp)] -> [V.Stm]
            go [] = []

            go ((sig, v, e) : binds) =
                [vstms|$id:v := $e;
                       write(l, $id:v);
                       info($lbl & " = " & to_string($v) & " (" & to_string($sig) & ")");|]
                ++
                if null binds
                  then []
                  else [vstms|write(l, ',');|]
                ++
                go binds
              where
                lbl = idName sig
