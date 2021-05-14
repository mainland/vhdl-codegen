{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (
    main
  ) where

import Prelude hiding ( id )

import Control.Category ( (>>>), id )
import Control.Monad ( when )
import Control.Monad.IO.Class ( liftIO )
import Data.Foldable ( toList )
import Data.Fixed.Q ( UQ )
import Data.Loc ( noLoc )
import Data.Maybe ( fromMaybe )
import Data.Sequence ( Seq )
import Data.String ( fromString )
import Language.VHDL.Quote
import qualified Language.VHDL.Syntax as V
import System.Environment
import System.IO
import Text.PrettyPrint.Mainland
import Text.PrettyPrint.Mainland.Class

import Language.VHDL.Codegen.VExp ( VExp )
import Language.VHDL.Codegen.Pipeline
import Language.VHDL.Codegen.Monad

import Opt

main :: IO ()
main = do
    (conf, args) <- getArgs >>= compilerOpts
    when (not (help conf)) $ do
      (unit, p) <- evalCg $ mkIncrementer conf
      case output conf of
        Nothing   -> return ()
        Just path -> liftIO $ withFile path WriteMode $ \h -> hPutDoc h (ppr (toList unit))
      case tb_output conf of
        Nothing   -> return ()
        Just path -> do let unit = vunitTestbench "tb_incrementer" p
                        withFile path WriteMode $ \h -> hPutDoc h (ppr unit)

type M = 8
type F = 8

type N = UQ M F

mkIncrementer :: MonadCg m
              => Config
              -> m (Seq V.DesignUnit, Pipeline (VExp N) (VExp N))
mkIncrementer conf = withDesignUnit $ do
    p <- iter "incrementer"
              id
              (\(x :: VExp N) _ -> return $ x + 1)
              id
              ["x"]
              ["x"]
              16
    wrapP "main" p
  where
    iter = case variant conf of
             Serial -> siter
             Parallel -> piter

-- | Create a VUnit testbench module for a pipeline.
vunitTestbench :: V.Id
               -> Pipeline a b
               -> [V.DesignUnit]
vunitTestbench entity p = do
  [vfile|
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.fixed_pkg.all;

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

  constant m_I : typename integer_array_t := load_csv(tb_path & csv_i);
  constant m_O : typename integer_array_t := load_csv(tb_path & csv_o);

  signal clk : std_logic := '1';
  signal rst : std_logic := '0';
  signal start, done, compared : boolean := false;

  signal in_ready : std_logic;
  signal in_valid : std_logic;
  $decls:in_sigs

  signal out_ready : std_logic;
  signal out_valid : std_logic;
  $decls:out_sigs
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

  test_runner_watchdog(runner, 5000*clk_period);

  stimuli: process
    variable rnd : typename RandomPType;
    variable i : integer;
  begin
    rnd.InitSeed("common_seed");

    wait until start and rising_edge(clk);
    done <= false;
    wait until rising_edge(clk);

    i := 0;
    while i < height(m_I) loop
      $stms:ins

      wait for rnd.RandInt(0, 5)*clk_period;
      in_valid <= '1';

      wait until rising_edge(clk) and in_ready = '1';
      in_valid <= '0';

      -- info("in_x = " & to_string(get(m_I, i)) & " (" & to_string(i) & " of " & to_string(height(m_I)-1) & ")");
      $stms:in_msgs

      i := i + $(length (pipe_in p));
    end loop;

    wait until rising_edge(clk);
    done <= true;

    wait;
  end process;

  compare: process
    variable rnd : typename RandomPType;
    variable i : integer;
  begin
    compared <= false;

    wait until start and rising_edge(clk);

    i := 0;
    while i < height(m_O) loop
      out_ready <= '0';
      wait for rnd.RandInt(0, 5)*clk_period;
      out_ready <= '1';

      wait until rising_edge(clk) and out_valid = '1';

      -- info("out_x = " & to_string(to_integer(out_x)) & " (" & to_string(i) & " of " & to_string(height(m_O)-1) & ")");
      $stms:check_msgs

      $stms:checks

      i := i + $(length (pipe_out p));
    end loop;

    wait until rising_edge(clk);
    compared <= true;

    wait;
  end process;

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
    in_sigs, out_sigs :: [V.Decl]
    in_sigs  = [[vdecl|signal $id:v : $ty:tau;|] | (v, tau) <- pipe_in p]
    out_sigs = [[vdecl|signal $id:v : $ty:tau;|] | (v, tau) <- pipe_out p]

    ins :: [V.Stm]
    ins = zipWith go (pipe_in p) [0..]
      where
        go :: (V.Id, V.Subtype) -> Int -> V.Stm
        go (v, tau) i = [vstm|$id:v <= $e;|]
          where
            e = from_integer tau [vexp|get(m_I, i+$i)|]

    in_msgs :: [V.Stm]
    in_msgs = zipWith go (pipe_in p) [0..]
      where
        go :: (V.Id, V.Subtype) -> Int -> V.Stm
        go (v, tau) i = [vstm|info($lbl & to_string($e) & " (" & to_string(i+$i) & " of " & to_string(height(m_I)-1) & ")");|]
          where
            lbl = idName v ++ " = "
            e = from_integer tau [vexp|get(m_I, i+$i)|]

    checks :: [V.Stm]
    checks = zipWith go (pipe_out p) [0..]
      where
        go :: (V.Id, V.Subtype) -> Int -> V.Stm
        go (v, tau) i = [vstm|check_equal($e, get(m_O, i+$i), "Comparing output with reference");|]
          where
            e = to_integer tau (toExp v noLoc)

    check_msgs :: [V.Stm]
    check_msgs = zipWith go (pipe_out p) [0..]
      where
        go :: (V.Id, V.Subtype) -> Int -> V.Stm
        go (v, tau) i = [vstm|info($lbl & to_string($v) & " (" & to_string(i+$i) & " of " & to_string(height(m_O)-1) & ")");|]
          where
            lbl = idName v ++ " = "
            -- e = to_integer tau (toExp v noLoc)

    in_assocs, out_assocs :: [V.AssocElem]
    in_assocs = [[vassoc|$id:v => $id:v|] | (v, _) <- pipe_in p]
    out_assocs = [[vassoc|$id:v => $id:v|] | (v, _) <- pipe_out p]

    from_integer :: V.Subtype -> V.Exp -> V.Exp
    from_integer [vtype|ufixed ($m downto $f)|] e = [vexp|to_ufixed($e, $m, $f)|]
    from_integer [vtype|sfixed ($m downto $f)|] e = [vexp|to_sfixed($e, $m, $f)|]
    from_integer _ e = e

    to_integer :: V.Subtype -> V.Exp -> V.Exp
    to_integer _ e = [vexp|to_integer($e)|]

    idName :: V.Id -> String
    idName = prettyCompact . ppr
