{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :  Main
-- Copyright   :  (c) 2020 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Main where

import Data.Bits
import Data.Fixed.Q
import Data.Foldable (toList)
import Data.Loc ( noLoc )
import Data.Proxy
import GHC.TypeLits
import Language.VHDL.Quote
import qualified Language.VHDL.Syntax as V
import Text.PrettyPrint.Mainland.Class ( pprint )

import Language.VHDL.Codegen.Instances ()
import Language.VHDL.Codegen.Monad

import CORDIC

main :: IO ()
main = evalCg $ do
    (file, _) <- withDesignUnit $ do
        library "ieee"
        use "ieee.std_logic_1164"
        use "ieee.fixed_pkg.all"
        let foo = [vtype|std_logic_vector(3 downto -$int:(2*5))|]
        pprint foo
        cordicgen (Circular :: Coordinates (Q 1 14)) Rotation (-2) 17
    pprint $ toList file

cordicgen :: forall a . (ToType (Proxy a), ToLit a, Ord a, GenShift a)
          => Coordinates a
          -> Mode
          -> Int
          -> Int
          -> Cg ()
cordicgen coord mode i0 n = do
    library "ieee"
    use "ieee.std_logic_1164"
    use "ieee.fixed_pkg.all"
    append [vunit|
      entity cordic is
        port (clk : in std_logic;
              ce : in std_logic;
              in_ready : out std_logic;
              in_valid : in std_logic;
              in_x : in $ty:tau;
              in_y : in $ty:tau;
              out_ready : in std_logic;
              out_valid : out std_logic;
              z_out : out $ty:tau);
      end cordic;|]
    append [vunit|
        architecture behavioral of cordic is
            type rom_type is array (0 to $int:n-1) of $ty:tau;

            signal ws : rom_type := ($lits:(take n (zks coord i0) :: [a])
            );
        begin
            data <= to_integer(unsigned(addr));
        end behavioral;
    |]
  where
    tau :: V.Subtype
    tau = toType (Proxy :: Proxy a) noLoc
