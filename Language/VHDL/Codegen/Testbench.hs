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

module Language.VHDL.Codegen.Testbench (
  TextIO(..),
  VUnit(..)
) where

import Prelude hiding ( id )

import Control.Monad.Fail (MonadFail)
import Control.Monad.State (MonadState,
                            StateT,
                            evalStateT,
                            get,
                            put)
import Data.Fixed.Q ( Q, UQ )
import Data.Proxy ( Proxy(..) )
import Data.String ( fromString )
import GHC.TypeLits ( KnownNat, natVal )
import Language.VHDL.Quote ( ToExp(..), vexp, vtype )
import qualified Language.VHDL.Syntax as V

import Language.VHDL.Codegen.VExp ( VExp )

-- | A type that can be used with the VHDL std.textio library.
class TextIO a where
  -- | VHDL types used to perform IO with an unpacked value of type 'a'.
  ioType :: Proxy a -> [V.Subtype]

  -- | Convert unpacked textio values to unpacked value of type 'a'
  fromTextIO :: (ToExp e, MonadFail m) => Proxy a -> [e] -> m [V.Exp]
  fromTextIO p es = evalStateT (fromTextIOM p) es

  -- | Convert unpacked value of type 'a' to unpacked textio values
  toTextIO :: (ToExp e, MonadFail m) => Proxy a -> [e] -> m [V.Exp]
  toTextIO p es = evalStateT (toTextIOM p) es

  -- | Monadic version of fromTextIO.
  fromTextIOM :: (ToExp e, MonadFail m) => Proxy a -> StateT [e] m [V.Exp]

  -- | Monadic version of toTextIO.
  toTextIOM :: (ToExp e, MonadFail m) => Proxy a -> StateT [e] m [V.Exp]

-- | A type that can be used with the VUnit library
class TextIO a => VUnit a where
  -- | Convert unpacked value of type 'a' to values that can be compared with
  -- VUnit
  toVUnitCompare :: (ToExp e, MonadFail m) => Proxy a -> [e] -> m [V.Exp]
  toVUnitCompare p es = evalStateT (toVUnitCompareM p) es

  -- | Monadic version of toVUnitCompareM.
  toVUnitCompareM :: (ToExp e, MonadFail m) => Proxy a -> StateT [e] m [V.Exp]

-- | Consume the first element of the list that is the monad's state.
consume :: (MonadState [e] m, MonadFail m) => m e
consume = do
    es <- get
    case es of
      e:es' -> do put es'
                  return e
      _ -> fail "empty state"

instance (TextIO a, TextIO b) => TextIO (a, b) where
  ioType _ = ioType (Proxy :: Proxy a) ++ ioType (Proxy :: Proxy b)

  fromTextIOM _ = (++) <$> fromTextIOM (Proxy :: Proxy a) <*> fromTextIOM (Proxy :: Proxy b)

  toTextIOM _ = (++) <$> toTextIOM (Proxy :: Proxy a) <*> toTextIOM (Proxy :: Proxy b)

instance (VUnit a, VUnit b) => VUnit (a, b) where
  toVUnitCompareM _ = (++) <$> toVUnitCompareM (Proxy :: Proxy a) <*> toVUnitCompareM (Proxy :: Proxy b)

instance (KnownNat m, KnownNat f) => TextIO (VExp (UQ m f)) where
  ioType _ | f == 0    = [[vtype|integer|]]
           | otherwise = [[vtype|real|]]
    where
      f :: Integer
      f = natVal (Proxy :: Proxy f)

  fromTextIOM _ = do
      e <- consume
      return [[vexp|to_ufixed($e, $(m-1), $(-f))|]]
    where
      m, f :: Integer
      m = natVal (Proxy :: Proxy m)
      f = natVal (Proxy :: Proxy f)

  toTextIOM _ = do
      e <- consume
      return [[vexp|to_real($e)|]]

instance (KnownNat m, KnownNat f) => VUnit (VExp (UQ m f)) where
  toVUnitCompareM _ = do
      e <- consume
      return [[vexp|to_slv($e)|]]

instance (KnownNat m, KnownNat f) => TextIO (VExp (Q m f)) where
  ioType _ | f == 0    = [[vtype|integer|]]
           | otherwise = [[vtype|real|]]
    where
      f :: Integer
      f = natVal (Proxy :: Proxy f)

  fromTextIOM _ = do
      e <- consume
      return [[vexp|to_sfixed($e, $(m), $(-f))|]]
    where
      m, f :: Integer
      m = natVal (Proxy :: Proxy m)
      f = natVal (Proxy :: Proxy f)

  toTextIOM _ = do
      e <- consume
      return [[vexp|to_real($e)|]]

instance (KnownNat m, KnownNat f) => VUnit (VExp (Q m f)) where
  toVUnitCompareM _ = do
      e <- consume
      return [[vexp|to_slv($e)|]]

