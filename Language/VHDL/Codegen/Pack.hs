{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :  Language.VHDL.Codegen.Pack
-- Copyright   :  (c) 2021 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Language.VHDL.Codegen.Pack where

import Control.Monad.Fail (MonadFail)
import Control.Monad.State (MonadState,
                            StateT,
                            evalStateT,
                            get,
                            put)
import Control.Monad.Uniq
import Control.Monad.Writer (WriterT,
                             runWriterT,
                             tell)
import Data.Loc (noLoc)
import Data.Proxy
import Language.VHDL.Quote
import Language.VHDL.Syntax as V

import Language.VHDL.Codegen.Gensym
import Language.VHDL.Codegen.Trans
import Language.VHDL.Codegen.VExp

-- | A package of VHDL values.
class Pack a where
    -- | Generate a packaged VHDL value and associated flat list of VHDL
    -- bindings for the components of the packaged VHDL value.
    genPack :: (MonadCg m, ToId v) => [v] -> m (a, [(V.Id, V.Subtype)])
    genPack vs = evalStateT (runWriterT genPackM) vs

    -- | Pack a list of VHDL expressions into a packages VHDL value.
    pack :: (ToExp e, MonadFail m) => [(e, V.Subtype)] -> m a
    pack vs = evalStateT packM vs

    -- | Unpack a package of VHDL values to a list of VHDL expressions.
    unpack :: a -> [V.Exp]

    -- | Monad version of genPack.
    genPackM :: (MonadCg m, ToId v) => WriterT [(V.Id, V.Subtype)] (StateT [v] m) a

    -- | Monad version of pack.
    packM :: (ToExp e, MonadFail m) => StateT [(e, V.Subtype)] m a

-- | Consume a single variable name or generate a temporary name.
getVar :: (ToId v, MonadState [v] m, MonadUnique m) => m V.Id
getVar = do
    vs <- get
    case vs of
      v:vs' -> do put vs'
                  return $ toId v noLoc
      _ -> gensym "temp"

-- | Consume the first element of the list that is the monad's state.
consume :: (MonadState [v] m, MonadFail m) => m v
consume = do
    vs <- get
    case vs of
      v:vs' -> do put vs'
                  return v
      _ -> fail "empty state"

instance ToType (Proxy a) => Pack (VExp a) where
    genPackM = do
        sym <- getVar
        tell [(sym, tau)]
        return (VExp [vexp|$id:sym|])
      where
        tau :: V.Subtype
        tau = toType (undefined :: Proxy a) noLoc

    packM = do
        (e, _tau) <- consume
        return (VExp [vexp|$e|])

    unpack e = [toExp e noLoc]

instance (Pack a, Pack b) => Pack (a, b) where
    genPackM = (,) <$> genPackM <*> genPackM

    packM = (,) <$> packM <*> packM

    unpack (x, y) = unpack x <> unpack y

instance (Pack a, Pack b, Pack c) => Pack (a, b, c) where
    genPackM = (,,) <$> genPackM <*> genPackM <*> genPackM

    packM = (,,) <$> packM <*> packM <*> packM

    unpack (x, y, z) = unpack x <> unpack y <> unpack z
