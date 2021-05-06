{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module      :  Language.VHDL.Codegen.Monad
-- Copyright   :  (c) 2020-2021 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Language.VHDL.Codegen.Monad (
    module Language.VHDL.Codegen.Monad,
    module Language.VHDL.Codegen.Trans
  ) where

import Control.Monad.Exception (MonadException(..))
import Control.Monad.Fail (MonadFail)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader(..),
                             ReaderT,
                             asks,
                             runReaderT)
import Control.Monad.Ref (MonadRef(..))
import Control.Monad.State (MonadState(..))
import Control.Monad.Uniq (MonadUnique(..),
                           Uniq(..))
import Data.IORef (IORef)

import Language.VHDL.Codegen.Trans

data CgEnv = CgEnv { uniq :: !(IORef Int) }

defaultCgEnv :: MonadRef IORef m => m CgEnv
defaultCgEnv = do
    r <- newRef 0
    return CgEnv { uniq = r }

-- | A concrete code generation monad.
newtype Cg a = Cg { unCg :: ReaderT CgEnv (CgT IO) a }
    deriving (Functor, Applicative, Monad, MonadFail, MonadIO,
              MonadException,
              MonadReader CgEnv,
              MonadState CgState,
              MonadRef IORef,
              MonadCg)

runCg :: Cg a -> IO (a, CgState)
runCg m = runCgT (defaultCgEnv >>= runReaderT (unCg m))

evalCg :: Cg a -> IO a
evalCg m = evalCgT (defaultCgEnv >>= runReaderT (unCg m))

instance MonadUnique Cg where
    newUnique = do
        r <- asks uniq
        u <- readRef r
        let u' = u + 1
        u' `seq` writeRef r u'
        return $ Uniq u

    resetUnique = do
        r <- asks uniq
        writeRef r 0
