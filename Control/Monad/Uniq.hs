{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

-- |
-- Module      :  Control.Monad.Uniq
-- Copyright   :  (c) 2014-2021 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Control.Monad.Uniq (
    Uniq(..),
    MonadUnique(..)
  ) where

import Control.Monad.Except (ExceptT(..))
import Control.Monad.Exception (ExceptionT(..))
import Control.Monad.Reader (ReaderT(..))
import Control.Monad.State (StateT(..))
import qualified Control.Monad.State.Strict as S (StateT(..))
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Cont (ContT(..))
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Writer (WriterT(..))
import qualified Control.Monad.Writer.Strict as S (WriterT(..))
import Text.PrettyPrint.Mainland.Class (Pretty(ppr))

-- | A unique value.
newtype Uniq = Uniq Int
  deriving (Eq, Ord, Read, Show)

instance Pretty Uniq where
    ppr (Uniq u) = ppr u

-- | A monad that can generate unique values.
class Monad m => MonadUnique m where
    -- | Generate a new unique.
    newUnique :: m Uniq

    -- | Reset the unique counter.
    resetUnique :: m ()

instance MonadUnique m => MonadUnique (MaybeT m) where
    newUnique   = lift newUnique
    resetUnique = lift resetUnique

instance MonadUnique m => MonadUnique (ContT r m) where
    newUnique   = lift newUnique
    resetUnique = lift resetUnique

instance MonadUnique m => MonadUnique (ExceptT e m) where
    newUnique   = lift newUnique
    resetUnique = lift resetUnique

instance MonadUnique m => MonadUnique (ExceptionT m) where
    newUnique   = lift newUnique
    resetUnique = lift resetUnique

instance MonadUnique m => MonadUnique (ReaderT r m) where
    newUnique   = lift newUnique
    resetUnique = lift resetUnique

instance MonadUnique m => MonadUnique (StateT s m) where
    newUnique   = lift newUnique
    resetUnique = lift resetUnique

instance MonadUnique m => MonadUnique (S.StateT s m) where
    newUnique   = lift newUnique
    resetUnique = lift resetUnique

instance (Monoid w, MonadUnique m) => MonadUnique (WriterT w m) where
    newUnique   = lift newUnique
    resetUnique = lift resetUnique

instance (Monoid w, MonadUnique m) => MonadUnique (S.WriterT w m) where
    newUnique   = lift newUnique
    resetUnique = lift resetUnique
