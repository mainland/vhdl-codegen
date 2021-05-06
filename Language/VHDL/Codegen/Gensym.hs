{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

-- |
-- Module      :  Language.VHDL.Codegen.Gensym
-- Copyright   :  (c) 2014-2021 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@cs.drexel.edu

module Language.VHDL.Codegen.Gensym (
    Gensym(..)
  ) where

import Control.Monad.Uniq (MonadUnique(newUnique),
                           Uniq(Uniq))
import Data.Loc (Located,
                 Loc,
                 noLoc,
                 srclocOf)
import Data.String (fromString)
import Data.Symbol (Symbol,
                    intern,
                    unintern)

import qualified Language.VHDL.Syntax as V (NoCase(..),
                                            Id(..))

-- | A type that can be gensym'd.
class Gensym a where
    -- | Gensym a symbol using the given string as a basis.
    gensym :: MonadUnique m => String -> m a
    gensym s = gensymAt s (noLoc :: Loc)

    -- | Gensym a symbol using the given string and location as a basis.
    gensymAt :: (MonadUnique m, Located l) => String -> l -> m a
    gensymAt s _ = gensym s

    -- | Ensure the symbol is unique
    uniquify :: MonadUnique m => a -> m a

suffix :: Int -> String
suffix u = '_' : show u

instance Gensym String where
    gensymAt s _ = do
        Uniq u <- newUnique
        return $ if u == 0 then s else s ++ suffix u

    uniquify s = do
        Uniq u <- newUnique
        return $ if u == 0 then s else s ++ suffix u

instance Gensym Symbol where
    gensymAt s _ = do
        Uniq u <- newUnique
        return $ if u == 0 then intern s else intern (s ++ suffix u)

    uniquify s = do
        Uniq u <- newUnique
        return $ if u == 0 then s else intern (unintern s ++ suffix u)

instance Gensym V.NoCase where
    gensymAt s _ = do
        Uniq u <- newUnique
        return $ fromString $ if u == 0 then s else s ++ suffix u

    uniquify nc@(V.NoCase sym _) = do
        Uniq u <- newUnique
        return $ if u == 0 then nc else fromString (unintern sym ++ suffix u)

instance Gensym V.Id where
    gensymAt s l =
        V.Id <$> gensymAt s l <*> pure (srclocOf l)

    uniquify (V.Id s l) =
        V.Id <$> uniquify s <*> pure l

    uniquify cid =
        return cid
