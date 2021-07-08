{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module      :  Language.VHDL.Codegen.Trans
-- Copyright   :  (c) 2020-2021 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Language.VHDL.Codegen.Trans where

import Prelude

import Control.Monad.Exception (MonadException(..))
import Control.Monad.Extra (notM, whenM)
import Control.Monad.Fail (MonadFail)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Primitive (PrimMonad(..))
import Control.Monad.Ref (MonadRef(..))
import Control.Monad.State (MonadState(..),
                            StateT,
                            evalStateT,
                            gets,
                            modify,
                            runStateT)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Uniq (MonadUnique(..))
import Data.Foldable (toList)
import Data.IORef (IORef)
import Data.Loc (noLoc, srclocOf, srcspan)
import Data.Set (Set)
import Data.Sequence (Seq)
import Data.Sequence as Seq
import Data.Sequence ((|>))
import qualified Data.Set as Set
import Data.String ( fromString )
import Language.VHDL.Quote
import Language.VHDL.Syntax

import Language.VHDL.Codegen.Gensym
import Language.VHDL.Codegen.Lift

data CgState = CgState
    { -- | Current package
      pkg :: Id
    -- | Library clauses
    , library_clauses :: Set Name
      -- | Use clauses
    , use_clauses :: Set Name
      -- | Design unit context
    , context :: Seq Context
      -- | Design units
    , units :: Seq DesignUnit
      -- | Declarations
    , decls :: Seq Decl
      -- | Interface declarations
    , idecls :: Seq IDecl
      -- | Concurrent statements
    , cstms :: Seq CStm
      -- | Statements
    , stms :: Seq Stm
    }

defaultCgState :: CgState
defaultCgState = CgState
    { pkg             = "work"
    , library_clauses = mempty
    , use_clauses     = mempty
    , context         = mempty
    , units           = mempty
    , decls           = mempty
    , idecls          = mempty
    , cstms           = mempty
    , stms            = mempty
    }

-- | Code generation monad transformer.
newtype CgT m a = CgT { unCgT :: StateT CgState m a }
    deriving (Functor, Applicative, Monad, MonadFail, MonadIO,
              MonadException,
              MonadState CgState,
              MonadUnique)

instance MonadTrans CgT where
    lift = CgT . lift

deriving instance MonadRef IORef m => MonadRef IORef (CgT m)

instance PrimMonad m => PrimMonad (CgT m) where
    type PrimState (CgT m) = PrimState m
    primitive = CgT . primitive

-- | Type class for monads that support code generation.
class ( Monad m
      , MonadFail m
      , MonadIO m
      , MonadException m
      , MonadState CgState m
      , MonadUnique m) => MonadCg m where

-- | Evaluate a 'Cg' action.
evalCgT :: Monad m => CgT m a -> m a
evalCgT m = evalStateT (unCgT m) defaultCgState

-- | Execute a 'Cg' action.
runCgT :: Monad m => CgT m a -> m (a, CgState)
runCgT m = runStateT (unCgT m) defaultCgState

use :: MonadCg m => Name -> m ()
use n = do
    whenM (notM (gets $ \s -> n `elem` use_clauses s)) $ do
        modify $ \s -> s { use_clauses = Set.insert n (use_clauses s) }
        append $ UseC [n] noLoc

contextref :: MonadCg m => Name -> m ()
contextref n = do
    whenM (notM (gets $ \s -> n `elem` use_clauses s)) $ do
        modify $ \s -> s { use_clauses = Set.insert n (use_clauses s) }
        append $ ContextRefC [n] noLoc

library :: MonadCg m => Name -> m ()
library n =
    whenM (notM (gets $ \s -> n `elem` library_clauses s)) $ do
        modify $ \s -> s { library_clauses = Set.insert n (library_clauses s) }
        append $ LibC [n] noLoc

class Append a where
    append :: MonadCg m => a -> m ()

    appendList :: MonadCg m => [a] -> m ()
    appendList xs = mapM_ append xs

    appendSeq :: MonadCg m => Seq a -> m ()
    appendSeq xs = mapM_ append (toList xs)

instance Append a => Append [a] where
    append = appendList

instance Append a => Append (Seq a) where
    append = appendSeq

instance Append Context where
    append ctx = do
        maybe_idx <- gets $ \s -> Seq.elemIndexL ctx (context s)
        case maybe_idx of
            Nothing -> modify $ \s -> s { context = context s |> ctx }
            Just{}  -> return ()

instance Append DesignUnit where
    append (DesignUnit ctx decl l) = do
        ctx' <- gets context
        modify $ \s -> s { library_clauses = mempty
                         , use_clauses = mempty
                         , context = mempty
                         , units = units s |> DesignUnit (toList ctx' ++ ctx) decl l
                         }

instance Append Decl where
    append decl = modify $ \s -> s { decls = decls s |> decl }

instance Append IDecl where
    append idecl = modify $ \s -> s { idecls = idecls s |> idecl }

instance Append CStm where
    append cstm = modify $ \s -> s { cstms = cstms s |> cstm }

instance Append Stm where
    append stm = modify $ \s -> s { stms = stms s |> stm }

withDesignUnit :: MonadCg m => m a -> m (Seq DesignUnit, a)
withDesignUnit m = do
    s <- get
    put defaultCgState
    x <- m
    result <- gets units
    put s
    return (result, x)

withArchitecture :: MonadCg m => Id -> Name -> m a -> m a
withArchitecture ident n m = do
    old_state <- get
    modify $ \s -> s { decls = mempty
                     , cstms = mempty
                     }
    x <- m
    ds <- gets decls
    cs <- gets cstms
    modify $ \s -> s { decls = decls old_state
                     , cstms = cstms old_state
                     }
    append [vunit|architecture $id:ident of $name:n is $decls:(toList ds) begin $cstms:(toList cs) end $id:ident;|]
    return x

withProcess :: MonadCg m => [Name] -> m a -> m a
withProcess sensitivity m = do
    old_state <- get
    modify $ \s -> s { decls = mempty
                     , stms  = mempty
                     }
    x <- m
    ds <- gets decls
    ss <- gets stms
    modify $ \s -> s { decls = decls old_state
                     , stms  = stms old_state
                     }
    append [vcstm|process ($names:sensitivity)
                      $decls:(toList ds)
                  begin
                      $stms:(toList ss)
                  end process;|]
    return x

collectStms :: MonadCg m => m a -> m (Seq Stm, a)
collectStms m = do
    old_state <- get
    modify $ \s -> s { stms = mempty }
    x <- m
    ss <- gets stms
    modify $ \s -> s { stms = stms old_state }
    return (ss, x)

onRisingEdge :: MonadCg m => m a -> m a
onRisingEdge m = do
    (ss, x) <- collectStms m
    append [vstm|if rising_edge(clk) then
                   $stms:(toList ss)
                 end if;|]
    return x

forS :: MonadCg m
     => String
     -> DiscreteRange
     -> (Exp -> m ())
     -> m ()
forS i_name rng k = do
    i :: Id <- gensym i_name
    (ss, _) <- collectStms $ k [vexp|$id:i|]
    append [vstm|for $id:i in $range:rng loop
                   $stms:(toList ss)
                 end loop;|]

-- | Assign an expression to a variable
assign :: (ToName v, ToExp e, MonadCg m) => v -> e -> m ()
assign v e = append [vstm|$name:v := $cond:cond;|]
  where
    cond :: Conditional Exp
    cond = toConditionalE (toExp e noLoc)

-- | Assign an expression to a signal
sigassign :: (ToName v, ToExp e, MonadCg m) => v -> e -> m ()
sigassign v e0 = append [vstm|$name:v <= $cond:cond;|]
  where
    cond :: Conditional Waveform
    cond = fmap mkWaves (toConditionalE (toExp e0 noLoc))

    mkWaves :: Exp -> Waveform
    mkWaves e = [Wave (Just e) Nothing (srclocOf e)]

-- | Assign (concurrently) an expression to a signal
sigcassign :: (ToName v, ToExp e, MonadCg m) => v -> e -> m ()
sigcassign v e0 = append [vcstm|$name:v <= $cond:cond;|]
  where
    cond :: Conditional Waveform
    cond = fmap mkWaves (toConditionalE (toExp e0 noLoc))

    mkWaves :: Exp -> Waveform
    mkWaves e = [Wave (Just e) Nothing (srclocOf e)]

-- | Convert a (possibly conditional) expression to a @'Conditional' 'Exp'@.
toConditionalE :: Exp -> Conditional Exp
toConditionalE [vexp|ifte($c0, $th0, $el0)|] =
    ifteCond c0 (toConditionalE th0) (toConditionalE el0)
  where
    ifteCond :: Exp -> Conditional Exp -> Conditional Exp -> Conditional Exp
    ifteCond _ NilC{}             _   = error "can't happen"
    ifteCond _ AntiCond{}         _   = error "can't happen"
    ifteCond c (FinC e l)         th  = GuardC e c th (l `srcspan` th)
    ifteCond c (GuardC e c' th l) th' = GuardC e [vexp|$c and $c'|] th'' (l `srcspan` th'')
      where
        th'' = ifteCond c th th'

toConditionalE e = FinC e (srclocOf e)

-- The alternate implementations of assignment use if-then-else statements
-- instead of conditionals.

-- | Assign an expression to a variable
assign' :: (ToName v, ToExp e, MonadCg m) => v -> e -> m ()
assign' v e0 = go (toExp e0 noLoc)
  where
    go [vexp|ifte($c, $th, $el)|] = if c
                                    then assign v th
                                    else assign v el
    go e                          = append [vstm|$name:v := $e;|]

-- | Assign an expression to a signal
sigassign' :: (ToName v, ToExp e, MonadCg m) => v -> e -> m ()
sigassign' v e0 = go (toExp e0 noLoc)
  where
    go [vexp|ifte($c, $th, $el)|] = if c
                                    then sigassign v th
                                    else sigassign v el
    go e                          = append [vstm|$name:v <= $e;|]

-- | Add a variable declaration
var :: MonadCg m
    => Id        -- ^ Variable name
    -> Subtype   -- ^ Variable type
    -> Maybe Exp -- ^ Initial value
    -> m ()
var v tau Nothing    = append [vdecl|variable $id:v : $ty:tau;|]
var v tau (Just ini) = append [vdecl|variable $id:v : $ty:tau := $ini;|]

-- | Add a signal declaration
sig :: MonadCg m
    => Id        -- ^ Signal name
    -> Subtype   -- ^ Signal type
    -> Maybe Exp -- ^ Initial value
    -> m ()
sig v tau Nothing    = append [vdecl|signal $id:v : $ty:tau;|]
sig v tau (Just ini) = append [vdecl|signal $id:v : $ty:tau := $ini;|]

-- | Add signal declarations
sigs :: MonadCg m
     => [(Id, Subtype)] -- ^ Signal names and types
     -> m ()
sigs = mapM_ (\(v, tau) -> sig v tau Nothing)

-- | Generate a temporary variable
temp :: (ToType a, MonadCg m)
     => a      -- ^ Variable type
     -> String -- ^ Variable name
     -> m Id
temp a s = do
    sym <- gensym s
    append [videcl|variable $id:sym : $ty:tau|]
    return sym
  where
    tau :: Subtype
    tau = toType a noLoc

instance MonadCg m => IfThenElse Exp (m ()) where
    ifThenElse c t e = do
        (t_ss, _) <- collectStms t
        (e_ss, _) <- collectStms e
        append $ ifS c (toList t_ss) (toList e_ss)

instance MonadCg m => When Exp m where
    when c m = do
        (ss, _) <- collectStms m
        append [vstm|if $c then $stms:(toList ss) end if;|]
