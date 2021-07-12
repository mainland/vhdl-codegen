{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :  Language.VHDL.Codegen.Pipeline.VHDL
-- Copyright   :  (c) 2021 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Language.VHDL.Codegen.Pipeline.VHDL where

import Prelude hiding ((.))

import Control.Category (Category(..))
import Control.Monad (forM_,
                      zipWithM_)
import Control.Monad.State (gets)
import Data.Bit ( Bit )
import Data.Bits ( Bits(shift) )
import Data.Char (toLower)
import Data.Loc (noLoc)
import Data.Proxy ( Proxy(..) )
import Data.Sequence (Seq)
import Data.String (fromString)
import Data.Symbol
import GHC.TypeLits ( KnownNat, natVal )
import Language.VHDL.Quote
import Language.VHDL.Syntax as V

import Language.VHDL.Codegen.Gensym
import Language.VHDL.Codegen.Lift
import Language.VHDL.Codegen.Monad
import qualified Language.VHDL.Codegen.Pipeline as P
import Language.VHDL.Codegen.Pack
import Language.VHDL.Codegen.SLV ( SLV )
import Language.VHDL.Codegen.VExp

-- | A VHDL pipeline.
data Pipeline a b where
  Pipeline :: { pipe_context :: Seq V.Context
              , pipe_entity  :: V.Id
              , pipe_in      :: [(V.Id, V.Subtype)]
              , pipe_out     :: [(V.Id, V.Subtype)]
              } -> Pipeline a b

  IdP :: Pipeline a a

  SeqP :: Pipeline a b -> Pipeline b c -> Pipeline a c

  ArrP :: (Pack a, Pack b) => (a -> b) -> Pipeline a b

  SiterP :: (Pack a, Pack b, Pack c)
         => (a -> b)
         -> (b -> VExp Int -> b)
         -> (b -> c)
         -> Int
         -> Pipeline a c

  PiterP :: (Pack a, Pack b, Pack c)
         => (a -> b)
         -> (b -> VExp Int -> b)
         -> (b -> c)
         -> Int
         -> Pipeline a c

  MooreP :: (Pack s, Pack i, Pack o)
         => (s -> i -> s)
         -> (s -> o)
         -> s
         -> Pipeline i o

  MealyP :: (Pack s, Pack i, Pack o)
         => (s -> i -> (s, o))
         -> s
         -> Pipeline i o

instance Category Pipeline where
  id  = IdP
  (.) = flip SeqP

instance P.Pipeline Pipeline where
  seq   = SeqP
  arr   = ArrP
  siter = SiterP
  piter = PiterP
  moore = MooreP
  mealy = MealyP

-- | Flatten a value of type 'Pipeline a b' so that it consist of a 'Pipeline'
-- data constructor.
flattenP :: forall a b m . MonadCg m => Pipeline a b -> m (Pipeline a b)
flattenP p@Pipeline{} = return p

-- XXX This is the price we pay for making 'Pipeline' an instance of 'Category'.
-- Without a 'Pack a' constraint, we can't construct the identity pipeline
-- explicitly, but adding such a constraint to either the 'IdP' or 'SeqP' data
-- constructors  would prevent us from writing the 'Category' instance.
flattenP IdP = fail "Cannot flatten identity pipeline in isolation"

flattenP (SeqP IdP p) = flattenP p
flattenP (SeqP p IdP) = flattenP p

flattenP (SeqP p1_ p2_) = do
    p1 <- flattenP p1_
    p2 <- flattenP p2_

    let in_idecls  = [[videcl|$id:v : in  $ty:tau|] | (v, tau) <- pipe_in p1]
        out_idecls = [[videcl|$id:v : out $ty:tau|] | (v, tau) <- pipe_out p2]
        internal_sigs = [(internalize v, tau) | (v, tau) <- pipe_out p1]

    append (pipe_context p1)
    append (pipe_context p2)

    entity <- gensym "seq"
    append [vunit|
      entity $id:entity is
        port (clk : in std_logic;
              rst : in std_logic;
              in_ready : out std_logic;
              in_valid : in std_logic;
              $idecls:in_idecls;
              out_ready : in std_logic;
              out_valid : out std_logic;
              $idecls:out_idecls);
      end;|]

    withArchitecture "behavioral" (toName entity noLoc) $ do
        append [vdecl|signal internal_ready : std_logic;|]
        append [vdecl|signal internal_valid : std_logic;|]
        append [[vdecl|signal $id:v : $ty:tau;|] | (v, tau) <- internal_sigs]
        work <- gets pkg
        let in_assocs  = map (id_assoc . fst) (pipe_in p1)
            p1_assocs = zipWith (\(v1, _) (v2, _) -> [vassoc|$id:v1 => $id:v2|]) (pipe_out p1) internal_sigs
            p2_assocs = zipWith (\(v1, _) (v2, _) -> [vassoc|$id:v1 => $id:v2|]) (pipe_in p2) internal_sigs
            out_assocs = map (id_assoc . fst) (pipe_out p2)

        seq1 :: V.Id <- gensym "seqa"
        append [vcstm|$id:seq1: entity $id:work.$id:(pipe_entity p1)
                       port map(clk => clk,
                                rst => rst,
                                in_ready => in_ready,
                                in_valid => in_valid,
                                $assocs:in_assocs,
                                out_ready => internal_ready,
                                out_valid => internal_valid,
                                $assocs:p1_assocs);|]

        seq2 :: V.Id <- gensym "seqb"
        append [vcstm|$id:seq2: entity $id:work.$id:(pipe_entity p2)
                       port map(clk => clk,
                                rst => rst,
                                in_ready => internal_ready,
                                in_valid => internal_valid,
                                $assocs:p2_assocs,
                                out_ready => out_ready,
                                out_valid => out_valid,
                                $assocs:out_assocs);|]
    return $ Pipeline { pipe_context = pipe_context p1 <> pipe_context p2
                      , pipe_entity = entity
                      , pipe_in = pipe_in p1
                      , pipe_out = pipe_out p2
                      }
  where
    id_assoc :: V.Id -> V.AssocElem
    id_assoc v = [vassoc|$id:v => $id:v|]

    internalize :: V.Id -> V.Id
    internalize = mapId go
      where
        go :: String -> String
        go ('o' : 'u' : 't' : '_' : cs) = "internal_" ++ cs
        go cs = "internal_" ++ cs

flattenP (ArrP f :: Pipeline a b) = do
    (_ :: a, in_vars)  <- genPack ("x" : ["x" ++ show i | i <- [1::Int ..]])
    (_ :: b, out_vars) <- genPack ("y" : ["y" ++ show i | i <- [1::Int ..]])

    let in_idecls  = [[videcl|$id:(in_ v) : in  $ty:tau|] | (v, tau) <- in_vars]
        out_idecls = [[videcl|$id:(out_ v) : out $ty:tau|] | (v, tau) <- out_vars]

    addTypeContext [vtype|std_logic|]
    mapM_ addTypeContext [tau | (_, tau) <- in_vars]
    mapM_ addTypeContext [tau | (_, tau) <- out_vars]

    entity <- gensym "arr"
    append [vunit|
      entity $id:entity is
        port (clk : in std_logic;
              rst : in std_logic;
              in_ready : out std_logic;
              in_valid : in std_logic;
              $idecls:in_idecls;
              out_ready : in std_logic;
              out_valid : out std_logic;
              $idecls:out_idecls);
      end;|]
    withArchitecture "behavioral" (toName entity noLoc) $ do
        append [vdecl|signal valid : std_logic := '0';|]
        mapM_ (\(v, tau) -> sig v tau Nothing) out_vars
        withProcess ["clk"] $ do
            onRisingEdge $ do
                if [vexp|rst = '1'|]
                  then reset
                  else do
                    when [vexp|out_valid and out_ready|] $
                      append [vstm|valid <= '0';|]

                    when [vexp|in_valid and in_ready|] $ do
                      arg <- pack [(in_ v, tau) | (v, tau) <- in_vars]
                      zipWithM_ sigassign [v | (v, _) <- out_vars] (unpack (f arg))
                      append [vstm|valid <= '1';|]

        append [vcstm|in_ready <= out_ready or not valid;|]
        append [vcstm|out_valid <= valid;|]
        append [[vcstm|$id:(out_ v) <= $id:v;|] | (v, _) <- out_vars]

    return $ Pipeline { pipe_context = mempty
                      , pipe_entity = entity
                      , pipe_in = [(in_ v, tau) | (v, tau) <- in_vars]
                      , pipe_out = [(out_ v, tau) | (v, tau) <- out_vars]
                      }
  where
    reset :: m ()
    reset = append [vstm|valid <= '0';|]

flattenP (SiterP f g h n) = do
    entity <- gensym "siter"
    siter entity f (\x i -> pure $ g x i) h [] [] n

flattenP (PiterP f g h n) = do
    entity <- gensym "piter"
    piter entity f (\x i -> pure $ g x i) h [] [] n

flattenP (MooreP step out state0) = do
    entity <- gensym "moore"
    moore entity step out state0

flattenP (MealyP step state0) = do
    entity <- gensym "mealy"
    mealy entity step state0

-- | Construct the identity pipeline.
identityP :: forall a m . (Pack a, MonadCg m) => m (Pipeline a a)
identityP = do
    (_ :: a, vars) <- genPack ("x" : ["x" ++ show i | i <- [1::Int ..]])

    let in_idecls  = [[videcl|$id:(in_ v) : in  $ty:tau|] | (v, tau) <- vars]
        out_idecls = [[videcl|$id:(out_ v) : out $ty:tau|] | (v, tau) <- vars]

    addTypeContext [vtype|std_logic|]
    mapM_ addTypeContext [tau | (_, tau) <- vars]

    entity <- gensym "id"
    append [vunit|
      entity $id:entity is
        port (clk : in std_logic;
              rst : in std_logic;
              in_ready : out std_logic;
              in_valid : in std_logic;
              $idecls:in_idecls;
              out_ready : in std_logic;
              out_valid : out std_logic;
              $idecls:out_idecls);
      end;|]

    withArchitecture "behavioral" (toName entity noLoc) $ do
        append [vcstm|in_ready <= out_ready;|]
        append [vcstm|out_valid <= in_valid;|]
        append [[vcstm|$id:(out_ v) <= $id:(in_ v);|] | (v, _) <- vars]

    return $ Pipeline { pipe_context = mempty
                      , pipe_entity = entity
                      , pipe_in = [(in_ v, tau) | (v, tau) <- vars]
                      , pipe_out = [(out_ v, tau) | (v, tau) <- vars]
                      }

wrapP :: MonadCg m => V.Id -> Pipeline a b -> m (Pipeline a b)
wrapP entity p@Pipeline{} = do
    let in_idecls  = [[videcl|$id:v : in  $ty:tau|] | (v, tau) <- pipe_in p]
        out_idecls = [[videcl|$id:v : out $ty:tau|] | (v, tau) <- pipe_out p]

    append (pipe_context p)

    append [vunit|
      entity $id:entity is
        port (clk : in std_logic;
              rst : in std_logic;
              in_ready : out std_logic;
              in_valid : in std_logic;
              $idecls:in_idecls;
              out_ready : in std_logic;
              out_valid : out std_logic;
              $idecls:out_idecls);
      end;|]

    withArchitecture "behavioral" (toName entity noLoc) $ do
      work <- gets pkg
      let assocs = map (\(v, _tau) -> [vassoc|$id:v => $id:v|]) (pipe_in p ++ pipe_out p)
      append [vcstm|$id:(pipe_entity p): entity $id:work.$id:(pipe_entity p)
                    port map(clk => clk,
                             rst => rst,
                             in_ready => in_ready,
                             in_valid => in_valid,
                             out_ready => out_ready,
                             out_valid => out_valid,
                             $assocs:assocs);|]

    return $ Pipeline { pipe_context = pipe_context p
                      , pipe_entity = entity
                      , pipe_in = pipe_in p
                      , pipe_out = pipe_out p
                      }

wrapP entity p = flattenP p >>= wrapP entity

-- | Calculate integral log base 2. Fails if argument is not equal to 2^i for
-- some integer i.
ilog2 :: forall a m . (Num a, Ord a, Bits a, Monad m)
      => a
      -> m Int
ilog2 x = go 0
  where
    go :: Int -> m Int
    go i | x < pow2  = fail "No integer log"
         | x == pow2 = return i
         | otherwise = go (i+1)
      where
        pow2 = 1 `shift` i

--- | Return VHDL expression that is true when upper bound has been reached.
atUpperBound :: VExp Int  -- ^ Loop index
             -> Int       -- ^ Loop upper bound
             -> VExp Bool
atUpperBound i u = case ilog2 u of
                     Nothing -> VExp [vexp|$i = $u|]
                     Just y  -> VExp [vexp|funname to_unsigned($i, $(y+1))($y) = '1'|]

-- | Perform serial iteration with a function.
siter :: forall a b c m . (Pack a, Pack b, Pack c, MonadCg m)
      => Id                     -- ^ Name of VHDL entity
      -> (a -> b)               -- ^ Pre-processing function
      -> (b -> VExp Int -> m b) -- ^ Function to iterate
      -> (b -> c)               -- ^ Post-processing function
      -> [String]               -- ^ (Optional) signal names
      -> [String]               -- ^ (Optional) state names
      -> Int                    -- ^ Number of iterations to perform
      -> m (Pipeline a c)
siter entity f g h names snames n = do
    (_ :: a, in_vars)    <- genPack names
    (x :: b, state_vars) <- genPack snames
    (_ :: c, out_vars)   <- genPack names

    addTypeContext [vtype|natural|]
    addTypeContext [vtype|unsigned|]
    addTypeContext [vtype|std_logic|]
    mapM_ addTypeContext [tau | (_, tau) <- in_vars]
    mapM_ addTypeContext [tau | (_, tau) <- state_vars]
    mapM_ addTypeContext [tau | (_, tau) <- out_vars]

    let in_idecls  = [[videcl|$id:(in_ v) : in $ty:tau|]   | (v, tau) <- in_vars]
        out_idecls = [[videcl|$id:(out_ v) : out $ty:tau|] | (v, tau) <- out_vars]

    ctx <- gets context
    append [vunit|
      entity $id:entity is
        port (clk : in std_logic;
              rst : in std_logic;
              in_ready : out std_logic;
              in_valid : in std_logic;
              $idecls:in_idecls;
              out_ready : in std_logic;
              out_valid : out std_logic;
              $idecls:out_idecls);
      end;|]

    withArchitecture "behavioral" (toName entity noLoc) $ do
        -- Declare loop counter and state
        sig "i" [vtype|natural range 0 to $n|] (Just [vexp|0|])
        mapM_ (\(v, tau) -> sig v tau Nothing) state_vars

        let i :: VExp Int
            i = VExp [vexp|i|]

        -- Update loop index
        withProcess ["clk"] $ do
            onRisingEdge $ do
                if [vexp|rst = '1'|] then
                  append [vstm|i <= 0;|]
                else if [vexp|(i > 0 and i < $n) or (i = 0 and in_valid = '1')|] then
                  append [vstm|i <= i + 1;|]
                else when [vexp|i = $n and out_ready = '1'|] $
                  append [vstm|i <= 0;|]

        -- Update state
        withProcess ["clk"] $ do
            onRisingEdge $ do
                when [vexp|i = 0 and in_valid = '1'|] $ do
                  x_pre  <- pack [(in_ v, tau)| (v, tau) <- in_vars]
                  result <- unpack <$> g (f x_pre) 0
                  zipWithM_ sigassign (map fst state_vars) result

                -- We can take a step when in_valid is high and we are at
                -- the initial step or when we have remaining subsequent
                -- steps to take.
                when [vexp|i > 0 and i < $n|] $ do
                  result <- unpack <$> g x (VExp [vexp|i|])
                  zipWithM_ sigassign (map fst state_vars) result

        -- Update in_ready signal
        append [vcstm|in_ready <= '1' when (i = 0) else
                                  '0';|]

        -- Update out_valid signal
        append [vcstm|out_valid <= '1' when $(atUpperBound i n) else
                                   '0';|]

        -- Update state
        z <- pack state_vars
        zipWithM_ sigcassign [out_ v | (v, _) <- out_vars] (unpack (h z))

    return Pipeline { pipe_context = ctx
                    , pipe_entity  = entity
                    , pipe_in      = [(in_ v, tau)  | (v, tau) <- in_vars]
                    , pipe_out     = [(out_ v, tau) | (v, tau) <- out_vars]
                    }

-- | Perform parallel iteration with a function. This version will squeeze out
-- pipeline bubbles.
piter :: forall a b c m . (Pack a, Pack b, Pack c, MonadCg m)
      => Id                     -- ^ Name of VHDL entity
      -> (a -> b)               -- ^ Pre-processing function
      -> (b -> VExp Int -> m b) -- ^ Function to iterate
      -> (b -> c)               -- ^ Post-processing function
      -> [String]               -- ^ (Optional) signal names
      -> [String]               -- ^ (Optional) state names
      -> Int                    -- ^ Number of iterations to perform
      -> m (Pipeline a c)
piter entity f g h names snames n = do
    (_ :: a, in_vars)    <- genPack names
    (_ :: b, state_vars) <- genPack snames
    (_ :: c, out_vars)   <- genPack names

    addTypeContext [vtype|std_logic|]
    mapM_ addTypeContext [tau | (_, tau) <- in_vars]
    mapM_ addTypeContext [tau | (_, tau) <- state_vars]
    mapM_ addTypeContext [tau | (_, tau) <- out_vars]

    let in_idecls  = [[videcl|$id:(in_ v) : in $ty:tau|]   | (v, tau) <- in_vars]
        out_idecls = [[videcl|$id:(out_ v) : out $ty:tau|] | (v, tau) <- out_vars]

    ctx <- gets context
    append [vunit|
      entity $id:entity is
        port (clk : in std_logic;
              rst : in std_logic;
              in_ready : out std_logic;
              in_valid : in std_logic;
              $idecls:in_idecls;
              out_ready : in std_logic;
              out_valid : out std_logic;
              $idecls:out_idecls);
      end;|]
    withArchitecture "behavioral" (toName entity noLoc) $ do
        sig "valid" [vtype|std_logic_vector($(n-1) downto 0)|] Nothing
        sig "ready" [vtype|std_logic_vector($(n-1) downto 0)|] Nothing

        forM_ state_vars $ \(v, tau) -> do
          let array_t = mapId (++ "_t") v
          append [vdecl|type $id:array_t is array ($(n-1) downto 0) of $ty:tau;|]
          sig v [vtype|typename $id:array_t|] Nothing

        withProcess ["clk"] $ do
            onRisingEdge $ do
                if [vexp|rst = '1'|]
                  then reset
                  else do
                    when [vexp|arrname ready(0)|] $ do
                      x <- pack [(in_ v, tau)| (v, tau) <- in_vars]
                      result <- unpack <$> g (f x) 0
                      zipWithM_ (\(v, _) e -> sigassign [vname|arrname $id:v(0)|] e) state_vars result
                      append [vstm|valid(0) <= in_valid;|]

                    forS "i" [vrange|1 to $(n-1)|] $ \i -> do
                        when [vexp|arrname ready($i)|] $ do
                          x <- pack (map (\(v, tau) -> ([vexp|arrname $id:v($i-1)|], tau)) state_vars)
                          result <- unpack <$> g x (VExp i)
                          zipWithM_ (\(v, _) e -> sigassign [vname|arrname $id:v($i)|] e) state_vars result
                          append [vstm|valid($i) <= arrname valid($i-1);|]

        append [vcstm|ready($(n-1)) <= out_ready or not valid($(n-1));|]

        append [vcstm|gen_ready:
                      for i in $(n-2) downto 0 generate
                        begin
                          ready(i) <= ready(i+1) or not valid(i);
                        end;
                      end generate;|]

        append [vcstm|in_ready <= ready(0);|]

        z <- pack [([vexp|arrname $id:v($(n-1))|], tau) | (v, tau) <- state_vars]
        zipWithM_ (\(v, _) e -> sigcassign [vname|$id:(out_ v)|] e) out_vars (unpack (h z))

        append [vcstm|out_valid <= valid(valid'high);|]
    return Pipeline { pipe_context = ctx
                    , pipe_entity  = entity
                    , pipe_in      = [(in_ v, tau)  | (v, tau) <- in_vars]
                    , pipe_out     = [(out_ v, tau) | (v, tau) <- out_vars]
                    }
  where
    reset :: m ()
    reset = do
        append [vstm|valid <= (others => '0');|]
        append [vstm|ready <= (others => '1');|]

-- | Perform parallel iteration with a function. This version will /not/ squeeze
-- out pipeline bubbles.
piter' :: forall a b c m . (Pack a, Pack b, Pack c, MonadCg m)
       => Id                     -- ^ Name of VHDL entity
       -> (a -> b)               -- ^ Pre-processing function
       -> (b -> VExp Int -> m b) -- ^ Function to iterate
       -> (b -> c)               -- ^ Post-processing function
       -> [String]               -- ^ (Optional) signal names
       -> [String]               -- ^ (Optional) state names
       -> Int                    -- ^ Number of iterations to perform
       -> m (Pipeline a c)
piter' entity f g h names snames n = do
    (_ :: a, in_vars)    <- genPack names
    (_ :: b, state_vars) <- genPack snames
    (_ :: c, out_vars)   <- genPack names

    addTypeContext [vtype|std_logic|]
    mapM_ addTypeContext [tau | (_, tau) <- in_vars]
    mapM_ addTypeContext [tau | (_, tau) <- state_vars]
    mapM_ addTypeContext [tau | (_, tau) <- out_vars]

    let in_idecls  = [[videcl|$id:(in_ v) : in $ty:tau|]   | (v, tau) <- in_vars]
        out_idecls = [[videcl|$id:(out_ v) : out $ty:tau|] | (v, tau) <- out_vars]

    ctx <- gets context
    append [vunit|
      entity $id:entity is
        port (clk : in std_logic;
              rst : in std_logic;
              in_ready : out std_logic;
              in_valid : in std_logic;
              $idecls:in_idecls;
              out_ready : in std_logic;
              out_valid : out std_logic;
              $idecls:out_idecls);
      end;|]
    withArchitecture "behavioral" (toName entity noLoc) $ do
        sig "valid" [vtype|std_logic_vector($(n-1) downto 0)|] Nothing

        forM_ state_vars $ \(v, tau) -> do
          let array_t = mapId (++ "_t") v
          append [vdecl|type $id:array_t is array ($(n-1) downto 0) of $ty:tau;|]
          sig v [vtype|typename $id:array_t|] Nothing

        withProcess ["clk"] $ do
            onRisingEdge $ do
                if [vexp|rst = '1'|]
                  then reset
                  else when [vexp|out_ready|] $ do
                    do x <- pack [(in_ v, tau)| (v, tau) <- in_vars]
                       result <- unpack <$> g (f x) 0
                       zipWithM_ (\(v, _) e -> sigassign [vname|arrname $id:v(0)|] e) state_vars result

                    forS "i" [vrange|1 to $(n-1)|] $ \i -> do
                      x <- pack (map (\(v, tau) -> ([vexp|arrname $id:v($i-1)|], tau)) state_vars)
                      result <- unpack <$> g x (VExp i)
                      zipWithM_ (\(v, _) e -> sigassign [vname|arrname $id:v($i)|] e) state_vars result

                    append [vstm|valid <= valid(valid'high - 1 downto valid'low) & in_valid;|]

        append [vcstm|in_ready <= out_ready;|]

        z <- pack [([vexp|arrname $id:v($(n-1))|], tau) | (v, tau) <- state_vars]
        zipWithM_ (\(v, _) e -> sigcassign [vname|$id:(out_ v)|] e) out_vars (unpack (h z))

        append [vcstm|out_valid <= valid(valid'high);|]
    return Pipeline { pipe_context = ctx
                    , pipe_entity  = entity
                    , pipe_in      = [(in_ v, tau)  | (v, tau) <- in_vars]
                    , pipe_out     = [(out_ v, tau) | (v, tau) <- out_vars]
                    }
  where
    reset :: m ()
    reset = append [vstm|valid <= (others => '0');|]

-- | Construct a Moore machine pipeline stage with state @s@, input @i@, and
-- output @o@.
moore :: forall s i o m . (Pack s, Pack i, Pack o, MonadCg m)
      => Id            -- ^ Name of VHDL entity
      -> (s -> i -> s) -- ^ Moore machine state transfer function
      -> (s -> o)      -- ^ Moore machine output function
      -> s             -- ^ Initial state
      -> m (Pipeline i o)
moore entity step out state0 = do
    (state :: s, state_vars) <- genPack ([] :: [String])
    (_ :: i, in_vars)        <- genPack ([] :: [String])
    (_ :: o, out_vars)       <- genPack ([] :: [String])

    addTypeContext [vtype|std_logic|]
    mapM_ addTypeContext [tau | (_, tau) <- state_vars]
    mapM_ addTypeContext [tau | (_, tau) <- in_vars]
    mapM_ addTypeContext [tau | (_, tau) <- out_vars]

    let in_idecls  = [[videcl|$id:(in_ v) : in $ty:tau|]   | (v, tau) <- in_vars]
        out_idecls = [[videcl|$id:(out_ v) : out $ty:tau|] | (v, tau) <- out_vars]

    ctx <- gets context
    append [vunit|
      entity $id:entity is
        port (clk : in std_logic;
              rst : in std_logic;
              in_ready : out std_logic;
              in_valid : in std_logic;
              $idecls:in_idecls;
              out_ready : in std_logic;
              out_valid : out std_logic;
              $idecls:out_idecls);
      end;|]

    withArchitecture "behavioral" (toName entity noLoc) $ do
        -- Declare state
        sigs state_vars

        -- Declare (state) valid flag
        sig "valid" [vtype|std_logic|] Nothing

        -- Update state
        withProcess ["clk"] $ do
            onRisingEdge $ do
                if [vexp|rst = '1'|] then do
                  zipWithM_ sigassign (map fst state_vars) (unpack state0)
                  append [vstm|valid <= '0';|]
                else if [vexp|in_ready = '1' and in_valid = '1'|] then do
                  x <- pack [(in_ v, tau)| (v, tau) <- in_vars]
                  let state' = unpack $ step state x
                  zipWithM_ sigassign (map fst state_vars) state'
                  append [vstm|valid <= '1';|]
                else when [vexp|out_ready = '1'|] $
                  append [vstm|valid <= '0';|]

        -- Update in_ready signal
        append [vcstm|in_ready <= '1' when (valid = '0' or out_ready = '1') else
                                  '0';|]

        -- Update out_valid signal
        append [vcstm|out_valid <= valid;|]

        -- Update output
        zipWithM_ sigcassign [out_ v | (v, _) <- out_vars] (unpack (out state))

    return Pipeline { pipe_context = ctx
                    , pipe_entity  = entity
                    , pipe_in      = [(in_ v, tau)  | (v, tau) <- in_vars]
                    , pipe_out     = [(out_ v, tau) | (v, tau) <- out_vars]
                    }

-- | Construct a Mealy machine pipeline stage with state @s@, input @i@, and
-- output @o@.
mealy :: forall s i o m . (Pack s, Pack i, Pack o, MonadCg m)
      => Id                 -- ^ Name of VHDL entity
      -> (s -> i -> (s, o)) -- ^ Mealy machine state transfer function
      -> s                  -- ^ Initial state
      -> m (Pipeline i o)
mealy entity step state0 = do
    (state :: s, state_vars) <- genPack ([] :: [String])
    (_ :: i, in_vars)        <- genPack ([] :: [String])
    (_ :: o, out_vars)       <- genPack ([] :: [String])

    addTypeContext [vtype|std_logic|]
    mapM_ addTypeContext [tau | (_, tau) <- state_vars]
    mapM_ addTypeContext [tau | (_, tau) <- in_vars]
    mapM_ addTypeContext [tau | (_, tau) <- out_vars]

    let in_idecls  = [[videcl|$id:(in_ v) : in $ty:tau|]   | (v, tau) <- in_vars]
        out_idecls = [[videcl|$id:(out_ v) : out $ty:tau|] | (v, tau) <- out_vars]

    ctx <- gets context
    append [vunit|
      entity $id:entity is
        port (clk : in std_logic;
              rst : in std_logic;
              in_ready : out std_logic;
              in_valid : in std_logic;
              $idecls:in_idecls;
              out_ready : in std_logic;
              out_valid : out std_logic;
              $idecls:out_idecls);
      end;|]

    withArchitecture "behavioral" (toName entity noLoc) $ do
        -- Declare state
        sigs state_vars

        -- Declare (state) valid flag
        sig "valid" [vtype|std_logic|] Nothing

        -- Update state
        withProcess ["clk"] $ do
            onRisingEdge $ do
                if [vexp|rst = '1'|] then do
                  zipWithM_ sigassign (map fst state_vars) (unpack state0)
                  append [vstm|out_valid <= '0';|]
                  append [vstm|in_ready <= '1';|]
                else if [vexp|in_ready = '1' and in_valid = '1'|] then do
                  x <- pack [(in_ v, tau)| (v, tau) <- in_vars]
                  let (state', out) = step state x
                  zipWithM_ sigassign (map fst state_vars) (unpack state')
                  zipWithM_ sigassign [out_ v | (v, _) <- out_vars] (unpack out)
                  append [vstm|out_valid <= '1';|]
                  append [vstm|in_ready <= out_ready;|]
                else when [vexp|out_ready = '1'|] $ do
                  append [vstm|out_valid <= '0';|]
                  append [vstm|in_ready <= '1';|]

    return Pipeline { pipe_context = ctx
                    , pipe_entity  = entity
                    , pipe_in      = [(in_ v, tau)  | (v, tau) <- in_vars]
                    , pipe_out     = [(out_ v, tau) | (v, tau) <- out_vars]
                    }

-- | Serialize std_logic_vector elements
serialize :: forall n m . (KnownNat n, MonadCg m)
          => m (Pipeline (VExp (SLV n)) (VExp Bit))
serialize = do
    entity <- gensym "serialize"

    (_ :: VExp (SLV n), state_vars) <- genPack (["state"] :: [String])
    (_ :: VExp (SLV n), in_vars)    <- genPack (["slv"] :: [String])
    (_ :: VExp Bit, out_vars)       <- genPack (["bit"] :: [String])

    addTypeContext [vtype|natural|]
    addTypeContext [vtype|unsigned|]
    addTypeContext [vtype|std_logic|]
    addTypeContext [vtype|std_logic_vector|]

    let in_idecls  = [[videcl|$id:(in_ v) : in $ty:tau|] | (v, tau) <- in_vars]
        out_idecls = [[videcl|$id:(out_ v) : out $ty:tau|] | (v, tau) <- out_vars]

    ctx <- gets context
    append [vunit|
      entity $id:entity is
        port (clk : in std_logic;
              rst : in std_logic;
              in_ready : out std_logic;
              in_valid : in std_logic;
              $idecls:in_idecls;
              out_ready : in std_logic;
              out_valid : out std_logic;
              $idecls:out_idecls);
      end;|]

    withArchitecture "behavioral" (toName entity noLoc) $ do
        -- Declare loop counter and state
        sig "i" [vtype|natural range 0 to $(n-1)|] Nothing
        sig "valid" [vtype|std_logic|] Nothing
        mapM_ (\(v, tau) -> sig v tau Nothing) state_vars

        -- Update loop index
        withProcess ["clk"] $ do
            onRisingEdge $ do
                if [vexp|rst = '1'|] then do
                  append [vstm|i <= 0;|]
                  append [vstm|valid <= '0';|]
                else if [vexp|valid = '1' and out_ready = '1'|] then
                  if [vexp|i = $(n-1)|] then do
                    append [vstm|i <= 0;|]
                    if [vexp|in_valid = '1'|] then
                      zipWithM_ sigassign (map fst state_vars) [in_ v | (v, _) <- in_vars]
                    else
                      append [vstm|valid <= '0';|]
                  else
                    append [vstm|i <= i + 1;|]
                else when [vexp|valid = '0' and in_valid = '1'|] $ do
                  zipWithM_ sigassign (map fst state_vars) [in_ v | (v, _) <- in_vars]
                  append [vstm|valid <= '1';|]

        -- Update in_ready signal
        append [vcstm|in_ready <= '1' when valid = '0' or (i = $int:(n-1) and out_ready = '1') else
                                  '0';|]

        -- Update out_valid signal
        append [vcstm|out_valid <= valid;|]

        -- Update output
        append [vcstm|out_bit <= arrname state(i);|]

    return Pipeline { pipe_context = ctx
                    , pipe_entity  = entity
                    , pipe_in      = [(in_ v, tau)  | (v, tau) <- in_vars]
                    , pipe_out     = [(out_ v, tau) | (v, tau) <- out_vars]
                    }
  where
    n :: Int
    n = fromIntegral (natVal (Proxy :: Proxy n))

-- | Add necessary context for a type.
addTypeContext :: MonadCg m => V.Subtype -> m ()
addTypeContext [vtype|signed|] = do
  library "ieee"
  use "ieee.numeric_std.all"

addTypeContext [vtype|unsigned|] = do
  library "ieee"
  use "ieee.numeric_std.all"

addTypeContext [vtype|std_logic|] = do
  library "ieee"
  use "ieee.std_logic_1164.all"

addTypeContext [vtype|std_logic_vector|] = do
  library "ieee"
  use "ieee.std_logic_1164.all"

addTypeContext [vtype|std_ulogic|] = do
  library "ieee"
  use "ieee.std_logic_1164.all"

addTypeContext [vtype|std_ulogic_vector|] = do
  library "ieee"
  use "ieee.std_logic_1164.all"

addTypeContext [vtype|ufixed ($_ downto $_)|] = do
  library "ieee"
  use "ieee.fixed_float_types.all"
  use "ieee.fixed_pkg.all"

addTypeContext [vtype|sfixed ($_ downto $_)|] = do
  library "ieee"
  use "ieee.fixed_float_types.all"
  use "ieee.fixed_pkg.all"

addTypeContext _ = return ()

-- | Create a skid buffer pipeline.
skid :: forall a m . (Pack a, MonadCg m) => m (Pipeline a a)
skid = do
    entity <- gensym "skid"

    (_ :: a, vars) <- genPack ([] :: [String])

    let in_idecls  = [[videcl|$id:(in_ v) : in $ty:tau|]   | (v, tau) <- vars]
        out_idecls = [[videcl|$id:(out_ v) : out $ty:tau|] | (v, tau) <- vars]

    addTypeContext [vtype|std_logic|]
    mapM_ addTypeContext [tau | (_, tau) <- vars]

    ctx <- gets context
    append [vunit|
      entity $id:entity is
        port (clk : in std_logic;
              rst : in std_logic;
              in_ready : out std_logic;
              in_valid : in std_logic;
              $idecls:in_idecls;
              out_ready : in std_logic;
              out_valid : out std_logic;
              $idecls:out_idecls);
      end;|]
    withArchitecture "behavioral" (toName entity noLoc) $ do
        sig valid [vtype|std_logic|] Nothing
        sig skid_valid [vtype|std_logic|] Nothing

        forM_ vars $ \(v, tau) -> do
          sig v tau Nothing
          sig (skid_ v) tau Nothing

        withProcess ["clk"] $ do
            onRisingEdge $ do
                if [vexp|rst = '1'|]
                  then reset
                  else do
                    when [vexp|in_ready = '1'|] $ do
                      append [vstm|$id:valid <= in_valid;|]
                      mapM_ (\(v, _tau) -> append [vstm|$id:v <= $id:(in_ v);|]) vars

                      when [vexp|out_ready = '0'|] $ do
                        append [vstm|$id:skid_valid <= $id:valid;|]
                        mapM_ (\(v, _tau) -> append [vstm|$id:(skid_ v) <= $id:v;|]) vars

                    when [vexp|out_ready = '1'|] $
                      append [vstm|$id:skid_valid <= '0';|]

        append [vcstm|in_ready <= not $id:skid_valid;|]

        append [vcstm|out_valid <= $id:valid or $id:skid_valid;|]
        mapM_ (\(v, _tau) -> append [vcstm|$id:(out_ v) <= $id:(skid_ v) when $id:skid_valid else $id:v;|]) vars

    return Pipeline { pipe_context = ctx
                    , pipe_entity  = entity
                    , pipe_in      = [(in_ v, tau)  | (v, tau) <- vars]
                    , pipe_out     = [(out_ v, tau) | (v, tau) <- vars]
                    }
  where
    valid, skid_valid :: V.Id
    valid = "valid"
    skid_valid = "skid_valid"

    skid_ :: V.Id -> V.Id
    skid_ = mapId ("skid_" ++)

    reset :: m ()
    reset = do
        append [vstm|$id:valid <= '0';|]
        append [vstm|$id:skid_valid <= '0';|]

in_, out_ :: V.Id -> V.Id
in_  = mapId ("in_" ++)
out_ = mapId ("out_" ++)

-- | Map a function over a VHDL identifier
mapId :: (String -> String) -> V.Id -> V.Id
mapId f = go
  where
      go :: V.Id -> V.Id
      go (V.Id (V.NoCase s _) l) = V.Id (V.NoCase (intern s') (intern (map toLower s'))) l
        where
          s' = f (unintern s)
      go (V.ExtId s l)           = V.ExtId (intern (f (unintern s))) l
      go ident@V.AntiId{}        = ident
