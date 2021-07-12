{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :  Language.VHDL.Codegen.VExp.Vec
-- Copyright   :  (c) 2020-2021 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Language.VHDL.Codegen.VExp.Vec where

import Prelude
import qualified Prelude as P

import Data.Finite ( finite )
import Data.List ( foldl1' )
import Data.Loc ( noLoc )
import Data.Proxy ( Proxy(..) )
import Data.Type.Equality ( type (:~:)(..) )
import qualified Data.Vector.Sized as S
import GHC.TypeLits ( KnownNat, type (+), natVal )
import Language.VHDL.Quote ( vexp, ToExp(..) )
import qualified Language.VHDL.Syntax as V

import Language.VHDL.Codegen.Nat ( isZeroNat )
import Language.VHDL.Codegen.VExp
    ( Lift(lift), VExp(VExp, VConst) )
import Language.VHDL.Codegen.Vec (Vec)

-- | Reify a value of type @'Vec' n ('VExp' a)@ as a value of type @'Vec' n a@.
-- Only possible if all elements of the input vector are constants.
reifyVec :: forall n a m. (KnownNat n, Monad m)
         => Vec n (VExp a)
         -> m (Vec n a)
reifyVec v
    | S.all isConst v = return $ S.map unConst v
    | otherwise       = fail "reifyVec: non-constant vector elements"
  where
    isConst :: VExp a -> Bool
    isConst VConst{} = True
    isConst _        = False

    unConst :: VExp a -> a
    unConst (VConst k) = k
    unConst _          = error "unConst: not a constant"

-- | Lift a value of type @'Vec' n ('VExp' a)@ to a value of type @'VExp' ('Vec'
-- n a)@. If all elements on the input vector are constants, then the result
-- will also be a compile time constant.
liftVec :: forall n a. (KnownNat n, ToExp (S.Vector n a))
        => Vec n (VExp a)
        -> VExp (Vec n a)
liftVec v = case reifyVec v of
              Just v' -> VConst v'
              Nothing -> case isZeroNat @n of
                           Just Refl -> VConst S.empty
                           Nothing   -> VExp $ toExp (foldl1' (\e1 e2 -> VExp [vexp|$e2 & $e1|]) es) noLoc
  where
    es :: [VExp a]
    es = S.toList v

length :: forall n a . KnownNat n => VExp (Vec n a) -> VExp Int
length _ = VConst (fromIntegral (natVal (Proxy :: Proxy n)))

index :: (KnownNat n, ToExp a) => VExp (Vec n a) -> VExp Int -> VExp a
index (VConst v) (VConst i) = VConst $ S.index v (finite (fromIntegral i))
index v i | V.VarE n _ <- e = VExp [vexp|arrname $name:n($i)|]
          | otherwise       = error $ "Cannot index: " P.++ show e
  where
    e :: V.Exp
    e = toExp v noLoc

head :: (KnownNat n, ToExp a) => VExp (Vec (n+1) a) -> VExp a
head (VConst v)          = VConst $ S.head v
head v | V.VarE n _ <- e = VExp [vexp|arrname $name:n(0)|]
       | otherwise       = error $ "Cannot index: " P.++ show e
  where
    e :: V.Exp
    e = toExp v noLoc

last :: (KnownNat n, ToExp a) => VExp (Vec (n+1) a) -> VExp a
last (VConst v)          = VConst $ S.last v
last v | V.VarE n _ <- e = VExp [vexp|arrname $name:n($name:(n)'length-1)|]
       | otherwise       = error $ "Cannot index: " P.++ show e
  where
    e :: V.Exp
    e = toExp v noLoc

init :: (KnownNat n, ToExp (Vec n a)) => VExp (Vec (n+1) a) -> VExp (Vec n a)
init (VConst v)          = VConst $ S.init v
init v | V.VarE n _ <- e = VExp [vexp|arrname $name:n($name:(n)'length-2 downto 0)|]
       | otherwise       = error $ "Cannot index: " P.++ show e
  where
    e :: V.Exp
    e = toExp v noLoc

tail :: (KnownNat n, ToExp (Vec n a)) => VExp (Vec (n+1) a) -> VExp (Vec n a)
tail (VConst v)          = VConst $ S.tail v
tail v | V.VarE n _ <- e = VExp [vexp|arrname $name:n($name:(n)'length-1 downto 1)|]
       | otherwise       = error $ "Cannot index: " P.++ show e
  where
    e :: V.Exp
    e = toExp v noLoc

cons :: (KnownNat n, ToExp (Vec (1+n) a)) => VExp a -> VExp (Vec n a) -> VExp (Vec (1+n) a)
cons (VConst x) (VConst xs) = VConst $ S.cons x xs
cons x          xs          = VExp [vexp|$xs & $x|]

snoc :: (KnownNat n, ToExp (Vec (n+1) a)) => VExp (Vec n a) -> VExp a -> VExp (Vec (n+1) a)
snoc (VConst xs) (VConst x) = VConst $ S.snoc xs x
snoc xs          x          = VExp [vexp|$x & $xs|]

infixr 5 ++

(++) :: (KnownNat n, ToExp (Vec (n+m) a)) => VExp (Vec n a) -> VExp (Vec m a) -> VExp (Vec (n+m) a)
VConst xs ++ VConst ys = VConst $ xs S.++ ys
xs        ++ ys        = VExp [vexp|$ys & $xs|]

replicate :: forall n a. (KnownNat n, ToExp (S.Vector n a))
          => VExp a
          -> VExp (Vec n a)
replicate (VConst x) = VConst $ S.replicate x
replicate x          = VExp $ foldl1' (\e1 e2 -> [vexp|$e1 & $e2|]) (P.replicate n (toExp x noLoc))
  where
    n :: Int
    n = fromIntegral (natVal (Proxy :: Proxy n))

map :: forall n a b. (KnownNat n, ToExp a, ToExp (S.Vector n b))
    => (VExp a -> VExp b)
    -> VExp (Vec n a)
    -> VExp (Vec n b)
map f (VConst v) = liftVec $ S.map (f . VConst) v
map f v          = case isZeroNat @n of
                     Just Refl -> VConst S.empty
                     Nothing   -> VExp $ toExp (foldl1' (\e1 e2 -> VExp [vexp|$e2 & $e1|]) es) noLoc
  where
    n :: Int
    n = fromIntegral (natVal (Proxy :: Proxy n))

    es :: [VExp b]
    es = [f (index v (lift i)) | i <- [n-1,n-2..0]]
