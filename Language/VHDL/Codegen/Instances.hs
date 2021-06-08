{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Module      :  Language.VHDL.Codegen.Instances
-- Copyright   :  (c) 2020-2021 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Language.VHDL.Codegen.Instances () where

import Data.Bits
import Data.Fixed.Q ( Q, UQ )
import Data.Proxy ( Proxy(..) )
import GHC.TypeLits ( KnownNat, type (+), natVal )
import Language.VHDL.Quote
import qualified Language.VHDL.Syntax as V
import Text.PrettyPrint.Mainland
import Text.PrettyPrint.Mainland.Class

instance (KnownNat m, KnownNat f, KnownNat (m + f)) => Pretty (UQ m f) where
    ppr = text . show

instance (KnownNat m, KnownNat f, KnownNat (m + f)) => Pretty (Q m f) where
    ppr = text . show

bits :: Bits a => a -> String
bits x
  | n == 1    = if testBit x 0 then "'1'" else "'0'"
  | otherwise = quote $ map (\i -> if testBit x i then '1' else '0') [n-1, n-2..0]
  where
    Just n = bitSizeMaybe x

    quote s = '"' : s ++ ['"']

instance ToType (Proxy Bool) where
    toType _ _ = [vtype|std_logic|]

instance (KnownNat m, KnownNat f) => ToType (Proxy (UQ m f)) where
    toType _ _ = [vtype|ufixed($int:(m-1) downto $int:(-f))|]
      where
        m = natVal (Proxy :: Proxy m)
        f = natVal (Proxy :: Proxy f)

instance (KnownNat m, KnownNat f) => ToType (Proxy (Q m f)) where
    toType _ _ = [vtype|sfixed($int:m downto $int:(-f))|]
      where
        m = natVal (Proxy :: Proxy m)
        f = natVal (Proxy :: Proxy f)

instance (KnownNat m, KnownNat f) => ToLit (Q m f) where
    toLit x loc = V.BitStringLit (bits x) loc

instance (KnownNat m, KnownNat f) => ToLit (UQ m f) where
    toLit x loc = V.BitStringLit (bits x) loc

instance (KnownNat m, KnownNat f) => ToExp (Q m f) where
    toExp n _ = resize [vexp|std_logic_vector'($lit:n)|]
      where
        resize e = [vexp|to_sfixed($e, $int:(m-1), $int:(-f))|]
          where
            m, f :: Integer
            m = natVal (Proxy :: Proxy m)
            f = natVal (Proxy :: Proxy f)

instance (KnownNat m, KnownNat f) => ToExp (UQ m f) where
    toExp n _ = resize [vexp|std_logic_vector'($lit:n)|]
      where
        resize e = [vexp|to_ufixed($e, $int:(m-1), $int:(-f))|]
          where
            m, f :: Integer
            m = natVal (Proxy :: Proxy m)
            f = natVal (Proxy :: Proxy f)
