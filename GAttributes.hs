{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module GAttributes 
  ( gAttributeList
  ) where

import qualified Data.GI.Base.Attributes as GA
-- import qualified Data.GI.Base.Overloading as GO
import           Data.Proxy (Proxy(..))
import           GHC.TypeLits (Symbol, KnownSymbol, symbolVal)

data GAttributeInfo = GAttributeInfo
  { gAttributeAllowedOps :: [GA.AttrOpTag]
  }

class Demote a b where
  demote :: proxy a -> b

instance KnownSymbol s => Demote (s :: Symbol) String where
  demote = symbolVal

instance Demote '[] [a] where
  demote _ = []

instance (Demote a b, Demote l [b]) => Demote (a ': l) [b] where
  demote _ = demote (Proxy :: Proxy a) : demote (Proxy :: Proxy l)

instance (Demote a b, Demote c d) => Demote '(a, c) (b, d) where
  demote _ = (demote (Proxy :: Proxy a), demote (Proxy :: Proxy c))

instance Demote 'GA.AttrGet       GA.AttrOpTag where demote _ = GA.AttrGet
instance Demote 'GA.AttrSet       GA.AttrOpTag where demote _ = GA.AttrSet
instance Demote 'GA.AttrConstruct GA.AttrOpTag where demote _ = GA.AttrConstruct
instance Demote 'GA.AttrClear     GA.AttrOpTag where demote _ = GA.AttrClear

instance {- GA.AttrInfo a => -} Demote a GAttributeInfo where
  demote _ = GAttributeInfo
    { gAttributeAllowedOps = [] -- demote (Proxy :: Proxy (GA.AttrAllowedOps a :: [GA.AttrOpTag]))
    }

gAttributeList :: Demote a [(String, GAttributeInfo)] => Proxy a -> [String]
gAttributeList = map (fst :: (String, GAttributeInfo) -> String) . demote
