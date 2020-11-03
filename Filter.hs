{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Filter
  ( SiteFilter
  , parserSiteFilter
  , filterRules
  ) where

import           Control.Monad (forM_)
import           Data.Default (Default(def))
import           Data.Maybe (catMaybes)
import qualified Data.Text as T

import Util
import JSON
import qualified Data.ListMap as LM
import qualified Data.BitSet as BS
import qualified Data.EnumVec as EV
import JS (quoteRegExp)
import ContentFilter
import Domain
import URLMap

data FilterType
  = FilterBlock
  | FilterBlockThird
  | FilterBlockCookies
  | FilterAllowFirst
  | FilterAllow
  deriving (Eq, Ord, Enum, Bounded, Show)

filterName :: FilterType -> T.Text
filterName FilterBlock        = "block"
filterName FilterBlockThird   = "block-third"
filterName FilterBlockCookies = "block-cookies"
filterName FilterAllowFirst   = "allow-first"
filterName FilterAllow        = "allow"

type FilterMap = EV.EnumVec FilterType
type ResourceSet = BS.BitSet ResourceType

data SiteFilter = SiteFilter
  { siteFilters :: FilterMap (URLMap ResourceSet)
  , siteHide :: URLMap T.Text
  }

makeLenses' ''SiteFilter

instance Default SiteFilter where
  def = SiteFilter (EV.set FilterBlock (LM.singleton [] BS.full) mempty) mempty

instance Semigroup SiteFilter where
  SiteFilter r1 h1 <> SiteFilter r2 h2 =
    SiteFilter (r1 <> r2) (LM.unionWith ((<>) . (<> ", ")) h1 h2)

instance Monoid SiteFilter where
  mempty = SiteFilter mempty mempty

parserSiteFilter :: ObjectParser SiteFilter
parserSiteFilter = do
  forM_ (enumFromTo minBound maxBound) $ \a ->
    (siteFilters' . EV.lens a) .<> filterName a
  siteHide' .<> "hide"

type SiteFilters = DomainMap SiteFilter

data Filters = Filters
  { filters :: URLMap (DomainMap (FilterMap ResourceSet))
  , filtersHide :: DomainMap (URLMap T.Text)
  }

permuteFilters :: SiteFilters -> Filters
permuteFilters sf = Filters
  { filters = LM.fromListWith (LM.unionWith EV.union)
    [ (u, LM.singleton d (EV.set a v mempty))
    | (d, SiteFilter{ siteFilters = am }) <- LM.toList sf
    , (a, um) <- EV.assocList am
    , (u, v) <- LM.toList um
    ] 
  , filtersHide = fmap siteHide sf
  }

urlFilter :: URL -> T.Text
urlFilter (Domain []) = "."
urlFilter d = "^[^:]+:(//)?([^/:]*\\.)?" <> quoteRegExp (joinDomain d) <> "[/:]"

domainFilter :: Domain -> DomainList
domainFilter (Domain []) = DomainUnless []
domainFilter d = DomainIf [d]

filterRule :: URL -> Domain -> FilterType -> ResourceSet -> Maybe ContentRule
filterRule u d f r
  | BS.null r = Nothing
  | otherwise = Just $ ContentRule (urlFilter u) (if BS.isFull r then [] else BS.toList r) (domainFilter d) lt a
  where
  (lt, a) = case f of
    FilterBlock -> (Nothing, ActionBlock)
    FilterBlockThird -> (Just LoadThird, ActionBlock)
    FilterBlockCookies -> (Nothing, ActionBlockCookies)
    FilterAllowFirst -> (Just LoadFirst, ActionIgnorePrevious)
    FilterAllow -> (Nothing, ActionIgnorePrevious)

hideRule :: URL -> Domain -> T.Text -> ContentRule
hideRule u d = ContentRule (urlFilter u) [] (domainFilter d) Nothing . ActionDisplayNone

filterRules :: SiteFilters -> [ContentRule]
filterRules df = catMaybes
  [ filterRule (Domain u) (Domain d) a rm
  | (u, dm) <- LM.toList fm
  , (d, am) <- LM.toList dm
  , (a, rm) <- EV.assocList am
  ] ++
  [ hideRule (Domain u) (Domain d) h
  | (d, um) <- LM.toList hm
  , (u, h) <- LM.toList um
  ]
  where
  Filters fm hm = permuteFilters df
