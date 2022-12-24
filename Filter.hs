{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Filter
  ( ResourceType(..)
  , resourceTypeName
  , FilterType(..)
  , filterName
  , SiteFilter
  , parserSiteFilter
  , Filters
  , buildFilters
  , lookupFilter
  , setFilter
  , filterRules
  ) where

import           Control.Monad (forM_)
import           Data.Default (Default(def))
import           Data.Foldable (fold)
import           Data.List (sort)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import           Data.String (IsString)

import Util
import JSON
import qualified Data.ListMap as LM
import qualified Data.BitSet as BS
import qualified Data.EnumVec as EV
import qualified Data.PrefixMap as PM
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

instance Semigroup FilterType where
  (<>) = min

filterName :: IsString s => FilterType -> s
filterName FilterBlock        = "block"
filterName FilterBlockThird   = "block-third"
filterName FilterBlockCookies = "block-cookies"
filterName FilterAllowFirst   = "allow-first"
filterName FilterAllow        = "allow"

type ResourceSet = BS.BitSet ResourceType

data SiteFilter = SiteFilter
  { siteFilters :: EV.EnumVec FilterType (URLMap ResourceSet)
  , siteHide :: URLMap T.Text
  }

makeLenses' ''SiteFilter

instance Default SiteFilter where
  def = SiteFilter (EV.insert FilterBlock (LM.singleton [] BS.full) mempty) mempty

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
  { filters :: URLMap (DomainMap (EV.EnumVec ResourceType (Maybe FilterType)))
  , filtersHide :: DomainMap (URLMap T.Text)
  }

buildFilters :: SiteFilters -> Filters
buildFilters sf = Filters
  { filters = LM.fromListWith (LM.unionWith EV.union)
    [ (u, LM.singleton d (EV.insert r (Just a) mempty))
    | (d, SiteFilter{ siteFilters = am }) <- LM.toList sf
    , (a, um) <- EV.assocList am
    , (u, rs) <- LM.toList um
    , r <- BS.toList rs
    ] 
  , filtersHide = fmap siteHide sf
  }

lookupFilter :: Filters -> URL -> Domain -> ResourceType -> Maybe FilterType
lookupFilter f (Domain u) (Domain d) r = EV.lookup r =<<
  LM.lookup d =<<
  LM.lookup u (filters f)

setFilter :: URL -> Domain -> ResourceType -> Maybe FilterType -> Filters -> Filters
setFilter (Domain u) (Domain d) r a f = f
  { filters = LM.alter (Just . maybe
        (LM.singleton d $ EV.insert r a mempty)
        (LM.alter (Just . EV.insert r a . fold) d))
      u (filters f) }

data FilterLine = FilterLine
  { filterType :: !FilterType
  , filterResources :: !ResourceSet
  , filterURL :: URL
  , filterDomains :: NE.NonEmpty Domain
  } deriving (Eq, Ord, Show)

actionResources :: EV.EnumVec ResourceType (Maybe FilterType) -> EV.EnumVec FilterType ResourceSet
actionResources = EV.efoldr art mempty where
  art _ Nothing e = e
  art r (Just a) e = EV.insert a (BS.insert r $ EV.lookup a e) e

joinFilters :: FilterLine -> FilterLine -> Maybe FilterLine
joinFilters (FilterLine t1 r1 u1 d1) (FilterLine t2 r2 u2 d2)
  | t1 == t2 && r1 == r2 && u1 == u2 =
    Just $ FilterLine t1 r1 u1 $ if d1 == d2 then d1 else d1 <> d2
    -- should this be union?
  | otherwise = Nothing

foldWhile :: (a -> a -> Maybe a) -> a -> [a] -> (a, [a])
foldWhile _ a [] = (a, [])
foldWhile f a bl@(b:l) = maybe (a, bl) (\r -> foldWhile f r l) $ f a b

combineFilters :: [FilterLine] -> [FilterLine]
combineFilters [] = []
combineFilters (f:l) = a : combineFilters b where
  (a, b) = foldWhile joinFilters f l

sortFilters :: URLMap (DomainMap (EV.EnumVec ResourceType (Maybe FilterType))) -> [FilterLine]
sortFilters f = concat
  [ combineFilters $ sort
    [ FilterLine a r (Domain u) (Domain d NE.:| [])
    | (d, rm) <- PM.toList dg
    , (a, r) <- EV.assocList $ actionResources rm
    , not $ BS.null r
    ]
  | (u, dm) <- LM.toList f
  , dg <- LM.groupPrefixes dm
  ]

urlFilter :: URL -> T.Text
urlFilter (Domain []) = "."
urlFilter d = "^[^:]+:(//)?([^/:]*\\.)?" <> quoteRegExp (joinDomain d) <> "[/:]"

domainFilter :: NE.NonEmpty Domain -> DomainList
domainFilter (Domain [] NE.:| _) = DomainUnless []
domainFilter d = DomainIf d

lineRule :: FilterLine -> ContentRule
lineRule (FilterLine t r u d) = ContentRule (urlFilter u) (if BS.isFull r then [] else BS.toList r) (domainFilter d) lt a
  where
  (lt, a) = case t of
    FilterBlock -> (Nothing, ActionBlock)
    FilterBlockThird -> (Just LoadThird, ActionBlock)
    FilterBlockCookies -> (Nothing, ActionBlockCookies)
    FilterAllowFirst -> (Just LoadFirst, ActionIgnorePrevious)
    FilterAllow -> (Nothing, ActionIgnorePrevious)

hideRule :: URL -> Domain -> T.Text -> ContentRule
hideRule u d = ContentRule (urlFilter u) [] (domainFilter (d NE.:| [])) Nothing . ActionDisplayNone

filterRules :: Filters -> [ContentRule]
filterRules (Filters fm hm) =
  map lineRule (sortFilters fm) ++
  [ hideRule (Domain u) (Domain d) h
  | (d, um) <- LM.toList hm
  , (u, h) <- LM.toList um
  ]
