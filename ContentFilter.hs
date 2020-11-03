{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ContentFilter
  ( ResourceType(..)
  , LoadType(..)
  , DomainList(..)
  , Action(..)
  , ContentRule(..)
  ) where

import qualified Data.Aeson as J
import           Data.Default (Default(..))
import qualified Data.Text as T

import Util
import JSON
import Domain

data ResourceType
  = ResourceDocument
  | ResourceImage
  | ResourceStyleSheet
  | ResourceScript
  | ResourceFont
  | ResourceRaw
  | ResourceSVGDocument
  | ResourceMedia
  | ResourcePopup
  deriving (Eq, Enum, Bounded)

resourceTypeName :: ResourceType -> T.Text
resourceTypeName ResourceDocument    = "document"
resourceTypeName ResourceImage       = "image"
resourceTypeName ResourceStyleSheet  = "style-sheet"
resourceTypeName ResourceScript      = "script"
resourceTypeName ResourceFont        = "font"
resourceTypeName ResourceRaw         = "raw"
resourceTypeName ResourceSVGDocument = "svg-document"
resourceTypeName ResourceMedia       = "media"
resourceTypeName ResourcePopup       = "popup"

instance J.ToJSON ResourceType where
  toJSON = J.String . resourceTypeName

instance J.FromJSON ResourceType where
  parseJSON = J.withText "resource-type" $ prt . T.toLower where
    prt "document"     = return ResourceDocument    
    prt "image"        = return ResourceImage       
    prt "img"          = return ResourceImage       
    prt "style-sheet"  = return ResourceStyleSheet  
    prt "css"          = return ResourceStyleSheet  
    prt "script"       = return ResourceScript      
    prt "font"         = return ResourceFont        
    prt "raw"          = return ResourceRaw         
    prt "svg-document" = return ResourceSVGDocument 
    prt "svg"          = return ResourceSVGDocument 
    prt "media"        = return ResourceMedia       
    prt "popup"        = return ResourcePopup       
    prt _ = fail "Unknown resource-type"

data LoadType
  = LoadFirst
  | LoadThird

loadTypeName :: LoadType -> T.Text
loadTypeName LoadFirst = "first-party"
loadTypeName LoadThird = "third-party"

instance J.ToJSON LoadType where
  toJSON = J.String . loadTypeName

data DomainList
  = DomainIf [Domain]
  | DomainUnless [Domain]

domainRep :: JSONRep p r => Domain -> r
domainRep = repJSON . T.cons '*' . joinDomain

data Action
  = ActionBlock
  | ActionBlockCookies
  | ActionDisplayNone T.Text
  | ActionIgnorePrevious

actionType :: Action -> T.Text
actionType ActionBlock = "block"
actionType ActionBlockCookies = "block-cookies"
actionType ActionIgnorePrevious = "ignore-previous-rules"
actionType ActionDisplayNone{} = "css-display-none"

actionRep :: JSONRep p r => Action -> r
actionRep a = repObject $ "type" J..= actionType a <> more a where
  more (ActionDisplayNone s) = "selector" J..= s
  more _ = mempty

instance J.ToJSON Action where
  toJSON = actionRep
  toEncoding = actionRep

data ContentRule = ContentRule
  { contentURL :: !T.Text
  , contentResourceType :: [ResourceType]
  , contentDomains :: !DomainList
  , contentLoadType :: Maybe LoadType
  , contentAction :: !Action
  }

contentRuleRep :: JSONRep p r => ContentRule -> r
contentRuleRep ContentRule{..} = repObject
  $  "trigger" `repPair` (repObject
    $  "url-filter" J..= contentURL
    <> (mwhen (not $ null contentResourceType) $
      "resource-type" J..= contentResourceType)
    <> (foldMap (("load-type" J..=) . (:[])) contentLoadType)
    <> (case contentDomains of
      DomainUnless [] -> mempty
      DomainIf l -> "if-domain" `repPair` repList (map domainRep l)
      DomainUnless l -> "unless-domain" `repPair` repList (map domainRep l)))
  <> "action" `repPair` actionRep contentAction

instance J.ToJSON ContentRule where
  toJSON = contentRuleRep
  toEncoding = contentRuleRep

instance Default ContentRule where
  def = ContentRule "." [] (DomainUnless []) Nothing ActionBlock


