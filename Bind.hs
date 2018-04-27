{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Bind
  ( runBind
  ) where

import           Control.Arrow (first, second, (&&&))
import           Control.Monad (void, when)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (ask, asks)
import           Data.Default (def)
import           Data.Foldable (fold)
import qualified Data.GI.Base as G
import qualified Data.GI.Base.Attributes as GA
import           Data.Int (Int32)
import           Data.List ((\\))
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Vector as V
import           Data.Word (Word32)
import           GHC.TypeLits (KnownSymbol, symbolVal)

import qualified GI.Gio as Gio
import qualified GI.Gdk as Gdk
import qualified GI.Gtk as Gtk
import qualified GI.WebKit2 as WK

import Types
import Config
import Prompt
import Cookies
import Script
import UI

commandMode :: HawkM ()
commandMode = do
  _ <- countMaybe
  setStatusLeft T.empty
  writeRef hawkBindings def

settingStatus :: (KnownSymbol attr, GA.AttrGetC info WK.Settings attr a, Show a) => GA.AttrLabelProxy attr -> HawkM ()
settingStatus attr = do
  sets <- askSettings
  x <- G.get sets attr
  setStatusLeft $ T.pack $ symbolVal attr ++ " " ++ show x

promptTextSetting :: (KnownSymbol attr, GA.AttrGetC info WK.Settings attr T.Text, GA.AttrSetC info WK.Settings attr T.Text) => GA.AttrLabelProxy attr -> HawkM ()
promptTextSetting attr = do
  sets <- askSettings
  x <- G.get sets attr
  prompt (T.pack $ symbolVal attr) Nothing x $ G.set sets . return . (attr G.:=)
  settingStatus attr

paste :: (T.Text -> HawkM ()) -> HawkM ()
paste f = do
  sel <- Gtk.clipboardGet {- Gdk.SELECTION_PRIMARY: wrapPtr Gdk.Atom (intPtrToPtr 1) -} =<< Gdk.atomInternStaticString "PRIMARY"
  hawk <- ask
  #requestText sel $ \_ -> maybe (return ()) $ runHawkM hawk . f

modifyCount :: (Maybe Word32 -> Maybe Word32) -> HawkM (Maybe Word32)
modifyCount f = do
  c <- modifyRef hawkBindings $ \case
    b@Command{ commandCount = c } -> (b{ commandCount = f c }, c)
    b -> (b, Nothing)
  stat <- asks hawkStatusCount
  #setText stat $ maybe T.empty (T.pack . show) $ f c
  return c

digit :: Word32 -> HawkM ()
digit i = void $ modifyCount $ Just . (i+) . maybe 0 (10*)

countMaybe :: HawkM (Maybe Word32)
countMaybe = modifyCount (const Nothing)

zoom :: (Double -> Double) -> HawkM ()
zoom f = do
  wv <- asks hawkWebView
  G.set wv [#zoomLevel G.:~ f]
  x <- G.get wv #zoomLevel
  setStatusLeft $ T.pack $ "zoomLevel " ++ show x

toggleSetting :: (KnownSymbol attr, GA.AttrGetC info WK.Settings attr a, GA.AttrSetC info WK.Settings attr a, Eq a, Show a) => GA.AttrLabelProxy attr -> V.Vector a -> HawkM ()
toggleSetting attr opts = do
  sets <- askSettings
  i <- maybe
    (maybe 0 succ . (`V.elemIndex` opts) <$> G.get sets attr)
    (return . fromIntegral) =<< countMaybe
  G.set sets [attr G.:= opts V.! (i `mod` V.length opts)]
  settingStatus attr

toggleSettingBool :: (KnownSymbol attr, GA.AttrGetC info WK.Settings attr Bool, GA.AttrSetC info WK.Settings attr Bool) => GA.AttrLabelProxy attr -> HawkM ()
toggleSettingBool attr = toggleSetting attr $ V.enumFromTo False True

toggleUserAgent :: HawkM ()
toggleUserAgent = do
  glob <- asksGlobal globalUserAgent
  ua <- asksConfig configUserAgent
  toggleSetting #userAgent $ V.cons glob ua

toggleStyleSheet :: HawkM ()
toggleStyleSheet = do
  css <- asks hawkStyleSheets
  i <- modifyRef hawkStyleSheet $
    id &&& id . (`mod` V.length css) . succ
  usercm <- askUserContentManager
  #removeAllStyleSheets usercm
  #addStyleSheet usercm $ css V.! i

toggleCookiePolicy :: HawkM ()
toggleCookiePolicy = do
  cm <- askCookieManager
  stat <- asks hawkStatusLeft
  let set p = do
        #setAcceptPolicy cm p
        #setText stat $ T.pack $ "cookieAcceptPolicy " ++ show p
  maybe
    (#getAcceptPolicy cm Gio.noCancellable $ Just $ \_ cb ->
      set . next =<< #getAcceptPolicyFinish cm cb)
    (liftIO . set . pol) =<< countMaybe
  where
  pol 0 = WK.CookieAcceptPolicyNever
  pol 2 = WK.CookieAcceptPolicyAlways
  pol _ = WK.CookieAcceptPolicyNoThirdParty
  next WK.CookieAcceptPolicyNever = WK.CookieAcceptPolicyNoThirdParty
  next _ = WK.CookieAcceptPolicyNever

cookiesSave :: HawkM ()
cookiesSave = do
  wv <- asks hawkWebView
  uri <- G.get wv #uri
  prompt "save cookies for" (Just Gtk.InputPurposeUrl) (fold uri) $ saveCookies

backForward :: Int32 -> HawkM ()
backForward 0 = return ()
backForward (-1) = #goBack =<< asks hawkWebView
backForward 1 = #goForward =<< asks hawkWebView
backForward n = do
  wv <- asks hawkWebView
  bf <- #getBackForwardList wv
  mapM_ (#goToBackForwardListItem wv) =<< #getNthItem bf n

inspector :: HawkM ()
inspector = do
  ins <- #getInspector =<< asks hawkWebView
  #show ins

type BindMap = Map.Map ([Gdk.ModifierType], Word32) (HawkM ())

charToKey :: Char -> Word32
charToKey = fromIntegral . fromEnum

commandBinds :: BindMap
commandBinds = Map.fromList $ 
  [ (([], Gdk.KEY_Escape), commandMode)
  ] ++
  [ (([], i + charToKey '0'), digit i) | i <- [0..9]
  ] ++ map (first (second charToKey))
  [ (([], '@'), toggleSettingBool #enableCaretBrowsing)
  , (([], '%'), toggleSettingBool #enableJavascript)
  , (([], '&'), toggleStyleSheet)
  , (([], '*'), toggleSettingBool #enableWebgl)
  , (([], '['), linkSelect "prev" "\\bprev|^<")
  , (([], ']'), linkSelect "next" "\\bnext|^>")
  , (([], '='), zoom (const 1))
  , (([mod1], '='), toggleSettingBool #zoomTextOnly)
  , (([], '+'), zoom (0.1 +))
  , (([], '_'), zoom (subtract 0.1))
  , (([], 'p'), paste hawkGoto)
  , (([mod1], 'c'), toggleCookiePolicy)
  , (([ctrl, mod1], 'c'), cookiesSave)
  , (([], 'r'), #reload =<< asks hawkWebView)
  , (([], 'R'), #reloadBypassCache =<< asks hawkWebView)
  , (([mod1], 'a'), toggleUserAgent)
  , (([mod1], 'A'), promptTextSetting #userAgent)
  , (([], 'o'), prompt "goto" (Just Gtk.InputPurposeUrl) T.empty hawkGoto)
  , (([], 'e'), backForward . maybe (-1) (negate . fromIntegral) =<< countMaybe)
  , (([], 'u'), backForward . maybe   1            fromIntegral  =<< countMaybe)
  , (([], 'i'), passThruBind)
  , (([], 'I'), inspector)
  , (([], 'Q'), hawkClose)
  , (([], 'J'), prompt "js" Nothing T.empty runScript)
  , (([], 'z'), #stopLoading =<< asks hawkWebView)
  ] where
  mod1 = Gdk.ModifierTypeMod1Mask
  ctrl = Gdk.ModifierTypeControlMask

bindings :: Bindings -> BindMap
bindings Command{} = commandBinds
bindings (PassThru r) = Map.singleton ([], Gdk.KEY_Escape) $
  writeRef hawkBindings =<< r

runBind :: Gdk.EventKey -> HawkM Bool
runBind ev = do
  bind <- bindings <$> readRef hawkBindings
  evt <- G.get ev #type
  ks <- G.get ev #state
  kv <- G.get ev #keyval
  maybe
    (return False)
    ((<$) True . when (evt == Gdk.EventTypeKeyPress))
    $ Map.lookup (ks \\ [Gdk.ModifierTypeShiftMask], kv) bind
