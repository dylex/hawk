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
import qualified Data.GI.Base as G
import qualified Data.GI.Base.Attributes as GA
import           Data.List ((\\))
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Vector as V
import           Data.Word (Word32)
import           GHC.TypeLits (KnownSymbol, symbolVal)

import qualified GI.Gio as Gio
import qualified GI.Gdk as Gdk
import qualified GI.Gtk as Gtk
import qualified GI.WebKit2 as WK

import Types
import Prompt
import Cookies

hawkClose :: HawkM ()
hawkClose = do
  win <- asks hawkWindow
  #destroy win

hawkGoto :: T.Text -> HawkM ()
hawkGoto url = do
  wv <- asks hawkWebView
  #loadUri wv url

settingStatus :: (KnownSymbol attr, GA.AttrGetC info WK.Settings attr a, Show a) => GA.AttrLabelProxy attr -> HawkM ()
settingStatus attr = do
  sets <- asksSettings
  x <- G.get sets attr
  stat <- asks hawkStatusLeft
  #setText stat $ T.pack $ symbolVal attr ++ " " ++ show x

commandMode :: HawkM ()
commandMode = do
  _ <- countMaybe
  stat <- asks hawkStatusLeft
  #setText stat T.empty
  writeRef hawkBindings def

rawMode :: HawkM ()
rawMode = do
  css <- asks hawkStatusStyle
  #loadFromData css "*{background-color:#000;}"
  modifyRef_ hawkBindings $ \bind ->
    PassThru $ do
      #loadFromData css "*{}"
      return bind

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

toggleOrCountSetting :: (KnownSymbol attr, GA.AttrGetC info WK.Settings attr Bool, GA.AttrSetC info WK.Settings attr Bool) => GA.AttrLabelProxy attr -> HawkM ()
toggleOrCountSetting attr = do
  sets <- asksSettings
  G.set sets . return . maybe (attr G.:~ not) ((attr G.:=) . (0 /=)) =<< countMaybe
  settingStatus attr

zoom :: (Double -> Double) -> HawkM ()
zoom f = do
  wv <- asks hawkWebView
  G.set wv [#zoomLevel G.:~ f]
  x <- G.get wv #zoomLevel
  stat <- asks hawkStatusLeft
  #setText stat $ T.pack $ "zoomLevel " ++ show x

toggleStyleSheet :: HawkM ()
toggleStyleSheet = do
  css <- asks $ globalStyleSheets . hawkGlobal
  i <- modifyRef hawkStyleSheet $
    id &&& id . (`mod` V.length css) . succ
  usercm <- asksUserContentManager
  #removeAllStyleSheets usercm
  #addStyleSheet usercm $ css V.! i

toggleCookiePolicy :: HawkM ()
toggleCookiePolicy = do
  cm <- asksCookieManager
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
  prompt (fromMaybe T.empty uri) $
    saveCookies

type BindMap = Map.Map ([Gdk.ModifierType], Word32) (HawkM ())

charToKey :: Char -> Word32
charToKey = fromIntegral . fromEnum

commandBinds :: BindMap
commandBinds = Map.fromList $ 
  [ (([], Gdk.KEY_Escape), commandMode)
  ] ++
  [ (([], i + charToKey '0'), digit i) | i <- [0..9]
  ] ++ map (first (second charToKey))
  [ (([], '@'), toggleOrCountSetting #enableCaretBrowsing)
  , (([], '%'), toggleOrCountSetting #enableJavascript)
  , (([], '&'), toggleStyleSheet)
  , (([], '*'), toggleOrCountSetting #enableWebgl)
  , (([], '='), zoom (const 1))
  , (([mod1], '='), toggleOrCountSetting #zoomTextOnly)
  , (([], '+'), zoom (0.1 +))
  , (([], '_'), zoom (subtract 0.1))
  , (([], 'p'), paste hawkGoto)
  , (([], 'g'), hawkGoto "http://www.google.com/")
  , (([mod1], 'c'), toggleCookiePolicy)
  , (([ctrl, mod1], 'c'), cookiesSave)
  , (([], 'o'), prompt T.empty hawkGoto)
  , (([], 'i'), rawMode)
  , (([], 'Q'), hawkClose)
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
