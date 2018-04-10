{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Bind
  ( runBind
  ) where

import           Control.Arrow (first, second)
import           Control.Monad (void, when)
import           Control.Monad.Reader (ask, asks)
import           Data.Default (def)
import qualified Data.GI.Base as G
import qualified Data.GI.Base.Attributes as GA
import           Data.List ((\\))
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Vector as V
import           Data.Word (Word32)
import           GHC.TypeLits (KnownSymbol, symbolVal)

import qualified GI.Gdk as Gdk
import qualified GI.Gtk as Gtk
import qualified GI.WebKit2 as WK

import Types
import Prompt

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
  sets <- asks hawkSettings
  x <- G.get sets attr
  stat <- asks hawkStatusLeft
  #setText stat $ T.pack $ symbolVal attr ++ " " ++ show x

commandMode :: HawkM ()
commandMode = do
  _ <- countMaybe
  stat <- asks hawkStatusLeft
  #setText stat T.empty
  modifyState_ $ \state ->
    state{ stateBindings = def }

rawMode :: HawkM ()
rawMode = do
  css <- asks hawkStatusStyle
  #loadFromData css "*{background-color:#000;}"
  modifyState_ $ \state ->
    state{ stateBindings = PassThru $ do
      #loadFromData css "*{}"
      return $ stateBindings state
    }

paste :: (T.Text -> HawkM ()) -> HawkM ()
paste f = do
  sel <- Gtk.clipboardGet {- Gdk.SELECTION_PRIMARY: wrapPtr Gdk.Atom (intPtrToPtr 1) -} =<< Gdk.atomInternStaticString "PRIMARY"
  hawk <- ask
  #requestText sel $ \_ -> maybe (return ()) $ runHawkM hawk . f

modifyCount :: (Maybe Word32 -> Maybe Word32) -> HawkM (Maybe Word32)
modifyCount f = do
  c <- modifyState $ \state ->
    case stateBindings state of
      b@Command{ commandCount = c } -> (state{ stateBindings = b{ commandCount = f c } }, c)
      _ -> (state, Nothing)
  stat <- asks hawkStatusCount
  #setText stat $ maybe T.empty (T.pack . show) $ f c
  return c

digit :: Word32 -> HawkM ()
digit i = void $ modifyCount $ Just . (i+) . maybe 0 (10*)

countMaybe :: HawkM (Maybe Word32)
countMaybe = modifyCount (const Nothing)

toggleOrCountSetting :: (KnownSymbol attr, GA.AttrGetC info WK.Settings attr Bool, GA.AttrSetC info WK.Settings attr Bool) => GA.AttrLabelProxy attr -> HawkM ()
toggleOrCountSetting attr = do
  sets <- asks hawkSettings
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
  i <- modifyState $ \state ->
    let i = succ (stateStyleSheet state) `mod` V.length css in
    (state{ stateStyleSheet = i }, i)
  usercm <- asks hawkUserCM
  #removeAllStyleSheets usercm
  #addStyleSheet usercm $ css V.! i

cookiesSave :: HawkM ()
cookiesSave = return ()

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
  , (([ctrl, mod1], 'c'), cookiesSave)
  , (([], 'o'), prompt hawkGoto)
  , (([], 'i'), rawMode)
  , (([], 'Q'), hawkClose)
  , (([], 'z'), #stopLoading =<< asks hawkWebView)
  ] where
  mod1 = Gdk.ModifierTypeMod1Mask
  ctrl = Gdk.ModifierTypeControlMask

bindings :: Bindings -> BindMap
bindings Command{} = commandBinds
bindings (PassThru r) = Map.singleton ([], Gdk.KEY_Escape) $ do
  n <- r
  modifyState_ $ \state -> state{ stateBindings = n }

runBind :: Gdk.EventKey -> HawkM Bool
runBind ev = do
  bind <- asksState $ bindings . stateBindings
  evt <- G.get ev #type
  ks <- G.get ev #state
  kv <- G.get ev #keyval
  maybe
    (return False)
    ((<$) True . when (evt == Gdk.EventTypeKeyPress))
    $ Map.lookup (ks \\ [Gdk.ModifierTypeShiftMask], kv) bind
