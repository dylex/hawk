{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Bind
  ( runBind
  ) where

import           Control.Arrow (first, second)
import qualified Control.Lens as Lens
import           Control.Monad (unless, void)
import           Control.Monad.Reader (asks)
import           Data.Bits ((.|.))
import           Data.Char (isUpper)
import           Data.Default (def)
import           Data.Foldable (fold)
import qualified Data.GI.Base as G
import qualified Data.GI.Base.Attributes as GA
import           Data.Int (Int32)
import           Data.List ((\\))
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe, fromJust)
import           Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Vector as V
import           Data.Word (Word32)
import           GHC.TypeLits (KnownSymbol, symbolVal)

import qualified GI.Gdk as Gdk
import qualified GI.Gtk as Gtk
import qualified GI.WebKit2 as WK

import Types
import Config
import Prompt
import Cookies
import Script
import UI
import Event
import JS
import qualified Data.ListMap as LM
import qualified Data.BitSet as ES

settingStatus :: (KnownSymbol attr, GA.AttrGetC info WK.Settings attr a, Show a) => GA.AttrLabelProxy attr -> HawkM ()
settingStatus attr = do
  sets <- askSettings
  x <- G.get sets attr
  setStatusLeft $ T.pack $ symbolVal attr ++ " " ++ show x

promptTextSetting :: (KnownSymbol attr, GA.AttrGetC info WK.Settings attr T.Text, GA.AttrSetC info WK.Settings attr T.Text) => GA.AttrLabelProxy attr -> HawkM ()
promptTextSetting attr = do
  sets <- askSettings
  x <- G.get sets attr
  prompt def{ promptPrefix = T.pack $ symbolVal attr, promptInit = x } $ G.set sets . return . (attr G.:=)
  settingStatus attr

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

zero :: HawkM ()
zero = do
  c <- readRef hawkBindings
  case c of
    Command{ commandCount = Nothing } -> runScript "window.scrollTo({top:0})"
    _ -> digit 0

countMaybe :: HawkM (Maybe Word32)
countMaybe = modifyCount (const Nothing)

runScriptCount :: T.Text -> T.Text -> HawkM ()
runScriptCount a b = do
  n <- countMaybe
  runScript $ a <> maybe "1" (T.pack . show) n <> b

zoom :: (Double -> Double) -> HawkM ()
zoom f = do
  wv <- askWebView
  G.set wv [#zoomLevel G.:~ f]
  x <- #getZoomLevel wv
  setStatusLeft $ T.pack $ "zoom-level " ++ show x

indexMod :: V.Vector a -> Int -> a
indexMod v i = v V.! (i `mod` V.length v)

toggle' :: Eq a => V.Vector a -> V.Vector a -> a -> HawkM a
toggle' opts opts' cur = maybe
  (indexMod opts $ maybe 0 succ $ V.elemIndex cur opts)
  (indexMod opts' . fromIntegral) <$> countMaybe

toggle :: Eq a => V.Vector a -> a -> HawkM a
toggle opts = toggle' opts opts

enums :: (Enum a, Bounded a) => V.Vector a
enums = V.enumFromTo minBound maxBound

toggleSetting :: (KnownSymbol attr, GA.AttrGetC info WK.Settings attr a, GA.AttrSetC info WK.Settings attr a, Eq a, Show a) => GA.AttrLabelProxy attr -> V.Vector a -> HawkM ()
toggleSetting attr opts = do
  sets <- askSettings
  v <- toggle opts =<< G.get sets attr
  G.set sets [attr G.:= v]
  settingStatus attr

toggleSettingBool :: (KnownSymbol attr, GA.AttrGetC info WK.Settings attr Bool, GA.AttrSetC info WK.Settings attr Bool) => GA.AttrLabelProxy attr -> HawkM ()
toggleSettingBool attr = toggleSetting attr enums

toggleUserAgent :: HawkM ()
toggleUserAgent = do
  glob <- asksGlobal globalUserAgent
  ua <- asksConfig configUserAgent
  toggleSetting #userAgent $ V.cons glob ua

toggleStyleSheet :: HawkM ()
toggleStyleSheet = do
  n <- V.length <$> asks hawkStyleSheets
  loadStyleSheet =<< toggle (V.enumFromTo (-1) (pred n)) =<< readRef hawkStyleSheet

modifySiteOverride :: Lens.Lens' SiteConfig a -> (a -> a -> HawkM a) -> HawkM a
modifySiteOverride f m = do
  d <- asksConfig $ (Lens.^. f) . defaultSiteConfig
  c <- (Lens.^. f) <$> readRef hawkSiteOverride
  v <- m d c
  modifyRef_ hawkSiteOverride $ f Lens..~ v
  reapplySiteConfig
  return v

toggleSiteOverride :: (Eq a, Show a) => T.Text -> Lens.Lens' SiteConfig (Maybe a) -> V.Vector a -> [a] -> HawkM ()
toggleSiteOverride name f opts noopts = do
  v <- modifySiteOverride f $ \d c -> do
    let jopts = Nothing `V.cons` (Just <$> opts)
        opts' = V.fromList $ V.toList jopts \\ (d : map Just noopts)
    toggle' opts' jopts c
  setStatusLeft $ name <> T.pack (' ' : maybe "config" show v)

toggleKeepHistory :: HawkM ()
toggleKeepHistory = toggleSiteOverride "keep-history" configKeepHistory' enums []

toggleCookiePolicy :: HawkM ()
toggleCookiePolicy = toggleSiteOverride "cookie-accept-policy" configCookieAcceptPolicy'
  (V.fromList [WK.CookieAcceptPolicyNever, WK.CookieAcceptPolicyNoThirdParty, WK.CookieAcceptPolicyAlways])
  [WK.CookieAcceptPolicyAlways]

cookiesSave :: HawkM ()
cookiesSave = do
  wv <- askWebView
  uri <- #getUri wv
  prompt def{ promptPrefix = "save cookies for", promptPurpose = Gtk.InputPurposeUrl, promptInit = fold uri } saveCookies

toggleAllowLoad :: LoadElement -> HawkM ()
toggleAllowLoad e = do
  v <- modifySiteOverride configAllowLoad' $ \d c -> do
    let d' = fold $ get d
        c' = fromMaybe d' $ get c
    v <- toggle enums $ mem c'
    return $ LM.insert [] (bs v c') c
  setStatusLeft $ "allow-load " <> loadElementName e <> T.pack (' ' : show (mem $ fromJust $ get v))
  where
  get = LM.lookup []
  mem = ES.member e
  bs True  = ES.insert e
  bs False = ES.delete e

clearAllowLoad :: HawkM ()
clearAllowLoad = do
  modifyRef_ hawkSiteOverride $ \o -> o{ configAllowLoad = LM.delete [] $ configAllowLoad o }
  setStatusLeft $ "allow-load config"

backForward :: Int32 -> HawkM ()
backForward 0 = return ()
backForward (-1) = #goBack =<< askWebView
backForward 1 = #goForward =<< askWebView
backForward n = do
  wv <- askWebView
  bf <- #getBackForwardList wv
  mapM_ (#goToBackForwardListItem wv) =<< #getNthItem bf n

promptURL :: Maybe T.Text -> HawkM ()
promptURL i = prompt def{ promptPrefix = "goto", promptPurpose = Gtk.InputPurposeUrl, promptInit = fold i, promptCompletion = completeURI } hawkGoto

findSearch :: Bool -> T.Text -> HawkM ()
findSearch fwd x = do
  fc <- askFindController
  #search fc x' (
        (T.any isUpper x' `orOpt` WK.FindOptionsCaseInsensitive)
    .|. (T.null s         `orOpt` WK.FindOptionsAtWordStarts)
    .|. (fwd              `orOpt` WK.FindOptionsBackwards)
    .|.                      opt  WK.FindOptionsWrapAround)
    256
  where
  (s, x') = T.span (' '==) x
  opt = fromIntegral . fromEnum
  orOpt True = const 0
  orOpt False = opt

inspector :: HawkM ()
inspector = do
  ins <- #getInspector =<< askWebView
  #show ins

type BindMap = Map.Map ([Gdk.ModifierType], Word32) (HawkM ())

charToKey :: Char -> Word32
charToKey = fromIntegral . fromEnum

commandBinds :: BindMap
commandBinds = Map.fromList $ 
  [ (([], Gdk.KEY_Escape), commandModeBind)
  , (([], Gdk.KEY_Up)       , runScriptCount "window.scrollBy(0,-20*" ")")
  , (([], Gdk.KEY_Down)     , runScriptCount "window.scrollBy(0,+20*" ")")
  , (([], Gdk.KEY_Left)     , runScriptCount "window.scrollBy(-20*" ",0)")
  , (([], Gdk.KEY_Right)    , runScriptCount "window.scrollBy(+20*" ",0)")
  , (([], Gdk.KEY_Page_Up)  , runScriptCount "window.scrollBy(0,-window.innerHeight*" ")")
  , (([], Gdk.KEY_Page_Down), runScriptCount "window.scrollBy(0,+window.innerHeight*" ")")
  , (([], Gdk.KEY_space)    , runScriptCount "window.scrollBy(0,window.innerHeight*" ")")
  , (([], Gdk.KEY_Home)     , runScript "window.scrollTo({top:0})")
  , (([], Gdk.KEY_End)      , runScript "window.scrollTo({top:document.body.scrollHeight})")
  , (([], Gdk.KEY_Back)   , backForward (-1))
  , (([], Gdk.KEY_Forward), backForward 1)
  , (([], Gdk.KEY_Stop)   , #stopLoading =<< askWebView)
  , (([], Gdk.KEY_Refresh), #reload =<< askWebView)
  ] ++
  [ (([], i + charToKey '0'), digit i) | i <- [1..9]
  ] ++ map (first (second charToKey))
  [ (([], '0'), zero)
  , (([], ')'), digit 0)
  , (([], '@'), toggleSettingBool #enableCaretBrowsing)
  , (([], '$'), runScript "window.scrollTo({left:document.body.scrollWidth})")
  , (([], '%'), toggleSettingBool #enableJavascript)
  , (([], '^'), runScript "window.scrollTo({left:0})")
  , (([], '&'), toggleStyleSheet)
  , (([], '*'), toggleSettingBool #enableWebgl)
  , (([], '['), linkSelect "prev" "\\bprev|^<")
  , (([], ']'), linkSelect "next" "\\bnext|^>")
  , (([], '='), zoom (const 1))
  , (([mod1], '='), toggleSettingBool #zoomTextOnly)
  , (([], '+'), zoom (0.1 +))
  , (([], '_'), zoom (subtract 0.1))

  , (([], 'p'), pasteSelection hawkGoto)
  , (([mod1], 'p'), toggleKeepHistory)
  , (([], 'y'), mapM_ copySelection =<< #getUri =<< askWebView)
  , (([], 'G'),   runScript "window.scrollTo({top:document.body.scrollHeight})")
  , (([mod1], 'f'), toggleAllowLoad LoadIFRAME)
  , (([mod1], 'c'), toggleCookiePolicy)
  , (([ctrl, mod1], 'c'), mapM_ saveCookies =<< #getUri =<< askWebView)
  , (([ctrl, mod1], 'C'), cookiesSave)
  , (([], 'r'), #reload =<< askWebView)
  , (([], 'R'), #reloadBypassCache =<< askWebView)
  , (([], 'l'), #searchNext =<< askFindController)
  , (([mod1], 'l'), toggleSettingBool #enableHtml5LocalStorage)
  , (([], 'L'), #searchPrevious =<< askFindController)
  , (([], '/'), prompt def{ promptPrefix = "search" } $ findSearch True)
  , (([], '?'), prompt def{ promptPrefix = "search" } $ findSearch False)

  , (([mod1], 'a'), toggleUserAgent)
  , (([mod1], 'A'), promptTextSetting #userAgent)
  , (([], 'o'), promptURL Nothing)
  , (([], 'O'), promptURL =<< #getUri =<< askWebView)
  , (([], 'e'), backForward . maybe (-1) (negate . fromIntegral) =<< countMaybe)
  , (([], 'u'), backForward . maybe   1            fromIntegral  =<< countMaybe)
  , (([], 'i'), passThruBind)
  , (([], 'I'), inspector)
  , (([], 'h'), runScriptCount "window.scrollBy(-20*" ",0)")
  , (([], 't'), runScriptCount "window.scrollBy(0,+20*" ")")
  , (([], 'n'), runScriptCount "window.scrollBy(0,-20*" ")")
  , (([], 's'), runScriptCount "window.scrollBy(+20*" ",0)")
  , (([mod1], 'h'), hawkGoto "hawk:history")
  , (([mod1], 's'), toggleAllowLoad LoadSCRIPT)
  , (([mod1], '-'), clearAllowLoad)

  , (([], 'Q'), hawkClose)
  , (([], 'J'), prompt def{ promptPrefix = "js" } runScript)
  , (([], 'm'), hawkGoto "hawk:marks")
  , (([], 'z'), #stopLoading =<< askWebView)
  ] where
  mod1 = Gdk.ModifierTypeMod1Mask
  ctrl = Gdk.ModifierTypeControlMask

runBind :: Gdk.EventKey -> HawkM Bool
runBind ev = do
  bind <- readRef hawkBindings
  evt <- G.get ev #type
  unless (evt == Gdk.EventTypeKeyPress) $ fail $ show evt
  ks <- G.get ev #state
  kv <- G.get ev #keyval
  let run f = True <$ f
  case bind of
    Command{} ->
      run $ Map.findWithDefault
        (return ())
        (ks \\ [Gdk.ModifierTypeShiftMask], kv) commandBinds
    PassThru r
      | null ks && kv == Gdk.KEY_Escape ->
        run $ writeRef hawkBindings =<< r
      | otherwise -> return False
