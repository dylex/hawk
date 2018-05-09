module UI
  ( hawkClose
  , hawkGoto
  , setStatusLeft
  , loadStyleSheet
  , passThruBind
  ) where

import           Control.Monad.Reader (asks)
import qualified Data.Text as T
import qualified Data.Vector as V

import Types
import Expand

hawkClose :: HawkM ()
hawkClose = do
  win <- asks hawkWindow
  #destroy win

hawkGoto :: T.Text -> HawkM ()
hawkGoto url = do
  wv <- askWebView
  #loadUri wv =<< expandURI url

setStatusLeft :: T.Text -> HawkM ()
setStatusLeft t = do
  stat <- asks hawkStatusLeft
  #setText stat t

loadStyleSheet :: (Int -> Int -> Int) -> HawkM ()
loadStyleSheet f = do
  cm <- askUserContentManager
  glob <- asksGlobal globalStyleSheet
  css <- asks hawkStyleSheets
  i <- f (V.length css) <$> readRef hawkStyleSheet
  #removeAllStyleSheets cm
  #addStyleSheet cm glob
  maybe
    (                             writeRef hawkStyleSheet (-1))
    (\s -> #addStyleSheet cm s >> writeRef hawkStyleSheet i)
    $ css V.!? i

passThruBind :: HawkM ()
passThruBind = do
  css <- asks hawkStatusStyle
  #loadFromData css "*{background-color:#000;}"
  modifyRef_ hawkBindings $ \bind ->
    case bind of
      PassThru{} -> bind
      _ -> PassThru $ do
        #loadFromData css "*{}"
        return bind
