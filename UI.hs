module UI
  ( hawkClose
  , hawkGoto
  , setStatusLeft
  , loadStyleSheet
  ) where

import           Control.Monad.Reader (asks)
import qualified Data.Text as T
import qualified Data.Vector as V

import Types

hawkClose :: HawkM ()
hawkClose = do
  win <- asks hawkWindow
  #destroy win

hawkGoto :: T.Text -> HawkM ()
hawkGoto url = do
  wv <- asks hawkWebView
  #loadUri wv url

setStatusLeft :: T.Text -> HawkM ()
setStatusLeft t = do
  stat <- asks hawkStatusLeft
  #setText stat t

loadStyleSheet :: Int -> HawkM ()
loadStyleSheet i = do
  cm <- askUserContentManager
  glob <- asksGlobal globalStyleSheet
  css <- asks hawkStyleSheets
  #removeAllStyleSheets cm
  #addStyleSheet cm glob
  maybe
    (                             writeRef hawkStyleSheet (-1))
    (\s -> #addStyleSheet cm s >> writeRef hawkStyleSheet i)
    $ css V.!? i
