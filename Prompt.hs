module Prompt
  ( prompt
  ) where

import           Control.Monad.Reader (ask)
import qualified Data.GI.Base as G
import qualified Data.Text as T

import qualified GI.Gtk as Gtk

import Types
import UI

prompt :: T.Text -> T.Text -> (T.Text -> HawkM ()) -> HawkM ()
prompt p i f = do
  setStatusLeft p
  hawk <- ask
  ent <- G.new Gtk.Entry
    [ #halign G.:= Gtk.AlignStart
    , #hexpand G.:= True
    ]
  #setText ent i
  #packStart (hawkStatusBox hawk) ent True True 0

  modifyRef_ hawkBindings $ \bind ->
    PassThru $ do
      #destroy ent
      setStatusLeft ""
      return bind
  _ <- G.on ent #activate $ runHawkM hawk $ do
    t <- #getText ent
    b <- readRef hawkBindings
    case b of
      PassThru r -> do
        writeRef hawkBindings =<< r
        f t
      _ -> return ()
  #show ent
  #grabFocus ent
