module Prompt
  ( prompt
  ) where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (ask, asks)
import qualified Data.GI.Base as G
import qualified Data.Text as T

import qualified GI.Gtk as Gtk

import Hawk
import State

prompt :: (T.Text -> HawkM ()) -> HawkM ()
prompt f = do
  hawk <- ask
  ent <- G.new Gtk.Entry
    [ #halign G.:= Gtk.AlignStart
    ]
  #packStart (hawkStatusBox hawk) ent True True 0

  modifyState $ \state ->
    state{ stateBindings = PassThru $ do
      #destroy ent
      return $ stateBindings state
    }
  _ <- G.on ent #activate $ runHawkM hawk $ do
    t <- #getText ent
    b <- asksState stateBindings
    case b of
      PassThru r -> do
        n <- r
        modifyState $ \state -> state{ stateBindings = n }
        f t
      _ -> return ()
  #show ent
  #grabFocus ent
