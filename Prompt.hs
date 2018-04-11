module Prompt
  ( prompt
  ) where

import           Control.Monad.Reader (ask)
import qualified Data.GI.Base as G
import qualified Data.Text as T

import qualified GI.Gtk as Gtk

import Types

prompt :: T.Text -> (T.Text -> HawkM ()) -> HawkM ()
prompt i f = do
  hawk <- ask
  ent <- G.new Gtk.Entry
    [ #halign G.:= Gtk.AlignStart
    ]
  #setText ent i
  #packStart (hawkStatusBox hawk) ent True True 0

  modifyState_ $ \state ->
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
        modifyState_ $ \state -> state{ stateBindings = n }
        f t
      _ -> return ()
  #show ent
  #grabFocus ent
