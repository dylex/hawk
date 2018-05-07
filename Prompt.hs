{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Prompt
  ( Prompt(..)
  , prompt
  , completeURI
  ) where

import           Control.Arrow (first, second)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (ask)
import qualified Data.GI.Base as G
import           Data.Default (Default(..))
import           Data.Foldable (fold)
import qualified Data.HashMap.Strict as HM
import           Data.IORef (newIORef, readIORef, writeIORef, modifyIORef', atomicModifyIORef')
import           Data.Int (Int64)
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import           Data.Tuple (swap)
import           Database.PostgreSQL.Typed (pgSQL)
import qualified Deque as D

import qualified GI.Gdk as Gdk
import qualified GI.Gtk as Gtk

import Types
import Config
import Database
import UI

useTPGConfig

data Prompt = Prompt
  { promptPrefix :: T.Text
  , promptPurpose :: Gtk.InputPurpose
  , promptInit :: T.Text
  , promptCompletion :: T.Text -> Int -> HawkM (Maybe T.Text)
  }

instance Default Prompt where
  def = Prompt
    { promptPrefix = ""
    , promptPurpose = Gtk.InputPurposeFreeForm
    , promptInit = ""
    , promptCompletion = \_ _ -> return Nothing
    }

prompt :: Prompt -> (T.Text -> HawkM ()) -> HawkM ()
prompt Prompt{..} run = do
  histr <- maybe (do
    h <- liftIO $ newIORef mempty
    modifyRef_ hawkPromptHistory $ HM.insert promptPrefix h
    return h)
    return . HM.lookup promptPrefix =<< readRef hawkPromptHistory
  setStatusLeft promptPrefix
  hawk <- ask
  ent <- G.new Gtk.Entry $
    [ #halign G.:= Gtk.AlignStart
    , #hexpand G.:= True
    , #widthChars G.:= 128
    , #inputPurpose G.:= promptPurpose
    , #text G.:= promptInit
    ]
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
        liftIO $ modifyIORef' histr $ D.cons t
        writeRef hawkBindings =<< r
        run t
      _ -> return ()
  ctr <- liftIO $ newIORef Nothing
  let set t = do
        #setText ent t
        #setPosition ent (-1)
      comp ks = do
        (ct, cn) <- maybe
          ((, 0) <$> #getText ent)
          (return . second (if ks then pred else succ)) =<< readIORef ctr
        ct' <- runHawkM hawk $ promptCompletion ct cn
        set $ fromMaybe ct ct'
        writeIORef ctr ((ct, cn) <$ ct')
        return True
      hist f = do
        set =<< atomicModifyIORef' histr . f =<< #getText ent
        return True
  _ <- G.on ent #keyPressEvent $ \ev -> do
    kv <- G.get ev #keyval
    case kv of
      Gdk.KEY_Tab -> comp . elem Gdk.ModifierTypeShiftMask =<< G.get ev #state
      Gdk.KEY_ISO_Left_Tab -> comp True
      Gdk.KEY_Up   -> hist $ \t d -> maybe (d, t) (first (D.snoc t) . swap) $ D.uncons d
      Gdk.KEY_Down -> hist $ \t d -> maybe (d, t) (first (D.cons t) . swap) $ D.unsnoc d
      _ -> return False

  #show ent
  #grabFocus ent

completeURI :: T.Text -> Int -> HawkM (Maybe T.Text)
completeURI t i
  | i < 0 = return Nothing
  | otherwise =
    fold <$> hawkDBQuery [pgSQL|$SELECT COALESCE(browse.uri, mark.uri)
      FROM mark FULL JOIN browse ON (mark.browse = browse.id)
      WHERE text(coalesce(mark.uri, browse.uri)) LIKE '%' || ${t} || '%'
      ORDER BY mark.id IS NULL, browse.last DESC NULLS LAST OFFSET ${fromIntegral i :: Int64} LIMIT 1|]
