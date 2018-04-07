module Bind
  ( runBind
  ) where

import           Control.Arrow (first, second)
import           Control.Monad (when)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (ask, asks)
import qualified Data.GI.Base as G
import           Data.IORef (modifyIORef')
import           Data.List ((\\))
import qualified Data.Map.Strict as Map
import           Data.Word (Word32)

import qualified GI.Gdk as Gdk

import State
import Hawk
import Prompt

type BindMap = Map.Map ([Gdk.ModifierType], Word32) (HawkM ())

charToKey :: Char -> Word32
charToKey = fromIntegral . fromEnum

rawMode :: HawkM ()
rawMode = do
  css <- asks hawkStatusStyle
  #loadFromData css "*{background-color:#000;}"
  modifyState $ \state ->
    state{ stateBindings = PassThru $ do
      #loadFromData css "*{}"
      return $ stateBindings state
    }

commandBinds :: BindMap
commandBinds = Map.fromAscList $ map (first (second charToKey))
  [ (([], 'Q'),      hawkClose)
  , (([], 'g'),      hawkGoto "http://www.google.com/")
  , (([], 'i'),      rawMode)
  , (([], 'o'),      prompt hawkGoto)
  ]

bindings :: Bindings -> BindMap
bindings Command{} = commandBinds
bindings (PassThru r) = Map.singleton ([], Gdk.KEY_Escape) $ do
  n <- r
  modifyState $ \state -> state{ stateBindings = n }

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
