module Bind
  ( runBind
  ) where

import           Control.Arrow (first, second)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.GI.Base as G
import           Data.List ((\\))
import qualified Data.Map.Strict as Map
import           Data.Word (Word32)

import qualified GI.Gdk as Gdk

import State
import Hawk

type BindMap = Map.Map ([Gdk.ModifierType], Word32) (HawkM ())

charToKey :: Char -> Word32
charToKey = fromIntegral . fromEnum

commandBinds :: BindMap
commandBinds = Map.fromAscList $ map (first (second charToKey))
  [ (([], 'Q'),      hawkClose)
  , (([], 'g'),      hawkGoto "http://www.google.com/")
  ]

bindings :: Bindings -> BindMap
bindings Command{} = commandBinds

runBind :: Gdk.EventKey -> HawkM Bool
runBind ev = do
  bind <- bindings <$> asksState stateBindings
  evt <- G.get ev #type
  if evt == Gdk.EventTypeKeyPress
    then do
      ks <- G.get ev #state
      kv <- G.get ev #keyval
      liftIO $ print (ks, kv)
      maybe (return False) (True <$) $ Map.lookup (ks \\ [Gdk.ModifierTypeShiftMask], kv) bind
    else return False
