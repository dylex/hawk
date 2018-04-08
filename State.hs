module State
  ( Global(..)
  , Bindings(..)
  , State(..)
  ) where

import           Data.Default (Default(def))
import qualified Data.Vector as V
import           Data.Word (Word32)

import qualified GI.WebKit2 as WK

import {-# SOURCE #-} Hawk

data Global = Global
  { globalWebContext :: !WK.WebContext
  , globalStyleSheets :: V.Vector WK.UserStyleSheet
  }

data Bindings
  = Command
    { commandCount :: Maybe Word32
    }
  | PassThru
    { bindingsReturn :: HawkM Bindings
    }
    {-
  | Prompt 
    { promptPrompt :: !String
    , promptInput :: !Input
    , promptCompleter :: Completer
    , promptExec :: Maybe String -> UzblM ()
    }
  | Capture
    { captureFun :: ModKey -> UzblM ()
    , bindingsReturn :: Bindings 
    }
    -}

instance Default Bindings where
  def = Command Nothing

data State = State
  { stateBindings :: !Bindings
  , stateStyleSheet :: !Int
  }

instance Default State where
  def = State
    { stateBindings = def
    , stateStyleSheet = 0
    }
