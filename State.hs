module State
  ( Bindings(..)
  , State(..)
  ) where

import           Data.Default (Default(def))
import           Data.Word (Word32)

import {-# SOURCE #-} Hawk

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
  { stateBindings :: Bindings
  }

instance Default State where
  def = State
    { stateBindings = def
    }
