module State
  ( Bindings(..)
  , State(..)
  ) where

import           Data.Default (Default(def))

data Bindings
  = Command
    { commandCount :: Maybe Int
    }
    {-
  | PassThrough
    { bindingsReturn :: Bindings 
    }
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
