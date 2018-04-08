module Hawk where

import           Control.Monad.Reader (ReaderT)

data Hawk
type HawkM = ReaderT Hawk IO
