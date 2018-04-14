{-# LANGUAGE RankNTypes #-}

module JSON
  ( defaultParse
  , parseJSON
  , (.<~)
  , (.<-)
  , parseObject
  , modifyObject
  ) where

import           Control.Arrow (second)
import           Control.Lens (Lens', (^.), (.~))
import           Control.Monad (unless)
import           Control.Monad.State (StateT, execStateT, get, put, modify, lift)
import           Data.Aeson (parseJSON)
import qualified Data.Aeson as J
import qualified Data.Aeson.Types as J (Parser, parseEither, emptyObject)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

defaultParse :: J.FromJSON a => a
defaultParse = either error id $ J.parseEither parseJSON J.emptyObject

type ObjectParser a = StateT (J.Object, a) J.Parser ()

(.<~) :: Lens' b a -> T.Text -> (a -> J.Value -> J.Parser a) -> ObjectParser b
(.<~) f k p = do
  (o, r) <- get
  mapM_
    (\v -> do
      x <- lift $ p (r ^. f) v
      put (HM.delete k o, f .~ x $ r))
    $ HM.lookup k o

(.<-) :: J.FromJSON a => Lens' b a -> T.Text -> ObjectParser b
(.<-) f k = f .<~ k $ const parseJSON

parseObject :: a -> String -> ObjectParser a -> J.Value -> J.Parser a
parseObject r _ _ J.Null = return r
parseObject r n p v = J.withObject n (\o -> do
  (o', r') <- execStateT p (o, r)
  unless (HM.null o') $ fail $ "Unknown fields in " ++ n ++ ": " ++ show (HM.keys o')
  return r') v

modifyObject :: (a -> a) -> ObjectParser a
modifyObject = modify . second
