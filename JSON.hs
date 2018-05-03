{-# LANGUAGE RankNTypes #-}

module JSON
  ( defaultParse
  , parseJSON
  , ObjectParser
  , (.<~)
  , (.<-)
  , parseObject
  , parseSubObject
  , modifyObject
  ) where

import           Control.Arrow (second)
import           Control.Lens (Lens', (^.), (.~))
import           Control.Monad (unless)
import           Control.Monad.State (StateT(..), execStateT, get, put, modify, lift)
import           Data.Aeson (parseJSON)
import qualified Data.Aeson as J
import qualified Data.Aeson.Internal as J ((<?>), JSONPathElement(Key))
import qualified Data.Aeson.Types as J (Parser, parseEither, emptyObject)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

defaultParse :: J.FromJSON a => a
defaultParse = either error id $ J.parseEither parseJSON J.emptyObject

type ObjectParserM a = StateT (J.Object, a) J.Parser
type ObjectParser a = ObjectParserM a ()

(.<~) :: Lens' b a -> T.Text -> (a -> J.Value -> J.Parser a) -> ObjectParser b
(.<~) f k p = do
  (o, r) <- get
  mapM_
    (\v -> do
      x <- lift $ p (r ^. f) v J.<?> J.Key k
      put (HM.delete k o, f .~ x $ r))
    $ HM.lookup k o

(.<-) :: J.FromJSON a => Lens' b a -> T.Text -> ObjectParser b
(.<-) f k = f .<~ k $ const parseJSON

parseObject :: a -> String -> ObjectParser a -> J.Value -> J.Parser a
parseObject a _ _ J.Null = return a
parseObject a n p v = J.withObject n (\o -> do
  (o', a') <- execStateT p (o, a)
  unless (HM.null o') $ fail $ "Unknown fields in " ++ n ++ ": " ++ show (HM.keys o')
  return a') v

parseSubObject :: b -> ObjectParser b -> ObjectParserM a b
parseSubObject b p = StateT $ \(o, a) -> do
  (o', b') <- execStateT p (o, b)
  return (b', (o', a))

modifyObject :: (a -> a) -> ObjectParser a
modifyObject = modify . second
