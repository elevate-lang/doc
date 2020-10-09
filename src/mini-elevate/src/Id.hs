{-# LANGUAGE DataKinds, FlexibleContexts, TypeApplications, TypeOperators #-}

module Id where

import HList
import Data.IORef
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader
import qualified Control.Monad.Fail as Fail

newtype Id = Id String deriving (Eq, Ord)

instance Show Id where
  show (Id name) = name

strId :: String -> Id
strId = Id

getName :: Id -> String
getName (Id s) = s

genFreshId :: (MonadIO m, Occurs ("NameCounter" :- IORef Int) ts HList, MonadReader (HList ts) m) => String -> m Id
genFreshId prefix = do
  counter <- select @"NameCounter" @(IORef Int) <$> ask
  freshName <- liftIO $ ((prefix ++) . show) <$> readIORef counter
  liftIO $ modifyIORef counter (+ 1)
  return (Id freshName)
