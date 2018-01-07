module Consolation.Cli (
  Cli(..)
)
where

import qualified Control.Monad.State as ST

class Monad m => Cli m where
  putALine :: String -> m ()
  getALine :: m (Maybe String)

instance Cli IO where
  putALine = putStrLn
  getALine = Just <$> getLine

instance Cli m => Cli (ST.StateT s m) where
  putALine = ST.lift . putALine
  getALine = ST.lift getALine
