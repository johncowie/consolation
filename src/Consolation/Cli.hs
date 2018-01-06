module Consolation.Cli (
  Cli(..)
)
where

class Monad m => Cli m where
  putALine :: String -> m ()
  getALine :: m (Maybe String)

instance Cli IO where
  putALine = putStrLn
  getALine = Just <$> getLine
