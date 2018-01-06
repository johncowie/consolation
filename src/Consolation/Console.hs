module Consolation.Console (
  Console
, lift
, input
, output
, stop
, run
)
where

import Consolation.Cli (Cli(getALine, putALine))
import Control.Monad (liftM2)

data Console m a =   Input (String -> Console m a)
                   | Output String (Console m a)
                   | DoMonad (m (Console m a))
                   | Return a
                   | Stop

instance (Cli m) => Functor (Console m) where
  fmap f (Return a) = return (f a)
  fmap f (Stop)     = stop
  fmap f c          = monad $ fmap f <$> continue c

instance (Cli m) => Applicative (Console m) where
  (<*>) (Return f) a = f <$> a
  (<*>) (Stop) a     = stop
  (<*>) c a          = monad $ liftM2 (<*>) (continue c) (return a)
  pure               = Return

instance (Cli m) => Monad (Console m) where
  (>>=) (Return a) f = f a
  (>>=) (Stop) f     = stop
  (>>=) c f          = monad $ liftM2 (>>=) (continue c) (return f)

output = Output
input = Input
stop = Stop
monad = DoMonad

continue :: (Cli m) => Console m a -> m (Console m a)
continue (Input f) = maybe stop f <$> getALine
continue (Output s c) = putALine s >> return c
continue (DoMonad m) = m
continue x = return x

run :: (Cli m) => Console m a -> m ()
run (Stop) = return ()
run (Return a) = return ()
run c = continue c >>= run

lift :: (Cli m) => m a -> Console m a
lift aM = monad $ return <$> aM
