module Consolation.Console (
  Console
, lift
, input
, output
, exit
, continue
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

instance Cli m => Functor (Console m) where
  fmap f (Return a) = return (f a)
  fmap f (Stop)     = exit
  fmap f c          = DoMonad $ fmap f <$> step c

instance Cli m => Applicative (Console m) where
  (<*>) (Return f) a = f <$> a
  (<*>) (Stop) a     = exit
  (<*>) c a          = DoMonad $ liftM2 (<*>) (step c) (return a)
  pure               = Return

instance Cli m => Monad (Console m) where
  (>>=) (Return a) f = f a
  (>>=) (Stop) f     = exit
  (>>=) c f          = DoMonad $ liftM2 (>>=) (step c) (return f)

continue :: Cli m => Console m ()
continue = return ()

output :: Cli m => String -> Console m ()
output s = Output s (return ())

input :: Cli m => Console m String
input = Input return

exit :: Cli m => Console m a
exit = Stop

step :: (Cli m) => Console m a -> m (Console m a)
step (Input f) = maybe exit f <$> getALine
step (Output s c) = putALine s >> return c
step (DoMonad m) = m
step x = return x

run :: (Cli m) => Console m a -> m ()
run (Stop) = return ()
run (Return a) = return ()
run c = step c >>= run

lift :: (Cli m) => m a -> Console m a
lift aM = DoMonad $ return <$> aM
