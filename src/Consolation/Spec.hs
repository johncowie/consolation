{-# LANGUAGE FlexibleInstances #-}

module Consolation.Spec
( runCliState
, runExpectedIO
, stdin
, stdout
, ExpectedIO
, expectedOutput
, IOState
) where

import Consolation.Cli (Cli(..))
import Consolation.Console (Console, run)
import Control.Monad.State (State, StateT)
import qualified Control.Monad.State as ST
import Data.List (partition)

data IOState = IOState [String] [String]

type ExpectedIO = [(String, Bool)]

instance (Monad m) => Cli (StateT IOState m) where
  putALine s = ST.modify (addToOutputs s)
              where addToOutputs s (IOState inputs outputs) = IOState inputs (outputs ++ [s])
  getALine = do
    (IOState is os) <- ST.get
    ST.put (IOState (maybeTail is) os)
    return (maybeHead is)
    where maybeHead [] = Nothing
          maybeHead (x:xs) = Just x
          maybeTail [] = []
          maybeTail xs = tail xs

stdin :: String -> ExpectedIO
stdin s = [(s, True)]

separate :: ExpectedIO -> ([String], [String])
separate ls = (map fst is, map fst os)
  where (is, os) = partition snd ls

expectedOutput :: ExpectedIO -> [String]
expectedOutput = snd . separate

stdout :: String -> ExpectedIO
stdout s = [(s, False)]

getOutputs :: IOState -> [String]
getOutputs (IOState is os) = os

runCliState :: (State IOState (a, b)) -> [String] -> (b, [String])
runCliState s is = (b, getOutputs ioState)
  where ((a, b), ioState) = ST.runState s (IOState is [])

runExpectedIO :: (Monad m) => Console (StateT IOState m) a -> ExpectedIO -> m [String]
runExpectedIO app io = do
    (a, ioState) <- ST.runStateT (run app) (IOState inputs [])
    return $ getOutputs ioState
  where (inputs, expectedOutputs) = separate io
