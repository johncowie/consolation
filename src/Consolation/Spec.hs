{-# LANGUAGE FlexibleInstances #-}

module Consolation.Spec
( runCliState
, runExpectedIO
, stdin
, stdout
, ExpectedIO
) where

import Consolation.Cli (Cli(..))
import Control.Monad.State (State, StateT)
import qualified Control.Monad.State as ST
import Data.List (partition)

data IOState = IOState [String] [String]

type ExpectedIO = [(String, Bool)]

instance {-# OVERLAPPING #-} Cli (State IOState) where
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

stdout :: String -> ExpectedIO
stdout s = [(s, False)]

getOutputs :: IOState -> [String]
getOutputs (IOState is os) = os

runCliState :: (State IOState b) -> [String] -> [String]
runCliState s is = getOutputs $ snd $ ST.runState s (IOState is [])

runExpectedIO :: (StateT a (State IOState) b) -> a -> ExpectedIO -> ([String], [String])
runExpectedIO app initState io = (actualOutputs, expectedOutputs)
  where (inputs, expectedOutputs) = separate io
        actualOutputs = runStateStack app initState inputs
        runStateStack app a inputs = runCliState (ST.runStateT app a) inputs
