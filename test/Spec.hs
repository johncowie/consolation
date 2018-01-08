{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Consolation.Cli (Cli(..))
import Consolation.Console (Console, input, output, run, exit, continue)
import Consolation.Spec (runExpectedIO, ExpectedIO, stdin, stdout, expectedOutput, IOState)
import Test.Hspec (Expectation, shouldReturn, describe, it, hspec, before_, after_)
import qualified Control.Monad.State as ST
import Text.Read (readEither)
import System.Directory (removeFile)
import qualified Control.Monad.Trans as T
import Control.Exception (try, IOException)
import Control.Monad.Fix (fix)

class SumAppender m where
  appendSum :: Int -> m (Either IOException ())

instance SumAppender IO where
  appendSum s = try $ appendFile "sums.txt" (show s ++ "\n")

instance (Monad m, SumAppender m, T.MonadTrans t) => SumAppender (t m) where
  appendSum = T.lift . appendSum

enterNumber :: (Cli m) => Console m Int
enterNumber = do
  output "Enter a number"
  s <- input
  case readEither s of
    (Left err) -> output "Invalid number" >> enterNumber
    (Right i) -> return i

enterSum :: (SumAppender m, Cli m) => Console m ()
enterSum = do
  x <- enterNumber
  y <- enterNumber
  result <- T.lift $ appendSum (x + y)
  case result of
    (Left err) -> output (show err) >> exit
    (Right v) -> output "Saved a sum" >> continue
  output "Continue (y) or quit (n)"
  s <- input
  fix $ \continueOrQuit -> do
    case s of
      "y" -> continue
      "n" -> output "Bye!" >> exit
      _  -> output "unrecognised option - please enter y or n" >> continueOrQuit
  enterSum

entryPoint :: (SumAppender m, Cli m) => Console m ()
entryPoint = enterSum

testFlow :: Console (ST.StateT IOState IO) a -> ExpectedIO -> Expectation
testFlow console expectedIO = runExpectedIO console expectedIO `shouldReturn` (expectedOutput expectedIO)

setupFile :: IO ()
setupFile = writeFile "sums.txt" ""

teardownFile :: IO ()
teardownFile = removeFile "sums.txt"

main :: IO ()
main = hspec $ before_ setupFile $ after_ teardownFile $ do
  describe "facts about sum appender app" $ do
    it "can test an individual step" $ do
      testFlow (output "hello") $ stdout "hello"
    it "can write some sums to file" $ do
      testFlow entryPoint $  stdout "Enter a number"
                          ++ stdin "2"
                          ++ stdout "Enter a number"
                          ++ stdin "7"
                          ++ stdout "Saved a sum"
                          ++ stdout "Continue (y) or quit (n)"
                          ++ stdin "y"
                          ++ stdout "Enter a number"
                          ++ stdin "23"
                          ++ stdout "Enter a number"
                          ++ stdin "dave"
                          ++ stdout "Invalid number"
                          ++ stdout "Enter a number"
                          ++ stdin "25"
                          ++ stdout "Saved a sum"
                          ++ stdout "Continue (y) or quit (n)"
                          ++ stdin "n"
                          ++ stdout "Bye!"
      lines <$> readFile "sums.txt" `shouldReturn` ["9", "48"]
