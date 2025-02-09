module Main (main) where

import Lib (wordleCompare, wordlePrint, readDict, checkIsWord)
import System.Random (randomRIO)
import System.IO
import Data.Char (toUpper)
import Data.Text (pack)
import Data.Function ((&))
import Rainbow
import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)
-- import Text.Printf (printf)

main :: IO ()
main = do
  wordlePath' <- lookupEnv "WORDLE_PATH"
  dictPath' <- lookupEnv "DICT_PATH"
  let wordlePath = fromMaybe "words/CET6/5.txt" wordlePath'
  let dictPath = fromMaybe "words/all.txt" dictPath'

  wordles <- readDict wordlePath
  dict' <- readDict dictPath

  let wordleLength = length wordles
  wordleSelect <- randomRIO (0,wordleLength - 1)
  let goal = wordles!!wordleSelect
  let goalLength = length goal
  let dict = filter (\x -> length x == goalLength) dict'

  putChunksLn $ ["The answer has ", (chunk (pack (show goalLength))) & fore green, " letters.\n"]

  win <- tryGuess goal dict 5 0
  let goalChunk = chunk $ pack $ goal
  if win
    then
    putChunksLn $ ["You've got the answer ", goalChunk & fore green, "!"]
    else
    putChunksLn $ ["You've run out of attemps, the answer is ", goalChunk & fore red, "."]

tryGuess :: String -> [String] -> Int -> Int -> IO Bool
tryGuess goal dict lives attempts
  | (lives == attempts) = return False
  | otherwise = do
      putStr $ "[" ++ show (attempts + 1) ++ "/" ++  show lives ++ "] Guess a word: "
      hFlush stdout
      guess' <- getLine
      let guess = map toUpper guess'
      case (length guess == length goal) of
        True | guess == goal -> do
                 putStrLn ""
                 wordlePrint guess $ wordleCompare goal guess
                 putStrLn ""
                 return True
             | (not $ checkIsWord guess dict) -> do
                 putChunkLn $ "Not a word!" & fore red
                 tryGuess goal dict lives attempts
             | otherwise -> do
                 putStrLn ""
                 wordlePrint guess $ wordleCompare goal guess
                 putStrLn ""
                 tryGuess goal dict lives $ attempts + 1
        False -> do
          putChunkLn $ "Word length doesn't match!" & fore red
          tryGuess goal dict lives attempts
