module Lib
    ( wordleCompare
    , wordlePrint
    , readDict
    , checkIsWord
    ) where

import Data.Char (toUpper)
import Data.Function ((&))
import Data.Text (singleton)
import Rainbow
import GHC.IO.Encoding (getForeignEncoding)

data Condition = In | Hit | Out deriving (Eq,Show)
-- instance Show Condition where
--   show In = "I"
--   show Hit = "H"
--   show Out = "O"

toColor :: Condition -> (Chunk -> Chunk)
toColor c = case c of
              Hit -> fore green
              In -> fore yellow
              Out -> fore white

conPlus :: Condition -> Condition -> Condition
conPlus ca cb
  |(ca == Hit || cb == Hit) = Hit
  |(ca == In || cb == In) = In
  |otherwise = Out

wordleCompare :: String -> String -> [Condition]
wordleCompare goal guess = let hits = zipWith (\x y -> if x == y then Hit else Out) goal guess
                               ins = [if elem letter goal then In else Out | letter <- guess ]
                           in
                             zipWith conPlus hits ins

wordlePrint :: String -> [Condition] -> IO ()
wordlePrint guess conditions = do
  let colors = map toColor conditions
  let guessChunk = map (\x -> chunk $ singleton x) guess
  let outChunks = zipWith (&) guessChunk colors
  putChunksLn outChunks

readDict :: FilePath -> IO [String]
readDict f = do
  contents <- readFile f
  let dict' = lines contents
  let dict = map (\x -> map toUpper x) dict'
  return dict

checkIsWord :: String -> [String] -> Bool
checkIsWord guess dict = elem guess dict
