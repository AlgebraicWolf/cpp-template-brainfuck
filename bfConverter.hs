module Main where

import Data.Maybe (catMaybes)

charToCommand :: Char -> Maybe String 
charToCommand '+' = Just "Increment"
charToCommand '-' = Just "Decrement"
charToCommand '<' = Just "Left"
charToCommand '>' = Just "Right"
charToCommand ',' = Just "Input"
charToCommand '.' = Just "Output"
charToCommand '[' = Just "LoopStart"
charToCommand ']' = Just "LoopEnd"
charToCommand _ = Nothing

progToSymbs :: String -> [String]
progToSymbs = catMaybes . (map charToCommand)

symbsToInput :: [String] -> String 
symbsToInput [] = "Nil"
symbsToInput (x:xs) = "Cons<" ++ x ++ ", " ++ symbsToInput xs ++ ">"

convert :: String -> String 
convert = symbsToInput . progToSymbs

main :: IO ()
main = do
  input <- getContents 
  putStr $ convert input