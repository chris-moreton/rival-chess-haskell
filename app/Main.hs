module Main where

import Data.List.Split ( splitOn )

main :: IO ()
main = do
  putStrLn "Hello"
  command <- getLine
  let parts = splitOn " " command
  run parts

run :: [String] -> IO ()
run ("hello":xs) = do
    putStrLn $ head xs
run (x:xs) = do
    putStrLn x
    return ()
