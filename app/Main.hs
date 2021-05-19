module Main where

main :: IO ()
main = do
  command <- getLine
  run command

run :: String -> IO ()
run "hello" = do
    putStrLn "hi"
run s = do
    putStrLn s
    return ()
