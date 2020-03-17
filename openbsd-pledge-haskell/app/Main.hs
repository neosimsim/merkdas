module Main (main) where

import System.OpenBSD.Process

main :: IO ()
main = do
    pledge (Just "stdio") Nothing
    putStrLn "Hallo Welt"
    _ <- getLine
    putStrLn "done"



