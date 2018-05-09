module Main where

import App
import ElmGen
import System.Environment

main :: IO ()
main = do 
    [arg] <- getArgs
    if arg == "elm" then
        elmGen
    else 
        startApp