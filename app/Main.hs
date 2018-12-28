module Main where

import Lib
import System.Environment

usage = error "Usage: wfc-haskell-exe inputbmp outputbmp"

main :: IO ()
main = do
    args <- getArgs
    case args of
        [inputFile, outputFile] -> return () -- TODO: call into Lib.hs
        _ -> usage
