module Main where

import System.IO (stderr, hPutStrLn)
import System.Exit (exitFailure, exitSuccess)
import JVMCompiler
import MainCommon

run :: RunFun
run p s =
    let
        ts = myLLexer s
    in
    case p ts of
        Bad s -> do
            hPutStrLn stderr "Parse failed"
            hPutStrLn stderr s
            exitFailure
        Ok tree -> do
            compile tree
            exitSuccess

main :: IO ()
main = mainCommon run
