module Main where

import System.IO (stderr, hPutStrLn)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath (dropExtension)
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
            print tree
            compile tree
            exitSuccess

runFile :: RunFileFun
runFile p f = do
    content <- readFile f
    putStrLn $ dropExtension f
    output <- run p content
    exitSuccess

main :: IO ()
main = mainCommon runFile
