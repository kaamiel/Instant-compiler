module MainCommon (mainCommon, Err(..), RunFun, RunFileFun, myLLexer) where

import System.Environment (getArgs)
import System.Exit (exitFailure)

import LexInstant
import ParInstant
import PrintInstant
import AbsInstant
import ErrM

type ParseFun a = [Token] -> Err a
type RunFun = ParseFun (Program (Maybe (Int, Int))) -> String -> IO String
type RunFileFun = ParseFun (Program (Maybe (Int, Int))) -> FilePath -> IO ()

myLLexer = myLexer

usage :: IO ()
usage = do
  putStrLn $ unlines
    [ "usage: Call with one of the following argument combinations:"
    , "  --help          Display this help message."
    , "  (no arguments)  Parse stdin verbosely."
    , "  (files)         Parse content of files verbosely."
    ]
  exitFailure

mainCommon :: RunFileFun -> IO ()
mainCommon runFile = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    fs -> mapM_ (runFile pProgram) fs
