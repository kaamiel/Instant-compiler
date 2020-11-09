module MainCommon (mainCommon, Err(..), RunFun, myLLexer) where

import System.Environment (getArgs)
import System.Exit (exitFailure)

import LexInstant
import ParInstant
import PrintInstant
import AbsInstant
import ErrM

type ParseFun a = [Token] -> Err a
type RunFun = ParseFun (Program (Maybe (Int, Int))) -> String -> IO ()

myLLexer = myLexer

runFile :: ParseFun (Program (Maybe (Int, Int))) -> RunFun -> FilePath -> IO ()
runFile p r f = readFile f >>= r p

usage :: IO ()
usage = do
  putStrLn $ unlines
    [ "usage: Call with one of the following argument combinations:"
    , "  --help          Display this help message."
    , "  (no arguments)  Parse stdin verbosely."
    , "  (files)         Parse content of files verbosely."
    ]
  exitFailure

mainCommon :: RunFun -> IO ()
mainCommon run = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    [] -> getContents >>= run pProgram
    fs -> mapM_ (runFile pProgram run) fs
