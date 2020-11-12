module Main where

import System.IO (stderr, hPutStrLn, hPrint)
import System.Exit (exitFailure, exitSuccess, ExitCode(..))
import System.FilePath (replaceExtension)
import System.Process
import LLVMCompiler
import MainCommon

run :: RunFun
run parseFun source =
    let
        ts = myLLexer source
    in
    case parseFun ts of
        Bad s -> do
            hPutStrLn stderr "Parse failed"
            hPutStrLn stderr s
            exitFailure
        Ok tree ->
            compile tree

runFile :: RunFileFun
runFile parseFun filePath = do
    instantSourceCode <- readFile filePath
    let llvmSourceCodeFilePath = replaceExtension filePath "ll"
    let llvmBitcodeFilePath = replaceExtension filePath "bc"
    llvmSourceCode <- run parseFun instantSourceCode
    writeFile llvmSourceCodeFilePath llvmSourceCode
    (exitcode, out, err) <- readProcessWithExitCode ("llvm-as") ["-o", llvmBitcodeFilePath, llvmSourceCodeFilePath] ""
    case exitcode of
        ExitSuccess ->
            exitSuccess
        ExitFailure i -> do
            hPutStrLn stderr $ "An error occurred (exit code: " ++ show i ++ ")"
            hPutStrLn stderr out
            hPutStrLn stderr err
            exitFailure


main :: IO ()
main = mainCommon runFile
