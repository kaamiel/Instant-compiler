module Main where

import System.IO (stderr, hPutStrLn)
import System.Exit (exitFailure, exitSuccess, ExitCode(..))
import System.FilePath (takeBaseName, replaceExtension, takeDirectory)
import System.Process
import JVMCompiler
import MainCommon

run :: String -> RunFun
run className parseFun source =
    let
        ts = myLLexer source
    in
    case parseFun ts of
        Bad s -> do
            hPutStrLn stderr "Parse failed"
            hPutStrLn stderr s
            exitFailure
        Ok tree ->
            compile tree className

runFile :: RunFileFun
runFile parseFun filePath = do
    instantSourceCode <- readFile filePath
    let className = takeBaseName filePath
    let jasminSourceCodeFilePath = replaceExtension filePath "j"
    let outputPath = takeDirectory filePath
    jasminSourceCode <- run className parseFun instantSourceCode
    writeFile jasminSourceCodeFilePath jasminSourceCode
    (exitcode, out, err) <- readProcessWithExitCode ("java") ["-jar", "./lib/jasmin.jar", "-d", outputPath, jasminSourceCodeFilePath] ""
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
