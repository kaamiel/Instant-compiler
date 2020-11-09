module LLVMCompilerInternal where

import AbsInstant

type SourceLocation = Maybe (Int, Int)

compile :: Program SourceLocation -> IO ()
compile program = do
    putStrLn "LLVM działa!!!!!"

