module LLVMCompilerTests where

import LLVMCompilerInternal
import AbsInstant
import qualified Data.Set as Set
import Control.Monad.Except
import Control.Monad.State



{- | evalExpr tests

>>> runExcept . flip evalStateT (CompilerState 0 Set.empty id) . evalExpr $ ExpLit Nothing 11
Right "11"

>>> runExcept . flip evalStateT (CompilerState 0 (Set.singleton "a") id) . evalExpr $ ExpVar Nothing (Ident "a")
Right "%_0"

>>> runExcept . flip evalStateT (CompilerState 0 Set.empty id) . evalExpr $ ExpVar (Just (1, 2)) (Ident "b")
Left (Error (Just (1,2)) "undefined variable `b`")

>>> runExcept . flip evalStateT (CompilerState 0 Set.empty id) . evalExpr $ ExpAdd Nothing (ExpLit Nothing 1) (ExpLit Nothing 2)
Right "%_0"

-}

