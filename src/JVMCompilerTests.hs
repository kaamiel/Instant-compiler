module JVMCompilerTests where

import JVMCompilerInternal
import AbsInstant
import qualified Data.Map as Map
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State



{- | evalExpr tests

>>> runExceptT . flip evalStateT Map.empty . evalExpr $ ExpAdd Nothing (ExpLit Nothing 1) (ExpLit Nothing 2)
Right 3

>>> runExceptT . flip evalStateT Map.empty . evalExpr $ ExpSub Nothing (ExpLit Nothing 5) (ExpLit Nothing 3)
Right 2

>>> runExceptT . flip evalStateT Map.empty . evalExpr $ ExpMul Nothing (ExpLit Nothing 4) (ExpLit Nothing 6)
Right 24

>>> runExceptT . flip evalStateT Map.empty . evalExpr $ ExpDiv Nothing (ExpLit Nothing 10) (ExpLit Nothing 3)
Right 3

>>> runExceptT . flip evalStateT Map.empty . evalExpr $ ExpLit Nothing 42
Right 42

>>> :{
let
    state :: CompilerState
    state = Map.singleton (Ident "a") 13
in
runExceptT . flip evalStateT state . evalExpr $ ExpVar Nothing (Ident "a")
:}
Right 13

>>> runExceptT . flip evalStateT Map.empty . evalExpr $ ExpVar (Just (1, 2)) (Ident "b")
Left (Error (Just (1,2)) "undefined variable `b`")

-}



{- | execStmt tests

>>> runExceptT . flip execStateT Map.empty . execStmt $ SAss Nothing (Ident "c") (ExpLit Nothing 18)
Right (fromList [(Ident "c",18)])

>>> runExceptT . flip execStateT Map.empty . execStmt $ SExp Nothing (ExpLit Nothing 19)
19
Right (fromList [])



-}

