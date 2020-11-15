module JVMCompilerInternal where

import qualified Data.Map as Map
import Control.Monad.Except
import Control.Monad.State
import System.Exit (exitFailure)
import System.IO (stderr, hPutStrLn)
import AbsInstant

type SourceLocation = Maybe (Int, Int)
type ErrorMessage = String

data Error
    = Error SourceLocation ErrorMessage
  deriving Show

data CompilerState = CompilerState { localsUsed :: Int, currentStackSize :: Int, maxStackSize :: Int, variables :: Map.Map String Int, output :: ShowS }

type Computation = ExceptT Error IO

type CompilerStateT = StateT CompilerState Computation
type ExprState = CompilerStateT ()
type StmtState = CompilerStateT ()

prolog :: String -> Int -> Int -> String
prolog className locals stack =
    ".class public " ++ className ++ "\n\
    \.super java/lang/Object\n\n\
    \.method public <init>()V\n\
    \    aload_0\n\
    \    invokespecial java/lang/Object/<init>()V\n\
    \    return\n\
    \.end method\n\n\
    \.method public static main([Ljava/lang/String;)V\n\
    \.limit locals " ++ show locals ++ "\n\
    \.limit stack " ++ show stack ++ "\n"
epilog :: String
epilog =
    "    return\n.end method"

increaseCurrentStackSize :: CompilerStateT ()
increaseCurrentStackSize = do
    newStackSize <- gets $ (+1) . currentStackSize
    modify $ \state -> state { currentStackSize = newStackSize, maxStackSize = max (maxStackSize state) newStackSize }

decreaseCurrentStackSize :: CompilerStateT ()
decreaseCurrentStackSize =
    modify $ \state -> state { currentStackSize = currentStackSize state - 1 }

appendToOutput :: String -> CompilerStateT ()
appendToOutput s =
    modify $ \state -> state { output = output state . showString "    " . showString s . showChar '\n' }

includeHeight :: Exp a -> Exp (a, Int)
includeHeight expr =
    case expr of
        ExpAdd location e1 e2 ->
            let
                (e1', e2') = (includeHeight e1, includeHeight e2)
            in
            ExpAdd (location, max (getHeight e1') (getHeight e2') + 1) e1' e2'
        ExpSub location e1 e2 ->
            let
                (e1', e2') = (includeHeight e1, includeHeight e2)
            in
            ExpSub (location, max (getHeight e1') (getHeight e2') + 1) e1' e2'
        ExpMul location e1 e2 ->
            let
                (e1', e2') = (includeHeight e1, includeHeight e2)
            in
            ExpMul (location, max (getHeight e1') (getHeight e2') + 1) e1' e2'
        ExpDiv location e1 e2 ->
            let
                (e1', e2') = (includeHeight e1, includeHeight e2)
            in
            ExpDiv (location, max (getHeight e1') (getHeight e2') + 1) e1' e2'
        ExpLit location n ->
            ExpLit (location, 1) n
        ExpVar location ident ->
            ExpVar (location, 1) ident

getHeight :: Exp (a, Int) -> Int
getHeight (ExpLit (_, h) _) = h
getHeight (ExpVar (_, h) _) = h
getHeight (ExpAdd (_, h) _ _) = h
getHeight (ExpSub (_, h) _ _) = h
getHeight (ExpMul (_, h) _ _) = h
getHeight (ExpDiv (_, h) _ _) = h



evalExpr :: Exp (SourceLocation, Int) -> ExprState

evalExpr (ExpLit _ n) = do
    appendToOutput $ instruction n
    increaseCurrentStackSize
    where
        instruction :: Integer -> String
        instruction j
            | j == -1          = "iconst_m1"
            | 0 <= j && j <= 5 = "iconst_" ++ show j
            | otherwise        = "ldc " ++ show j

evalExpr (ExpVar (location, _) (Ident x)) = do
    maybeNumber <- gets $ Map.lookup x . variables
    case maybeNumber of
        Just k -> do
            appendToOutput $ "iload" ++ (if k <= 3 then "_" else " ") ++ show k
            increaseCurrentStackSize
        Nothing ->
            throwError . Error location $ "undefined variable `" ++ x ++ "`"

evalExpr (ExpAdd _ e1 e2) =
    evalArithmeticExpr "iadd" e1 e2

evalExpr (ExpSub _ e1 e2) =
    evalArithmeticExpr "isub" e1 e2

evalExpr (ExpMul _ e1 e2) =
    evalArithmeticExpr "imul" e1 e2

evalExpr (ExpDiv _ e1 e2) =
    evalArithmeticExpr "idiv" e1 e2

evalArithmeticExpr :: String -> Exp (SourceLocation, Int) -> Exp (SourceLocation, Int) -> ExprState
evalArithmeticExpr instruction arg1 arg2 = do
    let h1 = getHeight arg1
    let h2 = getHeight arg2
    if h1 < h2
        then do
            evalExpr arg2
            evalExpr arg1
            when (instruction == "isub" || instruction == "idiv") $ appendToOutput "swap"
        else do
            evalExpr arg1
            evalExpr arg2
    appendToOutput instruction
    decreaseCurrentStackSize



execStmt :: Stmt SourceLocation -> StmtState

execStmt (SAss _ (Ident x) e) = do
    maybeNumber <- gets $ Map.lookup x . variables
    evalExpr $ includeHeight e
    k <- maybe (do
                    j <- gets localsUsed
                    modify $ \state -> state { localsUsed = j + 1, variables = Map.insert x j $ variables state }
                    return j)
                return
                maybeNumber
    appendToOutput $ "istore" ++ (if k <= 3 then "_" else " ") ++ show k
    decreaseCurrentStackSize

execStmt (SExp _ e) = do
    appendToOutput "getstatic java/lang/System/out Ljava/io/PrintStream;"
    increaseCurrentStackSize
    evalExpr $ includeHeight e
    appendToOutput "invokevirtual java/io/PrintStream/println(I)V"
    decreaseCurrentStackSize
    decreaseCurrentStackSize



execProgram :: Program SourceLocation -> StmtState
execProgram (Prog _ stmtsList) =
    mapM_ execStmt stmtsList

compile :: Program SourceLocation -> String -> IO String
compile program className = do
    result <- runExceptT . flip execStateT (CompilerState 1 0 0 Map.empty id) . execProgram $ program
    case result of
        Right (CompilerState locals _ stack _ output) -> do
            return $ showString (prolog className locals stack) . output . showString epilog $ "\n"
        Left (Error location message) -> do
            let l = maybe "unknown location" (\(line, column) -> show line ++ ":" ++ show column) location
            hPutStrLn stderr "An error occurred"
            hPutStrLn stderr $ l ++ ": " ++ message
            exitFailure
