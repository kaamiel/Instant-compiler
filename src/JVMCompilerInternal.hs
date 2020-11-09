module JVMCompilerInternal where

import qualified Data.Map as Map
import Control.Monad.Except
import Control.Monad.State
import System.Exit (exitFailure, exitSuccess)
import System.IO (stderr, hPutStrLn, hPrint)
import AbsInstant

type SourceLocation = Maybe (Int, Int)
type ErrorMessage = String

data Error =
    Error SourceLocation ErrorMessage
  deriving Show

type CompilerState = Map.Map Ident Integer

type Computation = ExceptT Error IO

type CompilerStateT = StateT CompilerState Computation
type ExprState = CompilerStateT Integer
type StmtState = CompilerStateT ()


-- data Exp a
--     = ExpAdd a (Exp a) (Exp a)
--     | ExpSub a (Exp a) (Exp a)
--     | ExpMul a (Exp a) (Exp a)
--     | ExpDiv a (Exp a) (Exp a)
--     | ExpLit a Integer
--     | ExpVar a Ident

evalExpr :: Exp SourceLocation -> ExprState

evalExpr (ExpAdd location e1 e2) = do
    value1 <- evalExpr e1
    value2 <- evalExpr e2
    return $ value1 + value2

evalExpr (ExpSub location e1 e2) = do
    value1 <- evalExpr e1
    value2 <- evalExpr e2
    return $ value1 - value2

evalExpr (ExpMul location e1 e2) = do
    value1 <- evalExpr e1
    value2 <- evalExpr e2
    return $ value1 * value2

evalExpr (ExpDiv location e1 e2) = do
    value1 <- evalExpr e1
    value2 <- evalExpr e2
    return $ div value1 value2

evalExpr (ExpLit _ n) =
    return n

evalExpr (ExpVar location x@(Ident ident)) = do
    maybeValue <- gets (Map.lookup x)
    maybe (throwError . Error location $ "undefined variable `" ++ ident ++ "`") return maybeValue



-- data Stmt a
--     = SAss a Ident (Exp a)
--     | SExp a (Exp a)

execStmt :: Stmt SourceLocation -> StmtState

execStmt (SAss _ x e) = do
    value <- evalExpr e
    modify $ Map.insert x value

execStmt (SExp _ e) = do
    value <- evalExpr e
    liftIO $ print value



execProgram :: Program SourceLocation -> StmtState
execProgram (Prog location stmtsList) =
    mapM_ execStmt stmtsList

compile :: Program SourceLocation -> IO ()
compile program = do
    result <- runExceptT . flip evalStateT Map.empty . execProgram $ program
    case result of
        Right () -> exitSuccess
        Left (Error location message) -> do
            let l = maybe "unknown location" (\(line, column) -> show line ++ ":" ++ show column) location
            hPutStrLn stderr "An error occurred"
            hPutStrLn stderr $ l ++ ": " ++ message
            exitFailure
