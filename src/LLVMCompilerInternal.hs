module LLVMCompilerInternal where

import qualified Data.Set as Set
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

data CompilerState = CompilerState { nextRegister :: Int, variables :: Set.Set String, output :: ShowS }

type Computation = ExceptT Error IO

type CompilerStateT = StateT CompilerState Computation
type ExprState = CompilerStateT String
type StmtState = CompilerStateT ()

prolog, epilog :: String
prolog =
    "@dnl = internal constant [4 x i8] c\"%d\\0A\\00\"\n\n\
    \declare i32 @printf(i8*, ...)\n\n\
    \define void @printInt(i32 %x) {\n\
    \    %t0 = getelementptr [4 x i8], [4 x i8]* @dnl, i32 0, i32 0\n\
    \    call i32 (i8*, ...) @printf(i8* %t0, i32 %x)\n\
    \    ret void\n\
    \}\n\n\
    \define i32 @main() {\n\
    \entry:\n"
epilog =
    "    ret i32 0\n}"

newRegister :: CompilerStateT String
newRegister = do
    r <- gets nextRegister
    modify $ \state -> state { nextRegister = r + 1 }
    return $ "%_" ++ show r

appendToOutput :: String -> CompilerStateT ()
appendToOutput s =
    modify $ \state -> state { output = output state . showString "    " . showString s . showChar '\n' }



evalExpr :: Exp SourceLocation -> ExprState

evalExpr (ExpLit _ n) =
    return $ show n

evalExpr (ExpVar location (Ident x)) = do
    defined <- gets $ Set.member x . variables
    if defined
        then do
            ret <- newRegister
            appendToOutput $ ret ++ " = load i32, i32* %" ++ x
            return ret
        else
            throwError . Error location $ "undefined variable `" ++ x ++ "`"

evalExpr (ExpAdd _ e1 e2) =
    evalArithmeticExpr "add" e1 e2

evalExpr (ExpSub _ e1 e2) =
    evalArithmeticExpr "sub" e1 e2

evalExpr (ExpMul _ e1 e2) =
    evalArithmeticExpr "mul" e1 e2

evalExpr (ExpDiv _ e1 e2) =
    evalArithmeticExpr "sdiv" e1 e2

evalArithmeticExpr :: String -> Exp SourceLocation -> Exp SourceLocation -> ExprState
evalArithmeticExpr instruction arg1 arg2 = do
    value1 <- evalExpr arg1
    value2 <- evalExpr arg2
    ret <- newRegister
    appendToOutput $ ret ++ " = " ++ instruction ++ " i32 " ++ value1 ++ ", " ++ value2
    return ret



execStmt :: Stmt SourceLocation -> StmtState

execStmt (SAss _ (Ident x) e) = do
    defined <- gets $ Set.member x . variables
    value <- evalExpr e
    if defined
        then
            appendToOutput $ "store i32 " ++ value ++ ", i32* %" ++ x
        else do
            modify $ \state -> state { variables = Set.insert x $ variables state }
            appendToOutput $ "%" ++ x ++ " = alloca i32"
            appendToOutput $ "store i32 " ++ value ++ ", i32* %" ++ x

execStmt (SExp _ e) = do
    value <- evalExpr e
    appendToOutput $ "call void @printInt(i32 " ++ value ++ ")"



execProgram :: Program SourceLocation -> StmtState
execProgram (Prog _ stmtsList) =
    mapM_ execStmt stmtsList

compile :: Program SourceLocation -> IO String
compile program = do
    result <- runExceptT . flip execStateT (CompilerState 0 Set.empty id) . execProgram $ program
    case result of
        Right (CompilerState _ _ output) ->
            return $ showString prolog . output . showString epilog $ "\n"
        Left (Error location message) -> do
            let l = maybe "unknown location" (\(line, column) -> show line ++ ":" ++ show column) location
            hPutStrLn stderr "An error occurred"
            hPutStrLn stderr $ l ++ ": " ++ message
            exitFailure
