module Interpreter where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Except
import Control.Monad.Except
import Data.Map as Map
import Toast.AbsToast

import System.IO

type Var = String
type Loc = Int
type Env = Map Var Loc
type EnvFun = Map String Fun
type Store = Map Loc Value


data ArgKind = ByValue | ByReference deriving Show
data FunArg = FunArg { argKind :: ArgKind, argType :: Type, argName :: Var } deriving Show

data Value = VInt Integer | VStr String | VBool Bool 
data Fun = Fun { args :: [FunArg], block :: Block, staticEnv :: Env, staticEnvFun :: EnvFun } deriving Show
data Mem = Mem { env :: Env, store :: Store, envFun :: EnvFun } deriving Show
data StmtReturnValue = VValue Value | VVoid | VContinue | VBreak deriving Show
type InterpreterMonad a = ExceptT String (StateT Mem IO) a

instance Show Value where
    show (VInt n) = show n 
    show (VStr s) = s 
    show (VBool b) = show b


alloc :: Store -> Loc
alloc = Map.size


getLoc :: Var -> InterpreterMonad Loc
getLoc x = do 
    memory <- get
    case Map.lookup x (env memory) of
        Nothing -> except $ Left ("Variable not declared.")
        Just l -> return l


getVal :: Loc -> InterpreterMonad Value
getVal location = do
    -- nowyEnvik <- gets env
    -- liftIO $ putStrLn $ show nowyEnvik
    -- liftIO $ putStrLn $ show location
    memory <- get
    case Map.lookup location (store memory) of
        Nothing -> except $ Left ("No value under given location.")
        Just v -> return v

getFun :: String -> InterpreterMonad Fun
getFun f = do
    envF <- gets envFun
    case Map.lookup f envF of
        Nothing -> except $ Left("Function called " ++ f ++ " is not in the scope.")
        Just fun -> return fun

writeLoc :: Loc -> Value -> InterpreterMonad ()
writeLoc location val = do
    memory <- get
    put (Mem { env = env memory, store = Map.insert location val (store memory), envFun = envFun memory})
    return ()


evalAddOp :: AddOp -> Integer -> Integer -> Integer
evalAddOp Plus = (+)
evalAddOp Minus = (-)


evalMulOp :: MulOp -> Integer -> Integer -> Integer
evalMulOp Times = (*)
evalMulOp Div = div
evalMulOp Mod = mod


evalRelOp :: RelOp -> Integer -> Integer -> Bool
evalRelOp Toast.AbsToast.LTH = (<)
evalRelOp Toast.AbsToast.LE = (<=)
evalRelOp Toast.AbsToast.GTH = (>)
evalRelOp Toast.AbsToast.GE = (>=)
evalRelOp Toast.AbsToast.EQU = (==)
evalRelOp Toast.AbsToast.NE = (/=)


getBool :: Value -> Bool
getBool v = case v of
            VBool b -> b


getInt :: Value -> Integer
getInt v = case v of
            VInt n -> n


evalExpr :: Expr -> InterpreterMonad Value
-- return ints
evalExpr (ELitInt n) = return (VInt n)

evalExpr (Neg e) = do
    v <- evalExpr e 
    let i = getInt v
    return (VInt (-i)) 

evalExpr (EVar (Ident x)) = getLoc x >>= getVal

evalExpr (EAdd e1 op e2) = do
    v1 <- evalExpr e1
    v2 <- evalExpr e2
    let i1 = getInt v1 
    let i2 = getInt v2
    let simpleOp = evalAddOp op
    return (VInt (i1 `simpleOp` i2)) 

evalExpr (EMul e1 op e2) = do
    v1 <- evalExpr e1
    v2 <- evalExpr e2
    let i1 = getInt v1
    let i2 = getInt v2
    let simpleOp = evalMulOp op
    return (VInt (i1 `simpleOp` i2)) 

-- return strings
evalExpr (EString s) = return (VStr s)

-- return booleans
evalExpr (ELitTrue) = return (VBool True)

evalExpr (ELitFalse) = return (VBool False)

evalExpr (Not e) = do
    v <- evalExpr e 
    let i = getBool v
    return (VBool (not i))

evalExpr (EOr e1 e2) = do
    v1 <- evalExpr e1
    v2 <- evalExpr e2
    let i1 = getBool v1
    let i2 = getBool v2 
    return (VBool (i1 || i2))

evalExpr (EAnd e1 e2) = do
    v1 <- evalExpr e1
    v2 <- evalExpr e2
    let i1 = getBool v1
    let i2 = getBool v2 
    return (VBool (i1 && i2))

evalExpr (ERel e1 op e2) = do
    v1 <- evalExpr e1
    v2 <- evalExpr e2
    let i1 = getInt v1
    let i2 = getInt v2
    let simpleOp = evalRelOp op
    return (VBool (i1 `simpleOp` i2))

-- EApp.      Expr6 ::= Ident "(" [ExprArg] ")" ; -- !!! TODO
{-
EArg.      ExprArg ::= Expr ; 

EArgRef.   ExprArg ::= "&" Ident ;
-}

-- data Fun = Fun { args :: [Arg], block :: Block, staticEnv :: Env } deriving Show
evalExpr (EApp (Ident f) []) = do
    -- printuj pierwwszy argument obliczony



    function <- getFun f
    memoryBeforeCall <- get

    put (Mem { env = staticEnv function, store = store memoryBeforeCall, envFun = staticEnvFun function })
    blockValue <- execBlock $ block function
    storeAfterFunction <- gets store

    put (Mem { env = env memoryBeforeCall, store = storeAfterFunction, envFun = envFun memoryBeforeCall })    
    case blockValue of
        VValue v -> return v

-- argument przez wartość ((EArg e) : rest)
evalExpr (EApp (Ident f) arguments) = do
    function <- getFun f
    memoryBeforeCall <- get

    prepareEnv arguments (args function)
    memoryWithArguments <- get

    put (Mem { env = env memoryWithArguments, store = store memoryWithArguments, envFun = staticEnvFun function })
    blockValue <- execBlock $ block function
    storeAfterFunction <- gets store

    put (Mem { env = env memoryBeforeCall, store = storeAfterFunction, envFun = envFun memoryBeforeCall })    

    case blockValue of
        VValue v -> return v
    
prepareEnv :: [ExprArg] -> [FunArg] -> InterpreterMonad Env
{-
data ArgKind = ByValue | ByReference
data FunArg = FunArg { argKind :: ArgKind, argType :: Type, argName :: Var } 
-}
prepareEnv [] _ = gets env
prepareEnv ((EArg e) : rest) (funArg : other) = do
    evalItems (argType funArg) [(Init (Ident (argName funArg)) e)]
    prepareEnv rest other

-- argument przekazany przez referencję, w funkcji można zmieniać jego wartość
-- teraz lokacja zmiennej o nazwie 'x' jest równa zmiennej o nazwie 'argName funArg'
prepareEnv ((EArgRef (Ident x)) : rest) (funArg : other) = do
    memory <- get

    -- weź lokację zmiennej x
    referencedLocation <- getLoc x
    let newName = argName funArg
    -- gets env
    put (Mem { env = Map.insert newName referencedLocation (env memory), store = store memory, envFun = envFun memory})
    prepareEnv rest other

-- co w ogóle z tym faktem zrobić? muszę nadpisać istniejącego x
-- przy wywołaniu funkcji lokacja 'x' to np. 0, a pod 0 w store jest 10, a lokacja 'z' to np 1
-- wraz z wywołaniem funkcji należy zmienić mapowanie w env 'x', żeby teraz oznaczał miejsce, w którym jest 'z', czyli 1
-- trzeba zrobić, że env('x') := env ('z'). A wewnątrz funkcji 'z' jest niewidoczne.

{-
int x = 10;
int f(int &x) {
    return x;
}
int z = 50;
Print(f(&z)) # powinno wypisać 50
-}

execBlock :: Block -> InterpreterMonad StmtReturnValue

execBlock (Blk []) = return VVoid

execBlock (Blk stmts) = do
    frozenEnv <- gets env
    blockReturnValue <- execBlockImpl (Blk stmts)
    memory <- get
    put (Mem { env = frozenEnv, store = store memory, envFun = envFun memory })
    return blockReturnValue


execBlockImpl :: Block -> InterpreterMonad StmtReturnValue

execBlockImpl (Blk []) = return VVoid
execBlockImpl (Blk (stmt:rest)) = do
    stmtValue <- execStmt stmt
    case stmtValue of
        (VValue v) -> return stmtValue
        VBreak -> return stmtValue
        VContinue -> return stmtValue
        VVoid -> execBlockImpl (Blk rest)


evalItems :: Type -> [Item] -> InterpreterMonad StmtReturnValue

evalItems t [] = return VVoid

evalItems t ((NoInit (Ident x)) : rest) = do
    memory <- get
    let newLoc = alloc (store memory)
    let value = case t of
                TStr -> VStr ""
                TInt -> VInt 0
                TBool -> VBool False
    put (Mem { env = Map.insert x newLoc (env memory), store = Map.insert newLoc value (store memory), envFun = envFun memory})
    evalItems t rest

evalItems t ((Init (Ident x) e) : rest) = do
    memory <- get
    let newLoc = alloc (store memory)
    value <- evalExpr e
    put (Mem { env = Map.insert x newLoc (env memory), store = Map.insert newLoc value (store memory), envFun = envFun memory})
    evalItems t rest  


execStmt :: Stmt -> InterpreterMonad StmtReturnValue

execStmt (Empty) = return VVoid

execStmt (BStmt block) = execBlock block

execStmt (Decl t items) = evalItems t items

execStmt (CondElse condition block1 block2) = do
    v <- evalExpr condition
    let i = getBool v
    if i == True then execBlock block1 else execBlock block2

execStmt (Cond condition block) = do
    v <- evalExpr condition
    let i = getBool v
    if i == True then execBlock block else execStmt Empty

execStmt (While condition block) = do
    v <- evalExpr condition
    let i = getBool v
    if i == True 
        then do
            blockValue <- execBlock block
            case blockValue of
                VBreak -> execStmt Empty
                VContinue -> execStmt (While condition block)
                VVoid -> execStmt (While condition block)
                (VValue v) -> return blockValue
        else do
            execStmt Empty

execStmt Break = return VBreak

execStmt Continue = return VContinue

execStmt VRet = return VVoid

execStmt (Ret e) = do
    v <- evalExpr e 
    return $ VValue v

execStmt (Incr (Ident x)) = do
    location <- getLoc x
    currentValue <- getVal location
    let valueInt = getInt currentValue
    writeLoc location (VInt (valueInt + 1))
    return VVoid

execStmt (Decr (Ident x)) = do
    location <- getLoc x
    currentValue <- getVal location
    let valueInt = getInt currentValue
    writeLoc location (VInt (valueInt - 1))
    return VVoid

execStmt (Ass (Ident x) e) = do
    location <- getLoc x
    value <- evalExpr e 
    writeLoc location value
    return VVoid

execStmt (SPrint e) = do
    v <- evalExpr e 
    liftIO $ putStrLn $ show v
    return VVoid

execStmt (FnDef t (Ident funName) args block) = do
    memory <- get
    let staticEnv = env memory -- frozen, but with arguments (x, y, z...) overwritten to default value that will be set on calling the function
    let staticEnvFun = envFun memory

    let newFun = Fun { args = prepareArgs args, block = block, staticEnv = staticEnv, staticEnvFun = staticEnvFun }
    put (Mem { env = env memory, store = store memory, envFun = Map.insert funName newFun (envFun memory) })
    return VVoid

prepareArgs :: [Arg] -> [FunArg]
prepareArgs args = go args [] where
    go [] mapped = mapped
    go ((Ar t (Ident x)) : rest) mapped = go rest (FunArg { argKind = ByValue, argType = t, argName = x } : mapped)
    go ((ArgRef t (Ident x)) : rest) mapped = go rest (FunArg { argKind = ByReference, argType = t, argName = x } : mapped)
{-
Ar.    	Arg ::= Type Ident;
ArgRef.	Arg ::= Type "&" Ident ;
-}


    

runInterpreter :: Program -> IO ()
runInterpreter prog = execStateT (runExceptT (catchError (runP prog) handleErr)) emptyMem >> return () where
    emptyMem :: Mem
    emptyMem = Mem { env = Map.empty, store = Map.empty, envFun = Map.empty }

    handleErr :: String -> InterpreterMonad StmtReturnValue
    handleErr err = do
        liftIO $ putStrLn err
        return VVoid

    runP :: Program -> InterpreterMonad StmtReturnValue
    runP (Prg []) = return VVoid
    runP (Prg (stmt:rest)) = execStmt stmt >> runP (Prg rest)
