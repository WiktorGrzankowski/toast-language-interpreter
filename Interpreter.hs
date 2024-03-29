module Interpreter where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Except
import Control.Monad.Except
import Data.Map as Map
import Toast.AbsToast

import System.IO (hPutStrLn, stderr)

type Var = String
type Loc = Int
type Env = Map Var Loc
type EnvFun = Map String Fun
type Store = Map Loc Value

type Pos = BNFC'Position

data ArgKind = ByValue | ByReference deriving (Show, Eq)
data FunArg = FunArg { argKind :: ArgKind, argType :: Type, argName :: Var } deriving Show

data Value = VInt Integer | VStr String | VBool Bool 
data Fun = Fun { args :: [FunArg], block :: Block, staticEnv :: Env, staticEnvFun :: EnvFun } deriving Show
data Mem = Mem { env :: Env, store :: Store, envFun :: EnvFun } deriving Show
data StmtReturnValue = VValue Value | VVoid | VContinue | VBreak deriving Show
data InterpreterError = InterpreterError { text :: String, position :: Pos }
type InterpreterMonad a = ExceptT InterpreterError (StateT Mem IO) a

instance Show Value where
    show (VInt n) = show n 
    show (VStr s) = s 
    show (VBool b) = show b

instance Show InterpreterError where
    show err = "ERROR: " ++ (text err) ++ " Check your code at line " ++ show line ++ " column " ++ show column ++ "." where
        Just (line, column) = position err


alloc :: Store -> Loc
alloc = Map.size


getLoc :: Var -> Pos -> InterpreterMonad Loc
getLoc x pos = do 
    memory <- get
    case Map.lookup x (env memory) of
        Nothing ->  throwError $ InterpreterError { text = "Variable {" ++ x ++ "} is not in the scope.", position = pos}
        Just l -> return l


getVal :: Loc -> Pos -> InterpreterMonad Value
getVal location pos = do
    memory <- get
    case Map.lookup location (store memory) of
        Nothing -> throwError $ InterpreterError { text = "No value under given location.", position = pos}
        Just v -> return v

getFun :: String -> Pos -> InterpreterMonad Fun
getFun f pos = do
    envF <- gets envFun
    case Map.lookup f envF of
        Nothing -> except $ Left $ InterpreterError { text = "Function {" ++ f ++ "} is not in the scope.", position = pos}
        Just fun -> return fun


writeLoc :: Loc -> Value -> InterpreterMonad ()
writeLoc location val = do
    memory <- get
    put (Mem { env = env memory, store = Map.insert location val (store memory), envFun = envFun memory})
    return ()


getBool :: Value -> Bool
getBool v = case v of
            VBool b -> b


getInt :: Value -> Integer
getInt v = case v of
            VInt n -> n


evalExpr :: Expr -> InterpreterMonad Value

evalExpr (ELitInt _ n) = return (VInt n)

evalExpr (Neg pos e) = do
    v <- evalExpr e 
    case v of
        VInt n -> return $ VInt (-n)
        _ -> throwError $ InterpreterError { text = "Cannot negate non integers.", position = pos }


evalExpr (EVar pos (Ident x)) = do
    location <- getLoc x pos
    getVal location pos

evalExpr (EAdd pos e1 op e2) = do
    v1 <- evalExpr e1
    v2 <- evalExpr e2
    case (v1, v2) of
        (VInt val1, VInt val2) -> calcAdd op val1 val2
        (_, _) -> throwError $ InterpreterError { text = "Cannot do arithmetics with non integers.", position = pos }

    where
        calcAdd :: AddOp -> Integer -> Integer -> InterpreterMonad Value
        calcAdd (Toast.AbsToast.Plus _) a b = return $ VInt $ a + b
        calcAdd (Toast.AbsToast.Minus _) a b = return $ VInt $ a - b


evalExpr (EMul pos e1 op e2) = do
    v1 <- evalExpr e1
    v2 <- evalExpr e2
    case (v1, v2) of
        (VInt val1, VInt val2) -> calcMul op val1 val2
        (_, _) -> throwError $ InterpreterError { text = "Cannot do arithmetics with non integers.", position = pos }
    
    where
        calcMul :: MulOp -> Integer -> Integer -> InterpreterMonad Value
        calcMul (Toast.AbsToast.Times _) a b = return $ VInt $ a * b 
        calcMul (Toast.AbsToast.Div _)   _ 0 = throwError $ InterpreterError { text = "Division by zero.", position = pos}
        calcMul (Toast.AbsToast.Div _)   a b = return $ VInt $ a `div` b 
        calcMul (Toast.AbsToast.Mod _)   _ 0 = throwError $ InterpreterError { text = "Modulo by zero.", position = pos}
        calcMul (Toast.AbsToast.Mod _)   a b = return $ VInt $ a `mod` b

evalExpr (EString _ s) = return (VStr s)

evalExpr (ELitTrue _) = return (VBool True)

evalExpr (ELitFalse _) = return (VBool False)

evalExpr (Not pos e) = do
    b <- evalExpr e 
    case b of
        (VBool bb) -> return (VBool (not bb))
        _ -> throwError $ InterpreterError { text = "Cannot do {not} with non booleans.", position = pos}

evalExpr (EOr pos e1 e2) = do
    v1 <- evalExpr e1
    v2 <- evalExpr e2
    case (v1, v2) of
        (VBool val1, VBool val2) -> return (VBool (val1 || val2))
        (_, _) -> throwError $ InterpreterError { text = "Cannot do {or} with non booleans.", position = pos}

evalExpr (EAnd pos e1 e2) = do
    v1 <- evalExpr e1
    v2 <- evalExpr e2
    case (v1, v2) of
        (VBool val1, VBool val2) -> return (VBool (val1 && val2))
        (_, _) -> throwError $ InterpreterError { text = "Cannot do {and} with non booleans.", position = pos}

evalExpr (ERel pos e1 op e2) = do
    v1 <- evalExpr e1
    v2 <- evalExpr e2
    case (v1, v2) of
        (VInt val1, VInt val2) -> calcRel op val1 val2
        (_, _) -> throwError $ InterpreterError { text = "Cannot compare non integers.", position = pos}
    
    where
        calcRel :: RelOp -> Integer -> Integer -> InterpreterMonad Value
        calcRel (Toast.AbsToast.LTH _) a b = return $ VBool $ (a < b) 
        calcRel (Toast.AbsToast.LE _) a b = return $ VBool $ (a <= b) 
        calcRel (Toast.AbsToast.GTH _) a b = return $ VBool $ (a > b) 
        calcRel (Toast.AbsToast.GE _) a b = return $ VBool $ (a >= b) 
        calcRel (Toast.AbsToast.EQU _) a b = return $ VBool $ (a == b) 
        calcRel (Toast.AbsToast.NE _) a b = return $ VBool $ (a /= b) 

evalExpr (EApp pos (Ident f) arguments) = do
    function <- getFun f pos
    memoryBeforeCall <- get
    if not (checkArgumentsCount (args function) arguments)
        then 
            throwError $ InterpreterError { text = "Function {" ++ f ++ "} called with a wrong number of arguments. " ++ 
            "Should be " ++ show (length (args function)) ++ " but was " ++ show (length arguments) ++ ".",
            position = pos} 
        else
            if not (checkArgumentsKind (args function) arguments)
                then
                    throwError $ InterpreterError { text = "Function {" ++ f ++ "} called with wrong kinds of arguments. " ++
                    "Check if all arguments that are expected to be referenes are called by reference.",
                    position = pos} 
            else do
                prepareEnv arguments (args function)
                memoryWithArguments <- get

                put (Mem { env = env memoryWithArguments, store = store memoryWithArguments, envFun = Map.insert f (function) (staticEnvFun function) })
                blockValue <- execBlock $ block function
                storeAfterFunction <- gets store

                put (Mem { env = env memoryBeforeCall, store = storeAfterFunction, envFun = envFun memoryBeforeCall })    

                case blockValue of
                    VValue v -> return v
    where
    checkArgumentsCount :: [FunArg] -> [ExprArg] -> Bool
    checkArgumentsCount functionArguments callArguments = length functionArguments == length callArguments

    -- Should be called only when arguments count is correct.
    checkArgumentsKind :: [FunArg] -> [ExprArg] -> Bool
    checkArgumentsKind [] _ = True
    checkArgumentsKind (funArg : rest) ((EArg _ e) : other)
        | argKind funArg == ByValue = checkArgumentsCount rest other
        | otherwise = False
    checkArgumentsKind (funArg : rest) ((EArgRef _ (Ident x)) : other)
        | argKind funArg == ByReference = checkArgumentsCount rest other
        | otherwise = False



prepareEnv :: [ExprArg] -> [FunArg] -> InterpreterMonad Env

prepareEnv _ [] = gets env

prepareEnv ((EArg pos e) : rest) (funArg : other) = do
    evalItems (argType funArg) [(Init pos (Ident (argName funArg)) e)]
    prepareEnv rest other

prepareEnv ((EArgRef pos (Ident x)) : rest) (funArg : other) = do
    memory <- get
    referencedLocation <- getLoc x pos
    let newName = argName funArg
    put (Mem { env = Map.insert newName referencedLocation (env memory), store = store memory, envFun = envFun memory})
    prepareEnv rest other


execBlock :: Block -> InterpreterMonad StmtReturnValue

execBlock (Blk _ []) = return VVoid

execBlock (Blk pos stmts) = do
    frozenEnv <- gets env
    blockReturnValue <- execBlockImpl (Blk pos stmts)
    memory <- get
    put (Mem { env = frozenEnv, store = store memory, envFun = envFun memory })
    return blockReturnValue


execBlockImpl :: Block -> InterpreterMonad StmtReturnValue

execBlockImpl (Blk _ []) = return VVoid
execBlockImpl (Blk pos (stmt:rest)) = do
    stmtValue <- execStmt stmt
    case stmtValue of
        (VValue v) -> return stmtValue
        VBreak -> return stmtValue
        VContinue -> return stmtValue
        VVoid -> execBlockImpl (Blk pos rest)


evalItems :: Type -> [Item] -> InterpreterMonad StmtReturnValue

evalItems t [] = return VVoid

evalItems t ((NoInit _ (Ident x)) : rest) = do
    memory <- get
    let newLoc = alloc (store memory)
    let value = case t of
                (TStr _) -> VStr ""
                (TInt _) -> VInt 0
                (TBool _) -> VBool False
    put (Mem { env = Map.insert x newLoc (env memory), store = Map.insert newLoc value (store memory), envFun = envFun memory})
    
    evalItems t rest

evalItems t ((Init pos (Ident x) e) : rest) = do
    memory <- get
    let newLoc = alloc (store memory)
    value <- evalExpr e

    memoryAfterEval <- get

    case (t, value) of
        (TStr _, VStr _) -> put (Mem { env = Map.insert x newLoc (env memory), store = Map.insert newLoc value (store memoryAfterEval), envFun = envFun memory})
        (TInt _, VInt _) -> put (Mem { env = Map.insert x newLoc (env memory), store = Map.insert newLoc value (store memoryAfterEval), envFun = envFun memory})
        (TBool _, VBool _) -> put (Mem { env = Map.insert x newLoc (env memory), store = Map.insert newLoc value (store memoryAfterEval), envFun = envFun memory})
        (_, _) -> throwError $ InterpreterError { text = "Wrong type.", position = pos}

    evalItems t rest  



execStmt :: Stmt -> InterpreterMonad StmtReturnValue

execStmt (Empty _) = return VVoid

execStmt (BStmt _ block) = execBlock block

execStmt (Decl _ t items) = evalItems t items

execStmt (CondElse pos condition block1 block2) = do
    v <- evalExpr condition
    case v of
        (VBool i) -> if i == True then execBlock block1 else execBlock block2
        _ -> throwError $ InterpreterError { text = "Condition must be a boolean.", position = pos}

execStmt (Cond pos condition block) = do
    v <- evalExpr condition
    case v of
        (VBool i) -> if i == True then execBlock block else execStmt (Empty pos)
        _ -> throwError $ InterpreterError { text = "Condition must be a boolean.", position = pos}

execStmt (While pos condition block) = do
    v <- evalExpr condition
    case v of
        (VBool i) -> if i == True 
                        then do
                            blockValue <- execBlock block
                            case blockValue of
                                VBreak -> execStmt (Empty pos)
                                VContinue -> execStmt (While pos condition block)
                                VVoid -> execStmt (While pos condition block)
                                (VValue v) -> return blockValue
                        else do
                            execStmt (Empty pos)
        _ -> throwError $ InterpreterError { text = "Condition must be a boolean.", position = pos}

execStmt (Break _) = return VBreak

execStmt (Continue _) = return VContinue

execStmt (Ret _ e) = do
    v <- evalExpr e 
    return $ VValue v

execStmt (Incr pos (Ident x)) = do
    location <- getLoc x pos
    currentValue <- getVal location pos
    case currentValue of
        (VInt valueInt) -> writeLoc location (VInt (valueInt + 1))
        _ -> throwError $ InterpreterError { text = "Cannot increment non booleans.", position = pos}

    return VVoid

execStmt (Decr pos (Ident x)) = do
    location <- getLoc x pos
    currentValue <- getVal location pos
    case currentValue of
        (VInt valueInt) -> writeLoc location (VInt (valueInt - 1))
        _ -> throwError $ InterpreterError { text = "Cannot decrement non booleans.", position = pos}

    return VVoid

execStmt (Ass pos (Ident x) e) = do
    location <- getLoc x pos
    value <- evalExpr e 

    currentValue <- getVal location pos
    case (currentValue, value) of
        (VInt _, VInt _) -> writeLoc location value
        (VStr _, VStr _) -> writeLoc location value
        (VBool _, VBool _) -> writeLoc location value
        (_, _) -> throwError $ InterpreterError { text = "Wrong type.", position = pos}

    return VVoid

execStmt (SPrint _ e) = do
    v <- evalExpr e 
    liftIO $ putStrLn $ show v
    return VVoid

execStmt (FnDef _ t (Ident funName) args block) = do
    memory <- get
    let staticEnv = env memory 
    let staticEnvFun = envFun memory

    let newFun = Fun { args = prepareArgs args, block = block, staticEnv = staticEnv, staticEnvFun = staticEnvFun }
    put (Mem { env = env memory, store = store memory, envFun = Map.insert funName newFun (envFun memory) })
    return VVoid

prepareArgs :: [Arg] -> [FunArg]
prepareArgs args = go args [] where
    go [] mapped = reverse mapped
    go ((Ar _ t (Ident x)) : rest) mapped = go rest (FunArg { argKind = ByValue, argType = t, argName = x } : mapped)
    go ((ArgRef _ t (Ident x)) : rest) mapped = go rest (FunArg { argKind = ByReference, argType = t, argName = x } : mapped)
    

runInterpreter :: Program -> IO ()
runInterpreter prog = execStateT (runExceptT (catchError (runP prog) handleErr)) emptyMem >> return () where
    emptyMem :: Mem
    emptyMem = Mem { env = Map.empty, store = Map.empty, envFun = Map.empty }

    handleErr :: InterpreterError -> InterpreterMonad ()
    handleErr err = do
        liftIO $ hPutStrLn stderr (show err)
        return ()

    runP :: Program -> InterpreterMonad ()
    runP (Prg _ stmts) = mapM execStmt stmts >> return ()
