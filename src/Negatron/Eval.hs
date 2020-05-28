module Negatron.Eval where

import qualified Data.Map.Strict as Map
import Data.Text (Text, pack, unpack)
import System.IO.Unsafe

import Negatron.Ast

-- Type aliased for future addition of different types
type NVal = Int

type Vars = Map.Map Text NVal
type Scope = [Vars]

data State = State { scope :: Scope, functions :: PFunctionMap, globals :: Vars }
           | Error Text
           deriving (Eq, Show)

returnValue :: State -> NVal
returnValue (Error _) = 1 -- Idk if I like this
returnValue s = case slookup "return" s of
    Nothing  -> 0
    (Just x) -> x

scopeUp :: State -> State
scopeUp (State [] fs gs) = (State [] fs gs)
scopeUp (State (_:ss) fs gs) = (State ss fs gs)
scopeUp (Error e) = (Error e)

scopeEnter :: State -> State
scopeEnter (State ss fs gs) = (State (Map.empty:ss) fs gs)
scopeEnter (Error e) = (Error e)

nvalAsBool :: NVal -> Bool
nvalAsBool = (==) 1

slookup :: Text -> State -> Maybe NVal
slookup id (State [] _ gs) = Map.lookup id gs
slookup id (State (s:ss) fs gs) = case Map.lookup id s of
    (Just x) -> Just x
    Nothing -> slookup id (State ss fs gs)
slookup _ (Error _) = Nothing

sset :: Text -> NVal -> State -> State
sset id v (State [] fs gs) = case Map.lookup id gs of
    (Just _) -> gset id v (State [] fs gs)
    Nothing  -> (Error (pack $ "Variable not found in scope while trying to store " ++ (show v) ++ " to " ++ (unpack id)))
sset id v (State (s:ss) fs gs) = case Map.lookup id s of
    (Just _) -> (State ((Map.insert id v s):ss) fs gs)
    Nothing -> case sset id v (State ss fs gs) of 
        (State newScope _ newGlobals ) -> ((State (s:newScope)) fs newGlobals) where
        (Error e) -> (Error e)
sset _ _ (Error e) = (Error e)

gset :: Text -> NVal -> State -> State
gset id v (State ss fs gs) = (State ss fs (Map.insert id v gs))
gset _ _ (Error e) = (Error e)

geval :: Decl -> Vars -> Vars
geval (Decl _ name) globals = Map.insert name 0 globals

peval_ :: Program -> Vars -> IO (NVal)
peval_ (Program funcs []) globals = case Map.lookup "main" funcs of
    Nothing -> do
        putStrLn "No main function found"
        return 1
    (Just f) -> case feval f (State [Map.empty] funcs globals) of
        (State ss fs gs)   -> return $ returnValue (State ss fs gs)
        (Error e) -> do
            putStrLn $ unpack e
            return $ returnValue (Error e)
peval_ (Program funcs (g:gs)) globals = peval_ (Program funcs gs) $ geval g globals

peval :: Program -> IO (NVal)
peval x = peval_ x Map.empty

feval :: Function -> State -> State
feval (Function _ _ _ _ []) s = s
feval (Function t n f l (b:bs)) s = feval (Function t n f l bs) $ unsafePerformIO $ seval b s

fcallEval :: Function -> State -> (NVal, State)
fcallEval _ (Error e) = (1, (Error e))
fcallEval f (State ss fs gs) = case feval f (State [head ss] fs gs) of
    (State newS _ newG) -> (returnValue (State newS fs newG), (State (tail ss) fs newG))
    (Error e) -> (1, (Error e))

seval :: Statement -> State -> IO (State)
seval _ (Error e) = return (Error e)
seval stmt s = case slookup "return" s of
    Nothing  -> sevalImpl stmt s
    (Just _) -> return s

sevalImpl :: Statement -> State ->  IO (State)
sevalImpl (Expr e) s = return $ snd $ eeval e s
sevalImpl (Block stmts) s = do
    let oldScope = foldl (\acc x -> unsafePerformIO $ seval x acc) (scopeEnter s) stmts
    let newScope = scopeUp oldScope
    case slookup "return" oldScope of
        Nothing  -> return newScope
        (Just retr) -> return $ reteval (retr, newScope)
        
sevalImpl (Return e) s = return $ reteval $ eeval e s
sevalImpl (StmtDecl d) s = return $ ( State ((geval d $ head $ scope s):(tail $ scope s)) (functions s) (globals s))
sevalImpl (If e tBody fBody) s = case nvalAsBool $ value of
    True  -> seval tBody state
    False -> seval fBody state
    where
        (value, state) = eeval e s
sevalImpl (While e body) s = case nvalAsBool $ value of
    True  -> seval (While e body) $ unsafePerformIO $ seval body state
    False -> return state
    where
        (value, state) = eeval e s
sevalImpl (Output (StrLit text)) s = do
    putStrLn $ unpack text
    return s
sevalImpl (Output e) s = do
    let (v', s') = eeval e s
    
    putStrLn $ show v'
    return s'
sevalImpl (Input id) s = do
    v <- readLn
    return $ sset id v s

reteval :: (NVal, State) -> State
reteval (value, (State (s:ss) fs gs)) = (State ((Map.insert "return" value s):ss) fs gs)
reteval (value, (State [] fs gs)) = (State [] fs (Map.insert "return" value gs))
reteval (_, (Error e)) = (Error e)

eeval :: Expr -> State -> (NVal, State)
eeval _ (Error e) = (1, (Error e))
eeval (IntLit x) s = (x, s)
eeval (StrLit _) s = (0, s)
eeval (BoolLit x) s = (fromEnum x, s)
eeval (Null) s = (0, s)
eeval (Id id) s = case slookup id s of
    Nothing  -> (0, (Error $ pack ("Variable not found in scope " ++ (unpack id))))
    (Just v) -> (v, s)
eeval (Bin op e1 e2) s = binevall op (eeval e1 s) e2
eeval (Un op e) s = uneval op (eeval e s)
eeval (Call id args) (State ss fs gs) = case Map.lookup id fs of
    Nothing    -> (0, (Error $ pack ("No function found name " ++ (unpack id))))
    (Just ftn) -> fcallEval ftn actuals where
        actuals = foldl (\acc (arg, id) -> sset id arg acc) updatedState argValues where
            updatedState = (State ((foldl (\iacc x -> Map.insert (declName x) 0 iacc) Map.empty $ formals ftn):ss) fs gs)
            argValues = zip (fst $ argeval args (State ss fs gs)) $ map declName $ formals ftn
            
eeval (Assign id e) s = assigneval id $ eeval e s
eeval Noexpr s = (0, s)

argeval :: [Expr] -> State -> ([NVal], State)
argeval es s = foldl (\(vals, state) e -> let updatedState = eeval e state in (((fst updatedState):vals), snd updatedState)) ([], s) es

uneval :: UnOp -> (NVal, State) -> (NVal, State)
uneval op (v, s) = case op of
    Neg -> (-v, s)
    Not -> (fromEnum $ not $ nvalAsBool v, s)

binevall :: BinOp -> (NVal, State) -> Expr -> (NVal, State)
binevall op (v1, s1) e2 = binevalr op (v1, s1) $ eeval e2 s1

binevalr :: BinOp -> (NVal, State) -> (NVal, State) -> (NVal, State)
binevalr op (v1, _) (v2, s2) = case op of
    Sub -> (v1 - v2, s2)
    Mult -> (v1 * v2, s2)
    Div -> (quot v1 v2, s2)
    Eq -> (fromEnum (v1 == v2), s2)
    Neq -> (fromEnum $ not (v1 == v2), s2)
    Less -> (fromEnum (v1 < v2), s2)
    Leq -> (fromEnum (v1 <= v2), s2)
    Greater -> (fromEnum (v1 > v2), s2)
    Geq -> (fromEnum (v1 >= v2), s2)
    And -> (fromEnum ((nvalAsBool v1) && (nvalAsBool v2)), s2)
    Or -> (fromEnum ((nvalAsBool v1) || (nvalAsBool v2)), s2)

assigneval :: Text -> (NVal, State) -> (NVal, State)
assigneval id (v, s) = (v, sset id v s)
