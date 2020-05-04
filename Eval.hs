module Eval where

import Grammar
import Data.List(intercalate,uncons)

type Environment = [(String, Type, Expr)]

typeOfList :: [Expr] -> Maybe Type -> Type
typeOfList [] Nothing = TypeEmpty
typeOfList [] (Just t) = t 
typeOfList (x:xs) Nothing = typeOfList xs (Just (typeOf x))
typeOfList (x:xs) (Just t) | t == typeOf x = typeOfList xs (Just t)
                           | otherwise = error "You cannot have multiple type in an array!"
typeOf :: Expr -> Type
typeOf ExprNothing = TypeEmpty
typeOf (ExprInt _) = TypeInt
typeOf (ExprBool _) = TypeBool
typeOf (ExprString _) = TypeString
typeOf (ExprArrayAssign xs) = typeOfList xs Nothing
typeOf _ = error "Cannot get type"

prettyPrint :: Expr -> String
prettyPrint ExprNothing = "null"
prettyPrint (ExprInt x) = show x
prettyPrint (ExprBool x) = show x
prettyPrint (ExprString x) = read $ "\"" ++ x ++ "\""
prettyPrint (ExprArrayAssign xs) = "[" ++ intercalate "," (map prettyPrint xs) ++ "]"
prettyPrint e = show e 

getNextByte [] acc = (ExprNothing, [])
getNextByte (x:xs) acc
        | null x = getNextByte xs (acc ++ [x])
        | otherwise = case uncons x of
                Just (h,t) -> (ExprInt h, acc ++ [t] ++ xs)
                Nothing -> (ExprNothing, acc ++ x : xs)

getByteAt :: Int -> [[Int]] -> [[Int]] -> (Expr,[[Int]])
getByteAt _ []  acc = (ExprNothing, acc)
getByteAt y (x:xs) acc
        | y <= 0 = case uncons x of
                Just (h,t) -> (ExprInt h, acc ++ [t] ++ xs)
                Nothing -> (ExprNothing, acc ++ x : xs)
        | otherwise = getByteAt (y-1) xs (acc ++ [x])

readNextByte :: [[Int]] -> [[Int]] -> (Expr, [[Int]])
readNextByte [] acc = (ExprNothing, [])
readNextByte (x:xs) acc
        | null x = readNextByte xs (acc ++ [x])
        | otherwise = case uncons x of
                Just (h,t) -> (ExprInt h, acc ++ x : xs)
                Nothing -> (ExprNothing, acc ++ x : xs)

readByteAt :: Int -> [[Int]] -> [[Int]] -> (Expr,[[Int]])
readByteAt _ []  acc =  (ExprNothing, acc)
readByteAt y (x:xs) acc
        | y <= 0 = case uncons x of
                Just (h,t) -> (ExprInt h, acc ++ x : xs)
                Nothing -> (ExprNothing, acc ++ x : xs)
        | otherwise = readByteAt (y-1) xs (acc ++ [x])

getStreamAt :: Int -> [[a]] -> [[a]] -> ([a],[[a]])
getStreamAt _ [] acc = ([], acc)
getStreamAt y (x:xs) acc
        | y <= 0 = (x, acc ++ []:xs)
        | otherwise = getStreamAt (y-1) xs (acc ++ [x])

toInt :: Expr -> Int
toInt (ExprInt a) = a
toInt (ExprBool True) = 1
toInt (ExprBool False) = 0
toInt _ = 0

addEnv :: Environment -> [Environment] -> [Environment]
addEnv e env = e : env

checkNothing :: Expr -> Bool
checkNothing ExprNothing = True
checkNothing _ = False

getValBinding :: String -> [Environment] -> Expr
getValBinding x [] = error ("Get: Variable binding '" ++ x ++ "' not found")
getValBinding x (e:env)
        | checkNothing currentFoundBinding = getValBinding x env
        | otherwise = currentFoundBinding 
                where currentFoundBinding = getValBindingInd x e
  
getValBindingInd :: String -> Environment -> Expr
getValBindingInd _ [] = ExprNothing 
getValBindingInd x ((y,_,e):env)
        | x == y && not (isEmpty e) = e
        | x == y && isEmpty e = error ("Variable '" ++ x ++ "' is not initialised with a value!")
        | otherwise = getValBindingInd x env
  
addBinding :: [Environment] -> String -> Type -> Expr -> [Environment]
addBinding env x t e = ((x, t, e) : head env) : tail env

getListValBinding :: String -> Expr -> [Environment] -> Expr
getListValBinding x _ [] = error ("Get list value: Value binding '" ++ x ++ "' not found!")
getListValBinding x index (e:env)
      | checkNothing currentFoundBinding = getListValBinding x index env
      | otherwise = currentFoundBinding 
              where currentFoundBinding = getListValBindingInd x index e

getListValBindingInd :: String -> Expr -> Environment -> Expr
getListValBindingInd _ _ [] = ExprNothing
getListValBindingInd x (ExprInt index) ((y,_,e):env)
      | x == y && isArr e = getExprList e !! index
      | x == y && not (isArr e) = error ("Variable '" ++ x ++ "' is not a list")
      | otherwise = getListValBindingInd x (ExprInt index) env
getListValBindingInd _ _ _ = error "Index is not of type integer!"  
                                                  

updateValBinding :: String -> Expr -> [Environment] -> [Environment]
updateValBinding x _ [] = error ("Update: Value binding '" ++ x ++ "' not found!")
updateValBinding x ne (e:env)
      | not $ snd lastEnvCheck = e : updateValBinding x ne env
      | otherwise = fst lastEnvCheck : env
              where lastEnvCheck = updateValBindingInd x ne e 
  
updateValBindingInd :: String -> Expr -> Environment -> (Environment, Bool)
updateValBindingInd _ _ [] = ([], False)
updateValBindingInd x ne ((y,t,e):env)
      | x == y && isArr ne && isArr e && typeOf ne == t = ((x,t,ne) : env, True)
      | x /= y = ((y, t, e) : fst nextOne, snd nextOne) 
      | x == y && not (isArr ne) && not (isArr e) && typeOf ne == t = ((x,t,ne) : env, True)
      | not (isArr ne) && isArr e = error ("'" ++ x ++ "': You cannot assign a single expression to a list!")                             
      | isArr ne && not (isArr e) = error ("'" ++ x ++ "': You cannot assign an expression list to a variable!")
      | otherwise = error ("'" ++ x ++ "': You cannot assign a value of type '" ++ show (typeOf ne) ++ "' to a variable of type: '" ++ show t ++ "'")
              where nextOne = updateValBindingInd x ne env

updateListValBinding :: String -> Expr -> Expr -> [Environment] -> [Environment]
updateListValBinding x _ _ [] = error ("Update list value: Value binding '" ++ x ++ "' not found!")
updateListValBinding x ne index (e:env)
        | not (snd lastEnvCheck) = e : updateListValBinding x ne index env
        | otherwise = fst lastEnvCheck : env
                where lastEnvCheck = updateListValBindingInd x ne index e 

updateListValBindingInd :: String -> Expr -> Expr -> Environment -> (Environment, Bool)
updateListValBindingInd _ _ _ [] = ([], False)
updateListValBindingInd x ne (ExprInt index) ((y,t,e): env)
        | x == y && not (isArr ne) && isArr e && typeOf ne == t = ((x,t,ExprArrayAssign (replaceNthEl index ne (getExprList e))) : env, True)
        | x == y && not (isArr ne) && not (isArr e) = error ("'" ++ x ++ "': Variable is not a list!") 
        | x == y && isArr ne && isArr e = error ("'" ++ x ++ "': You cannot assign a list to a variable!") 
        | x /= y = ((y, t, e) : fst nextOne, snd nextOne)
        | otherwise = error ("'" ++ x ++ "': You cannot assign a value of type '" ++ show (typeOf ne) ++ "' to a member of a list of type: '" ++ show t ++ "'")
                where nextOne = updateListValBindingInd x ne (ExprInt index) env
updateListValBindingInd _ _ _ _ = error "Index is not of type integer!"  

replaceNthEl :: Int -> a -> [a] -> [a]
replaceNthEl _ _ [] = error "Index out of range!"
replaceNthEl n newVal (x:xs) | n == 0 = newVal:xs
                           | otherwise = x:replaceNthEl (n-1) newVal xs

getExprList :: Expr -> [Expr]
getExprList (ExprArrayAssign x) = x
getExprList _ = []                                

isArr :: Expr -> Bool
isArr (ExprArrayAssign _) = True
isArr _ = False

getArrLength :: Expr -> Int
getArrLength (ExprArrayAssign e) = length e
getArrLength _ = -1

isEmpty :: Expr -> Bool
isEmpty ExprEmpty = True
isEmpty _ = False

isValFree :: String -> [Environment] -> Bool
isValFree _ [] = True
isValFree x (e:env)
        | lastEnvCheck = isValFree x env
        | otherwise = False
                where lastEnvCheck = isValFreeInd x e

isValFreeInd :: String -> Environment -> Bool
isValFreeInd _ [] = True
isValFreeInd x ((y,_,_):env)
        | x == y = False
        | otherwise = isValFreeInd x env

isVal :: Expr -> Bool
isVal (ExprInt _) = True
isVal (ExprBool _) = True
isVal (ExprString _) = True
isVal _ = False

evalExprNot :: Expr -> Expr
evalExprNot (ExprBool x) = ExprBool (not x)
evalExprNot _ = error "EvaluateExprNot: Invalid type"

evalExprOp :: Expr -> Op -> Expr -> Expr
evalExprOp (ExprInt x1) Plus (ExprInt x2) = ExprInt (x1 + x2)
evalExprOp (ExprInt x1) Minus (ExprInt x2) = ExprInt (x1 - x2)
evalExprOp (ExprInt x1) Multiply (ExprInt x2) = ExprInt (x1 * x2)
evalExprOp (ExprInt x1) Divide (ExprInt x2) = ExprInt (x1 `div` x2)
evalExprOp (ExprInt x1) Power (ExprInt x2) = ExprInt (x1 ^ x2)
evalExprOp (ExprString x1) Plus (ExprString x2) = ExprString (x1 ++ x2)
evalExprOp (ExprArrayAssign x1) Plus (ExprArrayAssign x2) = ExprArrayAssign (x1 ++ x2)
evalExprOp _ _ _ = error "EvaluateExprOp: Invalid type (Expected type INT or STRING/LIST { only concatenation works: '+'})"

evalExprComp :: Expr -> CompareOp -> Expr -> Expr
evalExprComp ExprNothing Equals ExprNothing = ExprBool True
evalExprComp (ExprArrayAssign x1) Equals ExprNothing = ExprBool (null x1)
evalExprComp ExprNothing Equals (ExprArrayAssign x1) = ExprBool (null x1)

evalExprComp (ExprInt x1) GreaterThan (ExprInt x2) = ExprBool (x1 > x2)
evalExprComp (ExprInt x1) LessThan (ExprInt x2) = ExprBool (x1 < x2)
evalExprComp (ExprInt x1) GreaterOrEqualThan (ExprInt x2) = ExprBool (x1 >= x2)
evalExprComp (ExprInt x1) LessOrEqualThan (ExprInt x2) = ExprBool (x1 <= x2)
evalExprComp (ExprInt x1) Equals (ExprInt x2) = ExprBool (x1 == x2)

evalExprComp (ExprBool x1) And (ExprBool x2) = ExprBool (x1 && x2)
evalExprComp (ExprBool x1) Or (ExprBool x2) = ExprBool (x1 || x2)
evalExprComp (ExprBool x1) Equals (ExprBool x2) = ExprBool (x1 == x2)

evalExprComp (ExprString x1) GreaterThan (ExprString x2) = ExprBool (length x1 > length x2)
evalExprComp (ExprString x1) LessThan (ExprString x2) = ExprBool (length x1 < length x2)
evalExprComp (ExprString x1) GreaterOrEqualThan (ExprString x2) = ExprBool (length x1 >= length x2)
evalExprComp (ExprString x1) LessOrEqualThan (ExprString x2) = ExprBool (length x1 <= length x2)
evalExprComp (ExprString x1) Equals (ExprString x2) = ExprBool (x1 == x2)

evalExprComp _ _ ExprNothing = ExprBool False
evalExprComp ExprNothing _ _ = ExprBool False

evalExprComp a _ b | typeOf a /= typeOf b = error "Unsupported operation between mismatched types."
evalExprComp _ _ _ = error "Unsupported operation." --undefined

evalExprList :: [Expr] -> [[Int]] -> [Environment] -> ([Expr], [[Int]])
evalExprList [] input _ = ([], input)
evalExprList (e:es) input env = (fst evaluatedExpr : fst evaluatedExprList, snd evaluatedExprList) 
        where
                evaluatedExpr = evalExpr e input env
                evaluatedExprList = evalExprList es (snd evaluatedExpr) env

evalExpr :: Expr -> [[Int]] -> [Environment] -> (Expr, [[Int]])
-- (+,-,/,*,^)
evalExpr (ExprArrayAssign e) input env = (ExprArrayAssign (fst evaluatedExpr), snd evaluatedExpr)
        where evaluatedExpr = evalExprList e input env

evalExpr (ExprOp (ExprIdent x1) p (ExprIdent x2)) input env = evalExpr (ExprOp (getValBinding x1 env) p (getValBinding x2 env)) input env
evalExpr (ExprOp x1 p x2) input env
      | isVal x1 && isVal x2 = (evalExprOp x1 p x2, input)
      | otherwise = (evalExprOp (fst evaluatedExpr1) p (fst evaluatedExprComb), snd evaluatedExprComb)
              where
                      evaluatedExpr1 = evalExpr x1 input env
                      evaluatedExpr2 = evalExpr x2 input env 
                      evaluatedExprComb = evalExpr x2 (snd evaluatedExpr1) env  
-- (<,>,<=,>=,==,and,or)
evalExpr (ExprCompareOp (ExprIdent x1) p (ExprIdent x2)) input env = evalExpr (ExprCompareOp (getValBinding x1 env) p (getValBinding x2 env)) input env
evalExpr (ExprCompareOp (ExprIdent x1) p x2) input env = evalExpr (ExprCompareOp (getValBinding x1 env) p x2) input env
evalExpr (ExprCompareOp x1 p (ExprIdent x2)) input env = evalExpr (ExprCompareOp x1 p (getValBinding x2 env)) input env
evalExpr (ExprCompareOp x1 p x2) input env
      | isVal x1 && isVal x2 = (evalExprComp x1 p x2, input)
      | otherwise = (evalExprComp (fst evaluatedExpr1) p (fst evaluatedExprComb), snd evaluatedExprComb)
              where
                      evaluatedExpr1 = evalExpr x1 input env
                      evaluatedExpr2 = evalExpr x2 input env 
                      evaluatedExprComb = evalExpr x2 (snd evaluatedExpr1) env

-- (not)
evalExpr (ExprNot (ExprIdent x1)) input env = evalExpr (ExprNot (getValBinding x1 env)) input env
evalExpr (ExprNot x1) input env
      | isVal x1 = (evalExprNot x1, input)
      | otherwise = (evalExprNot (fst evaluatedExpr), snd evaluatedExpr)
              where evaluatedExpr = evalExpr x1 input env

-- ((Expr))
evalExpr (ExprExpr (ExprIdent x1)) input env = evalExpr (getValBinding x1 env) input env
evalExpr (ExprExpr x1) input env = evalExpr x1 input env
                              
evalExpr (ExprArrayValue i e) input env = (getListValBinding i (fst evaluatedExpr) env, snd evaluatedExpr)
                                            where evaluatedExpr = evalExpr e input env
evalExpr (ExprIdent i) input env = (getValBinding i env, input)

-- (input)
evalExpr (ExprGet ExprNothing) input _ = let (at,after) = getNextByte input [] in (at, after)
evalExpr (ExprGet e) input env = 
        let (at,after) = getByteAt (toInt $ fst $ evalExpr e input env) input [] in (at, after)

evalExpr (ExprRead ExprNothing) input _ = let (at,after) = readNextByte input [] in (at, after)
evalExpr (ExprRead e) input env = 
        let (at,after) = readByteAt (toInt $ fst $ evalExpr e input env) input [] in (at, after)

evalExpr (ExprStream e) input env =
        let (at,after) = getStreamAt (toInt $ fst $ evalExpr e input env) input []
        in (ExprArrayAssign (map ExprInt at), after)

evalExpr (ExprLength e) input env
        | isArr (fst evaluatedExpr) = (ExprInt (getArrLength (fst evaluatedExpr)), snd evaluatedExpr)
        | otherwise = error "Couldn't apply length to a variable. (List expected as parameter)"
                where evaluatedExpr = evalExpr e input env

evalExpr ExprNothing input env = (ExprNothing, input)

evalExpr x input _
      | isVal x = (x, input)
      | otherwise = error "Not implemented";
  
checkBoolAndTrue :: Expr -> Bool
checkBoolAndTrue (ExprBool True) = True
checkBoolAndTrue (ExprBool False) = False
checkBoolAndTrue _ = error "Couldn't match expression with type Boolean.";

evalStatementPrint :: Statement -> [[Int]] -> [Environment] -> IO([Environment], [[Int]])
evalStatementPrint (StatementPrint e) input env
      = do
              let evaluatedExpr = evalExpr e input env
              putStr (prettyPrint (fst evaluatedExpr))
              return (env, snd evaluatedExpr)
evalStatementPrint (StatementPrintLine e) input env
      = do
              let evaluatedExpr = evalExpr e input env
              putStrLn (prettyPrint (fst evaluatedExpr))
              return (env, snd evaluatedExpr)
evalStatementPrint _ _ _ = error "."
  
evalProgram :: Program -> [[Int]] -> IO[Environment]
evalProgram (Program ss) input
        = do
                evaluatedStatements <- evalStatementList ss input [[]]
                return (fst evaluatedStatements)

evalStatementList :: [Statement] -> [[Int]] -> [Environment] -> IO([Environment], [[Int]])
evalStatementList [] input env = return (env, input)
evalStatementList (s:ss) input env
        = do
                newEnv <- evalStatement s input env
                evalStatementList ss (snd newEnv) (fst newEnv)

evalStatement :: Statement -> [[Int]] -> [Environment] -> IO([Environment], [[Int]])
evalStatement (StatementVarDeclr (VarDeclrOnly t i)) input env
      = if isValFree i env 
              then return (addBinding env i t ExprEmpty, input)
              else error "Variable name is in use."

evalStatement (StatementVarDeclr (VarDeclrAssign t i e)) input env
      = if isValFree i env
              then do let evaluatedExpr = evalExpr e input env
                      if typeOf (fst evaluatedExpr) == t
                      then do let binding = addBinding env i t (fst evaluatedExpr)
                              return (binding, snd evaluatedExpr)
                      else error ("'" ++ i ++ "': You cannot assign a value of type '" ++ show (typeOf e) ++ "' to a variable of type: '" ++ show t ++ "'")
              else error "Variable name is in use."
  
evalStatement (StatementArrayDeclr (ArrayDeclrOnly t i)) input env
        = if isValFree i env
                then return (addBinding env i t (ExprArrayAssign []), input)
                else error "Variable name is in use."

evalStatement (StatementArrayDeclr (ArrayDeclrAssign t i e)) input env
        = if isValFree i env
                then do let evaluatedExpr = evalExpr e input env
                        if typeOf (fst evaluatedExpr) == TypeEmpty || typeOf (fst evaluatedExpr) == t
                        then do let evaluatedEnv = addBinding env i t (fst evaluatedExpr)
                                return (evaluatedEnv, snd evaluatedExpr)
                        else error ("'" ++ i ++ "': You cannot assign a value of type '" ++ show (typeOf e) ++ "' to a list of type: '" ++ show t ++ "'")
                else error "Variable name is in use."

evalStatement (StatementArrayAssign i index e) input env
        = do
                let evaluatedExpr = evalExpr e input env
                let evaluatedIndex = evalExpr index (snd evaluatedExpr) env
                return (updateListValBinding i (fst evaluatedExpr) (fst evaluatedIndex) env, snd evaluatedIndex)

evalStatement (StatementAssign i e) input env
        = do
                let evaluatedExpr = evalExpr e input env
                return (updateValBinding i (fst evaluatedExpr) env, snd evaluatedExpr)
  
evalStatement (StatementIf (ExprBool expr) s) input env
        = if expr
                then do sEval <- evalStatementList s input ([]:env)
                        return (tail (fst sEval), snd sEval)
                else return (env, input)

evalStatement (StatementIf (ExprInt _) _ ) _ _ = error "Couldn't match expression with type Boolean.";
evalStatement (StatementIf (ExprString _) _ ) _ _  = error "Couldn't match expression with type Boolean.";

evalStatement (StatementIf e s) input env
        = let evaluatedExpr = evalExpr e input env in
                evalStatement (StatementIf (fst evaluatedExpr) s) (snd evaluatedExpr) env

evalStatement (StatementIfElse (ExprBool expr) s1 s2) input env
      = if expr 
              then do s1Eval <- evalStatementList s1 input ([]:env)
                      return (tail (fst s1Eval), snd s1Eval)
              else do s2Eval <- evalStatementList s2 input ([]:env)
                      return (tail (fst s2Eval), snd s2Eval)

evalStatement (StatementIfElse (ExprInt _) _ _) _ _ = error "Couldn't match expression with type Boolean.";
evalStatement (StatementIfElse (ExprString _) _ _) _ _  = error "Couldn't match expression with type Boolean.";                                                         

evalStatement (StatementIfElse e s1 s2) input env
        = let evaluatedExpr = evalExpr e input env
                in evalStatement (StatementIfElse (fst evaluatedExpr) s1 s2) (snd evaluatedExpr) env

evalStatement (StatementWhile e s) input env
        = do
                let evaluatedExpr = evalExpr e input env
                if checkBoolAndTrue (fst evaluatedExpr)
                then do sEval <- evalStatementList s (snd evaluatedExpr) ([]:env)
                        evalStatement (StatementWhile e s) (snd sEval) (tail (fst sEval)) 
                else return (env, snd evaluatedExpr)

evalStatement (StatementPrint e) input env = evalStatementPrint (StatementPrint e) input env
evalStatement (StatementPrintLine e) input env = evalStatementPrint (StatementPrintLine e) input env
evalStatement _ _ _ = error "Not a statement!"
