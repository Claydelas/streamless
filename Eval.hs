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
typeOf (ExprInt _) = TypeInt
typeOf (ExprBool _) = TypeBool
typeOf (ExprString _) = TypeString
typeOf (ExprArrayAssign xs) = typeOfList xs Nothing
typeOf _ = error "Cannot get type"

prettyPrint :: Expr -> String
prettyPrint (ExprInt x) = show x
prettyPrint (ExprBool x) = show x
prettyPrint (ExprString x) = read $ "\"" ++ x ++ "\""
prettyPrint (ExprArrayAssign xs) = "[" ++ intercalate "," (map prettyPrint xs) ++ "]"
prettyPrint e = show e 

nextByte :: [[a]] -> (a,[[a]])
nextByte [] = error "No input."
nextByte xs = case filter (not . null) xs of
                (x:xss) -> let Just (h,t) = uncons x in (h, takeWhile null xs ++ t : xss)
                [] -> error "There is no input left."

getInputAfterRead :: [[Int]] -> [[Int]]
getInputAfterRead xs | null f = tail xs
                     | otherwise = f : tail xs
                     where f = tail (head xs)

byteAt :: Int -> [[a]] -> [[a]] -> (a,[[a]])
byteAt _ []  _ = error "Index out of bounds!"
byteAt y (x:xs) acc
        | y <= 0 = case uncons x of
                Just (h,t) -> (h, acc ++ [t] ++ xs)
                Nothing -> error "There is no input left on this stream."
        | otherwise = byteAt (y-1) xs (acc ++ [x])

streamAt :: Int -> [[a]] -> [[a]] -> ([a],[[a]])
streamAt _ []  _ = error "Index out of bounds!"
streamAt y (x:xs) acc
        | y <= 0 = (x, acc ++ xs)
        | otherwise = streamAt (y-1) xs (acc ++ [x])

toInt :: Expr -> Int
toInt (ExprInt a) = a
toInt (ExprBool True) = 1
toInt (ExprBool False) = 0
toInt _ = 0

getLineInput :: [[Int]] -> [Int]
getLineInput [] = []
getLineInput xs = head xs

addEnv :: Environment -> [Environment] -> [Environment]
addEnv e env = e : env

checkIfNothing :: Expr -> Bool
checkIfNothing ExprNothing = True
checkIfNothing _ = False

getValueBinding :: String -> [Environment] -> Expr
getValueBinding x [] = error ("Get: Variable binding '" ++ x ++ "' not found")
getValueBinding x (e:env) | checkIfNothing currentFoundBinding = getValueBinding x env
                          | otherwise = currentFoundBinding 
                          where currentFoundBinding = getValueBindingInd x e
  
getValueBindingInd :: String -> Environment -> Expr
getValueBindingInd _ [] = ExprNothing 
getValueBindingInd x ((y,_,e):env) | x == y && not (isEmpty e) = e
                                   | x == y && isEmpty e = error ("Variable '" ++ x ++ "' is not initialised with a value!")
                                   | otherwise = getValueBindingInd x env
  
addBinding :: [Environment] -> String -> Type -> Expr -> [Environment]
addBinding env x t e = ((x, t, e) : head env) : tail env

getListValueBinding :: String -> Expr -> [Environment] -> Expr
getListValueBinding x _ [] = error ("Get list value: Value binding '" ++ x ++ "' not found!")
getListValueBinding x index (e:env)
      | checkIfNothing currentFoundBinding = getListValueBinding x index env
      | otherwise = currentFoundBinding 
              where currentFoundBinding = getListValueBindingInd x index e

getListValueBindingInd :: String -> Expr -> Environment -> Expr
getListValueBindingInd _ _ [] = ExprNothing
getListValueBindingInd x (ExprInt index) ((y,_,e):env)
      | x == y && isArray e = getExprList e !! index
      | x == y && not (isArray e) = error ("Variable '" ++ x ++ "' is not a list")
      | otherwise = getListValueBindingInd x (ExprInt index) env
getListValueBindingInd _ _ _ = error "Index is not type of integer!"  
                                                  

updateValueBinding :: String -> Expr -> [Environment] -> [Environment]
updateValueBinding x _ [] = error ("Update: Value binding '" ++ x ++ "' not found!")
updateValueBinding x ne (e:env)
      | not $ snd lastEnvCheck = e : updateValueBinding x ne env
      | otherwise = fst lastEnvCheck : env
              where lastEnvCheck = updateValueBindingInd x ne e 
  
updateValueBindingInd :: String -> Expr -> Environment -> (Environment, Bool)
updateValueBindingInd _ _ [] = ([], False)
updateValueBindingInd x ne ((y,t,e):env)
      | x == y && isArray ne && isArray e && typeOf ne == t = ((x,t,ne) : env, True)
      | x /= y = ((y, t, e) : fst nextOne, snd nextOne) 
      | x == y && not (isArray ne) && not (isArray e) && typeOf ne == t = ((x,t,ne) : env, True)
      | not (isArray ne) && isArray e = error ("'" ++ x ++ "': You cannot assign a single expression to a list!")                             
      | isArray ne && not (isArray e) = error ("'" ++ x ++ "': You cannot assign an expression list to a variable!")
      | otherwise = error ("'" ++ x ++ "': You cannot assign a value of type '" ++ show (typeOf ne) ++ "' to a variable of type: '" ++ show t ++ "'")
              where nextOne = updateValueBindingInd x ne env

updateListValueBinding :: String -> Expr -> Expr -> [Environment] -> [Environment]
updateListValueBinding x _ _ [] = error ("Update list value: Value binding '" ++ x ++ "' not found!")
updateListValueBinding x ne index (e:env) | not (snd lastEnvCheck) = e : updateListValueBinding x ne index env
                                          | otherwise = fst lastEnvCheck : env
                                          where lastEnvCheck = updateListValueBindingInd x ne index e 

updateListValueBindingInd :: String -> Expr -> Expr -> Environment -> (Environment, Bool)
updateListValueBindingInd _ _ _ [] = ([], False)
updateListValueBindingInd x ne (ExprInt index) ((y,t,e): env) | x == y && not (isArray ne) && isArray e && typeOf ne == t = ((x,t,ExprArrayAssign (replaceNth index ne (getExprList e))) : env, True)
                                                            | x == y && not (isArray ne) && not (isArray e) = error ("'" ++ x ++ "': Variable is not a list!") 
                                                            | x == y && isArray ne && isArray e = error ("'" ++ x ++ "': You cannot assign a list to a variable!") 
                                                            | x /= y = ((y, t, e) : fst nextOne, snd nextOne)
                                                            | otherwise = error ("'" ++ x ++ "': You cannot assign a value of type '" ++ show (typeOf ne) ++ "' to a member of a list of type: '" ++ show t ++ "'")
                                                            where nextOne = updateListValueBindingInd x ne (ExprInt index) env
updateListValueBindingInd _ _ _ _ = error "Index is not type of integer!"  

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = error "Index out of range!"
replaceNth n newVal (x:xs) | n == 0 = newVal:xs
                           | otherwise = x:replaceNth (n-1) newVal xs

getExprList :: Expr -> [Expr]
getExprList (ExprArrayAssign x) = x
getExprList _ = []                                

isArray :: Expr -> Bool
isArray (ExprArrayAssign _) = True
isArray _ = False

getArrayLength :: Expr -> Int
getArrayLength (ExprArrayAssign e) = length e
getArrayLength _ = -1

isEmpty :: Expr -> Bool
isEmpty ExprEmpty = True
isEmpty _ = False

isValueFree :: String -> [Environment] -> Bool
isValueFree _ [] = True
isValueFree x (e:env) | lastEnvCheck = isValueFree x env
                      | otherwise = False
                      where lastEnvCheck = isValueFreeInd x e

isValueFreeInd :: String -> Environment -> Bool
isValueFreeInd _ [] = True
isValueFreeInd x ((y,_,_):env) | x == y = False
                             | otherwise = isValueFreeInd x env

isValue :: Expr -> Bool
isValue (ExprInt _) = True
isValue (ExprBool _) = True
isValue (ExprString _) = True
isValue _ = False

evaluateExprNot :: Expr -> Expr
evaluateExprNot (ExprBool x) = ExprBool (not x)
evaluateExprNot _ = error "EvaluateExprNot: Invalid type"

evaluateExprOp :: Expr -> Op -> Expr -> Expr
evaluateExprOp (ExprInt x1) Plus (ExprInt x2) = ExprInt (x1 + x2)
evaluateExprOp (ExprInt x1) Minus (ExprInt x2) = ExprInt (x1 - x2)
evaluateExprOp (ExprInt x1) Multiply (ExprInt x2) = ExprInt (x1 * x2)
evaluateExprOp (ExprInt x1) Divide (ExprInt x2) = ExprInt (x1 `div` x2)
evaluateExprOp (ExprInt x1) Power (ExprInt x2) = ExprInt (x1 ^ x2)
evaluateExprOp (ExprString x1) Plus (ExprString x2) = ExprString (x1 ++ x2)
evaluateExprOp (ExprArrayAssign x1) Plus (ExprArrayAssign x2) = ExprArrayAssign (x1 ++ x2)
evaluateExprOp _ _ _ = error "EvaluateExprOp: Invalid type (Expected type INT or STRING/LIST { only concatenation works: '+'})"

evaluateExprComp :: Expr -> CompareOp -> Expr -> Expr
evaluateExprComp (ExprInt x1) GreaterThan (ExprInt x2) = ExprBool (x1 > x2)
evaluateExprComp (ExprInt x1) LessThan (ExprInt x2) = ExprBool (x1 < x2)
evaluateExprComp (ExprInt x1) GreaterOrEqualThan (ExprInt x2) = ExprBool (x1 >= x2)
evaluateExprComp (ExprInt x1) LessOrEqualThan (ExprInt x2) = ExprBool (x1 <= x2)
evaluateExprComp (ExprInt x1) Equals (ExprInt x2) = ExprBool (x1 == x2)

evaluateExprComp (ExprBool x1) And (ExprBool x2) = ExprBool (x1 && x2)
evaluateExprComp (ExprBool x1) Or (ExprBool x2) = ExprBool (x1 || x2)
evaluateExprComp (ExprBool x1) Equals (ExprBool x2) = ExprBool (x1 == x2)

evaluateExprComp (ExprString x1) GreaterThan (ExprString x2) = ExprBool (length x1 > length x2)
evaluateExprComp (ExprString x1) LessThan (ExprString x2) = ExprBool (length x1 < length x2)
evaluateExprComp (ExprString x1) GreaterOrEqualThan (ExprString x2) = ExprBool (length x1 >= length x2)
evaluateExprComp (ExprString x1) LessOrEqualThan (ExprString x2) = ExprBool (length x1 <= length x2)
evaluateExprComp (ExprString x1) Equals (ExprString x2) = ExprBool (x1 == x2)

evaluateExprComp a _ b | typeOf a /= typeOf b = error "Unsupported operation between mismatched types."
evaluateExprComp _ _ _ = error "Unsupported operation." --undefined

evaluateExprList :: [Expr] -> [[Int]] -> [Environment] -> ([Expr], [[Int]])
evaluateExprList [] input _ = ([], input)
evaluateExprList (e:es) input env = (fst evaluatedExpr : fst evaluatedExprList, snd evaluatedExprList) 
                                  where evaluatedExpr = evaluateExpr e input env
                                        evaluatedExprList = evaluateExprList es (snd evaluatedExpr) env

evaluateExpr :: Expr -> [[Int]] -> [Environment] -> (Expr, [[Int]])
--evaluate expressions with operations (+,-,/,*,^)
evaluateExpr (ExprArrayAssign e) input env = (ExprArrayAssign (fst evaluatedExpr), snd evaluatedExpr)
                                           where evaluatedExpr = evaluateExprList e input env

evaluateExpr (ExprOp (ExprIdent x1) p (ExprIdent x2)) input env = evaluateExpr (ExprOp (getValueBinding x1 env) p (getValueBinding x2 env)) input env
evaluateExpr (ExprOp x1 p x2) input env
      | isValue x1 && isValue x2 = (evaluateExprOp x1 p x2, input)
      | otherwise = (evaluateExprOp (fst evaluatedExpr1) p (fst evaluatedExprComb), snd evaluatedExprComb)
              where
                      evaluatedExpr1 = evaluateExpr x1 input env
                      evaluatedExpr2 = evaluateExpr x2 input env 
                      evaluatedExprComb = evaluateExpr x2 (snd evaluatedExpr1) env  
--evaluate expressions with compare operations (<,>,<=,>=,==,and,or)
evaluateExpr (ExprCompareOp (ExprIdent x1) p (ExprIdent x2)) input env = evaluateExpr (ExprCompareOp (getValueBinding x1 env) p (getValueBinding x2 env)) input env
evaluateExpr (ExprCompareOp (ExprIdent x1) p x2) input env = evaluateExpr (ExprCompareOp (getValueBinding x1 env) p x2) input env
evaluateExpr (ExprCompareOp x1 p (ExprIdent x2)) input env = evaluateExpr (ExprCompareOp x1 p (getValueBinding x2 env)) input env
evaluateExpr (ExprCompareOp x1 p x2) input env
      | isValue x1 && isValue x2 = (evaluateExprComp x1 p x2, input)
      | otherwise = (evaluateExprComp (fst evaluatedExpr1) p (fst evaluatedExprComb), snd evaluatedExprComb)
              where
                      evaluatedExpr1 = evaluateExpr x1 input env
                      evaluatedExpr2 = evaluateExpr x2 input env 
                      evaluatedExprComb = evaluateExpr x2 (snd evaluatedExpr1) env

--evaluate not expressions
evaluateExpr (ExprNot (ExprIdent x1)) input env = evaluateExpr (ExprNot (getValueBinding x1 env)) input env
evaluateExpr (ExprNot x1) input env
      | isValue x1 = (evaluateExprNot x1, input)
      | otherwise = (evaluateExprNot (fst evaluatedExpr), snd evaluatedExpr)
              where evaluatedExpr = evaluateExpr x1 input env

--evaluate expressions surrounded by paranthesises
evaluateExpr (ExprExpr (ExprIdent x1)) input env = evaluateExpr (getValueBinding x1 env) input env
evaluateExpr (ExprExpr x1) input env = evaluateExpr x1 input env
                              
evaluateExpr (ExprArrayValue i e) input env = (getListValueBinding i (fst evaluatedExpr) env, snd evaluatedExpr)
                                            where evaluatedExpr = evaluateExpr e input env
evaluateExpr (ExprIdent i) input env = (getValueBinding i env, input)

evaluateExpr (ExprRead e) input env = 
        let (at,after) = byteAt (toInt $ fst $ evaluateExpr e input env) input [] in (ExprInt at, after)
evaluateExpr (ExprReadLine e) input env =
        let (at,after) = streamAt (toInt $ fst $ evaluateExpr e input env) input [] in (ExprArrayAssign (map ExprInt at), after)

evaluateExpr ExprReadNext input _ = let (at,after) = nextByte input in (ExprInt at, after)
evaluateExpr ExprReadNextLine input _ = (ExprArrayAssign (map ExprInt (getLineInput input)), tail input)

evaluateExpr (ExprLength e) input env | isArray (fst evaluatedExpr) = (ExprInt (getArrayLength (fst evaluatedExpr)), snd evaluatedExpr)
                                      | otherwise = error "Couldn't apply the method length on a variable. (List expected as parameter)"
                                      where evaluatedExpr = evaluateExpr e input env

evaluateExpr x input _
      | isValue x = (x, input)
      | otherwise = error "Not implemented";
  
checkIfBoolAndTrue :: Expr -> Bool
checkIfBoolAndTrue (ExprBool True) = True
checkIfBoolAndTrue (ExprBool False) = False
checkIfBoolAndTrue _ = error "Couldn't match the given expression with the bool type!";

evaluateStatementPrint :: Statement -> [[Int]] -> [Environment] -> IO([Environment], [[Int]])
evaluateStatementPrint (StatementPrint e) input env
      = do
              let evaluatedExpr = evaluateExpr e input env
              putStr (prettyPrint (fst evaluatedExpr))
              return (env, snd evaluatedExpr)
evaluateStatementPrint (StatementPrintLine e) input env
      = do
              let evaluatedExpr = evaluateExpr e input env
              putStrLn (prettyPrint (fst evaluatedExpr))
              return (env, snd evaluatedExpr)
evaluateStatementPrint _ _ _ = error "."
  
evaluateProgram :: Program -> [[Int]] -> IO[Environment]
evaluateProgram (Program ss) input = do evaluatedStatements <- evaluateStatementList ss input [[]]
                                        return (fst evaluatedStatements)

evaluateStatementList :: [Statement] -> [[Int]] -> [Environment] -> IO([Environment], [[Int]])
evaluateStatementList [] input env = return (env, input)
evaluateStatementList (s:ss) input env = do newEnv <- evaluateStatement s input env
                                            evaluateStatementList ss (snd newEnv) (fst newEnv)

evaluateStatement :: Statement -> [[Int]] -> [Environment] -> IO([Environment], [[Int]])
evaluateStatement (StatementVarDeclr (VarDeclrOnly t i)) input env
      = if isValueFree i env 
              then return (addBinding env i t ExprEmpty, input)
              else error "Variable name is in use."

evaluateStatement (StatementVarDeclr (VarDeclrAssign t i e)) input env
      = if isValueFree i env
              then do let evaluatedExpr = evaluateExpr e input env
                      if typeOf (fst evaluatedExpr) == t
                      then do let binding = addBinding env i t (fst evaluatedExpr)
                              return (binding, snd evaluatedExpr)
                      else error ("'" ++ i ++ "': You cannot assign a value of type '" ++ show (typeOf e) ++ "' to a variable of type: '" ++ show t ++ "'")
              else error "Variable name is in use."
  
evaluateStatement (StatementArrayDeclr (ArrayDeclrOnly t i)) input env = if isValueFree i env
                                                                            then return (addBinding env i t (ExprArrayAssign []), input)
                                                                            else error "Variable name is in use."

evaluateStatement (StatementArrayDeclr (ArrayDeclrAssign t i e)) input env = if isValueFree i env
                                                                                then do let evaluatedExpr = evaluateExpr e input env
                                                                                        if typeOf (fst evaluatedExpr) == TypeEmpty || typeOf (fst evaluatedExpr) == t
                                                                                        then do let evaluatedEnv = addBinding env i t (fst evaluatedExpr)
                                                                                                return (evaluatedEnv, snd evaluatedExpr)
                                                                                        else error ("'" ++ i ++ "': You cannot assign a value of type '" ++ show (typeOf e) ++ "' to a list of type: '" ++ show t ++ "'")
                                                                                else error "Variable name is in use."

evaluateStatement (StatementArrayAssign i index e) input env = do let evaluatedExpr = evaluateExpr e input env
                                                                  let evaluatedIndex = evaluateExpr index (snd evaluatedExpr) env
                                                                  return (updateListValueBinding i (fst evaluatedExpr) (fst evaluatedIndex) env, snd evaluatedIndex)

evaluateStatement (StatementAssign i e) input env = do let evaluatedExpr = evaluateExpr e input env
                                                       return (updateValueBinding i (fst evaluatedExpr) env, snd evaluatedExpr)
  
evaluateStatement (StatementIf (ExprBool expr) s) input env = if expr
                                                                  then do sEval <- evaluateStatementList s input ([]:env)
                                                                          return (tail (fst sEval), snd sEval)
                                                                  else return (env, input)

evaluateStatement (StatementIf (ExprInt _) _ ) _ _ = error "Couldn't match the given expression with the bool type!";
evaluateStatement (StatementIf (ExprString _) _ ) _ _  = error "Couldn't match the given expression with the bool type!";

evaluateStatement (StatementIf e s) input env = let evaluatedExpr = evaluateExpr e input env in
                                                evaluateStatement (StatementIf (fst evaluatedExpr) s) (snd evaluatedExpr) env

evaluateStatement (StatementIfElse (ExprBool expr) s1 s2) input env
      = if expr 
              then do s1Eval <- evaluateStatementList s1 input ([]:env)
                      return (tail (fst s1Eval), snd s1Eval)
              else do s2Eval <- evaluateStatementList s2 input ([]:env)
                      return (tail (fst s2Eval), snd s2Eval)

evaluateStatement (StatementIfElse (ExprInt _) _ _) _ _ = error "Couldn't match the given expression with the bool type!";
evaluateStatement (StatementIfElse (ExprString _) _ _) _ _  = error "Couldn't match the given expression with the bool type!";                                                         

evaluateStatement (StatementIfElse e s1 s2) input env = let evaluatedExpr = evaluateExpr e input env
      in evaluateStatement (StatementIfElse (fst evaluatedExpr) s1 s2) (snd evaluatedExpr) env

evaluateStatement (StatementWhile e s) input env = do let evaluatedExpr = evaluateExpr e input env
                                                      if checkIfBoolAndTrue (fst evaluatedExpr)
                                                      then do sEval <- evaluateStatementList s (snd evaluatedExpr) ([]:env)
                                                              evaluateStatement (StatementWhile e s) (snd sEval) (tail (fst sEval)) 
                                                      else return (env, snd evaluatedExpr)

evaluateStatement (StatementPrint e) input env = evaluateStatementPrint (StatementPrint e) input env
evaluateStatement (StatementPrintLine e) input env = evaluateStatementPrint (StatementPrintLine e) input env
evaluateStatement _ _ _ = error "."