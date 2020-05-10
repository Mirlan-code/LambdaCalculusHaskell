module Check where

import LambdaCalc

type TEnv = [(Name, Type)]

data TypeError
  = TypeMismatch Type Type
  | NotFunction Type
  | OutOfScope Name
  deriving (Eq)

opType :: Op -> Type
opType Add = TInt
opType Sub = TInt
opType Mul = TInt
opType And = TBool
opType Or = TBool

tEnvLookup :: TEnv -> Name -> Either TypeError Type
tEnvLookup env x = case (lookup x env) of
  Just t -> Right t
  Nothing -> Left $ OutOfScope x

checkType :: TEnv -> Expr -> Either TypeError Type
checkType env expr = case expr of

  Literal (XInt _) -> Right TInt

  Literal (XBool _) -> Right TBool

  Var x -> tEnvLookup env x

  Abs x t body -> case bodyType of
    Right rt -> Right (TArr t rt)
    _ -> bodyType
    where
      bodyType = checkType ((x , t) : env) body
  
  App e1 e2 -> do
    t1 <- checkType env e1
    t2 <- checkType env e2
    case t1 of
      (TArr a b) | a == t2 -> Right b
                 | otherwise -> Left $ TypeMismatch t2 a
      ty -> Left $ NotFunction ty


  Operation e1 op e2 -> do
    t1 <- checkType env e1
    t2 <- checkType env e2
    case (t1 == opType op) of
      True -> case (t2 == opType op) of 
        True -> Right t1
        False -> Left $ TypeMismatch t2 (opType op)
      False -> Left $ TypeMismatch t1 (opType op)
    
  Let x body -> checkType env body
