module Check where

import qualified Data.Map as Map
import qualified Data.Text as T
import LambdaCalc

type TEnv = Map.Map String Type

data TypeError
  = TypeMismatch Type Type
  | NotFunction Type
  | OutOfScope Name
  | Misc Name
  deriving (Eq)

opType :: Op -> Type
opType Add = TInt
opType Sub = TInt
opType Mul = TInt
opType And = TBool
opType Or = TBool

isCompOp :: Op -> Bool
isCompOp Eq = True
isCompOp Gr = True
isCompOp _ = False

tEnvLookup :: TEnv -> Name -> Either TypeError Type
tEnvLookup env x = case (Map.lookup x env) of
  Just t -> Right t
  Nothing -> Left $ OutOfScope x

getAppType :: Type -> Type -> Either TypeError Type

getAppType (TArr a b) sec@(TArr c d) 
  | d == a = Right (TArr c b)
  | sec == a = Right b
  | otherwise = Left $ TypeMismatch (TArr c d) a


getAppType (TArr a b) t2 
  | (t2 == a) = Right b
  | otherwise = Left $ TypeMismatch t2 a

getAppType ty _ = Left $ NotFunction ty

checkType :: TEnv -> Expr -> Either TypeError Type
checkType env expr = case expr of

  Literal (XInt _) -> Right TInt

  Literal (XBool _) -> Right TBool

  Var x -> tEnvLookup env x

  Abs x t body -> case bodyType of
    Right rt -> Right (TArr t rt)
    _ -> bodyType
    where
      bodyType = checkType (Map.insert x t env) body
  
  App e1 e2 -> do
    t1 <- checkType env e1
    t2 <- checkType env e2
    getAppType t1 t2


  Operation e1 op e2 -> do
    t1 <- checkType env e1
    t2 <- checkType env e2
    case (isCompOp op) of 
      True ->
        case (t1 == t2) of
          True -> Right TBool
          False -> Left $ TypeMismatch t1 t2
      False ->
        case (t1 == opType op) of
          True -> case (t2 == opType op) of 
            True -> Right t1
            False -> Left $ TypeMismatch t2 (opType op)
          False -> Left $ TypeMismatch t1 (opType op)
    
  Let x body -> checkType env body

  IfElse cond pos neg -> do
    tcond <- checkType env cond
    tpos <- checkType env pos
    tneg <- checkType env neg
    case tcond of
      TBool -> case (tpos == tneg) of
        True -> Right tpos
        False -> Left $ TypeMismatch tpos tneg
      _ -> Left $ TypeMismatch tcond TBool


  Fix expr -> do
    exptp <- checkType env expr
    case exptp of 
      (TArr t1 t2) -> Right t2
      _ -> Left $ Misc "recursion error"

shit :: Int -> Int
shit x = x `div` 0


ff improver = improver (improver (shit))
gg = ff
