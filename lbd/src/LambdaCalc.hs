module LambdaCalc where

import Data.List
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Text as T
import Control.Applicative ((<|>))


type Env = Map.Map String Expr
-- basic grammar for lambda calculus

type Name = String

data Expr 
  = Var Name -- single variable
  | App Expr Expr -- application
  | Abs Name Type Expr -- \x->y
  | Literal Atom -- single basic data type
  | Operation Expr Op Expr -- f + g .. etc
  | Let Name Expr -- let statement
  | IfElse Expr Expr Expr -- if else
  | Fix Expr -- fixed point combinator
  deriving (Eq)


data Atom = XInt Int | XBool Bool
  deriving (Eq)
data Op = Add | Sub | Mul | And | Or | Eq | Gr
  deriving (Eq)

data Type = 
  TInt 
  | TBool 
  | TArr Type Type
  | Star
  deriving (Eq)

-- a function to get the free variables from single expression as a list
freeVars :: Expr -> [Name]
freeVars (Var x) = [x]
freeVars (App f g) = freeVars f ++ freeVars g
freeVars (Abs x t fx) = filter (/=x) . freeVars $ fx
freeVars (Literal atom) = []
freeVars (Operation f _ g) = freeVars f ++ freeVars g
freeVars (IfElse c pos neg) = freeVars c ++ freeVars pos ++ freeVars neg
freeVars (Fix f) = freeVars f


-- a function that takes all definitions in global scope and substitue it
subGlobals :: Env -> Expr -> Expr
subGlobals env (Var x) = 
  case (Map.lookup x env) of
    Nothing -> Var x 
    Just res -> res
subGlobals env (App f g) = App (subGlobals env f) (subGlobals env g)
subGlobals env (Abs x t fx) = Abs x t (subGlobals env fx)
subGlobals env (Literal atom) = Literal atom
subGlobals env (Operation f op g) = Operation (subGlobals env f) op (subGlobals env g)
subGlobals env (Let x fx) = Let x (subGlobals env fx)
subGlobals env (IfElse c pos neg) = IfElse (subGlobals env c) (subGlobals env pos) (subGlobals env neg)
subGlobals env (Fix expr) = Fix (subGlobals env expr)


-- a function that takes a variable and lambda expression and another expression bound to the variable
-- it replaces all occurrences of variable inside the secnd parameter
sub :: Name -> Expr -> Expr -> Expr 


sub x expression@(Var y) boundExpression
  | x == y = boundExpression
  | otherwise = expression

sub x (App f g) boundExpression = 
    App  (sub x f boundExpression) (sub x g boundExpression)
  
sub x expression@(Abs y t fy) boundExpression
  | x == y = expression
  | otherwise = Abs y t (sub x fy boundExpression)

sub x expression@(Literal atom) _ = expression

sub x (Operation f op g) boundExpression =
    Operation  (sub x f boundExpression) op (sub x g boundExpression)

sub x (IfElse c pos neg) boundExpression = 
    IfElse (sub x c boundExpression) (sub x pos boundExpression) (sub x neg boundExpression)

sub x expression@(Fix expr) boundExpression = expression

-- a function that reduces operations on atoms to single atom
calc :: Expr -> Op -> Expr -> Expr

calc (Literal (XInt a)) Add (Literal (XInt b)) = Literal (XInt (a + b))
calc (Literal (XInt a)) Sub (Literal (XInt b)) = Literal (XInt (a - b))
calc (Literal (XInt a)) Mul (Literal (XInt b)) = Literal (XInt (a * b))
calc (Literal (XBool a)) And (Literal (XBool b)) = Literal (XBool (a && b))
calc (Literal (XBool a)) Or (Literal (XBool b)) = Literal (XBool (a || b))
calc (Literal (XInt a)) Eq (Literal (XInt b)) = Literal (XBool (a == b))
calc (Literal (XInt a)) Gr (Literal (XInt b)) = Literal (XBool (a > b))
calc (Literal (XBool a)) Eq (Literal (XBool b)) = Literal (XBool (a == b))
calc (Literal (XBool a)) Gr (Literal (XBool b)) = Literal (XBool (a == True && b == False))
calc x op y = Operation x op y


--evaluator
eval :: Expr -> Expr
eval expression@(Var _) = expression
eval (Abs x t fx) = Abs x t (eval fx)
eval (App f g) = beta (eval f) (eval g)
eval (Literal atom) = Literal atom
eval (Operation e1 op e2) = 
  calc (eval e1) op (eval e2)

eval (Let x fx) = Let x (eval fx)
eval self@(IfElse c pos neg) = case (eval c) of
  (Literal (XBool True)) -> eval pos
  (Literal (XBool False)) -> eval neg
  _ -> self
  


----------
--eval (Fix _) = (Abs ("x") TInt (Var "x"))
eval (Fix expression@(Abs x t body)) = eval (sub x body (Fix expression))
eval (Fix expr) = case (eval expr) of
  ( Literal _ ) -> (eval expr)
  _ -> (Fix (eval expr))
----------



--reduction rules:
-- beta reduction
beta :: Expr -> Expr -> Expr
beta e1@(Var _) e2 = App e1 e2
beta (App e1 e2) e3 = App (beta e1 e2) e3
beta (Abs x t fx) y = eval (sub x fx' y)
  where
    fx' = alpha (freeVars y) fx

beta  (Operation (Abs x t fx) op rest) e2 = beta (Abs x t (Operation fx op rest)) e2
--beta (Operation ((Abs x t fx) op rest)) e2 = 
--beta e1 (Operation $ (Abs x t fx) op rest)  = beta (Abs x t (Operation fx op rest)) e1

beta _x _y = App _x _y

-- alpha reduction
alpha :: [Name] -> Expr -> Expr
alpha freeVars (Abs x t fx)
  | x `elem` freeVars = Abs newName t (sub x fx (Var newName))
  | otherwise = Abs x t (alpha freeVars fx)
  where
    newName = fromMaybe x (find (`notElem` freeVars) englishWords)
alpha _ x = x

-- a function that generates the whole english language, just made for sake of getting unique words for alpha conversion
englishGenerator :: [Char] -> [[Char]]
englishGenerator l = []:[(x:ys) | ys <- englishGenerator l, x <- l]

englishWords = drop 1 (englishGenerator ['a'..'z'])


