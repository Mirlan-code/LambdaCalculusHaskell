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
  | Abs Name Expr -- \x->y
  | Literal Atom -- single basic data type
  | Operation Expr Op Expr -- f + g .. etc
  | Let Name Expr -- let statement
  deriving (Eq)


data Atom = XInt Int | XBool Bool
  deriving (Eq)
data Op = Add | Sub | Mul | And | Or 
  deriving (Eq)


-- a function to get the free variables from single expression as a list
freeVars :: Expr -> [Name]
freeVars (Var x) = [x]
freeVars (App f g) = freeVars f ++ freeVars g
freeVars (Abs x fx) = filter (/=x) . freeVars $ fx
freeVars (Literal atom) = []
freeVars (Operation f _ g) = freeVars f ++ freeVars g


-- a function that takes all definitions in global scope and substitue it
subGlobals :: Env -> Expr -> Expr
subGlobals env (Var x) = 
  case (Map.lookup x env) of
    Nothing -> Var x 
    Just res -> res
subGlobals env (App f g) = App (subGlobals env f) (subGlobals env g)
subGlobals env (Abs x fx) = Abs x (subGlobals env fx)
subGlobals env (Literal atom) = Literal atom
subGlobals env (Operation f op g) = Operation (subGlobals env f) op (subGlobals env g)
subGlobals env (Let x fx) = Let x (subGlobals env fx)


-- a function that takes a variable and lambda expression and another expression bound to the variable
-- it replaces all occurrences of variable inside the secnd parameter
sub :: Name -> Expr -> Expr -> Expr 


sub x expression@(Var y) boundExpression
  | x == y = boundExpression
  | otherwise = expression

sub x (App f g) boundExpression = 
    App  (sub x f boundExpression) (sub x g boundExpression)
  
sub x expression@(Abs y fy) boundExpression
  | x == y = expression
  | otherwise = Abs y (sub x fy boundExpression)

sub x expression@(Literal atom) _ = expression

sub x (Operation f op g) boundExpression =
    Operation  (sub x f boundExpression) op (sub x g boundExpression)


-- a function that reduces operations on atoms to single atom
calc :: Expr -> Op -> Expr -> Expr

calc (Literal (XInt a)) Add (Literal (XInt b)) = Literal (XInt (a + b))
calc (Literal (XInt a)) Sub (Literal (XInt b)) = Literal (XInt (a - b))
calc (Literal (XInt a)) Mul (Literal (XInt b)) = Literal (XInt (a * b))
calc (Literal (XBool a)) And (Literal (XBool b)) = Literal (XBool (a && b))
calc (Literal (XBool a)) Or (Literal (XBool b)) = Literal (XBool (a || b))
calc x op y = Operation x op y


--evaluator
eval :: Expr -> Expr
eval expression@(Var _) = expression
eval (Abs x fx) = Abs x (eval fx)
eval (App f g) = beta (eval f) (eval g)
eval (Literal atom) = Literal atom
eval (Operation e1 op e2) = calc (eval e1) op (eval e2)
eval (Let x fx) = Let x (eval fx)


--reduction rules:
-- beta reduction
beta :: Expr -> Expr -> Expr
beta e1@(Var _) e2 = App e1 e2
beta (App e1 e2) e3 = App (beta e1 e2) e3
beta (Abs x fx) y = eval (sub x fx' y)
  where
    fx' = alpha (freeVars y) fx
beta _x _y = App _x _y

-- alpha reduction
alpha :: [Name] -> Expr -> Expr
alpha freeVars (Abs x fx)
  | x `elem` freeVars = Abs newName (sub x fx (Var newName))
  | otherwise = Abs x (alpha freeVars fx)
  where
    newName = fromMaybe x (find (`notElem` freeVars) englishWords)
alpha _ x = x

-- a function that generates the whole english language, just made for sake of getting unique words for alpha conversion
englishGenerator :: [Char] -> [[Char]]
englishGenerator l = []:[(x:ys) | ys <- englishGenerator l, x <- l]

englishWords = drop 1 (englishGenerator ['a'..'z'])


