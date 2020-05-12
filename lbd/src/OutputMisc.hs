module OutputMisc where

import LambdaCalc
import Check
class Strable a where
    str :: a -> String

instance Strable Expr where
    str (Var x) = x
    str (App f g) = "(" ++ str f ++ ")" ++ " $ " ++ "(" ++ str g ++ ")"
    str (Abs x t fx) = "\\" ++ x ++ ":" ++ "(" ++ show t ++ ") " ++ "->" ++ "{" ++ str fx ++ "}"  
    str (Literal atom) = str atom
    str (Operation f op g) = str f ++ str op  ++ str g 
    str (Let x fx) = x ++ "=" ++ " " ++ str fx   
    str (IfElse c a b) = show c ++ " ? " ++ show a ++ " : " ++ show b
    str (Fix expr) = "fix " ++ show expr
instance Show Type where
    show TInt = "Int"
    show TBool = "Bool"
    --show (TArr x y) =  show x  ++ "->" ++   show y 
    show Star = "*"
    show (TArr x y) = "(" ++ show x ++ ")" ++ "->" ++  "(" ++ show y  ++ ")"

instance Show TypeError where
    show (TypeMismatch bad good) = "Expected type " ++ (show good)  ++ " but " ++ (show bad) ++ " was given"
    show (NotFunction t) = "Not a function"
    show (OutOfScope name) = name ++ " is out of scope"
    show (Misc xx) = show xx

instance Strable Atom where
    str (XInt x) = show x 
    str (XBool b) = show b 

instance Strable Op where
    str Add = "+"
    str Mul = "*"
    str Sub = "-"
    str And = "&"
    str Or = "|"
    str Eq = "="
    str Gr = ">"

instance Show Expr where
    show x = str x

instance Show Atom where
    show x = str x

instance Show Op where
    show x = str x
