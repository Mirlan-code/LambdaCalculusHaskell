module OutputMisc where

import LambdaCalc

class Strable a where
    str :: a -> String

instance Strable Expr where
    str (Var x) = x
    str (App f g) = "(" ++ str f ++ ")" ++ " $ " ++ "(" ++ str g ++ ")"
    str (Abs x fx) = "\\" ++ x ++ "->" ++ "{" ++ str fx ++ "}"  
    str (Literal atom) = str atom
    str (Operation f op g) = str f ++ str op  ++ str g 
    str (Let x fx) = x ++ "=" ++ " " ++ str fx   

instance Strable Atom where
    str (XInt x) = show x 
    str (XBool b) = show b 

instance Strable Op where
    str Add = "+"
    str Mul = "*"
    str Sub = "-"
    str And = "&"
    str Or = "|"

instance Show Expr where
    show x = str x

instance Show Atom where
    show x = str x

instance Show Op where
    show x = str x
