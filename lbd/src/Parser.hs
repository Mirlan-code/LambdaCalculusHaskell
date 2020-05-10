module Parser where

import Data.Attoparsec.Text hiding (number)
import Data.Text (Text)
import qualified Data.Text as T
import Control.Applicative hiding (many,number)
import LambdaCalc
import Data.Char
import OutputMisc
import Data.List.Extra (trim)
import qualified Data.Map as Map
import Check

ch2op :: Char -> Op
ch2op '+' = Add
ch2op '-' = Sub
ch2op '*' = Mul 
ch2op '&' = And 
ch2op '|' = Or 
ch2op '=' = Eq
ch2op '>' = Gr

-- a function to eliminate successive spaces
preclean :: String -> String
preclean x = case x of
    [] -> []
    (' ':' ':xs) -> preclean (' ':xs)
    (x : xs) -> preclean (x : xs)

singleChar :: Char -> Parser Char
singleChar x = (many' space) *> (char x)  <* (many' space)

-- a function to read words of big letters
globalBinder :: Parser String
globalBinder = (many' space) *> (many1 (satisfy (isAlpha)))

-- a parser that parses the word given
rawstring :: String -> Parser String
rawstring str = (many' space) *> fmap T.unpack ( string ( T.pack str ) )

-- operations
binop :: Parser Op
binop = fmap ch2op ((singleChar '+') <|> (singleChar '-') <|> (singleChar '*') <|> (singleChar '&') <|> (singleChar '|'))

--comparators
compareop :: Parser Op
compareop = fmap ch2op ((singleChar '=') <|> singleChar '>')

-- raw string
rawvar :: Parser String
rawvar = ( (many' space) *> (many1 letter) )

var :: Parser Expr
var = fmap Var rawvar   

-- integer parser
number :: Parser Expr
number = fmap (Literal . XInt . read) ( (many' space) *> (many1 digit) )

-- boolean
boolean :: Parser Expr
boolean = fmap (Literal . XBool . read) ( (many' space) *> (rawstring  "True" <|> rawstring  "False"))

-- single atom
single :: Parser Expr
single = boolean <|> var <|> number

-- a function that surrounds a given parser with ( )
parensed :: Parser Expr -> Parser Expr
parensed _parser = ((singleChar '(') *> _parser <* (singleChar ')'))

-- arithmetic parser
arith :: Parser Expr
arith =  ( (pure Operation) <*> term <*> binop <*> arith ) <|> term 

term :: Parser Expr
term = single  <|> (parensed expr)

-- stuff that can be applied
applicable :: Parser Expr
applicable =  arith <|> lambdaExpr 


foldapp :: [Expr] -> Expr
foldapp [x] = x
foldapp (x:xs) = foldl App x xs

-- successive applications 
succapp :: Parser Expr
succapp = fmap foldapp (many1 applicable)


-- lambda expression starting with \
lambdaExpr :: Parser Expr
lambdaExpr = (pure Abs) <*> (singleChar '\\' *> rawvar ) <*> (predec ":" tTypeParser) <*> ( (singleChar '.' *> expr <* singleChar '$') <|> noSignLambdaExpr )

-- lambda expression starting with no sign
noSignLambdaExpr :: Parser Expr
noSignLambdaExpr = (pure Abs) <*> rawvar <*> (predec ":" tTypeParser)  <*> ( (singleChar '.' *> expr <* singleChar '$') <|> noSignLambdaExpr )

predec :: String -> Parser a -> Parser a 
predec c p = (rawstring c *> p)

--single type token parser
tAtomParser :: Parser Type
tAtomParser = tIntParser <|> tBoolParser

-- int type pasrer
tIntParser :: Parser Type
tIntParser = fmap (\x -> TInt) ( (many' space) *> (rawstring  "Int"))

-- tbool parser
tBoolParser :: Parser Type
tBoolParser = fmap (\x -> TBool) ( (many' space) *> (rawstring  "Bool"))

-- complete type parser
tTypeParser :: Parser Type
tTypeParser = tAtomParser <|> ( (pure TArr) <*> tAtomParser <*  (rawstring "->") *> tTypeParser) 

-- let statement parser
letParser :: Parser Expr
letParser =  do
    rawstring "let"
    many1 space
    id <- globalBinder
    singleChar '='
    ex <- expr
    singleChar ';'
    endOfInput
    return (Let id ex)

ifParser :: Parser Expr
ifParser = do
    rawstring "if"
    many1 space
    condition <- conditionParser
    singleChar '?'
    positive <- expr
    singleChar ':'
    negative <- expr
    return (IfElse condition positive negative)

conditionParser :: Parser Expr
conditionParser = (pure Operation) <*> succapp <*> compareop <*> succapp

expr :: Parser Expr
expr = ifParser <|> succapp

globalParser :: Parser Expr
globalParser = (letParser <|> expr)
    
runeval :: Env -> String -> Expr
runeval env x = case parseOnly globalParser (T.pack x) of
    (Right res) ->  eval (subGlobals env res)
    _ -> Var "Error"    



--runeval :: Env -> String -> Expr
--runeval env x = case parseOnly globalParser (T.pack x) of
--    (Right res) ->  eval (subGlobals env res)
--    _ -> Var "Errroooor"    


-- string -> int -> guy
-- int -> guy <*> Parser

parsey :: String -> Expr
parsey x = case parseOnly globalParser (T.pack x) of
    (Right res) ->  res
    _ -> Var "Error"  

checktest :: String -> Either TypeError Type
checktest x = case parseOnly globalParser (T.pack x) of
    (Right res) -> checkType [] res
    _ -> Left $ OutOfScope "parse error"    

jj  = checktest "\\x : Int . x + 1 $ True"
kk = parsey "\\x : Int . \\y : Int . x + y $ $ 3 "

qq = parseOnly ifParser $ T.pack "if x = 10 ? x + 1 : x + 5"