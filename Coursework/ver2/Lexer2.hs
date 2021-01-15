module Lexer2 where

import Data.Char

data Parenthesis = LParen | RParen deriving Eq
instance Show Parenthesis where
    show LParen = "("
    show RParen = ")"

data Operator = Plus | Minus | Multiplicate | Divide | Power deriving Eq
instance Show Operator where
    show Plus = "+"
    show Minus = "-"
    show Multiplicate = "*"
    show Divide = "/"
    show Power = "^"

data Function = Ln | Sin | Cos deriving Eq
instance Show Function where
    show Ln = "ln"
    show Sin = "sin"
    show Cos = "cos"

data Token = Number Double | Variable | Parenthesis Parenthesis | Operator Operator | Function Function | EndToken deriving Eq
instance Show Token where
    show (Number x) = show x
    show Variable = "x"
    show (Parenthesis p) = show p
    show (Operator op) = show op
    show (Function f) = show f

isNumberVariable :: Token -> Bool
isNumberVariable (Number _) = True
isNumberVariable Variable = True
isNumberVariable _ = False

isParenthesis :: Token -> Bool
isParenthesis (Parenthesis _) = True
isParenthesis _ = False

isPlusMinus :: Token -> Bool
isPlusMinus (Operator Plus) = True
isPlusMinus (Operator Minus) = True
isPlusMinus _ = False

isMulDiv :: Token -> Bool
isMulDiv (Operator Multiplicate) = True
isMulDiv (Operator Divide) = True
isMulDiv _ = False

isPower :: Token -> Bool
isPower (Operator Power) = True
isPower _ = False

isFunction :: Token -> Bool
isFunction (Function _) = True
isFunction _ = False

getTokens :: String -> [Token]
getTokens expression = reverse (getTokens' [] expression)

getTokens' :: [Token] -> String -> [Token]
getTokens' tokens "" = tokens
getTokens' tokens (' ':expression) = getTokens' tokens expression
getTokens' tokens (c:expression)
    | c == '+' = getTokens' ((Operator Plus):tokens) expression
    | c == '-' = getTokens' ((Operator Minus):tokens) expression
    | c == '*' = getTokens' ((Operator Multiplicate):tokens) expression
    | c == '/' = getTokens' ((Operator Divide):tokens) expression
    | c == '^' = getTokens' ((Operator Power):tokens) expression
    | c == '(' = getTokens' ((Parenthesis LParen):tokens) expression
    | c == ')' = getTokens' ((Parenthesis RParen):tokens) expression
    | c == 'x' && (null expression || not (isLetter . head $ expression)) = getTokens' (Variable:tokens) expression
    | isNumber c =
          let (number, expressionTail) = getNumber 0 (c:expression)
          in getTokens' (number:tokens) expressionTail
    | isLetter c =
          let (functionOrConst, expressionTail) = getFunctionOrConst "" (c:expression)
          in getTokens' (functionOrConst:tokens) expressionTail
    | otherwise = error $ "Unexpected token at " ++ c:expression

getNumber :: Double -> String -> (Token, String)
getNumber number "" = (Number number, "")
getNumber number ('.':expression) = (Number (number + fraction), expressionTail)
    where (fraction, expressionTail) = getFraction 0 0.1 expression
getNumber number (c:expression)
    | isNumber c = getNumber (number * 10 + fromIntegral (ord c - ord '0')) expression
    | otherwise = (Number number, c:expression)

getFraction :: Double -> Double -> String -> (Double, String)
getFraction fraction _ "" = (fraction, "")
getFraction fraction pow (c:expression)
    | isNumber c = getFraction (fraction + (fromIntegral (ord c - ord '0')) * pow) (pow / 10) expression
    | otherwise = (fraction, c:expression)

getFunctionOrConst :: String -> String -> (Token, String)
getFunctionOrConst function "" = (getFunctionOrConstName . reverse $ function, "")
getFunctionOrConst function (c:expression)
    | isLetter c = getFunctionOrConst (c:function) expression
    | otherwise = (getFunctionOrConstName . reverse $ function, c:expression)

getFunctionOrConstName :: String -> Token
getFunctionOrConstName "e" = Number (exp 1)
getFunctionOrConstName "pi" = Number pi
getFunctionOrConstName "ln" = Function Ln
getFunctionOrConstName "sin" = Function Sin
getFunctionOrConstName "cos" = Function Cos
getFunctionOrConstName name = error $ "Unknown name '" ++ name ++ "'" --if more than one variable then it's the name of a variable

tokensToString :: [Token] -> String
tokensToString [] = ""
tokensToString (t:tokens) = show t ++ tokensToString tokens
