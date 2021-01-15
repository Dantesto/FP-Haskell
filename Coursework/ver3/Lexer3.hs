module Lexer3 where

import Data.Char

data AST = Leaf Token | UnNode Token AST | BinNode Token AST AST deriving (Show, Eq)

data Parenthesis = LParen | RParen deriving Eq
instance Show Parenthesis where
    show LParen = "("
    show RParen = ")"

data Function = Ln | Sin | Cos deriving Eq
instance Show Function where
    show Ln = "ln"
    show Sin = "sin"
    show Cos = "cos"

data Token = Number Double
           | Variable
           | Parenthesis Parenthesis
           | Operator { symbol :: String --I added this because, for example, without it 'minus' and 'plus' are equal
                      , precedence :: Int
                      , associativity :: Int
                      }
           | Function Function
           | IdToken AST --it's like id function. is used to make left node for left-associative operators. for example, in 1 + 2 + 3 when I get (1 + 2) as AST I need to give it to (1 + 2) + 3 as lNode
           | EndToken deriving Eq
instance Show Token where
    show (Number x) = show x
    show Variable = "x"
    show (Parenthesis p) = show p
    show (Function f) = show f
    show op = symbol op

leftAssociative = 1
associative = 2
rightAssociative = 3

minPrecedence = 3 :: Int

plus = Operator "+" 3 associative
minus = Operator "-" 3 associative
multiplicate = Operator "*" 2 associative
divide = Operator "/" 2 leftAssociative
power = Operator "^" 1 rightAssociative

isNumberVariable :: Token -> Bool
isNumberVariable (Number _) = True
isNumberVariable Variable = True
isNumberVariable _ = False

isParenthesis :: Token -> Bool
isParenthesis (Parenthesis _) = True
isParenthesis _ = False

isUnaryOperator :: Token -> Bool
isUnaryOperator (Operator "+" _ _ _) = True --why compiler says "isUnaryOperator plus" and "isUnaryOperator minus" are same?
isUnaryOperator (Operator "-" _ _ _) = True --but know it works
isUnaryOperator _ = False

isOperator :: Token -> Bool
isOperator (Operator _ _ _ _) = True
isOperator _ = False

isFunction :: Token -> Bool
isFunction (Function _) = True
isFunction _ = False

isIdToken :: Token -> Bool
isIdToken (IdToken _) = True
isIdToken _ = False

getTokens :: String -> [Token]
getTokens expression = reverse (getTokens' [] expression)

getTokens' :: [Token] -> String -> [Token]
getTokens' tokens "" = tokens
getTokens' tokens (' ':expression) = getTokens' tokens expression
getTokens' tokens (c:expression)
    | c == '+' = getTokens' (plus:tokens) expression
    | c == '-' = getTokens' (minus:tokens) expression
    | c == '*' = getTokens' (multiplicate:tokens) expression
    | c == '/' = getTokens' (divide:tokens) expression
    | c == '^' = getTokens' (power:tokens) expression
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
