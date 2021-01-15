module Parser2 where

import Lexer2

--abstract syntax tree
data AST = Leaf Token | UnNode Token AST | BinNode Token AST AST deriving Show

--special "head" and "tail" for parsing
pHead :: [Token] -> Token
pHead [] = EndToken
pHead (t:tokens) = t

pTail :: [Token] -> [Token]
pTail [] = error "Incomplete expression"
pTail (t:tokens) = tokens

parse :: [Token] -> AST
parse tokens = let (ast, tokens') = thirdPriorityOperation' (Leaf (Number 0)) ((Operator Plus):tokens)
               in if null tokens'
                      then ast
                      else error $ "Leftover expression '" ++ tokensToString tokens' ++ "'"

thirdPriorityOperation' :: AST -> [Token] -> (AST, [Token])
thirdPriorityOperation' lNode tokens =
    if isPlusMinus (pHead tokens)
        then let (rNode, tokens') = secondPriorityOperation' (Leaf (Number 1)) (Operator Multiplicate:(pTail tokens))
             in thirdPriorityOperation' (BinNode (pHead tokens) lNode rNode) tokens'
        else (lNode, tokens)

secondPriorityOperation' :: AST -> [Token] -> (AST, [Token])
secondPriorityOperation' lNode tokens =
    if isMulDiv (pHead tokens)
        then let (rNode, tokens') = firstPriorityOperation' (pTail tokens)
             in secondPriorityOperation' (BinNode (pHead tokens) lNode rNode) tokens'
        else (lNode, tokens)

firstPriorityOperation' :: [Token] -> (AST, [Token])
firstPriorityOperation' tokens =
    let (lNode, tokens') = value' tokens
    in if isPower (pHead tokens')
           then let (rNode, tokens'') = firstPriorityOperation' (pTail tokens')
                in (BinNode (pHead tokens') lNode rNode, tokens'')
           else (lNode, tokens')

value' :: [Token] -> (AST, [Token])
value' tokens
    | isNumberVariable (pHead tokens) = (Leaf (pHead tokens), pTail tokens)
    | isFunction (pHead tokens) = if Parenthesis LParen == pHead (pTail tokens)
                                      then let (node, tokens') = thirdPriorityOperation' (Leaf (Number 0)) (Operator Plus:(pTail (pTail tokens)))
                                           in if Parenthesis RParen == pHead tokens'
                                                  then (UnNode (pHead tokens) node, pTail tokens')
                                                  else error "Missing right parenthesis"
                                      else error $ "Missing left parenthesis at " ++ tokensToString (pTail tokens)
    | isPlusMinus (pHead tokens) = let (node, tokens') = value' (pTail tokens)
                                   in (UnNode (pHead tokens) node, tokens')
    | Parenthesis LParen == pHead tokens = let (node, tokens') = thirdPriorityOperation' (Leaf (Number 0)) (Operator Plus:(pTail tokens))
                                           in if Parenthesis RParen == pHead tokens'
                                                  then (node, pTail tokens')
                                                  else error "Missing right parenthesis"
    | otherwise = error $ "Parsing error at " ++ tokensToString tokens
