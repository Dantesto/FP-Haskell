module Parser3 where

import Lexer3

--special "head" and "tail" for parsing
pHead :: [Token] -> Token
pHead [] = EndToken
pHead (t:tokens) = t

pTail :: [Token] -> [Token]
pTail [] = error "Incomplete expression"
pTail (t:tokens) = tokens

parse :: [Token] -> AST
parse tokens = let (ast, tokens') = operation tokens minPrecedence
               in if null tokens'
                      then ast
                      else error $ "Leftover expression '" ++ tokensToString tokens' ++ "'"

operation :: [Token] -> Int -> (AST, [Token])
operation tokens 0 = value tokens
operation tokens curPrecedence =
    let (lNode, tokens') = operation tokens (curPrecedence - 1)
    in
        if isOperator (pHead tokens') && precedence (pHead tokens') == curPrecedence
            then if associativity (pHead tokens') == rightAssociative
                     then let (rNode, tokens'') = operation (pTail tokens') curPrecedence
                          in (BinNode (pHead tokens') lNode rNode, tokens'')
                     else let (rNode, tokens'') = operation (pTail tokens') (curPrecedence - 1)
                          in operation ((IdToken (BinNode (pHead tokens') lNode rNode)):tokens'') curPrecedence 
            else (lNode, tokens')

value :: [Token] -> (AST, [Token])
value tokens
    | isIdToken (pHead tokens) = (getNodeFromIdToken (pHead tokens), pTail tokens)
    | isNumberVariable (pHead tokens) = (Leaf (pHead tokens), pTail tokens)
    | isFunction (pHead tokens) = if Parenthesis LParen == pHead (pTail tokens)
                                      then let (node, tokens') = operation (pTail (pTail tokens)) minPrecedence
                                           in if Parenthesis RParen == pHead tokens'
                                                  then (UnNode (pHead tokens) node, pTail tokens')
                                                  else error "Missing right parenthesis"
                                      else error $ "Missing left parenthesis at " ++ tokensToString (pTail tokens)
    | isUnaryOperator (pHead tokens) = let (node, tokens') = value (pTail tokens)
                                   in (UnNode (pHead tokens) node, tokens')
    | Parenthesis LParen == pHead tokens = let (node, tokens') = operation (pTail tokens) minPrecedence
                                           in if Parenthesis RParen == pHead tokens'
                                                  then (node, pTail tokens')
                                                  else error "Missing right parenthesis"
    | otherwise = error $ "Parsing error at " ++ tokensToString tokens

getNodeFromIdToken :: Token -> AST
getNodeFromIdToken (IdToken node) = node
