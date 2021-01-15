module Calculator3 where

import Lexer3
import Parser3

calculator :: IO ()
calculator = do
    putStrLn "---"
    putStr "Expression: "
    expression <- getLine
    if expression == "q"
        then return ()
        else do
            putStr "Variable (default = 0): "
            variable <- getLine
            if variable == ""
                then putStrLn ("Result: " ++ show (evaluate (parse . getTokens $ expression) 0))
                else putStrLn ("Result: " ++ show (evaluate (parse . getTokens $ expression) (read variable)))
            calculator

evaluate :: AST -> Double -> Double
evaluate (Leaf (Number num)) variable = num
evaluate (Leaf Variable) variable = variable
evaluate (UnNode (Operator "+" _ _ _) node) variable = evaluate node variable --again. why compiler says same as in "isUnaryOperator"
evaluate (UnNode (Operator "-" _ _ _) node) variable = -(evaluate node variable)
evaluate (UnNode (Function Ln) node) variable = log (evaluate node variable)
evaluate (UnNode (Function Sin) node) variable = sin (evaluate node variable)
evaluate (UnNode (Function Cos) node) variable = cos (evaluate node variable)
evaluate (BinNode (Operator "+" _ _ _) lNode rNode) variable = (evaluate lNode variable) + (evaluate rNode variable)
evaluate (BinNode (Operator "-" _ _ _) lNode rNode) variable = (evaluate lNode variable) - (evaluate rNode variable)
evaluate (BinNode (Operator "*" _ _ _) lNode rNode) variable = (evaluate lNode variable) * (evaluate rNode variable)
evaluate (BinNode (Operator "/" _ _ _) lNode rNode) variable = (evaluate lNode variable) / (evaluate rNode variable)
evaluate (BinNode (Operator "^" _ _ _) lNode rNode) variable = (evaluate lNode variable) ** (evaluate rNode variable)
