module Calculator where

import Lexer
import Parser

calculator :: IO ()
calculator = do
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
evaluate (UnNode (Operator Plus) node) variable = evaluate node variable
evaluate (UnNode (Operator Minus) node) variable = -(evaluate node variable)
evaluate (UnNode (Function Ln) node) variable = log (evaluate node variable)
evaluate (UnNode (Function Sin) node) variable = sin (evaluate node variable)
evaluate (UnNode (Function Cos) node) variable = cos (evaluate node variable)
evaluate (BinNode (Operator Plus) lNode rNode) variable = (evaluate lNode variable) + (evaluate rNode variable)
evaluate (BinNode (Operator Minus) lNode rNode) variable = (evaluate lNode variable) - (evaluate rNode variable)
evaluate (BinNode (Operator Multiplicate) lNode rNode) variable = (evaluate lNode variable) * (evaluate rNode variable)
evaluate (BinNode (Operator Divide) lNode rNode) variable = (evaluate lNode variable) / (evaluate rNode variable)
evaluate (BinNode (Operator Power) lNode rNode) variable = (evaluate lNode variable) ** (evaluate rNode variable)
