module JS (module Parser2, evalJS) where
import Data.List
import Data.Maybe
import Parser2

evalExpr (NullLiteral) = "null"
evalExpr (BoolLiteral True)  = "true"
evalExpr (BoolLiteral False) = "false"
evalExpr (NumberLiteral num) = show num
evalExpr (StringLiteral str) = str
evalExpr (ArrayLiteral arr)  = "[" ++ intercalate "," (map evalExpr arr) ++ "]"
evalExpr (VariableDeclaration ident operator expr) = "var " ++ ident ++ " " ++ operator ++ " " ++ evalExpr expr

evalStatement (Block statements) = "{\n" ++ intercalate ";\n" (map (("    " ++) . evalStatement) statements) ++ "\n}"
evalStatement (WhileStatement expr statement) = "while (" ++ evalExpr expr ++ ") " ++ evalStatement statement 

evalJS' parsed = (map evalStatement parsed)

evalJS (Right parsed) = (intercalate ";\n" (evalJS' parsed)) ++ ";"  
evalJS _ = ""