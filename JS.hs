module JS (module Parser, evalJS) where
import Data.List
import Data.Maybe
import Parser
eval (Assignment name exp global) = (if global then "" else "var ") ++ name ++ " = " ++ eval exp 
eval (ReturnStatement exp) = "return " ++ eval exp 
eval (VariableCall name)   = name
eval (InfixOperation op ex1 ex2) = eval ex1 ++ " " ++ op ++ " " ++ eval ex2
eval (FunctionCall name exps) = name ++ "(" ++ (intercalate "," $ map eval exps) ++ ")"
eval (NumberLiteral n) = show n
eval (StringLiteral s) = s
eval (FunctionDefinition name params exps) = "function " 
                                             ++ (maybe "" (++ " ") name) 
                                             ++ "("
                                             ++ (intercalate "," params) 
                                             ++ ")"
                                             ++ " { \n" 
                                             ++ intercalate ";\n" (map ("\t" ++) (evalJS' exps))
                                             ++ "\n}"

evalJS' parsed = (map eval parsed)

evalJS (Right parsed) = (intercalate ";\n" (evalJS' parsed)) ++ ";"  
evalJS _ = ""