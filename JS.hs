module JS (module Parser2, evalJS) where
import Data.List
import Data.Maybe
import Parser2

makeBlock block@(Block _) = block
makeBlock statement = Block [statement]

evalExpr d (NullLiteral) = "null"
evalExpr d (BoolLiteral True)  = "true"
evalExpr d (BoolLiteral False) = "false"
evalExpr d (NumberLiteral num) = show num
evalExpr d (StringLiteral str) = "\"" ++ str ++ "\""
evalExpr d (ArrayLiteral arr)  = "[" ++ intercalate "," (map (evalExpr d) arr) ++ "]"
evalExpr d (VariableDeclaration ident operator expr) = ident ++ " " ++ operator ++ " " ++ evalExpr d expr
evalExpr d (FunctionDeclaration False name args statement) = "function " ++ fromMaybe "" name ++ "(" ++ intercalate "," (args ++ [("next" ++ show d)]) ++ ") " ++ (evalStatement d $ makeBlock statement)
evalExpr d (FunctionDeclaration True  name args statement) = "function " ++ fromMaybe "" name ++ "(" ++ intercalate "," args  ++ ") " ++ (evalStatement d $ makeBlock statement)
evalExpr d (FunctionCall ident exprs) = ident ++ "(" ++ intercalate "," (map (evalExpr d) exprs) ++ ")" 
evalExpr d (NextReturn exprs) = evalExpr d $ FunctionCall ("next" ++ show d) exprs
evalExpr d (WrappedExpression expr) = "(" ++ evalExpr d expr ++ ")"
evalExpr d (IdentifierLookup i) = i
evalExpr d (InfixOperation expr1 op expr2) = evalExpr d expr1 ++ " " ++ op ++ " " ++ evalExpr d expr2
evalExpr d (MemberLookup obj expr) = obj ++ "." ++ (evalExpr d expr)

evalTLExpr d expr = evalExpr d expr

evalStatement d (Block statements) = "{\n" ++ intercalate "\n" (map (("    " ++) . evalStatement d) (evalContinuations statements)) ++ "\n}"
evalStatement d (WhileStatement expr statement) = "(function qs_while () { \n if (" ++ evalExpr d expr ++ ") " ++ evalStatement d (addRecurse statement) ++ "\n})()"
    where addRecurse (Block st) = Block (st ++ [IfStatement (expr) $ TopLevelExpression $ FunctionCall "qs_while" []])
          addRecurse st = addRecurse (Block [st])

evalStatement d (TopLevelExpression expr) = evalTLExpr d expr
evalStatement d (ReturnStatement expr) = "return " ++ evalExpr d expr
evalStatement d (IfStatement expr statement) = "if (" ++ evalExpr d expr ++ ") " ++ evalStatement d statement
evalStatement d (ElseStatement statment) = "else " ++ evalStatement d statment
evalStatement d (AssignmentStatement isinit expr) = (if isinit then "var " else "") ++ evalExpr d expr

evalContinuations :: [Statement] -> [Statement]
evalContinuations [] = []
evalContinuations ((Continuation ident expr):xs) = [TopLevelExpression $ FunctionCall (getIdent expr) (insertInParam expr (FunctionDeclaration True Nothing ident (Block $ evalContinuations xs)))]
    where getIdent (FunctionCall ident _) = ident
          insertInParam (FunctionCall ident args) rep = 
              if any (== Continuate) args then map (\x-> if x == Continuate then rep else x) args else args ++ [rep]

evalContinuations ((EachStatement ident expr statement):xs) = [TopLevelExpression $ FunctionCall "eachAsync" 
                                                               [expr
                                                               , FunctionDeclaration True Nothing [ident, "next"] (addNext statement)
                                                               , FunctionDeclaration True Nothing [] (Block (evalContinuations xs))]]
    where addNext (Block st) = Block (st ++ [TopLevelExpression $ FunctionCall "next" []])
          addNext st         = Block (st : [TopLevelExpression $ FunctionCall "next" []])

evalContinuations (x:xs) = x : evalContinuations xs


evalJS' parsed = (map (evalStatement 0) (evalContinuations parsed))

evalJS (Right parsed) = "require('./js/rt.js').globalLoadIterators()\n\n" ++ (intercalate ";\n" (evalJS' parsed)) ++ ";"  
evalJS _ = ""
