{-# LANGUAGE NoMonomorphismRestriction #-}
module Parser2 where
import Text.Parsec
import Text.Parsec.ByteString
import Data.Maybe

type Identifier = String
type Operator = String

data Expression = VariableDeclaration Identifier Operator Expression
                | FunctionDeclaration Bool (Maybe Identifier) [Identifier] Statement
                | IdentifierLookup Identifier
                | FunctionCall Identifier [Expression]
                | InfixOperation Expression Identifier Expression
                | WrappedExpression Expression
                | NullLiteral
                | BoolLiteral Bool
                | NumberLiteral Float
                | StringLiteral String
                | ArrayLiteral [Expression]
                | MemberLookup Identifier Expression
                | Continuate
                | NextReturn [Expression]
                  deriving (Show,Eq)

data Statement = Block [Statement]
               | ReturnStatement Expression
               | IfStatement Expression Statement
               | ElseStatement Statement
               | WhileStatement Expression Statement
               | TopLevelExpression Expression
               | AssignmentStatement Bool Expression
               | Continuation [Identifier] Expression
               | EachStatement Identifier Expression Statement
                 deriving (Show,Eq)

infixOperator = choice [symbol "+"
                       ,symbol "-"
                       ,symbol "*"
                       ,symbol "/"
                       ,symbol "=="
                       ,symbol "!="
                       ,symbol ">="
                       ,symbol "<="
                       ,symbol ">"
                       ,symbol "<"
                       ]

whiteSpace = try spaces

expression = do 
  ex <- try infixOperation <|>  expression'
  optional whiteSpace
  return ex

expression' = do
  ex <- try nextReturn <|> try continuate <|> try wrappedExpression <|> try memberLookup <|> try functionDeclaration <|> try functionCall <|> try variableDeclaration <|> try literal <|> try identifierLookup 
  optional whiteSpace
  return ex

wrappedExpression = do
  symbol "("
  ex <- expression
  symbol ")"
  return (WrappedExpression ex)

identifier = do 
  ident <- many1 (letter <|> char '_' <|> char '.' <|> char '$' <|> char '[' <|> char ']')
  optional whiteSpace
  return ident

identifierLookup = do
  ident <- identifier
  return $ IdentifierLookup ident

memberLookup = do
  obj <- identifier
  symbol "."
  member <- try memberLookup <|> try functionCall <|> try identifierLookup
  return $ MemberLookup obj member

functionCall = do
  ident <- identifier
  symbol "("
  args <- arguments
  optional $ whiteSpace
  symbol ")"
  return $ FunctionCall ident args

nextReturn = do
  ident <- symbol "next"
  symbol "("
  args <- arguments
  optional $ whiteSpace
  symbol ")"
  return $ NextReturn args


continuate = do
  symbol "continuation"
  notFollowedBy identifier
  return $ Continuate

arguments = do
  sepBy expression (try $ symbol ",")

functionDeclaration = do
  symbol "function" <|> symbol "\\"
  name <- optionMaybe identifier
  symbol "("
  params <- sepBy identifier (symbol ",")
  symbol ")"
  stat <- try block <|> try statement
  return $ FunctionDeclaration False name params stat


literal = do lit <- nullLiteral <|> boolLiteral <|> numberLiteral <|> stringLiteral  <|> arrayLiteral
             notFollowedBy identifier
             return lit

nullLiteral = string "null" >> notFollowedBy identifier >> return NullLiteral
boolLiteral = do
  b <- choice [string "true", string "false"]
  notFollowedBy identifier
  return $ case b of
             "true"  -> BoolLiteral True
             "false" -> BoolLiteral False

numberLiteral = do
  neg <- optionMaybe $ try (string "-")
  n <- many1 digit
  notFollowedBy alphaNum
  return $ NumberLiteral (read $ fromMaybe "" neg ++  n)

stringLiteral = do
  delimiter <- choice [char '"', char '\'']
  str <- manyTill anyChar (char delimiter <?> "end of string literal")
  return $ StringLiteral str

symbol c = do
  s <- string c
  whiteSpace
  return s

arrayLiteral = do
  symbol "["
  expr <- sepBy expression (symbol ",")
  symbol "]"
  return $ ArrayLiteral expr

variableDeclaration = do
  ident <- identifier <?> "identifier"
  whiteSpace
  operator <- choice [symbol "=", symbol "+=", symbol "-=", symbol "/=", symbol "*="] <?> "operator"
  expr <- expression <?> "expression"
  return $ VariableDeclaration ident operator expr

infixOperation = do 
  e1  <- expression'
  op  <- infixOperator
  e2  <- expression
  return $ InfixOperation e1 op e2

-- Statements

statement = try assignmentStatement 
            <|> try eachStatement
            <|> try continuation 
            <|> try ifStatement 
            <|> try elseStatement
            <|> try returnStatement 
            <|> try whileStatement 
            <|> try topLevelExpression 
            <?> "statement"

endOfStatement = do
  choice [newline, char ';', eof >> return ' '] <?> "end of statement"
  whiteSpace

returnStatement = do
  symbol "return"
  expr <- expression
  optional endOfStatement
  return $ ReturnStatement expr

block = do 
  symbol "{" <?> "start of block"
  whiteSpace
  stats <- many statement <?> "statement"
  whiteSpace
  symbol "}" <?> "end of block"
  return $ Block stats

endOfBlock = symbol "}" >> return ()

assignmentStatement = do
  init <- optionMaybe (symbol "var")
  assign <- variableDeclaration
  optional endOfStatement
  return $ AssignmentStatement (not $ isNothing init) assign

topLevelExpression = do
  expr <- try nextReturn <|> try memberLookup <|> try functionCall <|> try functionDeclaration
  optional endOfStatement
  return $ TopLevelExpression expr

continuation = do
  ident <- contParams <|> contIdent
  symbol "<-"
  expr <- expression
  optional endOfStatement
  return $ Continuation ident expr
  where contParams = do symbol "("
                        params <- identifier `sepBy` (symbol ",")
                        symbol ")"
                        return params
        contIdent = do ident <- identifier
                       return [ident]
       

whileStatement = do
  symbol "while"
  symbol "("
  exp <- expression
  whiteSpace
  symbol ")" <?> "closing para"
  stat <- try block <|> try statement <?> "while loop body"
  optional endOfStatement
  return $ WhileStatement exp stat

eachStatement = do
  symbol "each"
  symbol "("
  ident <- identifier
  symbol "<-"
  expr  <- expression
  symbol ")"
  state <- try block <|> try statement
  optional endOfStatement
  return $ EachStatement ident expr state
  
ifStatement = do
  symbol "if"
  symbol "("
  exp <- expression
  symbol ")" <?> "closing para"
  state <- try block <|> try statement
  optional endOfStatement
  return $ IfStatement exp state

elseStatement = do
  symbol "else"
  state <- try block <|> try statement
  optional endOfStatement
  return $ ElseStatement state