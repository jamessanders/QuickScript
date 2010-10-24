{-# LANGUAGE NoMonomorphismRestriction #-}
module Parser2 where
import Text.Parsec
import Text.Parsec.ByteString
import Data.Maybe

type Identifier = String
type Operator = String

data Expression = VariableDeclaration Identifier Operator Expression
                | FunctionDeclaration [Identifier] Statement
                | IdentifierLookup Identifier
                | FunctionCall Identifier [Expression]
                | InfixOperation Expression Identifier Expression
                | WrappedExpression Expression
                | NullLiteral
                | BoolLiteral Bool
                | NumberLiteral Float
                | StringLiteral String
                | ArrayLiteral [Expression]
                  deriving (Show)

data Statement = Block [Statement]
               | ReturnStatement Expression
               | WhileStatement Expression Statement
               | TopLevelExpression Expression
                 deriving (Show)

infixOperator = choice [symbol "+"
                       ,symbol "-"
                       ,symbol "*"
                       ,symbol "/"
                       ,symbol "=="
                       ,symbol "!="
                       ,symbol ">"
                       ,symbol "<"
                       ,symbol ">="
                       ,symbol "<="]

whiteSpace = try spaces

expression = do 
  ex <- try infixOperation <|>  expression'
  optional whiteSpace
  return ex

expression' = do
  ex <- try wrappedExpression <|> try functionDeclaration <|> try functionCall <|> try variableDeclaration <|> try literal <|> try identifierLookup 
  optional whiteSpace
  return ex

wrappedExpression = do
  symbol "("
  ex <- expression
  symbol ")"
  return (WrappedExpression ex)

identifier = do ident <- many1 letter
                optional whiteSpace
                return ident

identifierLookup = do
  ident <- identifier
  return $ IdentifierLookup ident

functionCall = do
  ident <- identifier
  symbol "("
  args <- arguments
  optional $ whiteSpace
  symbol ")"
  return $ FunctionCall ident args

arguments = do
  sepBy expression (try $ symbol ",")

functionDeclaration = do
  symbol "function"
  name <- optionMaybe identifier
  symbol "("
  params <- sepBy identifier (symbol ",")
  symbol ")"
  stat <- block
  return $ case name of 
    Nothing -> FunctionDeclaration params stat
    Just n  -> VariableDeclaration n "=" (FunctionDeclaration params stat)

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
  optional $ symbol "var"
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

statement = try returnStatement <|> try whileStatement <|> try topLevelExpression

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

topLevelExpression = do
  expr <- try functionCall <|> try variableDeclaration <|> try functionDeclaration
  optional endOfStatement
  return $ TopLevelExpression expr

whileStatement = do
  symbol "while"
  symbol "("
  exp <- expression
  whiteSpace
  symbol ")" <?> "closing para"
  stat <- try block <|> try statement <?> "while loop body"
  optional endOfStatement
  return $ WhileStatement exp stat
  
