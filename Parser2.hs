{-# LANGUAGE NoMonomorphismRestriction #-}
module Parser2 where
import Text.Parsec

type Idetifier = String
type Operator = String

data Expression = AssignmentExpression Expression
                | NullLiteral
                | BoolLiteral Bool
                | NumberLiteral Integer
                | StringLiteral String
                | ArrayLiteral [Expression]
                  deriving (Show)

data Statement = Block [Statement]
               | VariableDeclaration Idetifier Operator Expression
               | WhileStatement Expression Statement
                 deriving (Show)

statement = try whileStatement <|> try variableDeclaration

whiteSpace = try spaces

expression = do 
  ex <- literal <|> arrayLiteral
  whiteSpace
  return ex

identifier = many1 letter

literal = nullLiteral <|> boolLiteral <|> numberLiteral <|> stringLiteral 

nullLiteral = string "null" >> notFollowedBy identifier >> return NullLiteral
boolLiteral = do
  b <- choice [string "true", string "false"]
  notFollowedBy identifier
  return $ case b of
             "true"  -> BoolLiteral True
             "false" -> BoolLiteral False

numberLiteral = do
  n <- many1 digit
  notFollowedBy alphaNum
  return $ NumberLiteral (read n)

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

-- Statements

endOfStatement = do
  choice [newline, char ';', eof >> return ' '] <?> "end of statement"
  whiteSpace

variableDeclaration = do
  optional $ symbol "var"
  ident <- identifier <?> "identifier"
  whiteSpace
  operator <- choice [symbol "=", symbol "+=", symbol "-=", symbol "/=", symbol "*="] <?> "operator"
  expr <- expression <?> "expression"
  try endOfStatement <|> (lookAhead endOfBlock) <?> "end of block or statement"
  return $ VariableDeclaration ident operator (AssignmentExpression expr)


block = do 
  symbol "{" <?> "start of block"
  whiteSpace
  stats <- many statement <?> "statement"
  whiteSpace
  symbol "}" <?> "end of block"
  return $ Block stats

endOfBlock = symbol "}" >> return ()

whileStatement = do
  symbol "while"
  symbol "("
  exp <- expression
  whiteSpace
  symbol ")" <?> "closing para"
  stat <- try block <|> try statement <?> "while loop body"
  optional endOfStatement
  return $ WhileStatement exp stat
  
