{-# LANGUAGE FlexibleContexts #-}
module Parser where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char
import Control.Monad.Identity
import Control.Monad.Trans
import Debug.Trace
import Data.Maybe

type Name = String

data Expression = Assignment Name Expression Bool
                | ReturnStatement Expression
                | VariableCall Name
                | FunctionCall Name [Expression]
                | FunctionDefinition (Maybe Name) [Name] [Expression]
                | NumberLiteral Integer
                | StringLiteral String
                | InfixOperation String Expression Expression
                | EmptyLine
                | WhiteSpace
                  deriving (Show, Read, Eq)

language :: (Stream s m Char) => ParsecT s u m [Expression]
language = fmap removeWhiteSpace $ manyTill statement (try eof)

statement :: (Stream s m Char) => ParsecT s u m Expression
statement = trace "Parsing top level expression" $ do
  exp <- expression <?> "Statement"
  optional $ try endOfExpression
  return exp

endOfExpression :: (Stream s m Char) => ParsecT s u m Char
endOfExpression = try (char ';') <|> try newline 

expression :: (Stream s m Char) => ParsecT s u m Expression
expression = trace "Parsing expression" $ do
               try infixOperation <|> try whitespace <|> try functionDefinition <|> try returnStatement <|> try assignment <|> try functionCall <|> try variableCall 
  
whitespace :: (Stream s m Char) => ParsecT s u m Expression
whitespace = trace "Parsing whitespace" $ (try space <|> try newline)  >> return WhiteSpace

getName :: (Stream s m Char) => ParsecT s u m Name
getName = trace "Parsing name" $ do many letter

assignment :: (Stream s m Char) => ParsecT s u m Expression
assignment = trace "Parsing assignment" $ do 
  global <- optionMaybe $ try $ string "global"
  try spaces
  varibleName <- getName
  try spaces
  oneOf "="
  try spaces
  value <- try expression <|> try literal
  lookAhead $ endOfExpression <|> (eof >> return ' ')
  return $ Assignment varibleName value (isJust global)

literal :: (Stream s m Char) => ParsecT s u m Expression
literal = trace "Parsing literal" $ do
  lit <- try numberLiteral <|> try stringLiteral 
  return lit
      

endOfLiteral :: (Stream s m Char) => ParsecT s u m Char
endOfLiteral = do try (spaces) 
                  try (char ')') <|> try (char ';') <|> try newline <|> (try eof >> return ' ')

numberLiteral :: (Stream s m Char) => ParsecT s u m Expression
numberLiteral = trace "Parsing number literal" $ do 
  n <- many1 (try digit)
  return $ NumberLiteral (read n)

stringLiteral :: (Stream s m Char) => ParsecT s u m Expression
stringLiteral = trace "Parsing string literal" $ do 
  char '"' <|> char '\''
  str <- manyTill anyChar ((try $ char '"') <|> (try $ char '\''))
  return $ StringLiteral str

variableCall :: (Stream s m Char) => ParsecT s u m Expression
variableCall = trace "Parsing variable call" $ do
  name <- many1 letter
  return $ VariableCall name

functionCall :: (Stream s m Char) => ParsecT s u m Expression
functionCall = trace "Parsing function call" $ do
                 name <- many1 letter
                 try spaces
                 char '('
                 try spaces
                 args <- try ((expression <|> literal) `sepBy` (try spaces >> char ',' >> try spaces)) <|> return []
                 try spaces 
                 char ')'
                 return $ FunctionCall name args
                 

functionDefinition :: (Stream s m Char) => ParsecT s u m Expression
functionDefinition = trace "Parsing function definition" $ do
  string "function"
  try spaces
  name <- optionMaybe $ many1 letter
  try spaces
  char '('
  params <- parameters <?> "Function parameters"
  try spaces
  char ')'
  try spaces 
  char '{'
  exps <- (fmap removeWhiteSpace $ manyTill statement (char '}')) <?> "Function body"
  return $ FunctionDefinition name params exps

returnStatement :: (Stream s m Char) => ParsecT s u m Expression
returnStatement = trace "Parsing return statement" $ do
                    string "return"
                    spaces
                    exp <- try literal <|> try expression 
                    return $ ReturnStatement exp

removeWhiteSpace = filter (/= WhiteSpace) 

parameters :: (Stream s m Char) => ParsecT s u m [Name]
parameters = sepBy getName (try spaces >> char ',' >> try spaces) <|> return []

infixOperation :: (Stream s m Char) => ParsecT s u m Expression
infixOperation = trace "Parsing infix operation" $ do
  exp1 <- try literal <|> try functionCall <|> try variableCall
  try spaces
  pred <- operator
  try spaces
  exp2 <- try literal <|> try functionCall <|> try variableCall
  trace "IS INFIX" $ do return $ InfixOperation pred exp1 exp2
  where operator = (try $ string ">") <|> (try $ string "<") <|> (try $ string "==") <|> (try $ string "!=") <|> (try $ string ">=") <|> (try $ string "<=")