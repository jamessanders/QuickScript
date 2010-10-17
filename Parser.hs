{-# LANGUAGE FlexibleContexts, CPP #-}
module Parser where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char
import Control.Monad.Identity
import Control.Monad.Trans
import Debug.Trace

#define PARSER(X) = (Stream s m Char) => ParsecT s u m X

--parseQS = parse expression "" 

type Name = String

data Expression = Assignment Name Expression
                | VariableCall Name
                | FunctionCall Name [Expression]
                | FunctionDefinition [Name] [Expression]
                | NumberLiteral Integer
                | Stringliteral String
                | EmptyLine
                | WhiteSpace
                  deriving (Show, Read, Eq)

language :: (Stream s m Char) => ParsecT s u m [Expression]
language = fmap removeWhiteSpace $ manyTill topLevelExpression (try eof)

topLevelExpression :: (Stream s m Char) => ParsecT s u m Expression
topLevelExpression = trace "Parsing top level expression" $ do
  exp <- expression <?> "Top Level Expression"
  optional $ try endOfExpression
  return exp

endOfExpression :: (Stream s m Char) => ParsecT s u m Char
endOfExpression = try (char ';') <|> try newline 

expression :: (Stream s m Char) => ParsecT s u m Expression
expression = trace "Parsing expression" $ do
  try whitespace <|> try functionDefinition <|> try assignment <|> try functionCall <|> try variableCall 
  
whitespace :: (Stream s m Char) => ParsecT s u m Expression
whitespace = trace "Parsing whitespace" $ (try space <|> try newline)  >> return WhiteSpace

getName :: (Stream s m Char) => ParsecT s u m Name
getName = trace "Parsing name" $ do many letter

assignment :: (Stream s m Char) => ParsecT s u m Expression
assignment = trace "Parsing assignment" $ do 
  varibleName <- getName
  try spaces
  oneOf "="
  try spaces
  value <- try literal <|> try expression 
  return $ Assignment varibleName value

literal :: (Stream s m Char) => ParsecT s u m Expression
literal = trace "Parsing literal" $ do
  try numberLiteral <|> try stringLiteral 

numberLiteral :: (Stream s m Char) => ParsecT s u m Expression
numberLiteral = trace "Parsing number literal" $ do 
  n <- many1 (try digit)
  return $ NumberLiteral (read n)

stringLiteral :: (Stream s m Char) => ParsecT s u m Expression
stringLiteral = trace "Parsing string literal" $ do 
  char '"' <|> char '\''
  str <- manyTill anyChar ((try $ char '"') <|> (try $ char '\''))
  return $ Stringliteral str

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
                 args <- try ((literal <|> expression) `sepBy` (try spaces >> char ',' >> try spaces)) <|> return []
                 try spaces 
                 char ')'
                 return $ FunctionCall name args
                 

functionDefinition :: (Stream s m Char) => ParsecT s u m Expression
functionDefinition = trace "Parsing function definition" $ do
  string "function"
  try spaces
  char '('
  params <- parameters <?> "Function parameters"
  try spaces
  char ')'
  try spaces 
  char '{'
  exps <- (fmap removeWhiteSpace $ manyTill topLevelExpression (char '}')) <?> "Function body"
  return $ FunctionDefinition params exps

removeWhiteSpace = filter (/= WhiteSpace) 

parameters :: (Stream s m Char) => ParsecT s u m [Name]
parameters = sepBy getName (try spaces >> char ',' >> try spaces) <|> return []

