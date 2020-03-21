module Lexer where

import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)

import qualified Text.Parsec.Token as Tok

-- Uses a LanguageDef record to define a TokenParser,
-- which contains the lexical parsers
lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where
    ops = ["+", "*", "-", "/", ";", ",", "<"]
    names = ["def", "extern"]
    style = emptyDef {
              Tok.commentLine = "#"
            , Tok.reservedOpNames = ops
            , Tok.reservedNames = names
            }

-- Defining token types for the lexer
integer :: Parser Integer
integer = Tok.integer lexer

float :: Parser Double
float = Tok.float lexer

identifier :: Parser String
identifier = Tok.identifier lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer

whiteSpace :: Parser ()
whiteSpace = Tok.whiteSpace lexer

commaSep :: Parser a -> Parser [a]
commaSep = Tok.commaSep lexer

semiSep :: Parser a -> Parser [a]
semiSep = Tok.semiSep lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer
