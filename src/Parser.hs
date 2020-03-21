module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)
import Data.Functor.Identity (Identity)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Lexer
import Syntax

-- Helper function for building binary operators
binary :: String -> Op -> Ex.Assoc -> Ex.Operator String () Identity Expr
binary s f assoc = Ex.Infix (reservedOp s >> pure (BinOp f)) assoc

-- List of Operator lists. Ordered in descending presendence
-- i.e. * and / have the same priority, but + and - have lower priority
table :: Ex.OperatorTable String () Identity Expr
table = [ [binary "*" Times Ex.AssocLeft, binary "/" Divide Ex.AssocLeft]
        , [binary "+" Plus Ex.AssocLeft, binary "-" Minus Ex.AssocLeft]
        ]

int :: Parser Expr
int = do
  n <- integer
  pure $ Float (fromInteger n)

floating :: Parser Expr
floating = do
  n <- float
  pure $ Float n

variable :: Parser Expr
variable = do
  var <- identifier
  pure $ Var var

function :: Parser Expr
function = do
  reserved "def"
  name <- identifier
  args <- parens $ many variable
  body <- expr
  pure $ Function name args body

extern :: Parser Expr
extern = do
  reserved "extern"
  name <- identifier
  args <- parens $ many variable
  pure $ Extern name args

call :: Parser Expr
call = do
  name <- identifier
  args <- parens $ commaSep expr
  pure $ Call name args

factor :: Parser Expr
factor = try floating
      <|> try int
      <|> try extern
      <|> try function
      <|> try call
      <|> variable
      <|> parens expr

defn :: Parser Expr
defn = try extern
    <|> try function
    <|> expr

expr :: Parser Expr
expr = Ex.buildExpressionParser table factor

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  pure r

toplevel :: Parser [Expr]
toplevel = many $ do
    def <- defn
    reservedOp ";"
    pure def

parseExpr :: String -> Either ParseError Expr
parseExpr s = parse (contents expr) "<stdin>" s

parseToplevel :: String -> Either ParseError [Expr]
parseToplevel s = parse (contents toplevel) "<stdin>" s
