module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)
import Control.Applicative ((<$>))
import Data.Functor.Identity (Identity)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Lexer
import Syntax

-- Helper function for building binary operators
binary :: String -> Ex.Assoc -> Ex.Operator String () Identity Expr
binary s assoc = Ex.Infix (reservedOp s >> pure (BinaryOp s)) assoc

-- List of Operator lists. Ordered in descending presendence
-- i.e. * and / have the same priority, but + and - have lower priority
table :: Ex.OperatorTable String () Identity Expr
table = [ [binary "*" Ex.AssocLeft, binary "/" Ex.AssocLeft]
        , [binary "+" Ex.AssocLeft, binary "-" Ex.AssocLeft]
        , [binary "<" Ex.AssocLeft]
        ]

expr :: Parser Expr
expr = Ex.buildExpressionParser table factor

int :: Parser Expr
int = do
  n <- integer
  pure $ Float (fromInteger n)

floating :: Parser Expr
floating = Float <$> float

variable :: Parser Expr
variable = Var <$> identifier

function :: Parser Expr
function = do
  reserved "def"
  name <- identifier
  args <- parens $ many identifier
  body <- expr
  pure $ Function name args body

extern :: Parser Expr
extern = do
  reserved "extern"
  name <- identifier
  args <- parens $ many identifier
  pure $ Extern name args

call :: Parser Expr
call = do
  name <- identifier
  args <- parens $ commaSep expr
  pure $ Call name args

ifthen :: Parser Expr
ifthen = do
  reserved "if"
  cond <- expr
  reserved "then"
  tr <- expr
  reserved "else"
  fl <- expr
  pure $ If cond tr fl

for :: Parser Expr
for = do
  reserved "for"
  var <- identifier
  reservedOp "="
  start <- expr
  reservedOp ","
  cond <- expr
  reservedOp ","
  step <- expr
  reserved "in"
  body <- expr
  pure $ For var start cond step body

factor :: Parser Expr
factor = try floating
      <|> try int
      <|> try call
      <|> try variable
      <|> try ifthen
      <|> try for
      <|> (parens expr)

defn :: Parser Expr
defn = try extern
    <|> try function
    <|> expr

contents :: Parser a -> Parser a
contents p = do
  whiteSpace
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
