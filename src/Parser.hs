module Parser (parseProgram) where

import Text.Parsec
import Text.Parsec.Language
import Text.Parsec.Token

import AST

setPos :: SourcePos -> Expr SourcePos -> Expr SourcePos
setPos = setData

lang :: LanguageDef st
lang = emptyDef
  { commentStart    = "(*"
  , commentEnd      = "*)"
  , nestedComments  = True
  , reservedNames   =
    [ "\\", "in", "input", "_", "/"
    -- "unit", "fst", "snd"
    ]
  , reservedOpNames =
    [ "."
    ]
  }

tokenParser :: TokenParser st
tokenParser = makeTokenParser lang

ws = whiteSpace tokenParser

keyword :: String -> Parser ()
keyword = reserved tokenParser

kwIn    = keyword "in"
kwInput = keyword "input"
kwLam   = keyword "\\"
kwCFn   = keyword "/"
kwCVl   = keyword "_"
-- kwFst   = keyword "fst"
-- kwSnd   = keyword "snd"
-- kwUnit  = keyword "unit"

op :: String -> Parser ()
op = reservedOp tokenParser

opDot = op "."

ident :: Parser String
ident = identifier tokenParser

type Parser = Parsec String ()

program :: Parser (Program SourcePos)
program = ws *>
  -- (Program <$> programPreamble <*> expr <* eof)
  (Program <$> expr <* eof)

-- programPreamble :: Parser [(Var,SourcePos)]
-- programPreamble = choice
--   [ (kwInput *> many1 preambleVar <* kwIn)
--   , return ([])
--   ] <?> "program preamble"

preambleVar :: Parser (Var,SourcePos)
preambleVar =
  (\ pos x -> (x,pos)) <$> getPosition <*> ident

-- ===========================================================


appExpr :: Parser (Expr SourcePos)
appExpr = buildApp <$> getPosition <*> simpleExpr <*> many simpleExpr
  where
    buildApp pos = foldl (EApp pos)


expr :: Parser (Expr SourcePos)
expr = 
  choice
  [ 
    try(appExpr)
  , try(simpleExpr)
  ] <?> "expression"

simpleExpr :: Parser (Expr SourcePos)
simpleExpr = 
  choice
  [ 
    lamExpr
  , parExpr
  , cfunExpr
  , cvarExpr
  , varExpr
  ] <?> "expression"


lamExpr :: Parser (Expr SourcePos)
lamExpr =
  ELam <$> getPosition
    <*> (kwLam *> ident)
    <*> (opDot *> expr)

parExpr :: Parser (Expr SourcePos)
parExpr = setPos <$> getPosition <*> parens tokenParser expr

cfunExpr :: Parser (Expr SourcePos)
cfunExpr = ECFn <$> getPosition <*> (kwCFn *> ident) <*> expr

cvarExpr :: Parser (Expr SourcePos)
cvarExpr =
  ECVl <$> getPosition <*> (kwCVl *> ident)

varExpr :: Parser (Expr SourcePos)
varExpr =
  EVar <$> getPosition <*> ident


    
-- Main function
parseProgram :: SourceName -> String -> Either ParseError (Program SourcePos)
parseProgram = parse program
