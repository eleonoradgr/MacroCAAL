module MC_Parser where

import Ast
import Control.Applicative ((<*))
import Control.Monad
import Data.Maybe
import MC_Lexer
import System.Environment
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Expr
import Text.Parsec.Language
import Text.Parsec.String
import qualified Text.Parsec.Token as Token

program =
  do
    ast <- endBy1 statement semi
    eof
    return ast

statement =
  assignment
    <|> setDeclaration
    <|> defDeclaration

assignment =
  do
    var <- procName
    reservedOp "="
    p <- process
    return $ ProcDef var p

procName =
  do
    var <- procIdentifier
    param <- optionMaybe $braces aExpr
    if isJust param
      then return $ Param var (fromJust param)
      else return $ Cst var

process =
  summation

summation =
  do
    c <- composition
    ms <- optionMaybe sumcompose
    if isJust ms
      then return $ NonDetChoise c (fromJust ms)
      else return c

sumcompose =
  do
    reservedOp "+"
    summation

composition =
  do
    p <- actionPrefix --choice [try prefixC, try prefixA, reProcess]
    mc <- optionMaybe parcompose
    if isJust mc
      then return $ ParallelComp p (fromJust mc)
      else return p

parcompose =
  do
    reservedOp "|"
    composition

actionPrefix =
  try prefixC
    <|> try prefixA
    <|> try reProcess

prefixC =
  do
    c <- sepEndBy1 command comma
    mp <- optionMaybe prefixcompose
    if isJust mp
      then return $ PrefixP (CommandP (Concat c)) (fromJust mp)
      else return (CommandP (Concat c))

prefixA =
  do
    a <- action
    mp <- optionMaybe prefixcompose
    if isJust mp
      then return $ PrefixP (ActionP a) (fromJust mp)
      else return (ActionP a)

prefixcompose =
  do
    reservedOp "."
    actionPrefix

action =
  try output
    <|> input

output =
  do
    reservedOp "'"
    name <- identifier
    mess <- optionMaybe $ angles $ choice [identifier, string "0"]
    if isJust mess
      then return $ Output name (fromJust mess)
      else return $ Coaction name

input =
  do
    name <- identifier
    mess <- optionMaybe $ parens identifier
    if isJust mess
      then return $ Input name (fromJust mess)
      else return $ Action name

command =
  skip
    <|> ass
    <|> ifc
    <|> whilec

skip =
  do
    reserved "skip"
    return Skip

ass =
  do
    var <- identifier
    reservedOp ":="
    VarAssign var <$> aExpr

--seqc =
--do
--c1 <- command
--comma
--Concat c1 <$> command

ifc =
  do
    reserved "if"
    b <- bExpr
    reserved "then"
    t <- process
    reserved "else"
    e <- process
    return $ If b t e

whilec =
  do
    reserved "while"
    b <- bExpr
    reserved "do"
    c <- process
    return $ While b c

reProcess =
  try restriction1
    <|> try restirction2
    <|> try relabelling
    <|> try parenProcess

restriction1 =
  do
    p <- parenProcess
    reservedOp "\\"
    i <- procIdentifier
    return $ Restriction p [i]

restirction2 =
  do
    p <- parenProcess
    reservedOp "\\"
    i <- braces $ sepBy1 identifier comma
    return $ Restriction p i

relabelling =
  do
    p <- parenProcess
    i <- brackets $ sepBy1 relabel comma
    return $ Relabelling p i

relabel =
  do
    l1 <- identifier
    reservedOp "/"
    l2 <- identifier
    return (l1, l2)

parenProcess =
  parens process
    <|> constantProcess

constantProcess = nilProcess <|> procc

procc =
  do
    ProcVar <$> procName

nilProcess =
  do
    char '0'
    return Nil

setDeclaration =
  do
    reserved "set"
    var <- procIdentifier
    reserved "="
    l <- braces $ sepBy1 identifier comma
    return $ SetDef var l

defDeclaration =
  do
    reserved "def"
    var <- choice [identifier, procIdentifier]
    def <- choice [try rangeCmp, try range, try rangeBool]
    return $ VarDef var def

rangeCmp =
  do
    op <- relation
    i <- integer
    j <- optionMaybe integer
    if isJust j
      then return $ RangeCmp op i (fromJust j)
      else return $ RangeCmp op 0 i

range =
  do
    reservedOp "="
    li <- brackets $ sepBy1 integer comma
    return $ Range li

rangeBool =
  do
    reservedOp "="
    li <- brackets $ sepBy1 boolean comma
    return $ RangeBool li

bExpr = buildExpressionParser bOperators bTerm

aExpr = buildExpressionParser aOperators aTerm

rExpr = do
  a1 <- aExpr
  op <- relation
  a2 <- aExpr
  return $ CmpOp op a1 a2

relation =
  (reservedOp "<" >> return Lt)
    <|> (reservedOp ">" >> return Gt)
    <|> (reservedOp "<=" >> return Let)
    <|> (reservedOp ">=" >> return Get)
    <|> (reservedOp "==" >> return Eq)
    <|> (reservedOp "!=" >> return Neq)

aOperators =
  [ [Prefix (reservedOp "-" >> return Neg)],
    [Infix (reservedOp "*" >> return (BinOp Mul)) AssocLeft],
    [ Infix (reservedOp "+" >> return (BinOp Add)) AssocLeft,
      Infix (reservedOp "-" >> return (BinOp Sub)) AssocLeft
    ]
  ]

bOperators =
  [ [Prefix (reservedOp "not" >> return Not)],
    [ Infix (reservedOp "and" >> return (BBinOp And)) AssocLeft,
      Infix (reservedOp "&&" >> return (BBinOp And)) AssocLeft
    ],
    [ Infix (reservedOp "or" >> return (BBinOp Or)) AssocLeft,
      Infix (reservedOp "||" >> return (BBinOp Or)) AssocLeft
    ]
  ]

aTerm = parens aExpr <|> liftM Var identifier <|> liftM Var procIdentifier <|> liftM Int integer

bTerm =
  parens bExpr
    <|> boolean
    <|> try rExpr
    <|> liftM BVar identifier

boolean =
  (reservedOp "true" >> return T)
    <|> (reservedOp "false" >> return F)

parseSource code =
  case parse macroCaalParser "" code of
    Left e -> error $ show e
    Right r -> r
  where
    macroCaalParser = whiteSpace >> program

main = do
  [filename] <- getArgs
  source <- readFile filename
  let res = parseSource source
  print res