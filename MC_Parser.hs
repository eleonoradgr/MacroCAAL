module MC_Parser where

import Control.Applicative ((<*))
import Control.Monad
import Data.Maybe
import MC_Lexer
import System.Environment
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language
import Text.Parsec.String
import qualified Text.Parsec.Token as Token

------------------- AST -------------------
data BinOp
  = Sum
  | Diff
  | Mul
  deriving (Show)

data UnaryOp
  = Neg
  deriving (Show)

data CmpOp
  = Lt
  | Gt
  | Let
  | Get
  | Eq
  deriving (Show)

data BBinOp
  = And
  | Or
  deriving (Show)

data BUnaryOp
  = Not
  deriving (Show)

data Expr
  = Int Integer
  | Var String
  | BinOp BinOp Expr Expr
  | UnaryOp UnaryOp Expr
  deriving (Show)

data BExpr
  = T
  | F
  | ExCond Expr
  | CmpOp Expr Expr
  | BBinOp BBinOp BExpr BExpr
  | BUnaryOp BUnaryOp BExpr
  deriving (Show)

data Action
  = Action String
  | Coaction String
  | Input String String
  | Output String String
  deriving (Show)

data Command
  = Skip
  | VarAssign String Expr
  | Concat Command Command
  | If BExpr Proc Proc
  | While BExpr Proc Proc
  deriving (Show)

data ProcName
  = Param String Expr
  | Cst String
  deriving (Show)

data Proc
  = Nil
  | ProcVar ProcName
  | ActionPrefix Action Proc
  | CommandPrefix Command Proc
  | Restriction Proc String
  | Relabelling Proc [(String, String)]
  | NonDetChoise Proc Proc
  | ParallelComp Proc Proc
  deriving (Show)

data VarDef
  = RangeCmp CmpOp Integer Integer
  | Range [Integer]
  | RangeBool [BExpr]
  deriving (Show)

data Stmt
  = ProcDef ProcName Proc
  | SetDef String [String]
  | VarDef String VarDef
  | Seq [Stmt]
  deriving (Show)

--------------------------------------------

program =
  do
    endBy1 statement semi

statement =
  assignment
    <|> setDeclaration
    <|> defDeclaration

assignment =
  do
    var <- procIdentifier
    enum <- optionMaybe $ braces $ many integer
    if isJust enum
      then return $ ProcDef (Param var (Int 0)) Nil
      else return $ ProcDef (Cst var) Nil

paramAssignment =
  do
    var <- procIdentifier
    enum <- braces $ many integer
    return $ ProcDef (Param var (Int 0)) Nil

cstAssignment =
  do
    var <- procIdentifier
    return $ ProcDef (Cst var) Nil

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
    def <- choice [rangeCmp, range, rangeBool]
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
    li <- many integer
    return $ Range li

rangeBool =
  do
    op <- relation
    li <- many boolean
    return $ RangeBool li

boolean =
  (reservedOp "True" >> return T)
    <|> (reservedOp "False" >> return F)

relation =
  (reservedOp "<" >> return Lt)
    <|> (reservedOp ">" >> return Gt)
    <|> (reservedOp "<=" >> return Let)
    <|> (reservedOp ">=" >> return Get)
    <|> (reservedOp "=" >> return Eq)

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