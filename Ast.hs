module Ast where

data BinOp
  = Add
  | Sub
  | Mul
  deriving (Show)

data CmpOp
  = Lt
  | Gt
  | Let
  | Get
  | Eq
  | Neq
  deriving (Show)

data BBinOp
  = And
  | Or
  deriving (Show)

data Expr
  = Int Integer
  | Var String
  | Neg Expr
  | BinOp BinOp Expr Expr
  deriving (Show)

data BExpr
  = T
  | F
  | BVar String
  | Not BExpr
  | BBinOp BBinOp BExpr BExpr
  | CmpOp CmpOp Expr Expr
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
  | Concat [Command]
  | If BExpr Proc Proc
  | While BExpr Proc
  deriving (Show)

data ProcName
  = Param String Expr
  | Cst String
  deriving (Show)

data Proc
  = Nil
  | ProcVar ProcName
  | ActionP Action
  | CommandP Command
  | PrefixP Proc Proc
  | Restriction Proc [String]
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
  deriving (Show)

--------------------------------------------