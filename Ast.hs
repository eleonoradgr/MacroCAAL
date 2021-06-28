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

data AExpr
  = Int Integer
  | Var String
  | Neg AExpr
  | BinOp BinOp AExpr AExpr
  deriving (Show)

data BExpr
  = T
  | F
  | BVar String
  | Not BExpr
  | BBinOp BBinOp BExpr BExpr
  | CmpOp CmpOp AExpr AExpr
  deriving (Show)

data Action
  = Action String
  | Coaction String
  | Input String String
  | Output String String
  deriving (Show)

data Command
  = Skip
  | VarIAssign String AExpr
  | VarBAssign String BExpr
  | Inc String
  | Dec String
  | Concat Command Command
  | If BExpr Command Command
  | While BExpr Command
  deriving (Show)

data ProcName
  = Param String AExpr
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