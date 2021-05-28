module MC_Semant where

import Ast
import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import MC_Parser
import System.Environment

data Rho = Rho
  { pid :: [String],
    setid :: Map String [String],
    defIid :: Map String [Integer],
    defBid :: Map String [Bool],
    prog :: [Stmt]
  }
  deriving (Show)

data IType
  = IInt Integer
  | IBool Bool
  | IUndef

instance Show IType where
  show (IInt i) = show i
  show (IBool b) = show b
  show IUndef = "undef"

{- Expression evaluation for process parameters -}
evalAExpr rho (Int val) usedVar = (val, usedVar)
evalAExpr rho (Neg expr) usedVar =
  let (e, uvar) = evalAExpr rho expr usedVar
   in (negate e, uvar)
evalAExpr rho (Var name) usedVar =
  case Map.lookup name $ defIid rho of
    Just x -> (head x, usedVar ++ [name])
    Nothing -> error $ "Variable " ++ name ++ " undefined"
evalAExpr rho (BinOp op expr1 expr2) usedVar =
  let (e1, n1) = evalAExpr rho expr1 usedVar
   in let (e2, n2) = evalAExpr rho expr2 usedVar
       in let names = Set.toList . Set.fromList $ (n1 ++ n2)
           in case op of
                Add -> (e1 + e2, names)
                Sub -> (e1 - e2, names)
                Mul -> (e1 * e2, names)

evalCstAExpr (Int val) = val
evalCstAExpr (Neg expr) = negate e
  where
    e = evalCstAExpr expr
evalCstAExpr (BinOp op expr1 expr2) =
  case op of
    Add -> e1 + e2
    Sub -> e1 - e2
    Mul -> e1 * e2
  where
    e1 = evalCstAExpr expr1
    e2 = evalCstAExpr expr2
evalCstAExpr _ =
  error "Non constant evaluation"

evalCstBExpr T = True
evalCstBExpr F = False
evalCstBExpr (Not bexpr) =
  not b
  where
    b = evalCstBExpr bexpr
evalCstBExpr (BBinOp bop bexpr1 bexpr2) =
  case bop of
    And -> e1 && e2
    Or -> e1 || e2
  where
    e1 = evalCstBExpr bexpr1
    e2 = evalCstBExpr bexpr2

evalCstBexpr (CmpOp cop expr1 expr2) =
  case cop of
    Lt -> e1 < e2
    Gt -> e1 > e2
    Let -> e1 <= e2
    Get -> e1 >= e2
    Eq -> e1 == e2
    Neq -> e1 /= e2
  where
    e1 = evalCstAExpr expr1
    e2 = evalCstAExpr expr2

{-evalBExpr rho T usedVar = (True, usedVar)
evalBExpr rho F usedVar = (False, usedVar)
evalBExpr rho (Not expr) usedVar = let (b,n) = evalBExpr scope expr in
                                    (not b, n)
evalBExpr rho (BVar var) =
    case Map.lookup name $ defBid rho of
        Just x    -> (head x, uvar ++ [name])
        Nothing   -> error $ "Variable " ++ var ++ " undefined"
evalBExpr rho (BBinOp op exp1 exp2) =
    let (e1,n1) = evalAExpr rho exp1 in
    let (e2,n2) = evalAExpr rho exp2 in
    let names = Set.toList . Set.fromList (n1 ++ n2) in
      case op of
          Add       -> (e1 + e2, names)
          Sub       -> (e1 - e2, names)
          Mul       -> (e1 * e2, names)
    case op of
        And -> evalBExpr scope exp1 && evalBExpr scope exp2
        Or  -> evalBExpr scope exp1 || evalBExpr scope exp2
evalBExpr rho (CmpOp op exp1 exp2) =
    case op of
        Less    -> evalAExpr scope exp1 <  evalAExpr scope exp2
        Greater -> evalAExpr scope exp1 >  evalAExpr scope exp2
        Equal   -> evalAExpr scope exp1 == evalAExpr scope exp2-}

firstUpper = isUpper . head

firstLower = isLower . head

addProcDef rho procName =
  if procName `elem` pid rho
    then error $ "Process " ++ procName ++ " already defined defined"
    else rho {pid = pid rho ++ [procName]}

{-parToCstCom rho c =
  case c of
    Concat c1 c2 ->
      let clist' = map (parToCstCom rho) clist
       in Concat clist'
    If b p1 p2 ->
      let p1' = parToCstProc rho p1
          p2' = parToCstProc rho p2
       in If b p1' p2'
    While b p ->
      let p' = parToCstProc rho p
       in While b p'
    _ -> c -}

parToCstProc rho p =
  case p of
    ProcVar (Param name expr) ->
      let (ex, _) = evalAExpr rho expr []
          name' = name ++ show ex
       in ProcVar (Cst name')
    PrefixP p1 p2 ->
      let p1' = parToCstProc rho p1
          p2' = parToCstProc rho p2
       in PrefixP p1' p2'
    Restriction p1 l ->
      let p1' = parToCstProc rho p1
       in Restriction p1' l
    Relabelling p1 l ->
      let p1' = parToCstProc rho p1
       in Relabelling p1' l
    NonDetChoise p1 p2 ->
      let p1' = parToCstProc rho p1
          p2' = parToCstProc rho p2
       in NonDetChoise p1' p2'
    ParallelComp p1 p2 ->
      let p1' = parToCstProc rho p1
          p2' = parToCstProc rho p2
       in ParallelComp p1' p2'
    _ -> p

parToCstProcdef rho (Param name expr) p
  | length nu > 1 = error "Only one parameter can appear in process definition"
  | null nu =
    let procName = name ++ show ex
        rho' = addProcDef rho procName
        p' = parToCstProc rho' p
        pd' = ProcDef (Cst procName) p'
     in rho' {prog = prog rho' ++ [pd']}
  | length nu == 1 =
    let procName = name ++ show ex
        rho' = addProcDef rho procName
        p' = parToCstProc rho' p
        pd' = ProcDef (Cst procName) p'
        prog' = prog rho' ++ [pd']
        listval = fromJust $ Map.lookup (head nu) $ defIid rho'
     in if length listval > 1
          then
            let rho'' = rho' {defIid = Map.insert (head nu) (tail listval) (defIid rho'), prog = prog'}
             in parToCstProcdef rho'' (Param name expr) p
          else rho' {prog = prog'}
  where
    (ex, nu) = evalAExpr rho expr []

evalRangeCmp co i1 i2 =
  case co of
    Lt ->
      ( if i1 < i2
          then [i1 .. i2 - 1]
          else error "Invalid parameters for < range definition"
      )
    Let ->
      ( if i1 <= i2
          then [i1 .. i2]
          else error "Invalid parameters for <= range definition"
      )
    Gt -> error "Invalid relation operand for definition"
    Get -> error "Invalid relation operand for definition"
    Eq -> error "Invalid relation operand for definition"
    Neq -> error "Invalid relation operand for definition"

evalRangeBool bl =
  let mapB x = case x of
        T -> True
        F -> False
   in Set.toList . Set.fromList $ map mapB bl

semantCheck rho (ProcDef name p) =
  case name of
    Param s expr ->
      let rho' = parToCstProcdef rho name p
       in Right rho'
    Cst s ->
      let p' = parToCstProc rho p
       in Right $ rho {prog = prog rho ++ [ProcDef name p']}
semantCheck rho (SetDef name l) =
  if isJust $ Map.lookup name $setid rho
    then Left $ "Definition " ++ name ++ " already used"
    else
      let rho' = rho {setid = Map.insert name l $ setid rho, prog = prog rho ++ [SetDef name l]}
       in Right rho'
semantCheck rho (VarDef name v) =
  case v of
    RangeCmp co i1 i2 ->
      if isJust $ Map.lookup name $ defIid rho
        then Left $ "Variable " ++ name ++ " already defined"
        else
          ( let v' = evalRangeCmp co i1 i2
                rho' = rho {defIid = Map.insert name v' $ defIid rho}
             in Right rho'
          )
    Range il ->
      if isJust $ Map.lookup name $ defIid rho
        then Left $ "Variable " ++ name ++ " already defined"
        else
          let rho' = rho {defIid = Map.insert name il $ defIid rho}
              rho'' = varIProc rho' name il
           in Right rho''
    RangeBool bl ->
      if isJust $ Map.lookup name $ defBid rho
        then Left $ "Variable " ++ name ++ " already defined"
        else
          let v' = evalRangeBool bl
           in let rho' = rho {defBid = Map.insert name v' $ defBid rho}
                  rho'' = varBProc rho' name v'
               in Right rho''

varIProc rho name li =
  if firstLower name
    then
      let procName = map Data.Char.toUpper name
          chanName = map Data.Char.toLower name
          wp = ProcDef (Cst procName) $ writingProc procName chanName li
          varp = variableProc procName chanName li []
       in rho {prog = prog rho ++ [wp] ++ varp}
    else rho

variableProc procName chanName li lp =
  case li of
    [] -> lp
    (x : xs) ->
      let procName1 = procName ++ map Data.Char.toUpper (show x)
          outputchan = chanName ++ "r" ++ map Data.Char.toLower (show x)
          procread = PrefixP (ActionP $ Coaction outputchan) (ProcVar $ Cst procName1)
          procwrite = ProcVar $ Cst procName
          procBody = NonDetChoise procread procwrite
          lp' = lp ++ [ProcDef (Cst procName1) procBody]
       in variableProc procName chanName xs lp'

writingProc procName chanName li =
  case li of
    [x] ->
      let procDest = procName ++ map Data.Char.toUpper (show x)
          inputchan = chanName ++ "w" ++ map Data.Char.toLower (show x)
          procwrite = PrefixP (ActionP $ Action inputchan) (ProcVar $ Cst procDest)
       in procwrite
    (x : xs) ->
      let procDest = procName ++ map Data.Char.toUpper (show x)
          inputchan = chanName ++ "w" ++ map Data.Char.toLower (show x)
          procwrite = PrefixP (ActionP $ Action inputchan) (ProcVar $ Cst procDest)
       in NonDetChoise procwrite (writingProc procName chanName xs)

varBProc rho name v' =
  if firstLower name
    then
      let procName = map Data.Char.toUpper name
          chanName = map Data.Char.toLower name
       in case length v' of
            1 ->
              let procName1 = procName ++ "W"
                  procName2 = procName ++ map Data.Char.toUpper (show $ head v')
                  inputname = chanName ++ "w" ++ map Data.Char.toLower (show $ head v')
                  outputname = chanName ++ "r" ++ map Data.Char.toLower (show $ head v')
                  procBody1 = PrefixP (ActionP $ Action inputname) (ProcVar $ Cst procName2)
                  procBody2 =
                    NonDetChoise
                      (PrefixP (ActionP $ Coaction outputname) (ProcVar $ Cst procName2))
                      (ProcVar $ Cst procName1)
                  progExt = [ProcDef (Cst procName1) procBody1, ProcDef (Cst procName2) procBody2]
               in rho {pid = pid rho ++ [procName1, procName2], prog = prog rho ++ progExt}
            2 ->
              let procName1 = procName ++ "W"
                  procName2 = procName ++ "TRUE"
                  procName3 = procName ++ "FALSE"
                  inputname2 = chanName ++ "wtrue"
                  inputname3 = chanName ++ "wfalse"
                  outputname2 = chanName ++ "rtrue"
                  outputname3 = chanName ++ "rfalse"
                  procBody1 =
                    NonDetChoise
                      (PrefixP (ActionP $ Action inputname2) (ProcVar $ Cst procName2))
                      (PrefixP (ActionP $ Action inputname3) (ProcVar $ Cst procName3))
                  procBody2 =
                    NonDetChoise
                      (PrefixP (ActionP $ Coaction outputname2) (ProcVar $ Cst procName2))
                      (ProcVar $ Cst procName1)
                  procBody3 =
                    NonDetChoise
                      (PrefixP (ActionP $ Coaction outputname3) (ProcVar $ Cst procName3))
                      (ProcVar $ Cst procName1)
                  progExt = [ProcDef (Cst procName1) procBody1, ProcDef (Cst procName2) procBody2, ProcDef (Cst procName2) procBody3]
               in rho {pid = pid rho ++ [procName1, procName2, procName3], prog = prog rho ++ progExt}
            _ -> error "Variable cannot be instatiated"
    else error "The first letter of a boolean variable name must be lower case."

testSeman rho def =
  case semantCheck rho def of
    Left e -> error $ show e
    Right r -> r

transGuardTrue rho T c = c
transGuardTrue rho F c = ActionP $ Action "tau"
transGuardTrue rho (BVar v) c =
  let bRange = fromJust $ Map.lookup v $ defBid rho
      action = v ++ "rtrue"
   in if True `elem` bRange
        then PrefixP (ActionP $ Coaction action) c
        else ActionP $ Action "tau"
transGuardTrue rho (Not b) c =
  transGuardFalse rho b c
transGuardTrue rho (BBinOp And b1 b2) c =
  PrefixP
    (transGuardTrue rho b1 (ActionP $ Action "tau"))
    (transGuardTrue rho b2 c)
transGuardTrue rho (BBinOp Or b1 b2) c =
  ParallelComp (transGuardTrue rho b1 c) (transGuardTrue rho b2 c)

transGuardFalse rho T c = ActionP $ Action "tau"
transGuardFalse rho F c = c
transGuardFalse rho (BVar v) c =
  let bRange = fromJust $ Map.lookup v $ defBid rho
      action = v ++ "rfalse"
   in if False `elem` bRange
        then PrefixP (ActionP $ Action action) c
        else ActionP $ Action "tau"
transGuardFalse rho (Not b) c =
  transGuardTrue rho b c
transGuardFalse rho (BBinOp And b1 b2) c =
  ParallelComp (transGuardFalse rho b1 c) (transGuardFalse rho b2 c)
transGuardFalse rho (BBinOp Or b1 b2) c =
  PrefixP
    (transGuardFalse rho b1 (ActionP $ Action "tau"))
    (transGuardFalse rho b2 c)

translateCom rho c =
  case c of
    Skip -> ActionP $ Coaction "done"
    VarIAssign v e ->
      let e' = evalCstAExpr e
          eRange = fromJust $ Map.lookup v $ defIid rho
          action = v ++ "w" ++ show e'
       in if e' `elem` eRange
            then ActionP $ Coaction action
            else error $ "invalid value for " ++ v
    VarBAssign v be ->
      let be' = evalCstBExpr be
          beRange = fromJust $ Map.lookup v $ defBid rho
          action = v ++ "w" ++ show be'
       in if be' `elem` beRange
            then ActionP $ Coaction action
            else error $ "invalid value for " ++ v
    Concat c1 c2 ->
      Restriction (ParallelComp p1 p2) ["seq"]
      where
        p1 = PrefixP (translateCom rho c1) (ActionP $ Action "seq")
        p2 = PrefixP (ActionP $ Coaction "seq") (translateCom rho c2)
    If b c1 c2 ->
      NonDetChoise p1 p2
      where
        ct1 = translateCom rho c1
        ct2 = translateCom rho c2
        p1 = transGuardTrue rho b ct1
        p2 = transGuardFalse rho b ct2

{- Concat x:y:xs ->
   let channel = "done" + show $ incrCounter 1 counter
   let
   in
     counter = newCounter 0 -}

translateProc rho p =
  case p of
    ActionP a -> ActionP a
    CommandP c -> translateCom rho c
    _ -> p

translateStmt rho (ProcDef n p) =
  let p' = translateProc rho p
      rho' = rho {prog = prog rho ++ [ProcDef n p']}
   in Right rho'
translateStmt rho _ =
  Left "Invalid definition"

translate rho def =
  case translateStmt rho def of
    Left e -> error $ show e
    Right r -> r

main = do
  [filename] <- getArgs
  source <- readFile filename
  let res = parseSource source
  let rho = Rho {pid = [], setid = Map.empty, defIid = Map.empty, defBid = Map.empty, prog = []}
  let res1 = foldl testSeman rho res
  let res2 = foldl translate (res1 {prog = []}) $ prog res1
  print res
  print res1