module MC_Semant where

import Ast
import Data.Char
import Data.IORef
import Data.List
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import MC_Parser

data Rho = Rho
  { pid :: [String],
    setid :: Map String [String],
    defIid :: Map String [Integer],
    defBid :: Map String [Bool],
    prog :: [Stmt],
    counter :: Int
  }
  deriving (Show)

getIntVar rho name =
  case Map.lookup name $ defIid rho of
    Just x -> x
    Nothing -> error $ "Variable " ++ name ++ " undefined"

isIntVarDef rho name =
  case Map.lookup name $ defIid rho of
    Just x -> True
    Nothing -> False

getBoolVar rho name =
  case Map.lookup name $ defBid rho of
    Just x -> x
    Nothing -> error $ "Variable " ++ name ++ " undefined"

isBoolVarDef rho name =
  case Map.lookup name $ defBid rho of
    Just x -> True
    Nothing -> False

newtype Counter = Counter {x :: IORef Int}

{- Expression evaluation for process parameters -}
evalAExpr rho (Int val) usedVar = (val, usedVar)
evalAExpr rho (Neg expr) usedVar =
  let (e, uvar) = evalAExpr rho expr usedVar
   in (negate e, uvar)
evalAExpr rho (Var name) usedVar =
  (head x, usedVar ++ [name])
  where
    x = getIntVar rho name
evalAExpr rho (BinOp op expr1 expr2) usedVar =
  let (e1, n1) = evalAExpr rho expr1 usedVar
   in let (e2, n2) = evalAExpr rho expr2 usedVar
       in let names = Set.toList . Set.fromList $ n1 ++ n2
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
evalCstBExpr (CmpOp cop expr1 expr2) =
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
evalCstBExpr _ =
  error "Non constant evaluation"

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
    then error $ "Process " ++ procName ++ " already defined"
    else rho {pid = pid rho ++ [procName]}

tau = ActionP $ Action "tau"

act name = ActionP $ Action name

cact name = ActionP $ Coaction name

doneCommand = PrefixP (cact "done") Nil

newname i = "PROG" ++ show i

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
        listval = getIntVar rho' $head nu
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
          rho' = addProcDef rho s
       in Right $ rho' {prog = prog rho ++ [ProcDef name p']}
semantCheck rho (SetDef name l) =
  if isJust $ Map.lookup name $setid rho
    then Left $ "Definition " ++ name ++ " already used"
    else
      let rho' = rho {setid = Map.insert name l $ setid rho, prog = prog rho ++ [SetDef name l]}
       in Right rho'
semantCheck rho (VarDef name v) =
  case v of
    RangeCmp co i1 i2 ->
      if isIntVarDef rho name
        then Left $ "Variable " ++ name ++ " already defined"
        else
          ( let v' = evalRangeCmp co i1 i2
                rho' = rho {defIid = Map.insert name v' $ defIid rho}
                rho'' = varIProc rho' name v'
             in Right rho''
          )
    Range il ->
      if isIntVarDef rho name
        then Left $ "Variable " ++ name ++ " already defined"
        else
          let rho' = rho {defIid = Map.insert name il $ defIid rho}
              rho'' = varIProc rho' name il
           in Right rho''
    RangeBool bl ->
      if isBoolVarDef rho name
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
          rho' = addProcDef rho procName
          wp = ProcDef (Cst procName) $ writingProc procName chanName li
          varp = variableProc procName chanName li []
          rho'' = addProcDefs rho' varp
       in --inserire nomi di processi delle variabili
          rho'' {prog = prog rho ++ [wp] ++ varp}
    else rho

addProcDefs rho varp =
  case varp of
    [] -> rho
    (ProcDef (Cst procName) _ : xs) -> addProcDefs (addProcDef rho procName) xs
    _ -> error "Not valid process description"

variableProc procName chanName li lp =
  {-case li of
    [] -> lp
    (x : xs) ->
      let procName1 = procName ++ map Data.Char.toUpper (show x)
          outputchan = chanName ++ "r" ++ map Data.Char.toLower (show x)
          procread = PrefixP (ActionP $ Coaction outputchan) (ProcVar $ Cst procName1)
          procwrite = ProcVar $ Cst procName
          procBody = NonDetChoise procread procwrite
          lp' = lp ++ [ProcDef (Cst procName1) procBody]
       in variableProc procName chanName xs lp'-}
  variableProcAus procName chanName li lp False ""

variableProcAus procName chanName li lp prec precName =
  case li of
    [] -> lp
    [x] ->
      let procName1 = procName ++ "-" ++ map Data.Char.toUpper (show x)
          outputchan = chanName ++ "r" ++ map Data.Char.toLower (show x)
          procread = PrefixP (cact outputchan) (ProcVar $ Cst procName1)
          procwrite = ProcVar $ Cst procName
          decaction =
            if prec
              then
                let actName = chanName ++ "Dec"
                    actvar = PrefixP (act actName) (ProcVar $ Cst precName)
                 in Just actvar
              else Nothing
          procBody =
            if isJust decaction
              then NonDetChoise procread $ NonDetChoise procwrite $fromJust decaction
              else NonDetChoise procread procwrite
          lp' = lp ++ [ProcDef (Cst procName1) procBody]
       in lp'
    (x : y : xs) ->
      let procName1 = procName ++ "-" ++ map Data.Char.toUpper (show x)
          outputchan = chanName ++ "r" ++ map Data.Char.toLower (show x)
          procread = PrefixP (cact outputchan) (ProcVar $ Cst procName1)
          procwrite = ProcVar $ Cst procName
          decaction =
            if prec
              then
                let actName = chanName ++ "Dec"
                    actvar = PrefixP (act actName) (ProcVar $ Cst precName)
                 in Just actvar
              else Nothing
          incaction =
            let actName = chanName ++ "Inc"
                nextName = procName ++ "-" ++ map Data.Char.toUpper (show y)
             in PrefixP (act actName) (ProcVar $ Cst nextName)
          procBody =
            if isJust decaction
              then NonDetChoise procread $ NonDetChoise procwrite $ NonDetChoise incaction $fromJust decaction
              else NonDetChoise procread $ NonDetChoise procwrite incaction
          lp' = lp ++ [ProcDef (Cst procName1) procBody]
       in variableProcAus procName chanName (y : xs) lp' True procName1

writingProc procName chanName li =
  case li of
    [x] ->
      let procDest = procName ++ "-" ++ map Data.Char.toUpper (show x)
          inputchan = chanName ++ "w" ++ map Data.Char.toLower (show x)
          procwrite = PrefixP (act inputchan) (ProcVar $ Cst procDest)
       in procwrite
    (x : xs) ->
      let procDest = procName ++ "-" ++ map Data.Char.toUpper (show x)
          inputchan = chanName ++ "w" ++ map Data.Char.toLower (show x)
          procwrite = PrefixP (act inputchan) (ProcVar $ Cst procDest)
       in NonDetChoise procwrite (writingProc procName chanName xs)
    _ -> error "e"

varBProc rho name v' =
  if firstLower name
    then
      let procName = map Data.Char.toUpper name
          chanName = map Data.Char.toLower name
       in case length v' of
            1 ->
              let procName1 = procName
                  procName2 = procName ++ map Data.Char.toUpper (show $ head v')
                  rho' = foldl addProcDef rho [procName1, procName2]
                  inputname = chanName ++ "w" ++ map Data.Char.toLower (show $ head v')
                  outputname = chanName ++ "r" ++ map Data.Char.toLower (show $ head v')
                  procBody1 = PrefixP (act inputname) (ProcVar $ Cst procName2)
                  procBody2 =
                    NonDetChoise
                      (PrefixP (cact outputname) (ProcVar $ Cst procName2))
                      (ProcVar $ Cst procName1)
                  progExt = [ProcDef (Cst procName1) procBody1, ProcDef (Cst procName2) procBody2]
               in rho' {prog = prog rho ++ progExt}
            2 ->
              let procName1 = procName
                  procName2 = procName ++ "TRUE"
                  procName3 = procName ++ "FALSE"
                  rho' = foldl addProcDef rho [procName1, procName2, procName3]
                  inputname2 = chanName ++ "wtrue"
                  inputname3 = chanName ++ "wfalse"
                  outputname2 = chanName ++ "rtrue"
                  outputname3 = chanName ++ "rfalse"
                  procBody1 =
                    NonDetChoise
                      (PrefixP (act inputname2) (ProcVar $ Cst procName2))
                      (PrefixP (act inputname3) (ProcVar $ Cst procName3))
                  procBody2 =
                    NonDetChoise
                      (PrefixP (cact outputname2) (ProcVar $ Cst procName2))
                      (ProcVar $ Cst procName1)
                  procBody3 =
                    NonDetChoise
                      (PrefixP (cact outputname3) (ProcVar $ Cst procName3))
                      (ProcVar $ Cst procName1)
                  progExt = [ProcDef (Cst procName1) procBody1, ProcDef (Cst procName2) procBody2, ProcDef (Cst procName3) procBody3]
               in rho' {prog = prog rho ++ progExt}
            _ -> error "Variable cannot be instatiated"
    else error "The first letter of a boolean variable name must be lower case."

testSeman rho def =
  case semantCheck rho def of
    Left e -> error $ show e
    Right r -> r

outputSubs var val p =
  case p of
    ActionP (Output varout par) -> if par == var then cact $ varout ++ val else p
    PrefixP p1 p2 ->
      let p1' = outputSubs var val p1
          p2' = outputSubs var val p2
       in PrefixP p1' p2'
    Restriction p slist ->
      let p' = outputSubs var val p
       in Restriction p' slist
    Relabelling p rellist ->
      let p' = outputSubs var val p
       in Relabelling p' rellist
    NonDetChoise p1 p2 ->
      let p1' = outputSubs var val p1
          p2' = outputSubs var val p2
       in NonDetChoise p1' p2'
    ParallelComp p1 p2 ->
      let p1' = outputSubs var val p1
          p2' = outputSubs var val p2
       in ParallelComp p1' p2'
    _ -> p

inputPropagation var list cont =
  case cont of
    Nothing -> case length list of
      1 -> act $ var ++ show (head list)
      n -> NonDetChoise (act $ var ++ show (head list)) (inputPropagation var (tail list) Nothing)
    Just p ->
      case length list of
        1 ->
          let a = act $ var ++ show (head list)
           in PrefixP a $ outputSubs var (show (head list)) p
        n ->
          let a = act $ var ++ show (head list)
              p' = PrefixP a $ outputSubs var (show (head list)) p
           in NonDetChoise p' $ inputPropagation var (tail list) cont

inputFinder rho p =
  case p of
    ActionP (Input var par) ->
      let listval = getIntVar rho par
       in inputPropagation par listval Nothing
    PrefixP (ActionP (Input var par)) p2 ->
      let listval = getIntVar rho par
          newproc = inputPropagation par listval $ Just p2
       in inputFinder rho newproc
    PrefixP p1 p2 ->
      let p1' = inputFinder rho p1
          p2' = inputFinder rho p2
       in PrefixP p1' p2'
    Restriction p slist ->
      let p' = inputFinder rho p
       in Restriction p' slist
    Relabelling p rellist ->
      let p' = inputFinder rho p
       in Relabelling p' rellist
    NonDetChoise p1 p2 ->
      let p1' = inputFinder rho p1
          p2' = inputFinder rho p2
       in NonDetChoise p1' p2'
    ParallelComp p1 p2 ->
      let p1' = inputFinder rho p1
          p2' = inputFinder rho p2
       in ParallelComp p1' p2'
    _ -> p

valuePassingAus rho (ProcDef name p) =
  let p' = inputFinder rho p
   in rho {prog = prog rho ++ [ProcDef name p']}
valuePassingAus rho (SetDef name l) =
  rho {prog = prog rho ++ [SetDef name l]}
valuePassingAus rho (VarDef name v) =
  rho {prog = prog rho ++ [VarDef name v]}

valuePassing rho =
  foldl valuePassingAus (rho {prog = []}) $ prog rho

assAexpreval rho v e =
  let e' = evalCstAExpr e
      eRange = getIntVar rho v
      action = v ++ "w" ++ show e'
   in if e' `elem` eRange
        then action
        else error $ "invalid value for " ++ v

assBexpreval rho v be =
  let be' = evalCstBExpr be
      beRange = getBoolVar rho v
      action = v ++ "w" ++ show be'
   in if be' `elem` beRange
        then action
        else error $ "invalid value for " ++ v

genNonDetChoice proclist =
  case proclist of
    [x] -> x
    [x, y] -> NonDetChoise x y
    (x : xs) -> NonDetChoise x $ genNonDetChoice xs
    _ -> error "Impossible generation of non deterministic choice "

guardgen rho T trueAct falseAct isSecond
  | isSecond = ([PrefixP tau trueAct], [])
  | otherwise = ([tau], [])
guardgen rho F trueAct falseAct isSecond
  | isSecond = ([], [PrefixP tau falseAct])
  | otherwise = ([], [tau])
guardgen rho (BVar v) trueAct falseAct isSecond =
  let bRange = getBoolVar rho v
      tAct =
        [act $ v ++ "rtrue" | or bRange]
      fAct =
        [act $ v ++ "rfalse" | False `elem` bRange]
   in if isSecond
        then (map (`PrefixP` trueAct) tAct, map (`PrefixP` falseAct) fAct)
        else (tAct, fAct)
guardgen rho (CmpOp Eq (Var v) e) trueAct falseAct isSecond =
  let bRange = getIntVar rho v
      e' = evalCstAExpr e
      listT = [act $ v ++ "r" ++ show e']
      falseass = List.delete e' bRange
      listF = map (\x -> act $ v ++ "r" ++ show x) falseass
   in if e' `elem` bRange
        then
          if isSecond
            then
              let listT' = [PrefixP (head listT) trueAct]
                  listF' = map (`PrefixP` falseAct) listF
               in (listT', listF')
            else (listT, listF)
        else ([], listF)
guardgen rho (Not b) trueAct falseAct isSecond =
  let (listT, listF) = guardgen rho b falseAct trueAct isSecond
   in (listF, listT)
guardgen rho (BBinOp And b1 b2) trueAct falseAct isSecond =
  let (fstT, fstF) = guardgen rho b1 trueAct falseAct False
      (sndT, sndF) = guardgen rho b2 trueAct falseAct True
      ndcTrue = genNonDetChoice sndT
      newFst = map (`PrefixP` ndcTrue) fstT
      newSnd = map (`PrefixP` falseAct) fstF
   in (newFst, sndF ++ newSnd)
guardgen rho (BBinOp Or b1 b2) trueAct falseAct isSecond =
  let (fstT, fstF) = guardgen rho b1 trueAct falseAct False
      (sndT, sndF) = guardgen rho b2 trueAct falseAct True
      ndcFalse = genNonDetChoice sndF
      newFst = map (`PrefixP` trueAct) fstT
      newSnd = map (`PrefixP` ndcFalse) fstF
   in (sndT ++ newFst, newSnd)
guardgen rho _ _ _ _ = error "Not implemented yet"

concateval rho nameProc c procListname lastCommand restrictions i =
  case c of
    Concat c1 c2 ->
      case c1 of
        Skip ->
          let nn = newname $ counter rho
              proc = ProcDef (Cst nameProc) (PrefixP tau (ProcVar $ Cst nn))
              rho' = rho {prog = prog rho ++ [proc], counter = counter rho + 1}
           in concateval rho' nn c2 (procListname ++ [nameProc]) lastCommand restrictions (i + 1)
        VarIAssign v e ->
          let nn = newname $ counter rho
              action = assAexpreval rho v e
              proc = ProcDef (Cst nameProc) $ PrefixP (cact action) (ProcVar $ Cst nn)
              rho' = rho {prog = prog rho ++ [proc], counter = counter rho + 1}
           in concateval rho' nn c2 (procListname ++ [nameProc]) lastCommand restrictions (i + 1)
        VarBAssign v be ->
          let nn = newname $ counter rho
              action = assBexpreval rho v be
              proc = ProcDef (Cst nameProc) $ PrefixP (cact action) (ProcVar $ Cst nn)
              rho' = rho {prog = prog rho ++ [proc], counter = counter rho + 1}
           in concateval rho' nn c2 (procListname ++ [nameProc]) lastCommand restrictions (i + 1)
        Inc v ->
          let nn = newname $ counter rho
              incact = cact $ v ++ "Inc"
              proc = ProcDef (Cst nameProc) (PrefixP incact (ProcVar $ Cst nn))
              rho' = rho {prog = prog rho ++ [proc], counter = counter rho + 1}
           in concateval rho' nn c2 (procListname ++ [nameProc]) lastCommand restrictions (i + 1)
        Dec v ->
          let nn = newname $ counter rho
              decact = cact $ v ++ "Dec"
              proc = ProcDef (Cst nameProc) (PrefixP decact (ProcVar $ Cst nn))
              rho' = rho {prog = prog rho ++ [proc], counter = counter rho + 1}
           in concateval rho' nn c2 (procListname ++ [nameProc]) lastCommand restrictions (i + 1)
        If b cthen celse ->
          let nn = newname $ counter rho
              nn1 = newname $ counter rho + 1
              nn2 = newname $ counter rho + 2
              (tAct, fAct) = guardgen rho b (ProcVar (Cst nn1)) (ProcVar (Cst nn2)) True
              proc = ProcDef (Cst nameProc) $ genNonDetChoice $ tAct ++ fAct
              (rho1, procname1) = concateval rho {prog = prog rho ++ [proc], counter = counter rho + 3} nn1 cthen [] (ProcVar (Cst nn)) [] (i + 3)
              (rho2, procname2) = concateval rho1 nn2 celse [] (ProcVar (Cst nn)) [] (i + 4)
           in concateval rho2 nn c2 (procListname ++ [nameProc] ++ procname1 ++ procname2) lastCommand restrictions (i + 3)
        While b doc ->
          let nn1 = newname $ counter rho
              nn2 = newname $ counter rho + 1
              (tAct, fAct) = guardgen rho b (ProcVar (Cst nn1)) (ProcVar (Cst nn2)) True
              proc = ProcDef (Cst nameProc) $ genNonDetChoice $ tAct ++ fAct
              (rho1, procname1) = concateval rho {prog = prog rho ++ [proc], counter = counter rho + 3} nn1 doc [] (ProcVar (Cst nameProc)) [] (i + 3)
           in concateval rho1 nn2 c2 (procListname ++ [nameProc] ++ procname1) lastCommand restrictions (i + 2)
        _ -> error "Not implemented yet"
    Skip -> (rho {prog = prog rho ++ [ProcDef (Cst nameProc) $ PrefixP tau lastCommand]}, procListname ++ [nameProc])
    VarIAssign v e ->
      let action = assAexpreval rho v e
       in (rho {prog = prog rho ++ [ProcDef (Cst nameProc) $ PrefixP (cact action) lastCommand]}, procListname ++ [nameProc])
    VarBAssign v be ->
      let be' = evalCstBExpr be
          beRange = getBoolVar rho v
          action = v ++ "w" ++ show be'
       in if be' `elem` beRange
            then (rho {prog = prog rho ++ [ProcDef (Cst nameProc) $ PrefixP (cact action) lastCommand]}, procListname ++ [nameProc])
            else error $ "invalid value for " ++ v
    Inc v ->
      let incact = cact $ v ++ "Inc"
       in (rho {prog = prog rho ++ [ProcDef (Cst nameProc) $ PrefixP incact lastCommand]}, procListname ++ [nameProc])
    Dec v ->
      let decact = cact $ v ++ "Dec"
       in (rho {prog = prog rho ++ [ProcDef (Cst nameProc) $ PrefixP decact lastCommand]}, procListname ++ [nameProc])
    If b cthen celse ->
      let nn = newname $ counter rho
          nn1 = newname $ counter rho + 1
          nn2 = newname $ counter rho + 2
          (tAct, fAct) = guardgen rho b (ProcVar (Cst nn1)) (ProcVar (Cst nn2)) True
          proc = ProcDef (Cst nameProc) $ genNonDetChoice $ tAct ++ fAct
          (rho1, proc1) = concateval rho {prog = prog rho ++ [proc], counter = counter rho + 3} nn1 cthen [] lastCommand [] (i + 3)
          (rho2, proc2) = concateval rho1 nn2 celse [] lastCommand [] (i + 4) --translateCom rho (Cst nn2) celse
       in (rho2, procListname ++ [nameProc, nn1, nn2])
    While b doc ->
      let nn1 = newname $ counter rho
          nn2 = newname $ counter rho + 1
          (tAct, fAct) = guardgen rho b (ProcVar (Cst nn1)) doneCommand True
          proc = ProcDef (Cst nameProc) $ genNonDetChoice $ tAct ++ fAct
          (rho1, procname1) = concateval rho {prog = prog rho ++ [proc], counter = counter rho + 3} nn1 doc [] (ProcVar (Cst nameProc)) [] (i + 3)
       in (rho1, procListname ++ [nameProc] ++ procname1)

translateCom rho name c =
  case name of
    Cst s ->
      let (rho', procListName) = concateval rho s c [] doneCommand [] 0
       in rho'
    _ -> error "Only constant Process can be translated"

generateChannels a values acc =
  case values of
    [] -> acc ++ [a, a ++ "Inc", a ++ "Dec"]
    [x] -> generateChannels a [] $ acc ++ [a ++ "r" ++ (show x)] ++ [a ++ "w" ++ (show x)]
    (x : xs) -> generateChannels a xs $ acc ++ [a ++ "r" ++ (show x)] ++ [a ++ "w" ++ (show x)]

expandRest rho slist =
  let expandList a
        | isIntVarDef rho a =
          let values = getIntVar rho a
           in generateChannels a values []
        | isBoolVarDef rho a =
          let values = getBoolVar rho a
           in generateChannels a values []
        | otherwise = [a]
   in concat $ map expandList slist

translateProcAus rho p =
  case p of
    Nil -> (rho, Nil)
    ProcVar (Cst pn) ->
      if pn `elem` pid rho
        then (rho, p)
        else error $ "Process " ++ pn ++ " not defined"
    --ActionP a -> (rho, ActionP a)
    CommandP c ->
      let nn = newname $ counter rho
          rho' = translateCom rho {counter = counter rho + 1} (Cst nn) c
       in (rho', ProcVar $ Cst nn)
    PrefixP (ActionP a) p2 ->
      let (rho', proc1) = (rho, ActionP a)
          (rho'', proc2) = translateProcAus rho' p2
       in (rho'', PrefixP proc1 proc2)
    PrefixP p1 p2 ->
      let (rho', proc1) = translateProcAus rho p1
          (rho'', proc2) = translateProcAus rho' p2
       in (rho'', PrefixP proc1 proc2)
    Restriction p1 slist ->
      let (rho', proc1) = translateProcAus rho p1
          slist' = expandRest rho slist
       in (rho', Restriction proc1 slist')
    Relabelling p1 slist ->
      let (rho', proc1) = translateProcAus rho p1
       in (rho', Relabelling proc1 slist)
    NonDetChoise p1 p2 ->
      let (rho', proc1) = translateProcAus rho p1
          (rho'', proc2) = translateProcAus rho' p2
       in (rho'', NonDetChoise proc1 proc2)
    ParallelComp p1 p2 ->
      let (rho', proc1) = translateProcAus rho p1
          (rho'', proc2) = translateProcAus rho' p2
       in (rho'', ParallelComp proc1 proc2)
    _ -> error "Invalid semantic, process must end with nil/0 or process variable"

{-translateProc rho name p =
  case p of
    -- TODO: management of input and output
    ActionP a -> rho {prog = prog rho ++ [ProcDef name $ ActionP a]}
    CommandP c -> translateCom rho name c
    _ -> rho {prog = prog rho ++ [ProcDef name p]}-}
translateProc rho name p =
  let (rho', p') = translateProcAus rho p
   in rho' {prog = prog rho' ++ [ProcDef name p']}

translateStmt rho (ProcDef n p) =
  let rho' = translateProc rho n p
   in Right rho'
translateStmt rho (SetDef n l) =
  let rho' = rho {prog = prog rho ++ [SetDef n l]}
   in Right rho'
translateStmt rho _ =
  Left "Invalid definition"

translate rho def =
  case translateStmt rho def of
    Left e -> error $ show e
    Right r -> r
