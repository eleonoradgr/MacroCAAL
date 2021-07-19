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
  { -- | List of process names
    pid :: [String],
    -- | List of set variables identifiers with associated values
    setid :: Map String [String],
    -- | List of int variables identifiers with related values
    defIid :: Map String [Integer],
    -- | List of bool variables identifiers with related values
    defBid :: Map String [Bool],
    -- | Ast of the program
    prog :: [Stmt],
    -- | Counter for generation of new process names
    counter :: Int
  }
  deriving (Show)

-- | Given rho and a variable name
--  returns the possible values for the variable
getIntVar :: Rho -> String -> [Integer]
getIntVar rho name =
  case Map.lookup name $ defIid rho of
    Just x -> x
    Nothing -> error $ "Variable " ++ name ++ " undefined"

-- | Return True if name is an int variable, False ow
isIntVarDef :: Rho -> String -> Bool
isIntVarDef rho name =
  case Map.lookup name $ defIid rho of
    Just x -> True
    Nothing -> False

-- | Given rho and a variable name
--  returns the possible values for the variable
getBoolVar :: Rho -> String -> [Bool]
getBoolVar rho name =
  case Map.lookup name $ defBid rho of
    Just x -> x
    Nothing -> error $ "Variable " ++ name ++ " undefined"

-- | Return True if name is a bool variable, False ow
isBoolVarDef :: Rho -> String -> Bool
isBoolVarDef rho name =
  case Map.lookup name $ defBid rho of
    Just x -> True
    Nothing -> False

-- | Expression evaluation, used for parametric processes
-- also the list of variable used in the evaluation is returned
-- if a variable occurs the fist of the list of admissible values is used
evalAExpr :: Rho -> AExpr -> [String] -> (Integer, [String])
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

-- | Constant arithmetic expression evaluation, used for assignment command
evalCstAExpr :: AExpr -> Integer
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

-- | Constant boolean expression evaluation, used for assignment command
evalCstBExpr :: BExpr -> Bool
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

-- | Return True if the first char is upper case, False ow
firstUpper :: [Char] -> Bool
firstUpper = isUpper . head

-- | Return True if the first char is lower case, False ow
firstLower :: [Char] -> Bool
firstLower = isLower . head

-- | Add to pid a new process name
addProcDef :: Rho -> String -> Rho
addProcDef rho procName =
  if procName `elem` pid rho
    then error $ "Process " ++ procName ++ " already defined"
    else rho {pid = pid rho ++ [procName]}

-- | Generate the silent action
tau :: Proc
tau = ActionP $ Action "tau"

-- | Generate action
act :: String -> Proc
act name = ActionP $ Action name

-- | Generate coaction
cact :: String -> Proc
cact name = ActionP $ Coaction name

-- | Generate coaction done for commands
doneCommand :: Proc
doneCommand = PrefixP (cact "done") Nil

-- | Generate a new name for processes
newname :: Show a => a -> [Char]
newname i = "PROG" ++ show i

--------------------------------------------------------------------------------------

-- | Tranform the occurrences of a parametric process into a constant one
parToCstProc :: Rho -> Proc -> Proc
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

-- | Tranform a parametric process definition into a constant one.
--   A check on the number of variables used in the parameter is performed
--   to guarantee that at most one variable is used
parToCstProcdef :: Rho -> ProcName -> Proc -> Rho
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
parToCstProcdef rho _ p = rho

-- | Generate the list of possible values for a range definitition
evalRangeCmp :: (Ord a, Num a, Enum a) => CmpOp -> a -> a -> [a]
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

-- | Generate the list of possible values for a boolean range definitition
evalRangeBool :: [BExpr] -> [Bool]
evalRangeBool bl =
  let mapB x = case x of
        T -> True
        F -> False
        _ -> evalCstBExpr x
   in Set.toList . Set.fromList $ map mapB bl

-- | Generate the non-deterministic choice for all the values that can be written in a variable
-- ProcName is the variable process name (e.g X),
--          it is needed to create the destination process (e.g X-3)
-- chanName is the variable name (e.g x) to create an action (e.g xw3)
-- li is the list of possible values that the variable can assume
writingProc :: Show a => [Char] -> [Char] -> [a] -> Proc
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

-- | Generate the processes needed to treat variables as value servers.
-- ProcName is the variable process name (e.g X),
--          it is needed to create the destination process (e.g X-3)
-- chanName is the variable name (e.g x) to create an action (e.g xw3, 'xr3)
-- li is the list of possible values that the variable can assume
-- lp is the accumulator with processes generated
-- prec is a boolean value, True if the variable has a predecessor value
--      and a dec action can be performed, False ow
-- precName process destination in case of decrement
variableProcAus :: Show a => [Char] -> [Char] -> [a] -> [Stmt] -> Bool -> [Char] -> [Stmt]
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

-- | Generate the processes needed to treat variables as value servers.
-- ProcName is the variable process name (e.g X),
--          it is needed to create the destination process (e.g X-3)
-- chanName is the variable name (e.g x) to create an action (e.g xw3, 'xr3)
-- li is the list of possible values that the variable can assume
variableProc :: Show a => [Char] -> [Char] -> [a] -> [Stmt]
variableProc procName chanName li =
  variableProcAus procName chanName li [] False ""

-- | Add to pid the names of new processes, created for a variable management
addProcDefs :: Rho -> [Stmt] -> Rho
addProcDefs rho varp =
  case varp of
    [] -> rho
    (ProcDef (Cst procName) _ : xs) -> addProcDefs (addProcDef rho procName) xs
    _ -> error "Not valid process description"

-- | Generate processes for an integer variable
varIProc :: Show a => Rho -> [Char] -> [a] -> Rho
varIProc rho name li =
  if firstLower name
    then
      let procName = map Data.Char.toUpper name
          chanName = map Data.Char.toLower name
          rho' = addProcDef rho procName
          wp = ProcDef (Cst procName) $ writingProc procName chanName li
          varp = variableProc procName chanName li
          rho'' = addProcDefs rho' varp
       in rho'' {prog = prog rho ++ [wp] ++ varp}
    else rho

-- | Generate processes for an boolean variable
varBProc :: Show a => Rho -> [Char] -> [a] -> Rho
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

-- | Function for tranlation of a parametric process and variable definition
parVarTranslate rho (ProcDef name p) =
  case name of
    Param s expr -> parToCstProcdef rho name p
    Cst s ->
      let p' = parToCstProc rho p
          rho' = addProcDef rho s
       in rho' {prog = prog rho ++ [ProcDef name p']}
parVarTranslate rho (SetDef name l) =
  if isJust $ Map.lookup name $setid rho
    then error $ "Definition " ++ name ++ " already used"
    else
      let slist' = expandRest rho l
       in rho {setid = Map.insert name l $ setid rho, prog = prog rho ++ [SetDef name slist']}
parVarTranslate rho (VarDef name v) =
  case v of
    RangeCmp co i1 i2 ->
      if isIntVarDef rho name
        then error $ "Variable " ++ name ++ " already defined"
        else
          ( let v' = evalRangeCmp co i1 i2
                rho' = rho {defIid = Map.insert name v' $ defIid rho}
             in varIProc rho' name v'
          )
    Range il ->
      if isIntVarDef rho name
        then error $ "Variable " ++ name ++ " already defined"
        else
          let rho' = rho {defIid = Map.insert name il $ defIid rho}
           in varIProc rho' name il
    RangeBool bl ->
      if isBoolVarDef rho name
        then error $ "Variable " ++ name ++ " already defined"
        else
          let v' = evalRangeBool bl
           in let rho' = rho {defBid = Map.insert name v' $ defBid rho}
               in varBProc rho' name v'

firstStep rho text =
  foldl parVarTranslate rho text

--------------------------------------------------------------------------------------

-- | Given the variable v and its value val, substitute the output if it contains the variable.
outputSubs :: String -> [Char] -> Proc -> Proc
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

-- | Given the variable v and its possible values list,
--   create a non deterministic choice for all the possible inputs,
--   and ptopagate them where the parameter par is used.
inputPropagation :: Show a => [Char] -> String -> [a] -> Proc -> Proc
inputPropagation var par list cont =
  case length list of
    1 ->
      let a = act $ var ++ show (head list)
       in PrefixP a $ outputSubs par (show (head list)) cont
    n ->
      let a = act $ var ++ show (head list)
          p' = PrefixP a $ outputSubs par (show (head list)) cont
       in NonDetChoise p' $ inputPropagation var par (tail list) cont

-- | Given the variable v and its possible values list,
--   create a non deterministic choice for all the possible outputs,
outputPropagation :: Show a => [Char] -> [a] -> Proc -> Proc
outputPropagation var list cont =
  case length list of
    1 ->
      let a = cact $ var ++ show (head list)
       in PrefixP a cont
    n ->
      let a = cact $ var ++ show (head list)
          p' = PrefixP a cont
       in NonDetChoise p' $ outputPropagation var (tail list) cont

-- | Translate input and output operation
ioFinder :: Rho -> Proc -> Proc
ioFinder rho p =
  case p of
    PrefixP (ActionP (Input var par)) p2 ->
      let listval = getIntVar rho par
          newproc = inputPropagation var par listval p2
       in ioFinder rho newproc
    PrefixP (ActionP (Output var par)) p2 ->
      let listval =
            if isIntVarDef rho par
              then getIntVar rho par
              else [read par]
          newproc = outputPropagation var listval p2
       in ioFinder rho newproc
    PrefixP p1 p2 ->
      let p1' = ioFinder rho p1
          p2' = ioFinder rho p2
       in PrefixP p1' p2'
    Restriction p slist ->
      let p' = ioFinder rho p
       in Restriction p' slist
    Relabelling p rellist ->
      let p' = ioFinder rho p
       in Relabelling p' rellist
    NonDetChoise p1 p2 ->
      let p1' = ioFinder rho p1
          p2' = ioFinder rho p2
       in NonDetChoise p1' p2'
    ParallelComp p1 p2 ->
      let p1' = ioFinder rho p1
          p2' = ioFinder rho p2
       in ParallelComp p1' p2'
    _ -> p

-- | Translate input and output actions
valuePassingAus :: Rho -> Stmt -> Rho
valuePassingAus rho (ProcDef name p) =
  let p' = ioFinder rho p
   in rho {prog = prog rho ++ [ProcDef name p']}
valuePassingAus rho (SetDef name l) =
  rho {prog = prog rho ++ [SetDef name l]}
valuePassingAus rho (VarDef name v) =
  rho {prog = prog rho ++ [VarDef name v]}

secondStep rho =
  foldl valuePassingAus (rho {prog = []}) $ prog rho

--------------------------------------------------------------------------------------

-- | Translate assignement for a integer variable
assAexpreval :: Rho -> [Char] -> AExpr -> [Char]
assAexpreval rho v e =
  let e' = evalCstAExpr e
      eRange = getIntVar rho v
      action = v ++ "w" ++ show e'
   in if e' `elem` eRange
        then action
        else error $ "invalid value for " ++ v

-- | Translate assignement for a boolean variable
assBexpreval :: Rho -> [Char] -> BExpr -> [Char]
assBexpreval rho v be =
  let be' = evalCstBExpr be
      beRange = getBoolVar rho v
      action = v ++ "w" ++ show be'
   in if be' `elem` beRange
        then action
        else error $ "invalid value for " ++ v

-- | Generate a non deterministic choice for a list of process
genNonDetChoice :: [Proc] -> Proc
genNonDetChoice proclist =
  case proclist of
    [x] -> x
    [x, y] -> NonDetChoise x y
    (x : xs) -> NonDetChoise x $ genNonDetChoice xs
    _ -> error "Impossible generation of non deterministic choice "

-- | guardgen generates while and if guard.
guardgen ::
  -- | the environment
  Rho ->
  -- | guard
  BExpr ->
  -- | the process to execute in case of a true guard
  Proc ->
  -- | the process to execute in case of a false guard
  Proc ->
  -- | the boolean value is True if the boolean expression to translate
  --   is the second element of a binary operation, and so encapsulate it in a PrefixAction.
  --   If it is the first element only the action name is needed, indeed it will be
  --   extended with futher action by the second part of the boolean expression.
  Bool ->
  ([Proc], [Proc])
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

-- | Translation of a command
concateval ::
  -- | the environment
  Rho ->
  -- | name of the process associated to the command
  [Char] ->
  -- | command to transalte
  Command ->
  -- | List of new process generated
  [[Char]] ->
  -- | Last process element to insert after the cuttent command
  Proc ->
  (Rho, [[Char]])
concateval rho nameProc c procListname lastCommand =
  case c of
    Concat c1 c2 ->
      case c1 of
        Skip ->
          let nn = newname $ counter rho
              proc = ProcDef (Cst nameProc) (PrefixP tau (ProcVar $ Cst nn))
              rho' = rho {prog = prog rho ++ [proc], counter = counter rho + 1}
           in concateval rho' nn c2 (procListname ++ [nameProc]) lastCommand
        VarIAssign v e ->
          let nn = newname $ counter rho
              action = assAexpreval rho v e
              proc = ProcDef (Cst nameProc) $ PrefixP (cact action) (ProcVar $ Cst nn)
              rho' = rho {prog = prog rho ++ [proc], counter = counter rho + 1}
           in concateval rho' nn c2 (procListname ++ [nameProc]) lastCommand
        VarBAssign v be ->
          let nn = newname $ counter rho
              action = assBexpreval rho v be
              proc = ProcDef (Cst nameProc) $ PrefixP (cact action) (ProcVar $ Cst nn)
              rho' = rho {prog = prog rho ++ [proc], counter = counter rho + 1}
           in concateval rho' nn c2 (procListname ++ [nameProc]) lastCommand
        Inc v ->
          let nn = newname $ counter rho
              incact = cact $ v ++ "Inc"
              proc = ProcDef (Cst nameProc) (PrefixP incact (ProcVar $ Cst nn))
              rho' = rho {prog = prog rho ++ [proc], counter = counter rho + 1}
           in concateval rho' nn c2 (procListname ++ [nameProc]) lastCommand
        Dec v ->
          let nn = newname $ counter rho
              decact = cact $ v ++ "Dec"
              proc = ProcDef (Cst nameProc) (PrefixP decact (ProcVar $ Cst nn))
              rho' = rho {prog = prog rho ++ [proc], counter = counter rho + 1}
           in concateval rho' nn c2 (procListname ++ [nameProc]) lastCommand
        If b cthen celse ->
          let nn = newname $ counter rho
              nn1 = newname $ counter rho + 1
              nn2 = newname $ counter rho + 2
              (tAct, fAct) = guardgen rho b (ProcVar (Cst nn1)) (ProcVar (Cst nn2)) True
              proc = ProcDef (Cst nameProc) $ genNonDetChoice $ tAct ++ fAct
              (rho1, procname1) = concateval rho {prog = prog rho ++ [proc], counter = counter rho + 3} nn1 cthen [] (ProcVar (Cst nn))
              (rho2, procname2) = concateval rho1 nn2 celse [] (ProcVar (Cst nn))
           in concateval rho2 nn c2 (procListname ++ [nameProc] ++ procname1 ++ procname2) lastCommand
        While b doc ->
          let nn1 = newname $ counter rho
              nn2 = newname $ counter rho + 1
              (tAct, fAct) = guardgen rho b (ProcVar (Cst nn1)) (ProcVar (Cst nn2)) True
              proc = ProcDef (Cst nameProc) $ genNonDetChoice $ tAct ++ fAct
              (rho1, procname1) = concateval rho {prog = prog rho ++ [proc], counter = counter rho + 3} nn1 doc [] (ProcVar (Cst nameProc))
           in concateval rho1 nn2 c2 (procListname ++ [nameProc] ++ procname1) lastCommand
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
          (rho1, proc1) = concateval rho {prog = prog rho ++ [proc], counter = counter rho + 3} nn1 cthen [] lastCommand
          (rho2, proc2) = concateval rho1 nn2 celse [] lastCommand
       in (rho2, procListname ++ [nameProc, nn1, nn2])
    While b doc ->
      let nn1 = newname $ counter rho
          nn2 = newname $ counter rho + 1
          (tAct, fAct) = guardgen rho b (ProcVar (Cst nn1)) doneCommand True
          proc = ProcDef (Cst nameProc) $ genNonDetChoice $ tAct ++ fAct
          (rho1, procname1) = concateval rho {prog = prog rho ++ [proc], counter = counter rho + 3} nn1 doc [] (ProcVar (Cst nameProc))
       in (rho1, procListname ++ [nameProc] ++ procname1)

-- | For each command c a new process with name name is created
translateCom :: Rho -> ProcName -> Command -> Rho
translateCom rho name c =
  case name of
    Cst s ->
      let (rho', procListName) = concateval rho s c [] doneCommand
       in rho'
    _ -> error "Only constant Process can be translated"

generateChannels a values acc =
  case values of
    [] -> acc ++ [a, a ++ "Inc", a ++ "Dec"]
    [x] -> generateChannels a [] $ acc ++ [a ++ "r" ++ show x] ++ [a ++ "w" ++ show x]
    (x : xs) -> generateChannels a xs $ acc ++ [a ++ "r" ++ show x] ++ [a ++ "w" ++ show x]

-- | if a variable name is includeded in the restriction
--    all channels generated for its management are included too
expandRest :: Foldable t => Rho -> t String -> [[Char]]
expandRest rho slist =
  let expandList a
        | isIntVarDef rho a =
          let values = getIntVar rho a
           in generateChannels a values []
        | isBoolVarDef rho a =
          let values = getBoolVar rho a
           in generateChannels a values []
        | otherwise = [a]
   in concatMap expandList slist

translateProcAus :: Rho -> Proc -> (Rho, Proc)
translateProcAus rho p =
  case p of
    Nil -> (rho, Nil)
    ProcVar (Cst pn) ->
      if pn `elem` pid rho
        then (rho, p)
        else error $ "Process " ++ pn ++ " not defined"
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

translateProc rho name p =
  let (rho', p') = translateProcAus rho p
   in rho' {prog = prog rho' ++ [ProcDef name p']}

-- | Valid statement for translateStmt doesn't contain parametric processes or valiable
translateStmt :: Rho -> Stmt -> Rho
translateStmt rho (ProcDef n p) = translateProc rho n p
translateStmt rho (SetDef n l) = rho {prog = prog rho ++ [SetDef n l]}
translateStmt rho _ =
  error "Invalid definition"

-- | Given the enviroment and a statement returns the enviroment
--   including the statement that doesn't contain commands and value passing action anymore.
thirdStep rho def =
  foldl translateStmt rho def

translate text =
  let rho = Rho {pid = [], setid = Map.empty, defIid = Map.empty, defBid = Map.empty, prog = [], counter = 0}
      res1 = firstStep rho text
      res2 = secondStep res1
   in thirdStep (res2 {prog = []}) $ prog res2