module MC_writeast where

import Ast
import Data.List ()
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import MC_Parser
import MC_Semant
import System.Environment
import System.FilePath

procToStingAus :: Proc -> [a] -> Bool -> [Char]
procToStingAus p s paren =
  case p of
    Nil ->
      if paren then "(0) " else "0 "
    ProcVar (Cst pn) ->
      if paren then "( " ++ pn ++ " ) " else pn ++ " "
    ActionP (Action a) ->
      if paren then "( " ++ a ++ " ) " else a ++ " "
    ActionP (Coaction c) ->
      if paren then "( '" ++ c ++ " ) " else "'" ++ c ++ " "
    ActionP (Output var par) ->
      if paren then "( '" ++ var ++ par ++ " ) " else "'" ++ var ++ par ++ " "
    PrefixP p1 p2 ->
      let s1 = procToStingAus p1 [] False
       in case p2 of
            NonDetChoise _ _ ->
              let s2 = procToStingAus p2 [] True
                  sres = s1 ++ "." ++ s2
               in if paren then "( " ++ sres ++ " )" else sres
            ParallelComp _ _ ->
              let s2 = procToStingAus p2 [] True
                  sres = s1 ++ "." ++ s2
               in if paren then "( " ++ sres ++ " )" else sres
            _ ->
              let s2 = procToStingAus p2 [] False
                  sres = s1 ++ "." ++ s2
               in if paren then "( " ++ sres ++ " )" else sres
    Restriction p slist ->
      let ps = procToStingAus p [] True
          sres =
            if length slist > 1 || firstLower (head slist)
              then ps ++ "\\{ " ++ List.intercalate ", " slist ++ "}"
              else ps ++ "\\" ++ head slist
       in if paren then "( " ++ sres ++ " )" else sres
    Relabelling p rellist ->
      let ps = procToStingAus p [] True
          rl = List.intercalate ", " (map (\(x, y) -> x ++ "/" ++ y) rellist)
          sres = ps ++ "[ " ++ rl ++ " ]"
       in if paren then "( " ++ sres ++ " )" else sres
    NonDetChoise p1 p2 ->
      let ps1 = procToStingAus p1 [] False
          ps2 = procToStingAus p2 [] False
          ps2' = case p2 of
            ParallelComp _ _ -> "( " ++ ps2 ++ " )"
            _ -> ps2
          sres = ps1 ++ " + " ++ ps2'
       in if paren then "( " ++ sres ++ " )" else sres
    ParallelComp p1 p2 ->
      let ps1 = procToStingAus p1 [] False
          ps2 = procToStingAus p2 [] False
          ps2' = case p2 of
            NonDetChoise _ _ -> "( " ++ ps2 ++ " )"
            _ -> ps2
          sres = ps1 ++ " | " ++ ps2'
       in if paren then "( " ++ sres ++ " )" else sres
    _ -> error "Invalid directive"

procToString :: Proc -> [Char]
procToString p =
  procToStingAus p [] False

procDefToString :: Stmt -> [Char]
procDefToString (ProcDef (Cst name) p) =
  let nameString = name ++ " = "
      procString = procToString p
   in nameString ++ procString ++ ";"
procDefToString (SetDef name elem) =
  let nameString = "set " ++ name ++ " = "
      procString = "{ " ++ List.intercalate ", " elem ++ "}"
   in nameString ++ procString ++ ";"
procDefToString _ = "Invalid CAAL syntax"

astToCCS :: [Stmt] -> [Char]
astToCCS procList =
  let s = map procDefToString procList
   in List.intercalate "\n" s

main = do
  [filename] <- getArgs
  source <- readFile filename
  let text = parseSource source
  let res = translate text
  let p = astToCCS $ prog res
  let (dir, filename') = splitFileName filename
  writeFile (dir ++ "CCS" ++ filename') p
  print res
