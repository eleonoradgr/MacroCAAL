module MC_Semant where

import Ast
import Data.Map (Map)
import qualified Data.Map as Map
import MC_Parser
import System.Environment

data Rho = Rho
  { processnames :: [String],
    setnames :: Map String [String],
    defInames :: Map String [Int],
    defBnames :: Map String [Bool]
  }

{-semant_check rho (ProcDef name p) = error "not implemented"
semant_check rho (SetDef name l) = error "not implemented"
semant_check rho (VarDef name v) =
  case v of
    RangeCmp co i1 i2 -> if
    Range i:_         ->
    RangeBool b:_     ->-}

main = do
  [filename] <- getArgs
  source <- readFile filename
  let res = parseSource source
  --let rho = Rho [] Map.empty Map.empty Map.empty
  --let res1 = foldl semant_check rho res
  print res
