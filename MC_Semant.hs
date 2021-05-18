module MC_Semant where

import Ast
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import MC_Parser
import System.Environment

data Rho = Rho
  { pid :: [String],
    setid :: Map String [String],
    defIid :: Map String [Int],
    defBid :: Map String [Bool]
  }
  deriving (Show)

semant_check rho (ProcDef name p) = Left "not implemented"
semant_check rho (SetDef name l) = Left "not implemented"
semant_check rho (VarDef name v) =
  case v of
    RangeCmp co i1 i2 ->
      ( if isJust $ Map.lookup name $ defIid rho
          then Left "Variable already defined"
          else
            ( let v' = [1, 2, 3]
               in let rho' = Rho {pid = pid rho, setid = setid rho, defIid = Map.insert name v' $ defIid rho, defBid = defBid rho}
                   in Right rho'
            )
      )
    Range (i : _) -> Left "not implemented"
    RangeBool (b : _) -> Left "not implemented"
    _ -> Left "not implemented"

testSeman rho def =
  case semant_check rho def of
    Left e -> error $ show e
    Right r -> r

main = do
  [filename] <- getArgs
  source <- readFile filename
  let res = parseSource source
  let rho = Rho {pid = [], setid = Map.empty, defIid = Map.empty, defBid = Map.empty}
  let res1 = foldl testSeman rho res
  print res1
