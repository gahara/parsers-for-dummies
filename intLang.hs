--маленький язык программирования, который умеет в операции над integer, переменные и печатать результат
import Data.Map (Map)
import qualified Data.Map as M

type Name = String

data Exp = Const Integer
         | Var Name
         | Add Exp Exp
         | Sub Exp Exp
         | Prod Exp Exp
         deriving (Show, Eq)

data Statement = Set Name Exp
               | Print Exp
               deriving (Show, Eq)

data State = State { vars :: Map Name Integer
                   , output :: String
                   }
             deriving (Show, Eq)

eval :: State -> Exp -> Integer
eval s v = eval' v
  where eval' (Const i) = i
        eval' (Var n) = vars s M.! n
        eval' (Add a b) = eval' a + eval' b
        eval' (Sub a b) = eval' a - eval' b
        eval' (Prod a b) = eval' a * eval' b

run :: State -> Statement -> State
run s (Set n e) = s { vars = M.insert n (eval s e) (vars s) }
run s (Print e) = s { output = output s ++ show (eval s e) ++ "\n" }

runAll :: [Statement] -> State
runAll = foldl run firstState

firstState :: State
firstState = State { vars = M.empty
                   , output = ""
                   }

--runAll [Set "n" (Add (Const 8) (Const 9)), Print (Add (Const 1) (Var "n"))]
--State {vars = fromList [("n",17)], output = "18\n"}