
module PrintSymTable where

import AST
import Lexer
import qualified Data.Map as Hash
import qualified SymTable as ST
import qualified Data.List as L
import PrintParser as PP
import Control.Monad.State


--type MyPrintStateM a = State PrintState a

printSymTable :: ST.SymTable -> String 
printSymTable symT = unlines $ map (getSymString') (Hash.toList symT)


getSymString' :: (String, [ST.SymValue]) -> String
getSymString' (id, [symVal]) = getSymStrings id [symVal] 

getSymStrings :: String -> [ST.SymValue] -> String
getSymStrings id [s]    = getSymString id s
getSymStrings id (s:ss) = (getSymString id s) ++ "\n"
                          ++ (getSymStrings id ss)

getSymString :: String -> ST.SymValue -> String
getSymString id (ST.World _ _ 
  decBlock idBlock descW (wx,wy) baskSz
  objInBsk (x,y) willyDir (ST.FinalG goal)) =
  id ++ ":\n"
  ++ "  tipo: mundo\n"
  ++ "  identificador de bloque: " ++ (show idBlock) ++ "\n"
  ++ "  tamanio: " ++ (show x) ++ (show y) ++ "\n"
  ++ "  muros y objetos:\n" ++ (printWorldDesc' 4 descW)
  ++ "  posicion de willy: " ++ (show wx) ++ (show wy) ++ "\n"
  ++ "  direccion de willy: " ++ willyDir ++ "\n"
  ++ "  tamanio de la cesta: " ++ (show baskSz) ++ "\n"
  ++ "  objetos en la cesta:\n" ++ (printBasketObj 4 objInBsk)
  ++ "  final goal:\n" ++ (printFinalGoal 4 goal)

getSymString id (ST.ObjectType _ _ defBlock color) =
  id ++ ":\n"
  ++ "  tipo: Objeto\n"
  ++ "  bloque de declaracion: " ++ (show defBlock) ++ "\n"
  ++ "  color del objeto: " ++ color ++ "\n"

getSymString id (ST.WBoolean _ _ defBlock value) =
  id ++ ":\n"
  ++ "  tipo: booleano\n"
  ++ "  bloque de declaracion: " ++ (show defBlock) ++ "\n"
  ++ "  valor: " ++ (show value) ++ "\n"

getSymString id (ST.Goal _ _ defBlock goalTest) =
  id ++ ":\n"
  ++ "  tipo: goal\n"
  ++ "  bloque de declaracion: " ++ (show defBlock) ++ "\n"
  ++ "  expresion: " ++ (printGoalTest 4 goalTest)

getSymString id (ST.Instruction _ _ defBlock numBlock inst) =
  id ++ ":\n"
  ++ "  tipo: instrucción\n"
  ++ "  bloque de declaracion: " ++ (show defBlock) ++ "\n"
  ++ "  identificador de bloque: " ++ (show numBlock) ++ "\n"
  ++ "  AST asociado:\n" ++ (printInstr' 4 inst defBlock)

getSymString id (ST.Task _ _ _ numBlock onWorld) =
  id ++ ":\n"
  ++ "  tipo: tarea\n"
  ++ "  mundo asociado: " ++ onWorld ++ "\n"
  ++ "  identificador de bloque: " ++ (show numBlock) ++ "\n"


printFinalGoal :: Int -> FINALGOAL -> String
printFinalGoal spaces finalG = "JAJA"



printWorldDesc' :: Int -> ST.WorldDesc -> String
printWorldDesc' spaces wdesc = 
  unlines $ map (printWorldDesc spaces) (Hash.toList wdesc)

printWorldDesc :: Int -> ((Int,Int), ST.WorldElements) -> String
printWorldDesc spaces ((c,r), ST.Wall) = 
  replicate spaces ' ' ++ "En la casilla (" 
  ++ (show c) ++ ", " ++ (show r) ++ ") hay un muro.\n"

printWorldDesc spaces ((c,r), (ST.Objects objsH)) =
  replicate spaces ' ' ++ "En la casilla (" 
  ++ (show c) ++ ", " ++ (show r) 
  ++ ") se encuentran los siguientes objetos:\n"
  ++ (unlines $ map (printPair' (spaces+2)) (Hash.toList objsH) )



printBasketObj :: Int -> [String] -> String
printBasketObj spaces objs = 
  unlines $ map (printPair spaces) $ frequency objs

frequency :: Ord a => [a] -> [(Int,a)] 
frequency = map (\l -> (length l, head l)) . L.group . L.sort

printPair :: Int -> (Int, String) -> String
printPair spaces (cnt, objId) =
  replicate spaces ' ' ++ "hay " ++ (show cnt) 
  ++ "objetos de tipo " ++ objId

printPair' :: Int -> (String, Int) -> String
printPair' spaces (objId, cnt) =
  replicate spaces ' ' ++ "hay " ++ (show cnt) 
  ++ "objetos de tipo " ++ objId


printGoalTest :: Int -> GOALTEST -> String
printGoalTest spaces (WILLYISAT _ c r) =
  replicate spaces ' ' ++ "goal: willy is at " 
  ++ (show $ getValue c) ++ " " ++ (show $ getValue r) ++ "\n"

printGoalTest spaces (OBJECTSIN _ n objId) =
  replicate spaces ' ' ++ "goal: " ++ (show n) ++ (getStr objId)
  ++ " objects in Basket\n"

printGoalTest spaces (OBJECTSAT _ n objId c r) =
  replicate spaces ' ' ++ "goal: " ++ (show n) ++ (getStr objId)
  ++ " objects at "
  ++ (show $ getValue c) ++ " " ++ (show $ getValue r) ++ "\n"

printInstr' :: Int -> TASKINSTR -> Int -> String
printInstr' spaces inst currentScope = unlines $ reverse result
  where ((),(PrintState result _)) = do 
        runState (printInstr spaces inst) (PrintState [] currentScope)


