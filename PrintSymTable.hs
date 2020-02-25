{-
An Interpreter for the subject "Traductores e Interpretadores" (Translators and Interpreters)
 of the Simon Bolivar University (USB).
  
  Authors:
  
  Neil Villamizar  15-11523
  
  Jesus Wahrman    15-11540
-}

module PrintSymTable where

import AST
import Lexer
import qualified Data.Map as Hash
import qualified SymTable as ST
import qualified Data.List as L
import PrintParser as PP
import Control.Monad.State


-- Receives the SymTable and prints it in the wanted format
printSymTable :: ST.SymTable -> String 
printSymTable symT = unlines $ map (getSymString') (Hash.toList symT)


-- Receives a element of the table of the form (id,[value]) and prints it
getSymString' :: (String, [ST.SymValue]) -> String
getSymString' (id, symVal) = getSymStrings id symVal 


-- Receives an id and a list of values with that id and prints them
getSymStrings :: String -> [ST.SymValue] -> String
getSymStrings id [s]    = getSymString id s
getSymStrings id (s:ss) = (getSymString id s) ++ "\n"
                          ++ (getSymStrings id ss)


-- Receives an id and a value with that id and prints it
getSymString :: String -> ST.SymValue -> String
getSymString id (ST.World _ _ 
  decBlock idBlock descW (wx,wy) baskSz
  objInBsk (x,y) willyDir (ST.FinalG goal)) =
  id ++ ":\n"
  ++ "  tipo: mundo\n"
  ++ "  identificador de bloque: " ++ (show idBlock) ++ "\n"
  ++ "  tamanio: " ++ (show x) ++ " " ++ (show y) ++ "\n"
  ++ "  muros y objetos:" ++ (printWorldDesc' 4 descW)
  ++ "  posicion de willy: " ++ (show wx) ++ " " ++ (show wy) ++ "\n"
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
  ++ "  AST asociado:" ++ case (printInstr' 4 inst defBlock) of
    "" -> " null\n"
    str -> "\n" ++ str

getSymString id (ST.Task _ _ _ numBlock onWorld) =
  id ++ ":\n"
  ++ "  tipo: tarea\n"
  ++ "  mundo asociado: " ++ onWorld ++ "\n"
  ++ "  identificador de bloque: " ++ (show numBlock) ++ "\n"


-- Receives the indentation level and a FINALGOAl and prints it
printFinalGoal :: Int -> FINALGOAL -> String
printFinalGoal spaces (FGAND _ leftFG rightFG) =
  replicate spaces ' ' ++ "AND:\n"
  ++ replicate spaces ' ' ++ "lado izquierdo:\n"
  ++ printFinalGoal (spaces+2) leftFG
  ++ replicate spaces ' ' ++ "lado derecho:\n"
  ++ printFinalGoal (spaces+2) rightFG

printFinalGoal spaces (FGOR _ leftFG rightFG) =
  replicate spaces ' ' ++ "OR:\n"
  ++ replicate spaces ' ' ++ "lado izquierdo:\n"
  ++ printFinalGoal (spaces+2) leftFG
  ++ replicate spaces ' ' ++ "lado derecho:\n"
  ++ printFinalGoal (spaces+2) rightFG

printFinalGoal spaces (FGNOT _ expFG) =
  replicate spaces ' ' ++ "NOT:\n"
  ++ replicate spaces ' ' ++ "expresion:\n"
  ++ printFinalGoal (spaces+2) expFG

printFinalGoal spaces (FGID _ idFG) =
  replicate spaces ' ' ++ "ID: " 
  ++(getStr idFG) ++ "\n"


-- Receives the indentation level, the world description and 
-- prints it
printWorldDesc' :: Int -> ST.WorldDesc -> String
printWorldDesc' spaces wdesc = 
  case unlines $ map (printWorldDesc spaces) (Hash.toList wdesc) of
    ""  -> " []\n"
    str -> "\n" ++ str


-- Receives the indentation level, the elements of the world in the format
-- ( Position , Elements) and prints them 
printWorldDesc :: Int -> ((Int,Int), ST.WorldElements) -> String
printWorldDesc spaces ((c,r), ST.Wall) = 
  replicate spaces ' ' ++ "En la casilla (" 
  ++ (show c) ++ ", " ++ (show r) ++ ") hay un muro."

printWorldDesc spaces ((c,r), (ST.Objects objsH)) =
  replicate spaces ' ' ++ "En la casilla (" 
  ++ (show c) ++ ", " ++ (show r) ++ "):\n"
  ++ (init $ unlines $ map (printPair' (spaces+2)) (Hash.toList objsH) )


-- Receives the indentation level, the basket description and 
-- prints it
printBasketObj :: Int -> [String] -> String
printBasketObj spaces [] = replicate spaces ' ' ++ "cesta vacia\n"
printBasketObj spaces objs = 
  unlines $ map (printPair spaces) $ frequency objs


-- Receives a list of ids and transforms it to a list of (Int,id)
-- that represent how many times an id shows
frequency :: Ord a => [a] -> [(Int,a)] 
frequency = map (\l -> (length l, head l)) . L.group . L.sort


-- Receives the indentation level and a pair that represents 
-- the number of times that an id shows and prints it
printPair :: Int -> (Int, String) -> String
printPair spaces (cnt, objId) =
  replicate spaces ' ' ++ "hay " ++ (show cnt) 
  ++ " objetos de tipo " ++ objId


-- Receives the indentation level and a pair that represents 
-- the number of times that an id shows and prints it
printPair' :: Int -> (String, Int) -> String
printPair' spaces (objId, cnt) =
  replicate spaces ' ' ++ "hay " ++ (show cnt) 
  ++ " objetos de tipo " ++ objId


-- Receives the indentation level and a GOALTEST and prints it 
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


-- Receives the indentation level, a task instruction, the current scope
-- and prints it
printInstr' :: Int -> TASKINSTR -> Int -> String
printInstr' spaces inst currentScope = unlines $ reverse result
  where ((),(PrintState result _)) = do 
        runState (printInstr spaces inst) (PrintState [] currentScope)