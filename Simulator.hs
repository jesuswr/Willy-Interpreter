{-
An Interpreter for the subject "Traductores e Interpretadores" (Translators and Interpreters)
 of the Simon Bolivar University (USB).
  
  Authors:
  
  Neil Villamizar  15-11523
  
  Jesus Wahrman    15-11540
-}

module Simulator where

import System.Console.ANSI
import AST
import Lexer
import Control.Monad.State
import Control.Monad.Trans
import SymTable
import ContextChecker
import PrintSymTable
import qualified Data.List as L
import qualified Data.Map as Hash
import System.Exit


-- Function that takes the world id and returns a string with information
-- of the world to be printed
printMap :: String -> MyStateM [(String, Color)]
printMap wId = do
  symT <- gets symTable
  let world = getWorld wId symT
  let dir = ("Looking " ++ willyDirection world ++ "\n",White)
  let (c,r) = size world
  let maxSpace = length (show (c*r-1)) + 2
  --let map = unlines $ reverse $ lines $ concat $ printMatrix wId maxSpace (0,0) (c,r) symT
  let map = reverse $ printMatrix wId maxSpace (0,r-1) (c,r) symT
  let worldObj = (printWorldObjects (c,r) (desc world), White)
  let basket = (printBasketInfo (objectsInB world), White)
  return (basket:worldObj:dir:("\n",White):map)


-- Takes the world id, the max space of an element of the matrix,
-- the current position, the size of the map and the symbols table
-- and return a list of string with the information of the map
printMatrix :: String -> Int -> Pos -> Pos -> SymTable ->[(String, Color)]
printMatrix wId maxSpace (currCol,currRow) (maxCol,maxRow) symT
  | currRow == -1 = []
  | currCol == maxCol = 
    ("\n",White):(printMatrix wId maxSpace (0,currRow-1) (maxCol,maxRow) symT)
  | getWStartPos wId symT == (currCol+1,currRow+1) =
    (willy,Yellow):(printMatrix wId maxSpace (currCol+1,currRow) (maxCol,maxRow) symT)
  | cellWithoutWall (currCol+1) (currRow+1) wId symT = 
    (num, color):(printMatrix wId maxSpace (currCol+1,currRow) (maxCol,maxRow) symT)
  | otherwise = 
    (wall, Magenta):(printMatrix wId maxSpace (currCol+1,currRow) (maxCol,maxRow) symT)
  where
    num   = getSquare (show $ maxCol*currRow + currCol) maxSpace
    wall  = getSquare "X" maxSpace
    willy = getSquare "W" maxSpace
    color = getColor wId symT (currCol+1) (currRow+1)

-- Check if a cell is empty. If it is, return White, else Cyan
getColor :: String -> SymTable -> Int -> Int -> Color
getColor wId symT c r = case Hash.lookup wId symT of 
  (Just lst) -> do
    let grid = desc $ getWorld wId symT
    case Hash.lookup (c,r) grid of
      (Just (Objects objs)) -> if sizeObj objs == 0
        then White
        else Cyan
      otherwise -> White
  otherwise -> White

-- Returns the amount of objects in a cell of the world
sizeObj :: ObjectsInCell -> Int
sizeObj objs = snd $ foldr (\(_,x) (_,y) -> ("", x + y)) ("",0) (Hash.toList objs)

-- Receives a string and an integer and returns a new string that
-- is the same as the given one plus some spaces at the end
getSquare :: String -> Int -> String
getSquare _ 0 = ""
getSquare [] n = ' ':getSquare [] (n-1)
getSquare (x:xs) n = x:getSquare xs (n-1)


-- Takes the size of the map and the world description and returns
-- a string with the objects in the map
printWorldObjects :: Pos -> WorldDesc -> String
printWorldObjects p wdesc = 
  case unlines $ Prelude.map (printWorldObjects' p) objs of
    ""  -> "\n"
    str -> "\n" ++ str
  where
    objs = filter (\(_,x) -> isWorldObject x) (Hash.toList wdesc)


-- Takes the size of the map and a pair that has a position and 
-- the elements in that position and returns a string with
-- the objects in that position
printWorldObjects' :: Pos -> (Pos, WorldElements) -> String
printWorldObjects' (maxC,maxR) ((c,r), (Objects objsH)) =
  "En la casilla " ++ ind ++ ":\n"
  ++ (init $ unlines $ Prelude.map (printPairInfo') (Hash.toList objsH) )
  where
    ind = show $ maxC*(r-1) + c-1


-- Takes a pair with an object id and the ammount of times that element
-- appears and retuns a string with that information
printPairInfo' :: (String, Int) -> String
printPairInfo'  (objId,0) = 
  "  Ya fueron recogidos todos los objetos de tipo " ++ objId
printPairInfo' (objId, cnt) =
  "  hay " ++ (show cnt) 
  ++ " objetos de tipo " ++ objId


-- Takes a pair with an object id and the ammount of times that element
-- appears and retuns a string with that information
printPairInfo :: (Int, String) -> String
printPairInfo (cnt, objId) =
  "  hay " ++ (show cnt) 
  ++ " objetos de tipo " ++ objId



-- Takes the elements in the basket
-- and retuns a string with that information
printBasketInfo :: [String] -> String
printBasketInfo [] = "Cesta vacia\n"
printBasketInfo objs = 
  "En basket hay:\n" ++ (unlines $ Prelude.map (printPairInfo) $ frequency objs)