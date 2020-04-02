module Simulator where

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


printMap :: String -> MyStateM (String)
printMap wId = do
  (MySymState symT stck err nB ) <- get
  let world = getWorld wId symT
  let (c,r) = size world
  let maxSpace = length (show (c*r-1)) + 2
  let map = unlines $ reverse $ lines $ concat $ printMatrix wId maxSpace (0,0) (c,r) symT
  let worldObj = printWorldObjects (c,r) (desc world)
  let basket = printBasketInfo (objectsInB world)
  return ( map ++ worldObj ++ basket )


printMatrix :: String -> Int -> Pos -> Pos -> SymTable ->[String]
printMatrix wId maxSpace (currCol,currRow) (maxCol,maxRow) symT
  | currRow == maxRow = []
  | currCol == maxCol = 
    "\n":(printMatrix wId maxSpace (0,currRow+1) (maxCol,maxRow) symT)
  | getWStartPos wId symT == (currCol+1,currRow+1) =
    willy:(printMatrix wId maxSpace (currCol+1,currRow) (maxCol,maxRow) symT)
  | cellWithoutWall (currCol+1) (currRow+1) wId symT = 
    num:(printMatrix wId maxSpace (currCol+1,currRow) (maxCol,maxRow) symT)
  | otherwise = 
    wall:(printMatrix wId maxSpace (currCol+1,currRow) (maxCol,maxRow) symT)
  where
    num   = getSquare (show $ maxCol*currRow + currCol) maxSpace
    wall  = getSquare "X" maxSpace
    willy = getSquare "W" maxSpace


getSquare :: String -> Int -> String
getSquare _ 0 = ""
getSquare [] n = ' ':getSquare [] (n-1)
getSquare (x:xs) n = x:getSquare xs (n-1)


printWorldObjects :: Pos -> WorldDesc -> String
printWorldObjects p wdesc = 
  case unlines $ Prelude.map (printWorldObjects' p) objs of
    ""  -> " []\n"
    str -> "\n" ++ str
  where
    objs = filter (\(_,x) -> isWorldObject x) (Hash.toList wdesc)


printWorldObjects' :: Pos -> ((Int,Int), WorldElements) -> String
printWorldObjects' (maxC,maxR) ((c,r), (Objects objsH)) =
  "En la casilla " ++ ind ++ ":\n"
  ++ (init $ unlines $ Prelude.map (printPairInfo') (Hash.toList objsH) )
  where
    ind = show $ maxC*(r-1) + c-1


printPairInfo' :: (String, Int) -> String
printPairInfo'  (objId,0) = 
  "  Ya fueron recogidos todos los objetos de tipo " ++ objId
printPairInfo' (objId, cnt) =
  "  hay " ++ (show cnt) 
  ++ " objetos de tipo " ++ objId


printPairInfo :: (Int, String) -> String
printPairInfo (cnt, objId) =
  "  hay " ++ (show cnt) 
  ++ " objetos de tipo " ++ objId


printBasketInfo :: [String] -> String
printBasketInfo [] = "Cesta vacia\n"
printBasketInfo objs = 
  "En basket hay:\n" ++ (unlines $ Prelude.map (printPairInfo) $ frequency objs)

