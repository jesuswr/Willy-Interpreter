module RunTask2 where

import AST
import Lexer
import Control.Monad.State
import Control.Monad.Trans
import SymTable
import ContextChecker
import PrintSymTable
import qualified Data.Map as Hash


runTask :: String -> [BLOCK] -> MyStateM ()
runTask taskId [] = Prelude.error "No hay tareas ni mundos"
runTask taskId blocks = do 
  --let initTableState = MySymState Hash.empty [0] [] 0 -- esto se manda desde el main
  --case createSymTable blocks of
  --  Left errorStr -> do
  --    return $ Left $ "Errores de contexto:\n" ++ errorStr
  --  Right symT -> do
  --    return $ evalTask taskId blocks 
  createSymTable blocks
  (MySymState symT stck err nB ) <- get
  case err of
    []   -> do
      io $ putStr $ printSymTable symT
      io $ putStrLn "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAa\n\n\n"
      evalTask taskId blocks
    str  -> 
      io $ putStrLn $ unlines $ reverse err


evalTask :: String -> [BLOCK] -> MyStateM ()
evalTask taskId [] = Prelude.error "No se encontro la tarea"
evalTask taskId (x:xs) = case x of
  WORLD{} -> evalTask taskId xs
  (TASK (l,c) (TKId tP tId) (TKId wP wId) tasks) -> do
    if taskId == tId then do
      (MySymState symT stck err nB ) <- get
      let wSc = getWorldScope wId symT
      put(MySymState symT (wSc:stck) err nB) -- push world scope. From this time there is only one world in stack
      pushScope -- push this task  scope. The task is already in symT.
      io $ putStrLn $ wId ++ " " ++ tId ++ " " ++ taskId 
      runTaskInsts wId tasks
    else evalTask taskId xs


runTaskInsts :: String -> [TASKINSTR] -> MyStateM ()
runTaskInsts id [] = return ()
runTaskInsts id (x:xs) =  do  -- duda, funciona: runTaskInst x >>= runTaskInsts xs
  runTaskInst id x
  runTaskInsts id xs


runTaskInst :: String ->  TASKINSTR -> MyStateM ()
runTaskInst id (IF (l,c) guard instr) = do
  io $ putStrLn "Voy a un if"
  valid <- evalTaskTest id guard
  if True then do
    io $ putStrLn "Entre en un if "
    pushScope 
    runTaskInst id instr
    popScope
  else
    return ()

runTaskInst id (IFELSE (l,c) guard instr1 instr2) = do
  io $ putStrLn "Voy a un if"
  valid <- evalTaskTest id guard
  if valid then do
    io $ putStrLn "Entre en un if "
    pushScope
    runTaskInst id instr1
    popScope
  else do
    io $ putStrLn "Entre en un else "
    pushScope
    runTaskInst id instr2
    popScope

runTaskInst id (REPEAT (l,c) n instr) = do
  io $ putStrLn "Voy a un repeat "
  pushScope
  runRepeat id (l,c) n' instr
  popScope
  where
    n' = getValue n

runTaskInst id thisInstr@(WHILE (l,c) guard instr) = do
  io $ putStrLn "Voy a un while"
  pushScope
  runWhile id (l,c) guard instr
  popScope

runTaskInst id (BEGIN (l,c) instrs) = do
  pushScope
  runTaskInsts id instrs
  popScope

runTaskInst id (DEFINE (l,c) (TKId _ tId) instr) = do
  io $ putStrLn "Voy a definir algo"
  (MySymState symT (st:stck) err nB ) <- get
  let val = Instruction (l,c) id st (nB+1) instr
  pushScope
  insToTable tId val
  popScope

runTaskInst id inst@(MOVE _) = do
  (MySymState symT (st:stck) err nB ) <- get
  let p = getWStartPos id symT
  let d = getWDirection id symT
  let newP = moveInDir p d
  io $ putStrLn $ "cambiare la pos de willy " ++ show p
  case validStart id newP symT of
    1 -> Prelude.error "No se puede salir del mapa"
    2 -> Prelude.error "No se puede salir del mapa"
    3 -> Prelude.error "Hay una pared en la casilla a la que se quiere ir"
    0 -> do
      updStartPos id newP d
      (MySymState symT (st:stck) err nB ) <- get
      let p2 = getWStartPos id symT
      io $ putStrLn $ "ahora la pos de willy es " ++ show p2
      return ()

runTaskInst id inst@(TURNLEFT _) = do
  (MySymState symT (st:stck) err nB ) <- get
  let d = getWDirection id symT
  io $ putStrLn $ "veo hacia el " ++ d ++ " " ++ show inst
  let newD = changeDirection d inst
  updDirection id newD
  (MySymState symT (st:stck) err nB ) <- get
  let d = getWDirection id symT
  io $ putStrLn $ d

runTaskInst id inst@(TURNRIGHT _) = do
  (MySymState symT (st:stck) err nB ) <- get
  let d = getWDirection id symT
  io $ putStrLn $ "veo hacia el " ++ d ++ " " ++ show inst
  let newD = changeDirection d inst
  updDirection id newD
  (MySymState symT (st:stck) err nB ) <- get
  let d = getWDirection id symT
  io $ putStrLn $ d

runTaskInst id inst@(PICK (l,c) (TKId _ objId)) = do
  (MySymState symT (st:stck) err nB ) <- get
  io $ putStrLn $ "Intentare recoger " ++ objId
  io $ putStrLn $ show (basketCap id symT) ++ " haaaaa"
  case not $ isBasketFull id symT of
    True -> do
      let pos = getWStartPos id symT
      pickObject id objId pos
      insertObject id objId 1 symT
    False -> do
      Prelude.error "No hay suficiente espacio en basket"

runTaskInst id inst@(DROP (l,c) (TKId _ objId)) = do
  io $ putStrLn "Dropear un objeto"
  (MySymState symT (st:stck) err nB ) <- get
  let pos = getWStartPos id symT
  dropObject id objId
  placeObject id objId 1 pos



runTaskInst _ _ = return () -- Quitar este al final


evalTaskTest :: String -> TEST -> MyStateM (Bool)
evalTaskTest _ _ = return (True)


runRepeat :: String -> Pos -> Int -> TASKINSTR -> MyStateM ()
runRepeat _ _ 0 _ = return ()
runRepeat id (l,c) n task = do
  io $ putStrLn "Entre en un loop del repeat "
  runTaskInst id task
  runRepeat id (l,c) (n-1) task


runWhile :: String -> Pos -> TEST -> TASKINSTR -> MyStateM ()
runWhile id (l,c) guard inst = do
  valid <- evalTaskTest id guard
  if valid then do
    io $ putStrLn "Sigo en el while"
    runTaskInst id inst
    runWhile id (l,c) guard inst
  else
    return ()

moveInDir :: Pos -> String -> Pos
moveInDir (r,c) dir = case dir of
  "north" -> (r+1,c)
  "south" -> (r-1,c)
  "west"  -> (r,c-1)
  "east"  -> (r,c+1)


changeDirection :: String -> TASKINSTR -> String
changeDirection dir turn = case dir of
  "north" -> case turn of
    TURNLEFT _  -> "west"
    TURNRIGHT _ -> "east"
  "south" -> case turn of
    TURNLEFT _  -> "east"
    TURNRIGHT _ -> "west"
  "west"  -> case turn of
    TURNLEFT _  -> "south"
    TURNRIGHT _ -> "north"
  "east"  -> case turn of
    TURNLEFT _  -> "north"
    TURNRIGHT _ -> "south"


dropObject :: String -> String -> MyStateM ()
dropObject wId objId = do
  (MySymState symT (st:stck) err nB ) <- get
  case Hash.lookup wId symT of
    Nothing -> return()
    Just listVal -> do
      let oldWorld = filter isWorld listVal !! 0
      let objs = objectsInB oldWorld
      case elem objId objs of
        False -> Prelude.error "El objecto no existe en basket"
        True  -> do
          let newWorld = oldWorld{objectsInB = dropObject' objId objs}
          put(MySymState (Hash.insert wId (updateWListVal newWorld listVal) symT) stck err nB)
  where
    dropObject' oId (x:xs) 
      | x == oId  = xs
      | otherwise = x:(dropObject' oId xs)



