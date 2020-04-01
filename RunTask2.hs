module RunTask2 where

import AST
import Lexer
import Control.Monad.State
import Control.Monad.Trans
import SymTable
import ContextChecker
import PrintSymTable
import qualified Data.Map as Hash
import System.Exit


runTask :: String -> [BLOCK] -> MyStateM ()
runTask taskId [] = Prelude.error "No hay tareas ni mundos"
runTask taskId blocks = do 
  createSymTable blocks
  (MySymState symT stck err nB ) <- get
  case err of
    []   -> do
      io $ putStr $ printSymTable symT -- QUITAR LUEGO
      io $ putStrLn "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAa\n\n\n" -- QUITAR LUEGO
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
      io $ putStrLn $ wId ++ " " ++ tId ++ " " ++ taskId -- QUITAR LUEGO
      runTaskInsts wId tasks
    else evalTask taskId xs


runTaskInsts :: String -> [TASKINSTR] -> MyStateM ()
runTaskInsts id [] = return ()
runTaskInsts id (x:xs) =  do  -- duda, funciona: runTaskInst x >>= runTaskInsts xs
  runTaskInst id x
  runTaskInsts id xs


runTaskInst :: String ->  TASKINSTR -> MyStateM ()
runTaskInst id (IF (l,c) guard instr) = do
  io $ putStrLn "Voy a un if" -- QUITAR LUEGO
  valid <- evalTaskTest id guard
  if valid then do
    io $ putStrLn "Entre en un if "-- QUITAR LUEGO
    pushScope 
    runTaskInst id instr
    popScope
  else
    return ()

runTaskInst id (IFELSE (l,c) guard instr1 instr2) = do
  io $ putStrLn "Voy a un if"-- QUITAR LUEGO
  valid <- evalTaskTest id guard
  if valid then do
    io $ putStrLn "Entre en un if "-- QUITAR LUEGO
    pushScope
    runTaskInst id instr1
    popScope
  else do
    io $ putStrLn "Entre en un else "-- QUITAR LUEGO
    pushScope
    runTaskInst id instr2
    popScope

runTaskInst id (REPEAT (l,c) n instr) = do
  io $ putStrLn "Voy a un repeat "-- QUITAR LUEGO
  pushScope
  runRepeat id (l,c) n' instr
  popScope
  where
    n' = getValue n

runTaskInst id thisInstr@(WHILE (l,c) guard instr) = do
  io $ putStrLn "Voy a un while"-- QUITAR LUEGO
  pushScope
  runWhile id (l,c) guard instr
  popScope

runTaskInst id (BEGIN (l,c) instrs) = do
  pushScope
  runTaskInsts id instrs
  popScope

runTaskInst id (DEFINE (l,c) (TKId _ tId) instr) = do
  io $ putStrLn "Voy a definir algo"-- QUITAR LUEGO
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
  io $ putStrLn $ "cambiare la pos de willy " ++ show p-- QUITAR LUEGO
  case validStart id newP symT of
    1 -> Prelude.error "No se puede salir del mapa"
    2 -> Prelude.error "No se puede salir del mapa"
    3 -> Prelude.error "Hay una pared en la casilla a la que se quiere ir"
    0 -> do
      updStartPos id newP d
      (MySymState symT (st:stck) err nB ) <- get
      let p2 = getWStartPos id symT
      io $ putStrLn $ "ahora la pos de willy es " ++ show p2-- QUITAR LUEGO
      return ()

runTaskInst id inst@(TURNLEFT _) = do
  (MySymState symT (st:stck) err nB ) <- get
  let d = getWDirection id symT
  io $ putStrLn $ "veo hacia el " ++ d ++ " " ++ show inst-- QUITAR LUEGO
  let newD = changeDirection d inst
  updDirection id newD
  (MySymState symT (st:stck) err nB ) <- get-- QUITAR LUEGO
  let d = getWDirection id symT-- QUITAR LUEGO
  io $ putStrLn $ d-- QUITAR LUEGO

runTaskInst id inst@(TURNRIGHT _) = do
  (MySymState symT (st:stck) err nB ) <- get
  let d = getWDirection id symT
  io $ putStrLn $ "veo hacia el " ++ d ++ " " ++ show inst-- QUITAR LUEGO
  let newD = changeDirection d inst
  updDirection id newD
  (MySymState symT (st:stck) err nB ) <- get-- QUITAR LUEGO
  let d = getWDirection id symT-- QUITAR LUEGO
  io $ putStrLn $ d-- QUITAR LUEGO

runTaskInst id inst@(PICK (l,c) (TKId _ objId)) = do
  (MySymState symT (st:stck) err nB ) <- get
  io $ putStrLn $ "Intentare recoger " ++ objId-- QUITAR LUEGO
  io $ putStrLn $ show (basketCap id symT) ++ " haaaaa"-- QUITAR LUEGO
  case not $ isBasketFull id symT of
    True -> do
      let pos = getWStartPos id symT
      pickObject id objId pos
      insertObject id objId 1 symT
    False -> do
      Prelude.error "No hay suficiente espacio en basket"

runTaskInst id inst@(DROP (l,c) (TKId _ objId)) = do
  io $ putStrLn "Dropear un objeto"-- QUITAR LUEGO
  (MySymState symT (st:stck) err nB ) <- get
  let pos = getWStartPos id symT
  dropObject id objId
  placeObject id objId 1 pos

runTaskInst id inst@(SET (l,c) (TKId _ boolId)) = do
  return()

runTaskInst id inst@(SETTO (l,c) (TKId _ boolId) bool) = do
  return()

runTaskInst id inst@(CLEAR (l,c) (TKId _ boolId)) = do
  return()

runTaskInst id inst@(FLIP (l,c) (TKId _ boolId)) = do
  return()

runTaskInst id (TERMINATE _) = do
  io $ exitWith $ ExitSuccess

runTaskInst id (INSTRID (l,c) (TKId _ objId)) = do
  return() 


evalTaskTest :: String -> TEST -> MyStateM (Bool)
evalTaskTest id (TESTAND (l,c) left right) = do
  l <- evalTaskTest id left
  r <- evalTaskTest id right
  return (l && r)

evalTaskTest id (TESTOR (l,c) left right) = do
  l <- evalTaskTest id left
  r <- evalTaskTest id right
  return (l || r)

evalTaskTest id (TESTNOT (l,c) exp) = do
  e <- evalTaskTest id exp
  return (not e)

evalTaskTest wid (TESTID (l,c) (TKId _ id)) = do
  return (True)

evalTaskTest wid (FOUND (l,c) (TKId _ id)) = do
  (MySymState symT (st:stck) err nB ) <- get
  let world = getWorld wid symT
  let (x,y) = willyIsAt world
  let grid = desc world
  case Hash.lookup (x,y) grid of
    Just (Objects obj) -> case Hash.lookup id obj of
      Just x  -> return (x > 0) -- Found an obj of type id
      Nothing -> return (False)
    Just Wall -> return (False)
    Nothing -> return (False)

evalTaskTest wid (CARRYING (l,c) (TKId _ id)) = do
  (MySymState symT (st:stck) err nB ) <- get
  let world = getWorld wid symT
  let objs  = objectsInB world
  return $ elem id objs

evalTaskTest id (TESTTOF _ bool) = do
  io $ print bool-- QUITAR LUEGO
  return $ getBool bool

evalTaskTest id test@(FRONTCLEAR _ ) = do
  (MySymState symT (st:stck) err nB ) <- get
  let world = getWorld id symT
  let pos = moveInDir (willyIsAt world) (willyDirection world)
  io $ putStrLn $ "Voy a revisar la casilla " ++ show pos-- QUITAR LUEGO
  case validStart id pos symT of
    0 -> return (True)
    _ -> return (False)

evalTaskTest id test@(LEFTCLEAR _ ) = do
  (MySymState symT (st:stck) err nB ) <- get
  let world = getWorld id symT
  let newD = changeDirection (willyDirection world) (TURNLEFT (1,1))
  let pos = moveInDir (willyIsAt world) newD
  io $ putStrLn $ "Voy a revisar la casilla " ++ show pos-- QUITAR LUEGO
  case validStart id pos symT of
    0 -> return (True)
    _ -> return (False)

evalTaskTest id test@(RIGHTCLEAR _ ) = do
  (MySymState symT (st:stck) err nB ) <- get
  let world = getWorld id symT
  let newD = changeDirection (willyDirection world) (TURNRIGHT (1,1))
  let pos = moveInDir (willyIsAt world) newD
  io $ putStrLn $ "Voy a revisar la casilla " ++ show pos-- QUITAR LUEGO
  case validStart id pos symT of
    0 -> return (True)
    _ -> return (False)

evalTaskTest id (LOOKNORTH _ ) = do
  (MySymState symT (st:stck) err nB ) <- get
  let world = getWorld id symT
  let dir = willyDirection world
  return (dir == "north")

evalTaskTest id (LOOKEAST _ ) = do
  (MySymState symT (st:stck) err nB ) <- get
  let world = getWorld id symT
  let dir = willyDirection world
  return (dir == "east")

evalTaskTest id (LOOKSOUTH _ ) = do
  (MySymState symT (st:stck) err nB ) <- get
  let world = getWorld id symT
  let dir = willyDirection world
  return (dir == "south")

evalTaskTest id (LOOKWEST _ ) = do
  (MySymState symT (st:stck) err nB ) <- get
  let world = getWorld id symT
  let dir = willyDirection world
  return (dir == "west")


evalFinalGoal :: String -> FINALGOAL -> MyStateM (Bool)
evalFinalGoal _ _ = return (True)


evalGoal :: String -> GOALTEST -> MyStateM (Bool)
evalGoal wId (WILLYISAT (l,c) col row ) = do
  (MySymState symT (st:stck) err nB ) <- get
  let world = getWorld wId symT
  let pos = willyIsAt world
  return (pos == (c,r))
  where
    c = getValue col
    r = getValue row

evalGoal wId (OBJECTSIN (l,c) n oId) = do
  (MySymState symT (st:stck) err nB ) <- get
  let world = getWorld wId symT
  let nObjs = length $ filter (\x -> x == oId') (objectsInB world)
  return (n' == nObjs)
  where 
    n'   = getValue n
    oId' = getStr oId

evalGoal wId (OBJECTSAT (l,c) n oId col row) = do
  (MySymState symT (st:stck) err nB ) <- get
  let m = numberOfObjects wId oId' (c,r) symT
  return (m >= n')
  where 
    n'   = getValue n
    oId' = getStr oId
    c    = getValue col
    r    = getValue row

runRepeat :: String -> Pos -> Int -> TASKINSTR -> MyStateM ()
runRepeat _ _ 0 _ = return ()
runRepeat id (l,c) n task = do
  io $ putStrLn "Entre en un loop del repeat "-- QUITAR LUEGO
  runTaskInst id task
  runRepeat id (l,c) (n-1) task


runWhile :: String -> Pos -> TEST -> TASKINSTR -> MyStateM ()
runWhile id (l,c) guard inst = do
  valid <- evalTaskTest id guard
  if valid then do
    io $ putStrLn "Sigo en el while" -- QUITAR LUEGO
    runTaskInst id inst
    runWhile id (l,c) guard inst
  else
    return ()

moveInDir :: Pos -> String -> Pos -- No se si las direcciones funcionan asi
moveInDir (r,c) dir = case dir of
  "north" -> (r,c+1)
  "south" -> (r,c-1)
  "west"  -> (r-1,c)
  "east"  -> (r+1,c)


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


getWorld :: String -> SymTable -> SymValue
getWorld id symT = case Hash.lookup id symT of 
  Just listVal -> filter isWorld listVal !! 0
  otherwise -> World (1,1) id 0 0 Hash.empty (1,1) 1 [] (-1,-1) "north" None


numberOfObjects :: String -> String -> Pos -> SymTable -> Int
numberOfObjects worldId objId (c,r) symT = do
  case Hash.lookup worldId symT of
    Just listVal -> do
      let world = filter isWorld listVal !! 0
      case Hash.lookup (c,r) $ desc world of
        Just (Objects map) -> -- there already are objects in the cell
          case Hash.lookup objId map of
            Just m -> m
            Nothing -> 0
        Nothing -> 0
    Nothing -> 0