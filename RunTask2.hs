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
import Simulator


runTask :: String -> [BLOCK] -> MyStateM ()
runTask taskId [] = Prelude.error "No hay tareas ni mundos"
runTask taskId blocks = do 
  createSymTable blocks
  (MySymState symT stck err nB ) <- get
  case err of
    []   -> do
      evalTask taskId blocks
    str  -> do
      io $ putStrLn "\nErrores de contexto:\n"
      io $ putStrLn $ unlines $ reverse err


evalTask :: String -> [BLOCK] -> MyStateM ()
evalTask taskId [] = Prelude.error "No se encontro la tarea"
evalTask taskId (x:xs) = case x of
  WORLD{} -> evalTask taskId xs
  (TASK (l,c) (TKId tP tId) (TKId wP wId) tasks) -> do
    if taskId == tId then do
      (MySymState symT stck err nB ) <- get
      printWorld wId -- print initial world configuration
      let wSc = getWorldScope wId symT
      put(MySymState symT (wSc:stck) err nB) -- push world scope. From this time there is only one world in stack
      pushScope -- push this task  scope. The task is already in symT.
      runTaskInsts wId tasks
      (MySymState symT stck err nB ) <- get
      checkFinalGoal wId -- check final goal
    else 
      evalTask taskId xs


runTaskInsts :: String -> [TASKINSTR] -> MyStateM ()
runTaskInsts id [] = return ()
runTaskInsts id (x:xs) =  do  -- duda, funciona: runTaskInst x >>= runTaskInsts xs
  runTaskInst id x
  runTaskInsts id xs


checkFinalGoal :: String -> MyStateM ()
checkFinalGoal wId = do
  (MySymState symT stck err nB ) <- get
  let world = getWorld wId symT
  success <- evalFinalGoal wId (goal $ finalG world)
  if success then
    io $ putStrLn "El final goal del mundo fue logrado"
  else
    io $ putStrLn "El final goal del mundo no fue logrado"


runTaskInst :: String ->  TASKINSTR -> MyStateM ()
runTaskInst id (IF (l,c) guard instr) = do
  valid <- evalTaskTest id guard
  if valid then do
    pushScope 
    runTaskInst id instr
    popScope
  else
    return ()

runTaskInst id (IFELSE (l,c) guard instr1 instr2) = do
  valid <- evalTaskTest id guard
  if valid then do
    pushScope
    runTaskInst id instr1
    popScope
  else do
    pushScope
    runTaskInst id instr2
    popScope

runTaskInst id (REPEAT (l,c) n instr) = do
  pushScope
  runRepeat id (l,c) n' instr
  popScope
  where
    n' = getValue n

runTaskInst id thisInstr@(WHILE (l,c) guard instr) = do
  pushScope
  runWhile id (l,c) guard instr
  popScope

runTaskInst id (BEGIN (l,c) instrs) = do
  pushScope
  runTaskInsts id instrs
  popScope

runTaskInst id (DEFINE (l,c) (TKId _ tId) instr) = do
  (MySymState symT (st:stck) err nB ) <- get
  let val = Instruction (l,c) id st (nB+1) instr
  insToTable tId val

runTaskInst id inst@(MOVE _) = do
  (MySymState symT (st:stck) err nB ) <- get
  let p = getWStartPos id symT
  let d = getWDirection id symT
  let newP = moveInDir p d
  case validStart id newP symT of
    1 -> Prelude.error "No se puede salir del mapa"
    2 -> Prelude.error "No se puede salir del mapa"
    3 -> Prelude.error "Hay una pared en la casilla a la que se quiere ir"
    0 -> do
      updStartPos id newP d
      (MySymState symT stck err nB ) <- get
      printWorld id
      return ()

runTaskInst id inst@(TURNLEFT _) = do
  (MySymState symT (st:stck) err nB ) <- get
  let d = getWDirection id symT
  let newD = changeDirection d inst
  updDirection id newD
  printWorld id

runTaskInst id inst@(TURNRIGHT _) = do
  (MySymState symT (st:stck) err nB ) <- get
  let d = getWDirection id symT
  let newD = changeDirection d inst
  updDirection id newD
  printWorld id

runTaskInst id inst@(PICK (l,c) (TKId _ objId)) = do
  (MySymState symT (st:stck) err nB ) <- get
  case not $ isBasketFull id symT of
    True -> do
      let pos = getWStartPos id symT
      pickObject id objId pos
      insertObject id objId 1 symT
      printWorld id
    False -> 
      Prelude.error "No hay suficiente espacio en basket"

runTaskInst id inst@(DROP (l,c) (TKId _ objId)) = do
  (MySymState symT (st:stck) err nB ) <- get
  let pos = getWStartPos id symT
  dropObject id objId
  placeObject id objId 1 pos
  printWorld id

runTaskInst id inst@(SET p (TKId _ boolId)) =
  runTaskInst id (SETTO p (TKId p boolId) (TKtrue p))

runTaskInst id inst@(SETTO p (TKId _ boolId) bool) = do
  (MySymState symT stck err nB ) <- get
  case Hash.lookup boolId symT of -- search the bool variable in the table
    Just lst -> do
      let scope = getWorldScope id symT
      let boolLst = filter isBoolean lst
      let oldBool = filter (\x -> defBlock x == scope) boolLst !! 0
      let newBool = oldBool{value= getBool bool}
      put(MySymState (Hash.insert boolId (updateBoolLst scope newBool lst) symT) stck err nB)

runTaskInst id inst@(CLEAR p (TKId _ boolId)) =
  runTaskInst id (SETTO p (TKId p boolId) (TKfalse p))

runTaskInst id inst@(FLIP p (TKId _ boolId)) = do
  (MySymState symT stck err nB ) <- get
  case Hash.lookup boolId symT of -- search the bool variable in the table
    Just lst -> do
      let scope = getWorldScope id symT
      let boolLst = filter isBoolean lst
      let oldBool = filter (\x -> defBlock x == scope) boolLst !! 0
      let boolVal = value oldBool
      let newBool = oldBool{value= not boolVal}
      put(MySymState (Hash.insert boolId (updateBoolLst scope newBool lst) symT) stck err nB)


runTaskInst id (TERMINATE _) = do
  checkFinalGoal id
  io $ exitWith $ ExitSuccess

runTaskInst id (INSTRID (l,c) (TKId _ objId)) = do
  (MySymState symT (scope:stck) err nB ) <- get
  case Hash.lookup objId symT of -- search the define block in the table
    Just lst -> do 
      let defLst = filter isInstruction lst
      let defInScope = filter (\x -> defBlock x <= scope) defLst
      let defInst = foldr1 (\x y -> if defBlock x > defBlock y then x else y) defInScope
      pushScope
      runTaskInst id $ inst defInst
      popScope


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
  (MySymState symT stck err nB ) <- get
  case Hash.lookup id symT of -- search the bool variable in the table
    Just lst -> do
      let scope = getWorldScope wid symT          
      let boolLst = filter isBoolOrGuard lst
      let boolVar = filter (\x -> defBlock x == scope) boolLst !! 0
      case boolVar of
        WBoolean{} -> return $ value boolVar
        Goal{} -> evalGoal wid $ test boolVar
  where 
    isBoolOrGuard x = isBoolean x || isGoal x

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
  return $ getBool bool

evalTaskTest id test@(FRONTCLEAR _ ) = checkClear 1 (TURNLEFT (1,1)) id test

evalTaskTest id test@(LEFTCLEAR _ ) = checkClear 0 (TURNLEFT (1,1)) id test

evalTaskTest id test@(RIGHTCLEAR _ ) = checkClear 0 (TURNRIGHT (1,1)) id test

evalTaskTest id (LOOKNORTH _ ) = do
  symT <- gets symTable
  let world = getWorld id symT
  let dir = willyDirection world
  return (dir == "north")

evalTaskTest id (LOOKEAST _ ) = do
  symT <- gets symTable
  let world = getWorld id symT
  let dir = willyDirection world
  return (dir == "east")

evalTaskTest id (LOOKSOUTH _ ) = do
  symT <- gets symTable
  let world = getWorld id symT
  let dir = willyDirection world
  return (dir == "south")

evalTaskTest id (LOOKWEST _ ) = do
  symT <- gets symTable
  let world = getWorld id symT
  let dir = willyDirection world
  return (dir == "west")


evalFinalGoal :: String -> FINALGOAL -> MyStateM (Bool)
evalFinalGoal id (FGAND (l,c) left right) = do
  l <- evalFinalGoal id left
  r <- evalFinalGoal id right
  return (l && r)

evalFinalGoal id (FGOR (l,c) left right) = do
  l <- evalFinalGoal id left
  r <- evalFinalGoal id right
  return (l || r)

evalFinalGoal id (FGNOT (l,c) exp) = do
  e <- evalFinalGoal id exp
  return (not e)

evalFinalGoal wid (FGID (l,c) id) = do
  (MySymState symT stck err nB ) <- get
  case Hash.lookup id' symT of -- search the bool variable in the table
    Just lst -> do
      let scope = getWorldScope wid symT          
      let boolLst = filter isBoolOrGuard lst
      let boolVar = filter (\x -> defBlock x == scope) boolLst !! 0
      case boolVar of
        WBoolean{} -> return $ value boolVar
        Goal{} -> evalGoal wid $ test boolVar
  where 
    isBoolOrGuard x = isBoolean x || isGoal x
    id' = getStr id


evalGoal :: String -> GOALTEST -> MyStateM (Bool)
evalGoal wId (WILLYISAT (l,c) col row ) = do
  (MySymState symT stck err nB ) <- get
  let world = getWorld wId symT
  let pos = willyIsAt world
  return (pos == (c,r))
  where
    c = getValue col
    r = getValue row

evalGoal wId (OBJECTSIN (l,c) n oId) = do
  (MySymState symT stck err nB ) <- get
  let world = getWorld wId symT
  let nObjs = length $ filter (\x -> x == oId') (objectsInB world)
  return (n' <= nObjs)
  where 
    n'   = getValue n
    oId' = getStr oId

evalGoal wId (OBJECTSAT (l,c) n oId col row) = do
  (MySymState symT stck err nB ) <- get
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
  runTaskInst id task
  runRepeat id (l,c) (n-1) task


runWhile :: String -> Pos -> TEST -> TASKINSTR -> MyStateM ()
runWhile id (l,c) guard inst = do
  valid <- evalTaskTest id guard
  if valid then do
    runTaskInst id inst
    runWhile id (l,c) guard inst
  else
    return ()

moveInDir :: Pos -> String -> Pos
moveInDir (x,y) dir = case dir of
  "north" -> (x,y+1)
  "south" -> (x,y-1)
  "west"  -> (x-1,y)
  "east"  -> (x+1,y)


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

checkClear :: Int -> TASKINSTR -> String -> TEST -> MyStateM (Bool)
checkClear front dir id test = do
  (MySymState symT stck err nB ) <- get
  let world = getWorld id symT
  let newD = if (front==1)
               then changeDirection (willyDirection world) (dir)
               else willyDirection world
  let pos = moveInDir (willyIsAt world) newD
  case validStart id pos symT of
    0 -> return (True)
    _ -> return (False)



printWorld :: String -> MyStateM ()
printWorld wId = do
  map <- printMap wId
  io $ putStrLn $ map