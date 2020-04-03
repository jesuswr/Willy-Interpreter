module RunTask where

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
import qualified Text.Read as R
import Control.Concurrent
import System.Console.ANSI
import System.IO
import Data.Maybe


-- Gets the task id, a list of blocks and a mode to print the map
-- and tries to run the task if there is no error creating the symbols table
runTask :: String -> [BLOCK] -> String-> MyStateM ()
runTask taskId [] _ = Prelude.error "No hay tareas ni mundos"
runTask taskId blocks mode = do 
  createSymTable blocks
  (MySymState symT stck err nB ) <- get
  case err of
    []   -> do
      evalTask taskId blocks mode
    str  -> do
      io $ putStrLn "\nErrores de contexto:\n"
      io $ putStrLn $ unlines $ reverse err


-- Gets the task id, a list of blocks and a mode to print the map
-- and tries to run the task if it exists
evalTask :: String -> [BLOCK] -> String -> MyStateM ()
evalTask taskId [] _ = Prelude.error "No se encontro la tarea"
evalTask taskId (x:xs) mode = case x of
  WORLD{} -> evalTask taskId xs mode
  (TASK (l,c) (TKId tP tId) (TKId wP wId) tasks) -> do
    if taskId == tId then do
      (MySymState symT stck err nB ) <- get
      io $ setTitle "WILLY*"
      printWorld wId -- print initial world configuration
      let wSc = getWorldScope wId symT
      put(MySymState symT (wSc:stck) err nB) -- push world scope. From this time there is only one world in stack
      pushScope -- push this task  scope. The task is already in symT.
      runTaskInsts wId tasks mode
      (MySymState symT stck err nB ) <- get
      checkFinalGoal wId -- check final goal
    else 
      evalTask taskId xs mode


-- Gets the world id, a list of task instructions and a mode to print the map
-- and runs the instructions
runTaskInsts :: String -> [TASKINSTR] -> String -> MyStateM ()
runTaskInsts id [] _ = return ()
runTaskInsts id (x:xs) mode =  do  -- duda, funciona: runTaskInst x >>= runTaskInsts xs
  runTaskInst id x mode
  runTaskInsts id xs mode


-- Gets the world id and checks if the final goal was accomplished
checkFinalGoal :: String -> MyStateM ()
checkFinalGoal wId = do
  (MySymState symT stck err nB ) <- get
  let world = getWorld wId symT
  success <- evalFinalGoal wId (goal $ finalG world)
  if success then do
    io $ setSGR [SetColor Foreground Vivid Green]
    io $ putStrLn "El final goal del mundo fue logrado"
  else do
    io $ setSGR [SetColor Foreground Vivid Red]
    io $ putStrLn "El final goal del mundo no fue logrado"


-- Gets the world id, a task instruction and a mode to print the map
-- and runs the instruction
runTaskInst :: String ->  TASKINSTR -> String -> MyStateM ()
runTaskInst id (IF (l,c) guard instr) mode = do
  valid <- evalTaskTest id guard
  if valid then do
    pushScope 
    runTaskInst id instr mode
    popScope
  else
    return ()

runTaskInst id (IFELSE (l,c) guard instr1 instr2) mode = do
  (MySymState symT stck err nB ) <- get
  valid <- evalTaskTest id guard
  if valid then do
    pushScope
    runTaskInst id instr1 mode
    popScope

  else do
    pushScope
    runTaskInst id instr2 mode
    popScope

runTaskInst id (REPEAT (l,c) n instr) mode = do
  pushScope
  runRepeat id (l,c) n' instr mode
  popScope
  where
    n' = getValue n

runTaskInst id thisInstr@(WHILE (l,c) guard instr) mode = do
  pushScope
  runWhile id (l,c) guard instr mode
  popScope

runTaskInst id (BEGIN (l,c) instrs) mode = do
  pushScope
  runTaskInsts id instrs mode
  popScope

runTaskInst id (DEFINE (l,c) (TKId _ tId) instr) mode = do
  (MySymState symT (st:stck) err nB ) <- get
  let val = Instruction (l,c) id st (nB+1) instr
  insToTable tId val

runTaskInst id inst@(MOVE _) mode = do
  when (mode /= "a" && mode /= "m") $ io $ sleepSec mode
  when (mode == "m") $ io $ getKeyPress id
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

runTaskInst id inst@(TURNLEFT _) mode = do
  when (mode /= "a" && mode /= "m") $ io $ sleepSec mode
  when (mode == "m") $ io $ getKeyPress id
  (MySymState symT (st:stck) err nB ) <- get
  let d = getWDirection id symT
  let newD = changeDirection d inst
  updDirection id newD
  printWorld id

runTaskInst id inst@(TURNRIGHT _) mode = do
  when (mode /= "a" && mode /= "m") $ io $ sleepSec mode
  when (mode == "m") $ io $ getKeyPress id
  (MySymState symT (st:stck) err nB ) <- get
  let d = getWDirection id symT
  let newD = changeDirection d inst
  updDirection id newD
  printWorld id

runTaskInst id inst@(PICK (l,c) (TKId _ objId)) mode = do
  when (mode /= "a" && mode /= "m") $ io $ sleepSec mode
  when (mode == "m") $ io $ getKeyPress id
  (MySymState symT (st:stck) err nB ) <- get
  case not $ isBasketFull id symT of
    True -> do
      let pos = getWStartPos id symT
      pickObject id objId pos
      insertObject id objId 1 symT
      printWorld id
    False -> 
      Prelude.error "No hay suficiente espacio en basket"

runTaskInst id inst@(DROP (l,c) (TKId _ objId)) mode = do
  when (mode /= "a" && mode /= "m") $ io $ sleepSec mode
  when (mode == "m") $ io $ getKeyPress id
  (MySymState symT (st:stck) err nB ) <- get
  let pos = getWStartPos id symT
  dropObject id objId
  placeObject id objId 1 pos
  printWorld id

runTaskInst id inst@(SET p (TKId _ boolId)) mode =
  runTaskInst id (SETTO p (TKId p boolId) (TKtrue p)) mode

runTaskInst id inst@(SETTO p (TKId _ boolId) bool) mode = do
  (MySymState symT stck err nB ) <- get
  case Hash.lookup boolId symT of -- search the bool variable in the table
    Just lst -> do
      let scope = getWorldScope id symT
      let boolLst = filter isBoolean lst
      let oldBool = filter (\x -> defBlock x == scope) boolLst !! 0
      let newBool = oldBool{value= getBool bool}
      put(MySymState (Hash.insert boolId (updateBoolLst scope newBool lst) symT) stck err nB)

runTaskInst id inst@(CLEAR p (TKId _ boolId)) mode =
  runTaskInst id (SETTO p (TKId p boolId) (TKfalse p)) mode

runTaskInst id inst@(FLIP p (TKId _ boolId)) mode = do
  (MySymState symT stck err nB ) <- get
  case Hash.lookup boolId symT of -- search the bool variable in the table
    Just lst -> do
      let scope = getWorldScope id symT
      let boolLst = filter isBoolean lst
      let oldBool = filter (\x -> defBlock x == scope) boolLst !! 0
      let boolVal = value oldBool
      let newBool = oldBool{value= not boolVal}
      put(MySymState (Hash.insert boolId (updateBoolLst scope newBool lst) symT) stck err nB)


runTaskInst id (TERMINATE _) mode = do
  checkFinalGoal id
  io $ exitWith $ ExitSuccess

runTaskInst id (INSTRID (l,c) (TKId _ objId)) mode = do
  (MySymState symT (scope:stck) err nB ) <- get
  case Hash.lookup objId symT of -- search the define block in the table
    Just lst -> do 
      let defLst = filter isInstruction lst
      let defInScope = filter (\x -> defBlock x <= scope) defLst
      let defInst = foldr1 (\x y -> if defBlock x > defBlock y then x else y) defInScope
      pushScope
      runTaskInst id (inst defInst) mode
      popScope


-- Gets the world id, a test and retuns the evaluation of the test
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

evalTaskTest id (LOOKNORTH _ ) = checkLook id "north"


evalTaskTest id (LOOKEAST _ ) = checkLook id "east"


evalTaskTest id (LOOKSOUTH _ ) = checkLook id "south"


evalTaskTest id (LOOKWEST _ ) = checkLook id "west"


-- Gets the world id, a final goal and returns the evaluation
-- of the final goal
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


-- Gets the world id, a final goal and returns the evaluation
-- of a goal
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


-- Takes the world id, a position, the number of loops, the instruction
-- and a mode to print the map and runs the instrunction n times
runRepeat :: String -> Pos -> Int -> TASKINSTR -> String -> MyStateM ()
runRepeat _ _ 0 _ _ = return ()
runRepeat id (l,c) n task mode = do
  runTaskInst id task mode
  runRepeat id (l,c) (n-1) task mode


-- Takes the world id, a position, the condition of the loop, the instruction
-- and a mode to print the map and runs the instrunction while the condition
-- holds
runWhile :: String -> Pos -> TEST -> TASKINSTR -> String -> MyStateM ()
runWhile id (l,c) guard inst mode = do
  valid <- evalTaskTest id guard
  if valid then do
    runTaskInst id inst mode
    runWhile id (l,c) guard inst mode
  else
    return ()


-- Gets a position and a direction and moves in that direction
moveInDir :: Pos -> String -> Pos
moveInDir (x,y) dir = case dir of
  "north" -> (x,y+1)
  "south" -> (x,y-1)
  "west"  -> (x-1,y)
  "east"  -> (x+1,y)


-- Gets a direction and a instruction to change it and changes the direction
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


-- Gets the world id and an object id and drops it on the current position
dropObject :: String -> String -> MyStateM ()
dropObject wId objId = do
  (MySymState symT stck err nB ) <- get
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


-- Gets the world id, object id, a position and the symbols table
-- and returns the number of objects with that id on that position
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


-- Gets an int (1 to check north or 0 else), a instruction to change dir,
-- a direction, the id of the world and the test you want to check
-- and returns if that direction is clear
checkClear :: Int -> TASKINSTR -> String -> TEST -> MyStateM (Bool)
checkClear front dir id test = do
  (MySymState symT stck err nB ) <- get
  let world = getWorld id symT
  let newD = if (front==0)
               then changeDirection (willyDirection world) (dir)
             else willyDirection world
  let pos = moveInDir (willyIsAt world) newD
  case validStart id pos symT of
    0 -> return (True)
    _ -> return (False)


-- Recieves the id of the world and a direction and
-- returns if willy is looking that way
checkLook :: String -> String -> MyStateM (Bool)
checkLook id dirComp = do
  symT <- gets symTable
  let world = getWorld id symT
  let dir = willyDirection world
  return (dir == dirComp)


-- Gets the world id and prints it
printWorld :: String -> MyStateM ()
printWorld wId = do
  symT <- gets symTable
  let (c,r) = getWSize wId symT
  let maxSpace = length (show (c*r-1)) + 2
  let width = maxSpace * c
  map <- printMap wId
  io $ putStr "\n"
  io $ clearScreen
  io $ setSGR [SetColor Foreground Vivid Yellow]
  io $ putStr $ "\n" ++ replicate (max ((width `div` 2)-9) 0) '#'
  io $ putStr "  WILLY WORLD:  "
  io $ putStrLn $ replicate (max ((width `div` 2)-9) 0) '#'
  io $ setSGR [SetColor Foreground Vivid White]
  io $ putStrLn $ map


-- Function to wait for the user to continue execution
getKeyPress :: String -> IO ()
getKeyPress id = do
  c <- newEmptyMVar 
  hSetBuffering stdin NoBuffering
  putStr "\nPress any key (with ASCII code) to continue or 'e' to exit (WAITING)\n"
  saveCursor
  forkIO $ do
    a <- System.IO.getChar
    putMVar c a       
  (Just x) <- wait 0 c
  if x == 'e'
    then do
      putStrLn "\nEnding execution..."
      exitWith $ ExitSuccess
    else return ()

wait cnt c = do
  a <- tryTakeMVar c
  if isJust a then do
    return (a)
  else do
    let x = 40
    clearLine
    restoreCursor
    putStr $ replicate ((cnt `mod` x)+1) '.'
    hFlush stdout
    threadDelay 500000
    wait (cnt+1 `mod` x) c


-- Function to wait for some seconds before printing
sleepSec :: String -> IO ()
sleepSec sec = do
  secJust <- return $ (R.readMaybe sec :: Maybe Int)
  case secJust of
    (Just seconds) -> do
      let microSeconds = seconds * 1000000
      setSGR [SetColor Foreground Vivid Blue]
      putStr "\nTime: ["
      progressBar (microSeconds)
      setSGR [SetColor Foreground Vivid White]
      return ()
      --threadDelay microSeconds
    otherwise -> return ()


-- Function to show a progress bar while printing the map
progressBar :: Int -> IO ()
progressBar 0 = return ()

progressBar time = do
  let sec = 1000000
  putStr "#"
  when (time==sec) (putStr "]")
  hFlush stdout
  threadDelay sec
  progressBar (time - sec)
  return ()