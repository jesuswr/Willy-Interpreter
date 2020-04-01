module RunTask where

import AST
import Lexer
import Control.Monad.State
import Control.Monad.Trans
import SymTable
import ContextChecker
import qualified Data.Map as Hash



runTask :: String -> [BLOCK] -> MyStateM (Either String SymTable)
runTask taskId [] = return $ Left ("No se encontro la Tarea " ++ taskId)
runTask taskId blocks = do 
  --let initTableState = MySymState Hash.empty [0] [] 0 -- esto se manda desde el main
  --case createSymTable blocks of
  --  Left errorStr -> do
  --    return $ Left $ "Errores de contexto:\n" ++ errorStr
  --  Right symT -> do
  --    return $ evalTask taskId blocks 
  createSymTable blocks
    

evalTask :: String -> [BLOCK] -> MyStateM (Either String SymTable)
evalTask taskId [] = return $ Left ("No se encontro la Tarea " ++ taskId)
evalTask taskId (x:xs) = case x of
  WORLD{} -> evalTask taskId xs
  (TASK (l,c) (TKId tP tId) (TKId wP wId) tasks) -> do
    (MySymState symT stck err nB ) <- get
    let wSc = getWorldScope wId symT
    put(MySymState symT (wSc:stck) err nB) -- push world scope. From this time there is only one world in stack
    pushScope -- push this task  scope. The task is already in symT.
    if checkFinalGoalAPI
      then return $ Right 0
      else return $ runTaskInsts wId task
    --popScope
    --popScope
                            

runTaskInsts :: String -> [TASKINSTR] -> MyStateM (Either String SymTable)
runTaskInsts id [] = return (Right 0)
runTaskInsts id (x:xs) =  do  -- duda, funciona: runTaskInst x >>= runTaskInsts xs
  case runTaskInst id x of
    Left str -> return $ Left str
    Right n -> runTaskInsts id xs

runTaskInst :: String ->  TASKINSTR -> MyStateM (Either String SymTable)
runTaskInst id (IF (l,c) guard instr ) = do
  if evalTaskTest id guard
    then do
      pushScope 
      case runTaskInst id instr of
        Left str -> return $ Left str
        _ -> popScope >> return (Right 0)
    else return (Right 0)

runTaskInst id (IFELSE (l,c) guard instr1 instr2) = do
  if evalTaskTest id guard
    then do
      pushScope
      case runTaskInst id instr1 of
        Left str -> return $ Left str
        _ -> popScope >> return (Right 0)
    else do
      pushScope
      case runTaskInst id instr2 of
        Left str -> return $ Left str
        _ -> popScope >> return (Right 0)

runTaskInst id (REPEAT (l,c) n instr) = do
  pushScope
  if n > 0
    then do
      case runTaskInst id instr of
        Left str -> return $ Left str
        _ -> runTaskInst id (REPEAT (l,c) (n-1) instr)
    else popScope >> return (Right 0)

runTaskInst id thisInstr@(WHILE (l,c) guard instr) = do
  if evalTaskTest id guard
    then do
      pushScope
      case runTaskInst id instr of
        Left str -> return $ Left str
        _ -> runTaskInst id thisInstr
    else popScope >> return (Right 0)

runTaskInst id (BEGIN (l,c) instrs) = do
  pushScope
  case runTaskInsts id instrs of
    Left str -> return $ Left str
    _ -> popScope >> return (Right 0)

runTaskInst wid (DEFINE (l,c) (TKId _ id) instr) = do
  (MySymState symT (st:stck) err nB ) <- get
  let val = Instruction (l,c) id st (nB+1) instr
  insToTable id val
  return $ Right 0

runTaskInst id inst@(MOVE _) = do
  -- if mode == X seconds then sleep else return ()
  -- if mode == user enter then wait(enter) else return ()
  checkAndSimAPI id inst -- call the API

runTaskInst id inst@(TURNLEFT _) = do
  -- if mode == X seconds then sleep else return ()
  -- if mode == user enter then wait(enter) else return ()
  checkAndSimAPI id inst -- call the API

runTaskInst id inst@(TURNRIGHT _) = do
  -- if mode == X seconds then sleep else return ()
  -- if mode == user enter then wait(enter) else return ()
  checkAndSimAPI  id inst -- call the API

runTaskInst id inst@(PICK (l,c) (TKId _ objId)) = do
  -- if mode == X seconds then sleep else return ()
  -- if mode == user enter then wait(enter) else return ()
  checkAndSimAPI  id inst -- call the API

runTaskInst id inst@(DROP (l,c) (TKId _ objId)) = do
  -- if mode == X seconds then sleep else return ()
  -- if mode == user enter then wait(enter) else return ()
  checkAndSimAPI  id inst -- call the API

runTaskInst id inst@(SET (l,c) (TKId _ objId)) = do
  -- if mode == X seconds then sleep else return ()
  -- if mode == user enter then wait(enter) else return ()
  checkAndSimAPI  id inst -- call the API

runTaskInst id inst@(SETTO (l,c) (TKId _ objId) bool) = do
  -- if mode == X seconds then sleep else return ()
  -- if mode == user enter then wait(enter) else return ()
  checkAndSimAPI  id inst -- call the API

runTaskInst id inst@(CLEAR (l,c) (TKId _ objId)) = do
  -- if mode == X seconds then sleep else return ()
  -- if mode == user enter then wait(enter) else return ()
  checkAndSimAPI  id inst -- call the API

runTaskInst id inst@(FLIP (l,c) (TKId _ objId)) = do
  -- if mode == X seconds then sleep else return ()
  -- if mode == user enter then wait(enter) else return ()
  checkAndSimAPI  id inst -- call the API

-- terminate exec without reaching final goal
runTaskInst id (TERMINATE _) = return $ Left "TERMINATE" 

-- Run the define block associated with the ID
runTaskInst id (INSTRID (l,c) (TKId _ objId)) = do
  (MySymState symT (scope:stck) err nB ) <- get
  case Hash.lookup id symT of -- search the define block in the table
    Just lst -> do 
      let defLst = filter isInstruction lst
      let defInst = filter (\x -> defBlock x == scope) defLst !! 0
      pushScope
      case runTaskInst id $ inst defInst of
        Left str -> return $ Left str
        _ -> popScope >> return $ Right 0
    Nothing -> return $ Left "identificador no encontrado en la tabla de simbolos" -- this should never happend



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
  case Hash.lookup id symT of -- search the id in the table
    Just lst -> do 
      let l = filter (\x -> elem (defBlock x) stck) lst
      let test = filter isBoolOrGuard l !! 0
      case test of
        WBoolean{} -> return $ value test
        Goal{} -> evalGoalAPI id test -- Goal se puede usar en Task?
    Nothing -> return $ Left "Identificador no encontrado" -- Should never happend

evalTaskTest wid (FOUND (l,c) (TKId _ id)) = do
  let world = getWorld wid
  let (x,y) = willyIsAt world
  let grid = desc world
  case Hash.lookup (x,y) grid of
    Just (Objects obj) -> case Hash.lookup id obj of
      Just x  -> return (x > 0) -- Found an obj of type id
      Nothing -> return False
    Just Wall -> return False
    Nothing -> return False

evalTaskTest wid (CARRYING (l,c) (TKId _ id)) = do
  let world = getWorld wid
  let objs  = objectsInB world
  return $ elem id objs

evalTaskTest id (TESTTOF _ bool) = return $ getBool bool

evalTaskTest id test@(FRONTCLEAR _ ) = checkClear id test 0
  
evalTaskTest id test@(LEFTCLEAR _ ) = checkClear id test 3

evalTaskTest id test@(RIGHTCLEAR _ ) = checkClear id test 1

evalTaskTest id (LOOKNORTH _ ) = do
  let world = getWorld id
  let dir = willyDirection world
  return (dir == "north")

evalTaskTest id (LOOKEAST _ ) = do
  let world = getWorld id
  let dir = willyDirection world
  return (dir == "east")

evalTaskTest id (LOOKSOUTH _ ) = do
  let world = getWorld id
  let dir = willyDirection world
  return (dir == "south")

evalTaskTest id (LOOKWEST _ ) = do
  let world = getWorld id
  let dir = willyDirection world
  return (dir == "west")




-- Utilities
getWorld :: String -> SymValue
getWorld id = case Hash.lookup id symT of 
  Just listVal -> filter isWorld listVal !! 0
  otherwise -> World (1,1) id 0 0 Hash.empty (1,1) 1 [] (-1,-1) "north" None

newPos :: Int -> String -> (Int, Int) -> (Int, Int)
newPos mod dir (x,y)
  | dir == "north" = ans !! (mod `mod` 4)
  | dir == "east"  = ans !! ((mod + 1) `mod` 4)
  | dir == "south" = ans !! ((mod + 2) `mod` 4)
  | dir == "west"  = ans !! ((mod + 3) `mod` 4)
  where ans = [(x,y+1),(x+1,y),(x,y-1),(x-1,y)]

isInGrid :: (Int, Int) -> (Int, Int) -> Bool
isInGrid (xLim, yLim) (x, y) = 
  x*y>0 && x<=xLim && y<=yLim

checkClear :: String -> TEST -> Int -> MyStateM (Bool)
checkClear id _ d = do
  let world = getWorld id
  let sizeW = size world
  let (x,y) = willyIsAt world
  let grid = desc world
  let dir = willyDirection world
  let newp = newPos d dir (x,y)
  case Hash.lookup newp grid of
    Just Wall -> return False
    otherwise -> return $ isInGrid sizeW newp



io :: IO a -> MyStateM a
io = liftIO 