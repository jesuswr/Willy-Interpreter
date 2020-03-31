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
  valid <- evalTaskTest id guard
  if True then do
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
  runRepeat id (l,c) n instr
  popScope

runTaskInst _ _ = return ()

evalTaskTest :: String -> TEST -> MyStateM (Bool)
evalTaskTest _ _ = return (True)

runRepeat :: String -> Pos -> Int -> TASKINSTR -> MyStateM ()
