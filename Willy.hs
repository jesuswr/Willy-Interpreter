{-
An Interpreter for the subject "Traductores e Interpretadores" (Translators and Interpreters) 
of the Simon Bolivar University (USB).
  
  Authors:
  
  Neil Villamizar  15-11523
  
  Jesus Wahrman    15-11540
-}

import System.IO
import System.Directory
import System.Environment
import Lexer
import Parser
import AST
import PrintParser
import PrintSymTable
import Control.Monad.State
import SymTable
import ContextChecker
import RunTask2
import qualified Data.Map as Hash

--Main function
main = do
    args <- getArgs
    case args of
        [] -> do putStr ( "Archivo a Interpretar: " )
                 hFlush stdout
                 filePath <- getLine
                 putStr ( "Tarea a ejecutar: " )
                 hFlush stdout
                 taskName <- getLine
                 if (length $ words filePath) > 1 || (length $ words taskName) > 1 then do wrongFormatInput
                 else processFile filePath taskName
        [filePath, taskName] -> processFile filePath taskName
        _ -> wrongFormatInput
    
-- Check if file exist and if so then run the project with that file
processFile :: FilePath -> String -> IO ()
processFile filePath taskName = do 
  fileExists <- doesFileExist filePath
  if fileExists then do 
    str <- readFile filePath
    runProject str taskName
    return ()
  else do 
    putStrLn ( "Imposible abrir el archivo " ++ show filePath )

-- IO action that output a message of error in case of wrong format in the input
wrongFormatInput :: IO ()
wrongFormatInput = do 
  putStrLn ( "Formato incorrecto: demasiados argumentos." )
  putStrLn ( "Formato correcto 1: \n./willy <archivo>" )
  putStrLn ( "Formato correcto 2: \n./willy\nArchivo a Interpretar: <archivo>" )

-- Pass the string that represents the program to interpret to the scanner 
-- and show the resuts
runProject :: String -> String -> IO ()
runProject str taskName = 
  case scanner str of
    Left s -> putStr s
    Right toks -> do 
      let tk = reverse $ parse toks
      let initTableState = MySymState Hash.empty [0] [] 0
      --case evalState (createSymTable tk)  initTableState of
      --  Left errorStr -> do
      --    putStrLn "Errores de contexto:"
      --    putStr errorStr
      --  Right symT -> do
      --    putStr $ evalState (printParser tk) (PrintState [] 0)
      --    putStrLn "\n  -  -  - \n"
      --    putStr $ "TABLE:\n\n" ++ printSymTable symT
      runStateT (runTask taskName tk) initTableState
      return()