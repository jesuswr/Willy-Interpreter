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
import Text.Read
import qualified Data.Map as Hash

--Main function
main = do
    args <- getArgs
    case args of
        [] -> do putStr ( "Archivo a Interpretar: " )
                 hFlush stdout
                 filePath <- getLine
                 putStr ( "Tarea a ejecutar (modo automatico): " )
                 hFlush stdout
                 taskName <- getLine
                 if (length $ words filePath) > 1 || (length $ words taskName) > 1
                 then do wrongFormatInput
                 else processFile filePath taskName "a"

        [filePath, taskName, mode] -> do
          if( mode == "-a" || mode == "--auto" ) 
            then processFile filePath taskName "a"
          else if ( mode == "-m" || mode == "--manual")
            then processFile filePath taskName "m"
          else wrongFormatInput
          
        [filePath, taskName, mode, sec] -> do
          if( mode /= "-a" && mode /= "--auto" ) then wrongFormatInput
          else do
            case (readMaybe sec :: Maybe Int) of
              (Just x) -> processFile filePath taskName sec
              otherwise -> wrongFormatInput

        _ -> wrongFormatInput
    
-- Check if file exist and if so then run the project with that file
processFile :: FilePath -> String -> String-> IO ()
processFile filePath taskName mode = do 
  fileExists <- doesFileExist filePath
  if fileExists then do 
    str <- readFile filePath
    runProject str taskName mode
    return ()
  else do 
    putStrLn ( "Imposible abrir el archivo " ++ show filePath )

-- IO action that output a message of error in case of wrong format in the input
wrongFormatInput :: IO ()
wrongFormatInput = do 
  putStrLn ( "Formato incorrecto." )
  putStrLn ( "Formato correcto 1: \nwilly <archivo> <tarea> -m | --manual" )
  putStrLn ( "Formato correcto 2: \nwilly <archivo> <tarea> -a | --auto [<segundos>]" )
  putStrLn ( "Formato correcto 3: \nwilly\nArchivo a Interpretar: <archivo>" )
  putStrLn ( "Tarea a ejecutar (modo automatico): <tarea>")

-- Pass the string that represents the program to interpret to the scanner 
-- and show the resuts
runProject :: String -> String -> String -> IO ()
runProject str taskName mode = 
  case scanner str of
    Left s -> putStr s
    Right toks -> do 
      let tk = reverse $ parse toks
      let initTableState = MySymState Hash.empty [0] [] 0
      runStateT (runTask taskName tk mode) initTableState
      return()