import System.IO
import System.Directory
import System.Environment
import Lexer
import Parser
import AST

--Main function
main = do
    args <- getArgs
    case args of
        [] -> do putStr ( "Archivo a Interpretar: " )
                 hFlush stdout
                 filePath <- getLine
                 if (length $ words filePath) > 1 then do wrongFormatInput
                 else runLexer filePath
        [filePath] -> runLexer filePath
        _ -> wrongFormatInput
    
-- Check if file exist and if so then run the lexer with that file
runLexer :: FilePath -> IO ()
runLexer filePath = do fileExists <- doesFileExist filePath
                       if fileExists then do str <- readFile filePath
                                             showResults (str)
                                             return ()
                       else do putStrLn ( "Imposible abrir el archivo " ++ show filePath )

-- IO action that output a message of error in case of wrong format in the input
wrongFormatInput :: IO ()
wrongFormatInput = do putStrLn ( "Formato incorrecto: demasiados argumentos." )
                      putStrLn ( "Formato correcto 1: \n./willy <archivo>" )
                      putStrLn ( "Formato correcto 2: \n./willy\nArchivo a Interpretar: <archivo>" )

-- Pass the string that represents the program to interpret to the scanner and show the resuts
showResults :: String -> IO ()
showResults str = case scanner str of
                    Left s -> putStr s
                    Right toks -> do print $ reverse $ parse toks
                                     return ()