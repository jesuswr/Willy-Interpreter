import System.IO
import System.Directory
import System.Environment
import Lexer

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
    
runLexer :: FilePath -> IO ()
runLexer filePath = do fileExists <- doesFileExist filePath
                       if fileExists then do str <- readFile filePath
                                             showResults (str)
                                             return ()
                       else do putStrLn ( "Imposible abrir el archivo " ++ show filePath )

wrongFormatInput :: IO ()
wrongFormatInput = do putStrLn ( "Formato incorrecto: demasiados argumentos." )
                      putStrLn ( "Formato correcto 1: \n./Willy <archivo>" )
                      putStrLn ( "Formato correcto 2: \n./Willy\nArchivo a Interpretar: <archivo>" )


showResults :: String -> IO ()
showResults str = case scanner str of
                    Left s -> putStr s
                    Right toks -> do mapM_ putStr $ strTokens toks
                                     return ()