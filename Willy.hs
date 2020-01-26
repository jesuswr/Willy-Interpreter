import Lexer

main = do
    content <- getContents
    print content
    print( scanner content )
