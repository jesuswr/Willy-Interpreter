willy: Lexer
	ghc --make Willy.hs -o willy

Lexer: Lexer.x
	alex Lexer.x

Clear:
	rm *.o 
	rm *.hi