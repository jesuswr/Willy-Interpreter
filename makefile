willy: Lexer
	ghc --make Willy.hs -o willy

Lexer: Lexer.x
	alex Lexer.x

clear:
	rm willy
	rm *.o 
	rm *.hi