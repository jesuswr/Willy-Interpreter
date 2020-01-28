willy: Lexer
	ghc --make Willy.hs -o willy
	mv ./willy $$HOME/bin

Lexer: Lexer.x
	alex Lexer.x

clear:
	rm willy
	rm *.o 
	rm *.hi