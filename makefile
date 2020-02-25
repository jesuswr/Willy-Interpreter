willy: Lexer Parser Willy.hs ContextChecker.hs AST.hs PrintParser.hs PrintSymTable.hs SymTable.hs
	ghc --make Willy.hs -o willy
	mv ./willy $$HOME/bin

Lexer: Lexer.x
	alex Lexer.x

Parser: Parser.y
	happy Parser.y

clear:
	rm willy
	rm *.o 
	rm *.hi