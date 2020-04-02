willy: Willy.hs ContextChecker.hs AST.hs PrintParser.hs PrintSymTable.hs SymTable.hs RunTask2.hs Simulator.hs
	ghc --make Willy.hs -o willy

Lexer: Lexer.x
	alex Lexer.x

Parser: Parser.y
	happy Parser.y

clear:
	rm willy
	rm *.o 
	rm *.hi