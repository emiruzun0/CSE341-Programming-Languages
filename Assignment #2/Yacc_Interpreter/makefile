build: a.out

a.out: gpp_lexer.c
	gcc lex.yy.c y.tab.c -w

gpp_lexer.c: 
	lex gpp_lexer.l
	yacc -d gpp_interpreter.y


clean: 
	rm a.out
	rm lex.yy.c
	rm y.tab.c
	rm y.tab.h