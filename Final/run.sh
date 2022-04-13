lex ast.l
yacc -d ast.y
gcc lex.yy.c y.tab.c -ll -ly -o ast.o
lex icg.l
yacc -d icg.y
gcc lex.yy.c y.tab.c -ll -ly -o icg.o