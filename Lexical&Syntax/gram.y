%{
	#include <stdio.h>
	#include <stdlib.h>
	#include <string.h>
	void yyerror(const char*);
	int yylex();
	extern FILE * yyin, *yyout;
	
%}


%token  HASH INCLUDE DEFINE STDIO STDLIB MATH STRING TIME

%token	IDENTIFIER INTEGER_LITERAL STRING_LITERAL HEADER_LITERAL FLOAT_LITERAL CHARACTER_LITERAL

%token	INC_OP DEC_OP LE_OP GE_OP EQ_OP NE_OP

%token	MUL_ASSIGN DIV_ASSIGN MOD_ASSIGN ADD_ASSIGN SUB_ASSIGN

%token	CHAR INT FLOAT VOID 

%token	STRUCT

%token	FOR 

%start program

%%

program
	: HASH INCLUDE '<' libraries '>' program
	| HASH INCLUDE HEADER_LITERAL 	 program
	| translation_unit
	;


translation_unit
	: ext_dec
	| translation_unit ext_dec
	;

ext_dec
	: declaration
	| function_definition	
	;

libraries
	: STDIO
	| STDLIB
	| MATH
	| STRING
	| TIME
	;

primary_expression
	: IDENTIFIER
	| INTEGER_LITERAL
	| CHARACTER_LITERAL
	| FLOAT_LITERAL		
	| '(' expression ')'
	;

postfix_expression
	: primary_expression
	| postfix_expression INC_OP
	| postfix_expression DEC_OP
	;

unary_expression
	: postfix_expression
	| unary_operator unary_expression
	;

unary_operator
	: '+'
	| '-'
	| '!'		
	| '~'		
	| "INC_OP"	
	| "DEC_OP" 	
	;

multiplicative_expression
	: unary_expression
	| multiplicative_expression '*' unary_expression
	| multiplicative_expression '/' unary_expression
	| multiplicative_expression '%' unary_expression
	;

additive_expression
	: multiplicative_expression
	| additive_expression '+' multiplicative_expression
	| additive_expression '-' multiplicative_expression
	;

relational_expression
	: additive_expression
	| relational_expression '<' additive_expression
	| relational_expression '>' additive_expression
	| relational_expression LE_OP additive_expression
	| relational_expression GE_OP additive_expression
	;

equality_expression
	: relational_expression
	| equality_expression EQ_OP relational_expression
	| equality_expression NE_OP relational_expression
	;

conditional_expression
	: equality_expression
	| equality_expression '?' expression ':' conditional_expression
	;

assignment_expression
	: conditional_expression
	| unary_expression assignment_operator assignment_expression
	;

assignment_operator
	: '='
	| ADD_ASSIGN
	| SUB_ASSIGN
	;

expression
	: assignment_expression
	| expression ',' assignment_expression
	;

declaration
	: type_specifier init_declarator_list ';'
	;

init_declarator_list
	: init_declarator
	| init_declarator_list ',' init_declarator
	;

init_declarator
	: IDENTIFIER '=' assignment_expression
	| IDENTIFIER
	;

type_specifier
	: VOID
	| CHAR
	| INT
	| FLOAT
	;

statement
	: compound_statement
	| expression_statement
	| iteration_statement
	;

compound_statement
	: '{' '}' 
	| '{' block_item_list '}'
	;

block_item_list
	: block_item
	| block_item_list block_item
	;

block_item
	: declaration
	| statement
	;

expression_statement
	: ';'
	| expression ';'
	;

iteration_statement
	: FOR '(' expression_statement expression_statement ')' statement
	| FOR '(' expression_statement expression_statement expression ')' statement
	| FOR '(' declaration expression_statement ')' statement	
	| FOR '(' declaration expression_statement expression ')' statement
	;
function_definition
	: type_specifier declarator declaration_list compound_statement
	| type_specifier declarator compound_statement 				
	| declarator declaration_list compound_statement 			
	| declarator compound_statement 							
	;


declarator
	: IDENTIFIER        							    
	| declarator '(' parameter_list ')'				
	| declarator '(' identifier_list ')'				
	| declarator '(' ')'				
	;

parameter_list
	: parameter_declaration						
	| parameter_list ',' parameter_declaration	
	;

declaration_list
	: declaration					
	| declaration_list declaration 
	;	

parameter_declaration
	: type_specifier declarator		
	| type_specifier				
	;


identifier_list
	: IDENTIFIER						
	| identifier_list ',' IDENTIFIER	


%%
void yyerror(const char *str)
{
	fflush(stdout);
	fprintf(stderr, "*** %s\n", str);
}
int main(){
	// yyin = fopen("input.c", "r");
	yyout = fopen("output.c", "w");
	if(!yyparse())
		printf("Successful\n");
	else
		printf("Unsuccessful\n");
	fclose(yyout);
	return 0;
}