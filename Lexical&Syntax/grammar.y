%{
	#include <stdio.h>
	#include <stdlib.h>
	#include <string.h>



	void yyerror(const char*);
	int yylex();
	extern FILE * yyin, *yyout;
	
	
	extern int line;
	
	



%}

%token  HASH INCLUDE STDIO STDLIB MATH STRING TIME 
%token  STRING_LITERAL HEADER_LITERAL PRINT RETURN 
%left 	'+' '-'
%left 	'/' '*' '%'
%right 	'='

%union{
	int ival;
	float fval;
	char cval;
	char string[128];
	struct node *ptr;
}


%token 	INTEGER_LITERAL 
%token 	CHARACTER_LITERAL
%token 	FLOAT_LITERAL 
%token	IDENTIFIER  

%token	INC_OP 	DEC_OP 	LE_OP 	GE_OP 	EQ_OP 	NE_OP
%token	MUL_ASSIGN 	DIV_ASSIGN 	MOD_ASSIGN 	ADD_ASSIGN 	SUB_ASSIGN
%token	CHAR 	INT 	FLOAT 	VOID
%token 	FOR 






%start S

%%
S : program 
  	;

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
	| function_call ';'
	| RETURN expression_statement	
	| printstat ';'
	;


printstat
	: PRINT '(' STRING_LITERAL ')' 
	| PRINT '(' STRING_LITERAL ',' expression ')'
	;


declaration
	: type_specifier init_declarator_list ';' 
	;


statement
	: compound_statement 
	| expression_statement
	| iteration_statement
	;


iteration_statement
	: FOR '(' expression_statement expression_statement ')' statement 			
	| FOR '(' expression_statement  expression_statement expression  ')' statement 	
	| FOR '(' declaration expression_statement ')' statement	 				
	| FOR '(' declaration expression_statement expression ')' statement 			
	;


type_specifier
	: VOID 	
	| CHAR 	
	| INT 	
	| FLOAT 
	;


init_declarator_list
	: init_declarator 
	| init_declarator_list ',' init_declarator
	;


init_declarator
	: IDENTIFIER  '=' assignment_expression					
	| IDENTIFIER
	;


assignment_expression
	: conditional_expression	
	| unary_expression assignment_operator assignment_expression 		
	;


assignment_operator
	: '='			
	| ADD_ASSIGN	
	| SUB_ASSIGN	
	| MUL_ASSIGN	
	| DIV_ASSIGN	
	| MOD_ASSIGN	
	;


conditional_expression
	: equality_expression 	
	| equality_expression '?' expression ':' conditional_expression				
	;


expression_statement
	: ';'				
	| expression ';' 	
	;


expression
	: assignment_expression		
	| expression ',' assignment_expression 
	;


primary_expression
	: IDENTIFIER 
	| INTEGER_LITERAL
	| FLOAT_LITERAL	
	| CHARACTER_LITERAL
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


equality_expression
	: relational_expression
	| equality_expression EQ_OP relational_expression
	| equality_expression NE_OP relational_expression
	;


relational_expression
	: additive_expression
	| relational_expression '<' additive_expression
	| relational_expression '>' additive_expression
	| relational_expression LE_OP additive_expression
	| relational_expression GE_OP additive_expression		
	;


additive_expression
	: multiplicative_expression	
	| additive_expression '+' multiplicative_expression 	
	| additive_expression '-' multiplicative_expression					
	;


multiplicative_expression
	: unary_expression			
	| multiplicative_expression '*' unary_expression 						
	| multiplicative_expression '/' unary_expression											
	| multiplicative_expression '%' unary_expression						
	;


function_definition
	: type_specifier declarator compound_statement 					
	| declarator compound_statement 									
	;

function_call
	: declarator '(' identifier_list ')'
	| declarator '(' ')'
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


parameter_declaration
	: type_specifier IDENTIFIER		
	| type_specifier				
	;


identifier_list
	: IDENTIFIER						
	| identifier_list ',' IDENTIFIER	
	;


%%


void yyerror(const char *str){
	fflush(stdout);
	printf("Line:%d: ", line);
	printf("\033[1;31m");
	printf("error: ");
	printf("\033[0m");
	printf("%s\n", str);
}




int main(){
	yyout = fopen("output.c", "w");
	

	printf("\n");
	
	if(!yyparse())
		printf("Successful\n");
	else
		printf("Unsuccessful\n");


	fclose(yyout);
	return 0;
}

