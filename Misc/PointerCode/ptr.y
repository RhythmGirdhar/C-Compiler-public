%{
	#include <stdio.h>
	#include <stdlib.h>
	#include <string.h>
	#include <limits.h>
	

	void yyerror(const char*);
	int yylex();
	extern FILE * yyin, *yyout;

	int x=0;
	

	extern int line;
	extern int line;
	int scope = 0;

	int unaryop = -1;
	int assignop = -1;
	int datatype = -1;

	struct node{
		char name[30];
		int dtype;
		int scope;
		int lineno;
		int valid;
		union value{
			float f;
			int i;
			char c;
		}val;
		
		struct node *link;

	}*first = NULL, *tmp, *crt;

	struct node * checksym(char *);
	void addsymbol(struct node *,char *);
	void addInt(struct node *, int, int);
	void addFloat(struct node *, int, float);
	void addChar(struct node *, int, char);
	void checkvalid(struct node *);
	void printsymtable();
	struct node* fv(struct node *);
	void invalidate();


%}

%token  HASH INCLUDE STDIO STDLIB MATH STRING TIME 
%token  STRING_LITERAL HEADER_LITERAL PRINT
%left 	'+' '-'
%left 	'/' '*'
%right 	'='

%union{
	int ival;
	float fval;
	char cval;
	// char *str;
	struct node *ptr;
}


%token <ival> 	INTEGER_LITERAL 
%token <cval> 	CHARACTER_LITERAL
%token <fval> 	FLOAT_LITERAL 
%token <ptr> 	IDENTIFIER 

%token	INC_OP 	DEC_OP 	LE_OP 	GE_OP 	EQ_OP 	NE_OP
%token	MUL_ASSIGN 	DIV_ASSIGN 	MOD_ASSIGN 	ADD_ASSIGN 	SUB_ASSIGN
%token	<ival> 	CHAR 	INT 	FLOAT 	VOID
%token 	MAIN 	FOR 


%type <ptr>		postfix_expression 	
%type <ptr>		additive_expression 
%type <ptr>		multiplicative_expression 
%type <ptr>		primary_expression 
%type <ptr>		unary_expression 		//unary_operator
%type <ptr>		assignment_expression 	//assignment_operator 
%type <ptr>		equality_expression 
%type <ptr>		relational_expression 


%type <ptr>		conditional_expression
%type <ptr>		expression 				expression_statement



%start S

%%
S : program {	/*
				Todo: 
				Write a function to clean symbol symbol. Remove entries with dtype = -1
				Remove entries with name "tmp"
				cleansymbol();
				*/
                printsymtable();
                return 0;
            }

program: 
	  ext_dec
	| program ext_dec
	;

ext_dec
	: INT MAIN '(' ')' compound_statement
	| VOID MAIN '(' ')' compound_statement
	| declaration
	| headers 	
	;

headers
	: HASH INCLUDE HEADER_LITERAL 
	| HASH INCLUDE '<' libraries '>'
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
	;

declaration
	: type_specifier init_declarator_list ';'
	;

statement
	: compound_statement {
						struct node *ftp;
						ftp = first;
						while(ftp!=NULL){
							if(ftp->scope == scope && ftp->valid == 1){
								ftp = ftp->link;
							}
							ftp=ftp->link;
						}
						scope--;
					}
	| expression_statement
	| iteration_statement
	;

iteration_statement
	: FOR '(' expression_statement expression_statement ')' statement
	| FOR '(' expression_statement expression_statement expression ')' statement
	| FOR '(' declaration expression_statement ')' statement	
	| FOR '(' declaration expression_statement expression ')' statement
	;

type_specifier
	: VOID 	{	datatype = $1; }	
	| CHAR 	{	datatype = $1; }	
	| INT 	{	datatype = $1; }	
	| FLOAT {	datatype = $1; }	
	;

init_declarator_list
	: init_declarator 
	| init_declarator_list ',' init_declarator
	;

init_declarator
	: IDENTIFIER '=' assignment_expression
					{
						if($1->dtype!=-1)
							printf("Line:%d: error: Redefinition of \'%s\' \n",line, $1->name);
						else{
							if (datatype == 0 && $3->dtype == 0){	
								
								addInt($1, 0, $3->val.i);
							}
							else if(datatype == 0 && $3->dtype == 1){
								printf("Line:%d: Warning: Implicit type conversion from \'float\' to \'int\' \n", line);
								addInt($1, 0, (int)$3->val.f);
							}
							else if(datatype == 0 && $3->dtype == 2){
								addInt($1, 0, (int)$3->val.c);
							}
							else if(datatype == 1 && $3->dtype == 1){
								
								addFloat($1, 1, $3->val.f);
							}
							else if(datatype == 1 && $3->dtype == 0){

								addFloat($1, 1, (float)$3->val.i);
							}
							else if(datatype == 2 && $3->dtype == 2){
								
								addChar($1, 2, $3->val.c);
							}
							else if(datatype == 2 && $3->dtype == 0){
								
								addChar($1, 2, (char)$3->val.i);
							}
							else{
								printf("Line:%d: Warning: Implicit type conversion\n", line);


								/*
									Todo:
									Delete it from symbol table. This entry should not be created.
									For eg : float f = 'f';

									deleteNode(name);

								*/
							}
							
							x = datatype;
							// datatype = -1;
						}
						
					}	

	| IDENTIFIER	{	
						if($1->dtype!=-1){
							printf("Line:%d: error: Redefinition of \'%s\' \n",line, $1->name);
						}else{

							if (datatype == 0){	
								addInt($1, 0, INT_MIN);
							}
							else if(datatype == 1){
								addFloat($1, 1, INT_MIN);
							}
							else if(datatype == 2){
								addChar($1, 2, '-');
		
							}
							x = datatype;
							// datatype = -1;
						}
					}
	;

/*
Todo: 
It is important for unary_expression | $1 to be declared.
Check if it is declared. Implement a search function.
Update unary_expression value based on it's datatype
*/
assignment_expression
	: conditional_expression	{	$$ = $1;	}
	| unary_expression assignment_operator assignment_expression
			{
				switch(assignop){
					case 0: if($1->dtype == -1){
								printf("Line:%d: error: Use of undeclared variable \'%s\' \n", line, $1->name);
							}
							else{
								if($1->dtype == 0){
									if($3->dtype == 0)
										$1->val.i = $3->val.i;
									else if($3->dtype == 1)
										$1->val.i = (int)$3->val.f;
								}
								else if($1->dtype == 1){
									if($3->dtype == 0)
										$1->val.f = $3->val.i;
									else if($3->dtype == 1)
										$1->val.f = $3->val.f;
								}
								else if($1->dtype == 2){
									if($3->dtype == 0)
										$1->val.c = (char)$3->val.i;
									else if($3->dtype == 2)
										$1->val.c = $3->val.c;
								}
								else{
									printf("Line:%d: error: Invalid datatype\n", line);
								}
							}

							$$ = $1;		
							break;
					case 1:	
							if($1->dtype == -1){
								printf("Line:%d: error: Use of undeclared variable \'%s\' \n", line, $1->name);
							}
							else{
								if($1->dtype == 0){
									if($3->dtype == 0)
										$1->val.i += $3->val.i;
									else if($3->dtype == 1)
										$1->val.i += (int)$3->val.f;
								}
								else if($1->dtype == 1){
									if($3->dtype == 0)
										$1->val.f += $3->val.i;
									else if($3->dtype == 1)
										$1->val.f += $3->val.f;
								}
								else if($1->dtype == 2){
									if($3->dtype == 0)
										$1->val.c += (char)$3->val.i;
									else if($3->dtype == 2)
										$1->val.c += $3->val.c;
								}
								else{
									printf("Line:%d: error: Invalid datatype\n", line);
								}
							}

							$$ = $1;
							break;

					case 2:	if($1->dtype == -1){
								printf("Line:%d: error: Use of undeclared variable \'%s\' \n", line, $1->name);
							}
							else{
								if($1->dtype == 0){
									if($3->dtype == 0)
										$1->val.i -= $3->val.i;
									else if($3->dtype == 1)
										$1->val.i -= (int)$3->val.f;
								}
								else if($1->dtype == 1){
									if($3->dtype == 0)
										$1->val.f -= $3->val.i;
									else if($3->dtype == 1)
										$1->val.f -= $3->val.f;
								}
								else if($1->dtype == 2){
									if($3->dtype == 0)
										$1->val.c -= (char)$3->val.i;
									else if($3->dtype == 2)
										$1->val.c -= $3->val.c;
								}
								else{
									printf("Line:%d: Invalid datatype\n", line);
								}
							}

							$$ = $1;
							break;
					
				}
				assignop = -1;
			}
	;


assignment_operator
	: '='			{	assignop = 0;	}
	| ADD_ASSIGN	{	assignop = 1;	}
	| SUB_ASSIGN	{	assignop = 2;	}
	;

//can add *= /=

conditional_expression
	: equality_expression	{	$$ = $1;	} 
	| equality_expression '?' expression ':' conditional_expression	
			{
				if($1->dtype == 0){
					if($1->val.i == 1){		//true
						if($3->dtype == 0){	//int
							$$->dtype = 0;
							$$->val.i = $3->val.i;
						}
						else if($3->dtype == 1){	//float
							$$->dtype = 1;
							$$->val.f = $3->val.f;
						}
						else if($3->dtype == 2){	//char
							$$->dtype = 2;
							$$->val.c = $3->val.c;
						}
					}
					else if($1->val.i == 0){	//false
						if($5->dtype == 0){		
							$$->dtype = 0;
							$$->val.i = $5->val.i;
						}
						else if($5->dtype == 1){
							$$->dtype = 1;
							$$->val.f = $5->val.f;
						}
						else if($5->dtype == 2){
							$$->dtype = 2;
							$$->val.c = $5->val.c;
						}
					}
				}
				else{
					printf("Line:%d: error: Invalid expression\n", line);
				}
			}
	;


expression_statement
	: ';'				{		}
	| expression ';' 	{	$$ = $1;	}
	;

expression
	: assignment_expression					{	$$ = $1;	}
	| expression ',' assignment_expression	{	$$ = $3;	}
	;

primary_expression 
	: IDENTIFIER
			{   if($1->dtype==-1)	{
                        printf("Line:%d: error: Use of undeclared identifier \'%s\' \n", line, $1->name);
           
                        /*
                        	Todo:
							It is undeclared. No need to put in symbol table.
							deleteNode($1->name);
                        */

 	                    //free($1);
                        // return 0;
                    }
                else{
                	
                	//It is already declared
              
          			if($1->dtype == 0){
          				$$->dtype = 0;
          				$$->val.i = $1->val.i;
          			}
          			else if($1->dtype == 1){
          				$$->dtype = 1;
          				$$->val.f = $1->val.f;
          			}
          			else if($1->dtype == 2){
          				$$->dtype = 2;
          				$$->val.c = $1->val.c;
          			}   
                }
            }
	| INTEGER_LITERAL
			{
               	$$ = checksym("tmp");
                $$->val.i = $1; 
                $$->dtype = 0;
               
            }
	| FLOAT_LITERAL	
			{
                $$ = checksym("tmp");
                $$->val.f = $1;
                $$->dtype = 1;
            }
    | CHARACTER_LITERAL
			{
                $$ = checksym("tmp");
                $$->val.c = $1;
                $$->dtype = 2;
            }
	| '(' expression ')'		{	$$ = $2;	}
	;


postfix_expression
	: primary_expression		
			{	
				// if($1->dtype == -1){
				// 	printf("Line:%d: error: Use of undeclared identifier \'%s\' \n", line, $1->name);
				// }
				// else if($1->dtype == 0){
				// 	$$->dtype = 0;
				// 	$$->val.i = $1->val.i;
				// 	// printf("Check : %d\n", $$->val.i);
				// }
				// else if($1->dtype == 1){
				// 	$$->dtype = 1;
				// 	$$->val.f = $1->val.f;
				// }
				// else if($1->dtype == 2){
				// 	$$->dtype = 2;
				// 	$$->val.c = $1->val.c;
				// }

				$$ = $1;
			}
	| postfix_expression INC_OP	
			{
				if($1->dtype == 0){
					$$->dtype = 0;
					$$->val.i = ($1->val.i)++;
				}
				else if($1->dtype == 1){
					$$->dtype = 1;
					$$->val.f = ($1->val.f)++;
				}
				else if($1->dtype == 2){
					$$->dtype = 2;
					$$->val.c = ($1->val.c)++;
				}
			}
	| postfix_expression DEC_OP 
			{
				if($1->dtype == 0){
					$$->dtype = 0;
					$$->val.i = ($1->val.i)--;
				}
				else if($1->dtype == 1){
					$$->dtype = 1;
					$$->val.f = ($1->val.f)--;
				}
				else if($1->dtype == 2){
					$$->dtype = 2;
					$$->val.c = ($1->val.c)--;
				}
			}
	;


unary_expression
	: postfix_expression	
			{	
				// if($1->dtype == -1){
				// 	printf("Line:%d: error: Use of undeclared identifier \'%s\' \n", line, $1->name);
				// }
				// else if($1->dtype == 0){
				// 	$$->dtype = 0;
				// 	$$->val.i = $1->val.i;
				// }
				// else if($1->dtype == 1){
				// 	$$->dtype = 1;
				// 	$$->val.f = $1->val.f;
				// }
				// else if($1->dtype == 2){
				// 	$$->dtype = 2;
				// 	$$->val.c = $1->val.c;
				// }

				$$ = $1;
			}
	| unary_operator unary_expression 
				{
					switch(unaryop){
						case 1:	$$ = $2;
								break;

						case 2:	if($2->dtype == 0){
									$$->dtype = 0;
									$$->val.i = -($2->val.i);
								}
								else if($2->dtype == 1){
									$$->dtype = 1;
									$$->val.f = -($2->val.f);
								}
								else if($2->dtype == 2){
									$$->dtype = 2;
									$$->val.c = -($2->val.c);
								}
								break;			//if dtype == -1

						case 3:	if($2->dtype == 0){
									$$->dtype = 0;
									$$->val.i = !($2->val.i);
								}
								else if($2->dtype == 1){
									$$->dtype = 1;
									$$->val.f = !($2->val.f);
								}
								else if($2->dtype == 2){
									$$->dtype = 2;
									$$->val.c = !($2->val.c);
								}
								break;

						case 4:	if($2->dtype == 0){
									$$->dtype = 0;
									$$->val.i = ++($2->val.i);
								}
								else if($2->dtype == 1){
									$$->dtype = 1;
									$$->val.f = ++($2->val.f);
								}
								else if($2->dtype == 2){
									$$->dtype = 2;
									$$->val.c = ++($2->val.c);
								}
								break;

						case 5:	if($2->dtype == 0){
									$$->dtype = 0;
									$$->val.i = --($2->val.i);
								}
								else if($2->dtype == 1){
									$$->dtype = 1;
									$$->val.f = --($2->val.f);
								}
								else if($2->dtype == 2){
									$$->dtype = 2;
									$$->val.c = --($2->val.c);
								}
								break;
					
					}
					unaryop = -1;
				}
	;


unary_operator
	: '+' 		{	unaryop = 1;	}
	| '-'		{	unaryop = 2;	}
	| '!'		{	unaryop = 3;	}
	| INC_OP	{	unaryop = 4;	}
	| DEC_OP	{	unaryop = 5;	}
	;


equality_expression
	: relational_expression 	
			{	
				// if($1->dtype == -1){
				// 	printf("Line:%d: error: Use of undeclared identifier \'%s\' \n", line, $1->name);
				// }
				// else if($1->dtype == 0){
				// 	$$->dtype = 0;
				// 	$$->val.i = $1->val.i;

				// 	printf("CHECK 2 : \n%d \n", $1->val.i);
				// }
				// else if($1->dtype == 1){
				// 	$$->dtype = 1;
				// 	$$->val.f = $1->val.f;
				// }
				// else if($1->dtype == 2){
				// 	$$->dtype = 2;
				// 	$$->val.c = $1->val.c;
				// }

				$$ = $1;
			}
	| equality_expression EQ_OP relational_expression
				{
					$$->dtype=0;

					if($1->dtype == 0 && $3->dtype == 0){
						if($1->val.i == $3->val.i)
							$$->val.i = 1;
						else
							$$->val.i = 0;
					}
					else if($1->dtype == 0 && $3->dtype == 1){
						if((float)$1->val.i == $3->val.f)
							$$->val.i = 1;
						else
							$$->val.i = 0;
					}else if($1->dtype == 1 && $3->dtype == 0){
						if($1->val.f == (float)$3->val.i)
							$$->val.i = 1;
						else
							$$->val.i = 0;
					}
					else if($1->dtype == 1 && $3->dtype == 1){
						if($1->val.f == $3->val.f)
							$$->val.i = 1;
						else
							$$->val.i = 0;
					}
					else if($1->dtype == 0 && $3->dtype == 2){
						if($1->val.i == (int)$3->val.c)
							$$->val.i = 1;
						else
							$$->val.i = 0;
					}
					else if($1->dtype == 2 && $3->dtype == 0){
						if((int)$1->val.c == $3->val.i)
							$$->val.i = 1;
						else
							$$->val.i = 0;
					}
					else if($1->dtype == 2 && $3->dtype == 2){
						if((int)$1->val.c == (int)$3->val.c)
							$$->val.i = 1;
						else
							$$->val.i = 0;
					}
				}
	| equality_expression NE_OP relational_expression
				{
					$$->dtype=0;

					if($1->dtype == 0 && $3->dtype == 0){
						if($1->val.i != $3->val.i)
							$$->val.i = 1;
						else
							$$->val.i = 0;
					}
					else if($1->dtype == 0 && $3->dtype == 1){
						if((float)$1->val.i != $3->val.f)
							$$->val.i = 1;
						else
							$$->val.i = 0;
					}else if($1->dtype == 1 && $3->dtype == 0){
						if($1->val.f != (float)$3->val.i)
							$$->val.i = 1;
						else
							$$->val.i = 0;
					}
					else if($1->dtype == 1 && $3->dtype == 1){
						if($1->val.f != $3->val.f)
							$$->val.i = 1;
						else
							$$->val.i = 0;
					}
					else if($1->dtype == 0 && $3->dtype == 2){
						if($1->val.i != (int)$3->val.c)
							$$->val.i = 1;
						else
							$$->val.i = 0;
					}
					else if($1->dtype == 2 && $3->dtype == 0){
						if((int)$1->val.c != $3->val.i)
							$$->val.i = 1;
						else
							$$->val.i = 0;
					}
					else if($1->dtype == 2 && $3->dtype == 2){
						if((int)$1->val.c != (int)$3->val.c)
							$$->val.i = 1;
						else
							$$->val.i = 0;
					}
				}
	;


relational_expression
	: additive_expression	
			{	
				// if($1->dtype == -1){
				// 	printf("Line:%d: error: Use of undeclared identifier \'%s\' \n", line, $1->name);
				// }
				// else if($1->dtype == 0){
				// 	$$->dtype = 0;
				// 	$$->val.i = $1->val.i;
				// 	printf("CHECK 3 : \n%d \n", $1->val.i);
				// }
				// else if($1->dtype == 1){
				// 	$$->dtype = 1;
				// 	$$->val.f = $1->val.f;
				// }
				// else if($1->dtype == 2){
				// 	$$->dtype = 2;
				// 	$$->val.c = $1->val.c;
				// }
				$$ = $1;

			}
	| relational_expression '<' additive_expression
				{
					$$->dtype=0;

					if($1->dtype == 0 && $3->dtype == 0){
						if($1->val.i < $3->val.i)
							$$->val.i = 1;
						else
							$$->val.i = 0;
					}
					else if($1->dtype == 0 && $3->dtype == 1){
						if((float)$1->val.i < $3->val.f)
							$$->val.i = 1;
						else
							$$->val.i = 0;
					}else if($1->dtype == 1 && $3->dtype == 0){
						if($1->val.f < (float)$3->val.i)
							$$->val.i = 1;
						else
							$$->val.i = 0;
					}
					else if($1->dtype == 1 && $3->dtype == 1){
						if($1->val.f < $3->val.f)
							$$->val.i = 1;
						else
							$$->val.i = 0;
					}
					else if($1->dtype == 0 && $3->dtype == 2){
						if($1->val.i < (int)$3->val.c)
							$$->val.i = 1;
						else
							$$->val.i = 0;
					}
					else if($1->dtype == 2 && $3->dtype == 0){
						if((int)$1->val.c < $3->val.i)
							$$->val.i = 1;
						else
							$$->val.i = 0;
					}
					else if($1->dtype == 2 && $3->dtype == 2){
						if((int)$1->val.c < (int)$3->val.c)
							$$->val.i = 1;
						else
							$$->val.i = 0;
					}

					//else invalid datatype??
				}
	| relational_expression '>' additive_expression
				{
					$$->dtype=0;

					if($1->dtype == 0 && $3->dtype == 0){
						if($1->val.i > $3->val.i)
							$$->val.i = 1;
						else
							$$->val.i = 0;
					}
					else if($1->dtype == 0 && $3->dtype == 1){
						if((float)$1->val.i > $3->val.f)
							$$->val.i = 1;
						else
							$$->val.i = 0;
					}else if($1->dtype == 1 && $3->dtype == 0){
						if($1->val.f > (float)$3->val.i)
							$$->val.i = 1;
						else
							$$->val.i = 0;
					}
					else if($1->dtype == 1 && $3->dtype == 1){
						if($1->val.f > $3->val.f)
							$$->val.i = 1;
						else
							$$->val.i = 0;
					}
					else if($1->dtype == 0 && $3->dtype == 2){
						if($1->val.i > (int)$3->val.c)
							$$->val.i = 1;
						else
							$$->val.i = 0;
					}
					else if($1->dtype == 2 && $3->dtype == 0){
						if((int)$1->val.c > $3->val.i)
							$$->val.i = 1;
						else
							$$->val.i = 0;
					}
					else if($1->dtype == 2 && $3->dtype == 2){
						if((int)$1->val.c > (int)$3->val.c)
							$$->val.i = 1;
						else
							$$->val.i = 0;
					}
				}

	| relational_expression LE_OP additive_expression
				{
					$$->dtype=0;

					if($1->dtype == 0 && $3->dtype == 0){
						if($1->val.i <= $3->val.i)
							$$->val.i = 1;
						else
							$$->val.i = 0;
					}
					else if($1->dtype == 0 && $3->dtype == 1){
						if((float)$1->val.i <= $3->val.f)
							$$->val.i = 1;
						else
							$$->val.i = 0;
					}else if($1->dtype == 1 && $3->dtype == 0){
						if($1->val.f <= (float)$3->val.i)
							$$->val.i = 1;
						else
							$$->val.i = 0;
					}
					else if($1->dtype == 1 && $3->dtype == 1){
						if($1->val.f <= $3->val.f)
							$$->val.i = 1;
						else
							$$->val.i = 0;
					}
					else if($1->dtype == 0 && $3->dtype == 2){
						if($1->val.i <= (int)$3->val.c)
							$$->val.i = 1;
						else
							$$->val.i = 0;
					}
					else if($1->dtype == 2 && $3->dtype == 0){
						if((int)$1->val.c <= $3->val.i)
							$$->val.i = 1;
						else
							$$->val.i = 0;
					}
					else if($1->dtype == 2 && $3->dtype == 2){
						if((int)$1->val.c <= (int)$3->val.c)
							$$->val.i = 1;
						else
							$$->val.i = 0;
					}
				}
	| relational_expression GE_OP additive_expression
				{
					$$->dtype=0;

					if($1->dtype == 0 && $3->dtype == 0){
						if($1->val.i >= $3->val.i)
							$$->val.i = 1;
						else
							$$->val.i = 0;
					}
					else if($1->dtype == 0 && $3->dtype == 1){
						if((float)$1->val.i >= $3->val.f)
							$$->val.i = 1;
						else
							$$->val.i = 0;
					}else if($1->dtype == 1 && $3->dtype == 0){
						if($1->val.f >= (float)$3->val.i)
							$$->val.i = 1;
						else
							$$->val.i = 0;
					}
					else if($1->dtype == 1 && $3->dtype == 1){
						if($1->val.f >= $3->val.f)
							$$->val.i = 1;
						else
							$$->val.i = 0;
					}
					else if($1->dtype == 0 && $3->dtype == 2){
						if($1->val.i >= (int)$3->val.c)
							$$->val.i = 1;
						else
							$$->val.i = 0;
					}
					else if($1->dtype == 2 && $3->dtype == 0){
						if((int)$1->val.c >= $3->val.i)
							$$->val.i = 1;
						else
							$$->val.i = 0;
					}
					else if($1->dtype == 2 && $3->dtype == 2){
						if((int)$1->val.c >= (int)$3->val.c)
							$$->val.i = 1;
						else
							$$->val.i = 0;
					}
				}		
	;


additive_expression
	: multiplicative_expression	
			{
				// if($1->dtype == -1){
				// 	printf("Line:%d: error: Use of undeclared identifier \'%s\' \n", line, $1->name);
				// }
				// else if($1->dtype == 0){
				// 	$$->dtype = 0;
				// 	$$->val.i = $1->val.i;
				// }
				// else if($1->dtype == 1){
				// 	$$->dtype = 1;
				// 	$$->val.f = $1->val.f;
				// }
				// else if($1->dtype == 2){
				// 	$$->dtype = 2;
				// 	$$->val.c = $1->val.c;
				// }	
			
				$$ = $1;
			}
	| additive_expression '+' multiplicative_expression
				{
                    if(($1->dtype) != ($3->dtype)) {
                        $$->dtype = 1;
                        printf("Datatype mismatch in line : %d\nTrying to perform error correction\n",line);
                        if(($1->dtype) == 1) {
                            $$->val.f = ($1->val.f) + ($3->val.i);
                        }
                        else if($1->dtype == 0){
                            $$->val.f = ($1->val.i) + ($3->val.f);
                        }
                        
                    }
                    else {
                        if($1->dtype == 0) {
                            $$->dtype = 0;
                            $$->val.i = ($1->val.i) + ($3->val.i);
                            // printf("checking2: %d %d\n", $1->val.i, $3->val.i);
                        }
                        else if($1->dtype == 1) {
                      		$$->dtype = 1;
                            $$->val.f = ($1->val.f) + ($3->val.f);
                        }
                        else if($1->dtype == 2){
                            $$->dtype = 2;
                            $$->val.c = $1->val.c + $3->val.c;
                        }
                    }
                   
                }
	| additive_expression '-' multiplicative_expression
				{
                    if(($1->dtype) != ($3->dtype)) {
                        $$->dtype = 1;
                        printf("Datatype mismatch in line : %d\nTrying to perform error correction\n",line);
                        if(($1->dtype) == 1) {
                            $$->val.f = ($1->val.f) - ($3->val.i);
                        }
                        else if($1->dtype == 0){
                            $$->val.f = ($1->val.i) - ($3->val.f);
                        }
                        
                    }
                    else {
                        if($1->dtype == 0) {
                            $$->dtype = 0;
                            $$->val.i = ($1->val.i) - ($3->val.i);
                            // printf("checking2: %d %d\n", $1->val.i, $3->val.i);
                        }
                        else if($1->dtype == 1) {
                      		$$->dtype = 1;
                            $$->val.f = ($1->val.f) - ($3->val.f);
                        }
                        else if($1->dtype == 2){
                            $$->dtype = 2;
                            $$->val.c = $1->val.c - $3->val.c;
                        }
                    }
                   
                }
	;


multiplicative_expression
	: unary_expression			
			{	
				// if($1->dtype == -1){
				// 	printf("Line:%d: error: Use of undeclared identifier \'%s\' \n", line, $1->name);
				// }
				// else if($1->dtype == 0){
				// 	$$->dtype = 0;
				// 	$$->val.i = $1->val.i;
				// }
				// else if($1->dtype == 1){
				// 	$$->dtype = 1;
				// 	$$->val.f = $1->val.f;
				// }
				// else if($1->dtype == 2){
				// 	$$->dtype = 2;
				// 	$$->val.c = $1->val.c;
				// }

				$$ = $1;
			}
	| multiplicative_expression '*' unary_expression 
				{
                    if(($1->dtype)!=($3->dtype))
                    {
                    	//havent check for char but okay
                        $$->dtype=1;		//assuming one of them is 'float'
                        printf("Datatype mismatch in line : %d\nTrying to perform error correction\n",line);
                        if(($1->dtype) == 1) {
                            $$->val.f = ($1->val.f) * ((float)$3->val.i);
                        }
                        else if($1->dtype == 0){
                            $$->val.f= ((float)$1->val.i) * ($3->val.f);
                        }
                        
                    }
                    else
                    {
                        if($1->dtype == 0) {
                            $$->dtype = 0;
                            $$->val.i = ($1->val.i) * ($3->val.i);
                        }
                        else if($1->dtype == 1) {
                            $$->dtype = 1;
                            $$->val.f = ($1->val.f) * ($3->val.f);
                        }
                        else if($1->dtype == 2){
                            $$->dtype = 2;
                            $$->val.c = $1->val.c * $3->val.c;
                        }
                    }
                }			
	| multiplicative_expression '/' unary_expression	
				{
                    if(($1->dtype) != ($3->dtype))
                    {
                    	//havent check for char but okay
                        $$->dtype=1;		//assuming one of them is 'float'
                        printf("Datatype mismatch in line : %d\nTrying to perform error correction\n",line);
                        if(($1->dtype) == 1) {
                            $$->val.f = ($1->val.f) / ((float)$3->val.i);
                        }
                        else if($1->dtype == 0){
                            $$->val.f = ((float)$1->val.i) / ($3->val.f);
                        }
                        
                    }
                    else
                    {
                        if($1->dtype == 0) {
                            $$->dtype = 0;
                            $$->val.i = ($1->val.i) / ($3->val.i);
                        }
                        else if($1->dtype == 1) {
                            $$->dtype = 1;
                            $$->val.f = ($1->val.f) / ($3->val.f);
                        }
                        else if($1->dtype == 2){
                            $$->dtype = 2;
                            $$->val.c = $1->val.c / $3->val.c;
                        }
                    }
                }	
	| multiplicative_expression '%' unary_expression
				{
                    
                    if(($1->dtype)!=($3->dtype))
                    {
                        $$->dtype = 0;
                        printf("Line:%d: error: Invalid operands to binary \n",line);
                      	
                        if(($1->dtype) == 1) {
                            $$->val.i = (int)$1->val.f % ($3->val.i);
                        }
                        else if($1->dtype == 0){
                            $$->val.i = ($1->val.i) % (int)$3->val.f;
                        }

                    }
                    else
                    {
                        if($1->dtype == 0) {
                            $$->dtype = 0;
                            $$->val.i = ($1->val.i) % ($3->val.i);
                        }
                        else if($1->dtype == 1) {
							$$->dtype = 0;
							$$->val.i = (int)$1->val.f % (int)$3->val.f;                          
                        	printf("Line:%d: error: Invalid operands to binary (have float and float) \n",line);
                        }
                        else if($1->dtype == 2) {
                            $$->dtype = 0;
                        	$$->val.i = (int)$1->val.c % (int)$3->val.c;
                    	}
                	}
                }	
	;


%%

void yyerror(const char *str){
	fflush(stdout);
	printf("Line - %d : %s\n", line, str);
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



void addsymbol(struct node *tp, char *vname) {
    strcpy(tp->name,vname);
    tp->dtype = -1;
    tp->link = NULL;
    tp->scope = scope;
    tp->valid = 1;
    tp->lineno = line;
}

void addInt(struct node *t, int type, int val) {
    if(t->dtype==-1) {
        t->dtype=type;
        t->val.i=val;
    }
    else
        printf("Redeclaration of variable %s\n",t->name);
}

void addFloat(struct node *t,int type,float val) {
    if(t->dtype==-1) {
        t->dtype=type;
        // if (val - (int)val == 0)
        // printf("\n%f\n", val);
        t->val.f=(float)val;

    }
    else
        printf("Redeclaration of variable %s\n",t->name);
}

void addChar(struct node *t,int type, char val) {
   	if(t->dtype==-1) {
   	    t->dtype=type;
   	    t->val.c=(char)val;
   	}
   	else
   	    printf("Redeclaration of variable %s\n",t->name);
}

struct node * checksym(char *vname) {
	struct node *ftp;
	struct node *rp;
	struct node *nnode;
	if(first==NULL) {
	    nnode=(struct node *)malloc(sizeof(struct node));
	    addsymbol(nnode,vname);
	    first=nnode;
	}
	else {
	    ftp=first;
	    while(ftp!=NULL) {
	            if(strcmp(vname,ftp->name)==0 && ftp->scope<=scope && ftp->valid==1) {
	                    return ftp;
	            }
	            rp=ftp;
	            ftp=ftp->link;
	    }
	    nnode=(struct node *)malloc(sizeof(struct node));
	    addsymbol(nnode,vname);
	    rp->link=nnode;
	}
	return nnode;
}

void invalidate() {
    printf("HIII");
    struct node *ftp;
    struct node *rp;
    struct node *nnode;
    if(first==NULL) ;
    else {
        ftp=first;
        while(ftp!=NULL) {
            if (ftp->scope==scope && ftp->valid==1) {
                ftp->valid=0;
            }
        }
    }
}


struct node* fv(struct node *t) {
    if(t->dtype==-1)
        return NULL;
    else
        return t;
}

void checkvalid(struct node *t) {
    if(t->dtype==-1) {
        printf("Variable \"%s\" undeclared\n",t->name);
    }
}

void printsymtable(){
	struct node *ftp;
    ftp=first;
    printf("\n\nSymbol Table:\n");
    while(ftp!=NULL) {
    	if(strcmp(ftp->name, "tmp") != 0){

	        char data_type[10];
	        if(ftp->dtype==0)
	        	strcpy(data_type,"integer");
	        if(ftp->dtype==1)
	        	strcpy(data_type,"float");
	        if(ftp->dtype==2)
	        	strcpy(data_type,"character");

	        printf("name: %7s\tdatatype: %10s\tscope: %d\tline: %d\t\t",ftp->name,data_type, ftp->scope, ftp->lineno);

	        if(ftp->dtype==0){
	        	if(ftp->val.i == INT_MIN)
	        		printf("value: -\n");
	        	else
	           		printf("value: %d\n",ftp->val.i);
	        }
	        else if(ftp->dtype==1){
	        	if(ftp->val.f == INT_MIN)
	        		printf("value: -\n");
	        	else
	           		printf("value: %f\n",ftp->val.f);
	       	}
	        else if(ftp->dtype==2){
	            printf("value: %c\n",ftp->val.c);
	        }
	        else{
	        	printf("value: -	Delete it\n");
	        }
    	}
        ftp=ftp->link;
    }
}