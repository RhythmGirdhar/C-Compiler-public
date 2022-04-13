%{
	#include <stdio.h>
	#include <stdlib.h>
	#include <string.h>

	int line = 1;
	#include "y.tab.h"
	extern void yyerror(const char *);  
	static void comment(void);
%}


D   		[0-9]
L   		[a-zA-Z_]
A   		[a-zA-Z_0-9]
WS  		[ \t\v\f]


%%

[\n]		{ fprintf(yyout, "%s", yytext); line++;	}
"/*"		{ comment(); }
"//"[^\n]*	{ /* Consume Comment */ }


	/* Data Types */
int			{ fprintf(yyout, "%s", yytext);  return(INT); 	}
float		{ fprintf(yyout, "%s", yytext);  return(FLOAT); }
char		{ fprintf(yyout, "%s", yytext);  return(CHAR); }
void		{ fprintf(yyout, "%s", yytext);  return(VOID); }



	/* Headers */
"#"			{ fprintf(yyout, "%s", yytext);  return HASH; }
include		{ fprintf(yyout, "%s", yytext);  return INCLUDE; }

	/* C Libraries */
"math.h"	{ fprintf(yyout, "%s", yytext);  return MATH; }
"stdio.h"	{ fprintf(yyout, "%s", yytext);  return STDIO; }
"stdlib.h"	{ fprintf(yyout, "%s", yytext);  return STDLIB; }
"string.h"	{ fprintf(yyout, "%s", yytext);  return STRING; }
"time.h"	{ fprintf(yyout, "%s", yytext);  return TIME; }

	/* Control Structures */
for			{ fprintf(yyout, "%s", yytext);  return FOR; }


printf 		{ fprintf(yyout, "%s", yytext);	 return PRINT; }
return 		{ fprintf(yyout, "%s", yytext);	 return RETURN; }



	/* User Defined Data Types, Identifiers */
{L}{A}*							{fprintf(yyout, "%s", yytext);  return IDENTIFIER;}
{D}+\.{D}+                      {fprintf(yyout, "%s", yytext);  return FLOAT_LITERAL;}  		
{D}+							{fprintf(yyout, "%s", yytext);  return INTEGER_LITERAL;}	
"'"."'"							{fprintf(yyout, "%s", yytext);  return CHARACTER_LITERAL; }
\"{A}+(".h"|".c")\"				{fprintf(yyout, "%s", yytext);  return HEADER_LITERAL;}
\".*\"							{fprintf(yyout, "%s", yytext);  return(STRING_LITERAL); }

	/* Assignment Operators */
"+="		{fprintf(yyout, "%s", yytext);  return(ADD_ASSIGN); }
"-="		{fprintf(yyout, "%s", yytext);  return(SUB_ASSIGN); }
"/="		{fprintf(yyout, "%s", yytext);  return(DIV_ASSIGN); }
"%="		{fprintf(yyout, "%s", yytext);  return(MOD_ASSIGN); }
"*="		{fprintf(yyout, "%s", yytext);  return(MUL_ASSIGN); }


	/* Relational Operators */
"++"		{fprintf(yyout, "%s", yytext);  return(INC_OP); }
"--"		{fprintf(yyout, "%s", yytext);  return(DEC_OP); }
"<="		{fprintf(yyout, "%s", yytext);  return(LE_OP); }
">="		{fprintf(yyout, "%s", yytext);  return(GE_OP); }
"=="		{fprintf(yyout, "%s", yytext);  return(EQ_OP); }
"!="		{fprintf(yyout, "%s", yytext);  return(NE_OP); }

	/* Basic Syntax */
";"			{fprintf(yyout, "%s", yytext);  return(';'); }
"{"			{fprintf(yyout, "%s", yytext);  return('{'); }
"}"			{fprintf(yyout, "%s", yytext);  return('}'); }
","			{fprintf(yyout, "%s", yytext);  return(','); }
":"			{fprintf(yyout, "%s", yytext);  return(':'); }
"="			{fprintf(yyout, "%s", yytext);  return('='); }
"("			{fprintf(yyout, "%s", yytext);  return('('); }
")"			{fprintf(yyout, "%s", yytext);  return(')'); }
("["|"<:")	{fprintf(yyout, "%s", yytext);  return('['); }
("]"|":>")	{fprintf(yyout, "%s", yytext);  return(']'); }
"."			{fprintf(yyout, "%s", yytext);  return('.'); }
"&"			{fprintf(yyout, "%s", yytext);  return('&'); }
"!"			{fprintf(yyout, "%s", yytext);  return('!'); }
"~"			{fprintf(yyout, "%s", yytext);  return('~'); }
"-"			{fprintf(yyout, "%s", yytext);  return('-'); }
"+"			{fprintf(yyout, "%s", yytext);  return('+'); }
"*"			{fprintf(yyout, "%s", yytext);  return('*'); }
"/"			{fprintf(yyout, "%s", yytext);  return('/'); }
"%"			{fprintf(yyout, "%s", yytext);  return('%'); }
"<"			{fprintf(yyout, "%s", yytext);  return('<'); }
">"			{fprintf(yyout, "%s", yytext);  return('>'); }
"^"			{fprintf(yyout, "%s", yytext);  return('^'); }
"|"			{fprintf(yyout, "%s", yytext);  return('|'); }
"?"			{fprintf(yyout, "%s", yytext);  return('?'); }


{WS}+		{fprintf(yyout, "%s", yytext); /* whitespace separates tokens */}

.			{ printf("No Match, Invalid Expression %s\n", yytext);}

%%

int yywrap(void)
{
	return 1;
}


static void comment(void)
{
    int c;

    while ((c = input()) != 0)
        if (c == '*')
        {
            while ((c = input()) == '*');
            if (c == '/')
                return;

            if (c == 0)
                break;
        }
    yyerror("Unterminated comment");
}