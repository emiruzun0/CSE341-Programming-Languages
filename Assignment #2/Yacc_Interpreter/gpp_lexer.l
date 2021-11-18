%{
	#include "y.tab.h"
	#include <stdio.h>
	#include <string.h>
	int countForCC = -1;
%}

%option noyywrap
%option caseless



%%
and         { return KW_AND ; }
or          { return KW_OR; }
not         { return KW_NOT; }
equal       { return KW_EQUAL; }
less        { return KW_LESS; }
nil         { return KW_NIL; }
list        { return KW_LIST; }
append      { return KW_APPEND; }
concat      { return KW_CONCAT; }
set         { return KW_SET; }
deffun      { return KW_DEFFUN; }
defvar 		{ return KW_DEFVAR; }
for         { return KW_FOR; }
if          { return KW_IF; }
exit        { return KW_EXIT; }
load        { return KW_LOAD; }
disp        { return KW_DISP; }
true        { return KW_TRUE; }
false       { return KW_FALSE; } 

[+]          { return OP_PLUS; }
[-]          { return OP_MINUS; }
[/]          { return OP_DIV; }
[*][*]       { return OP_DBLMULT; }
[*]          { return OP_MULT; }
[(]          { return OP_OP; }
[)]          { return OP_CP; }
["]			 {
				countForCC = countForCC +1 ;
				if(countForCC%2 == 1) return OP_OC;
				return OP_CC ; }
				
[,]            { return OP_COMMA; }

[;][;][ a-zA-Z0-9\/.+-]+	return COMMENT ;


[a-zA-Z][0-9a-zA-Z]*    {
    strcpy(yylval.string, yytext);
    return IDENTIFIER;
}

(0)|([1-9][0-9]*)       {
    yylval.value = atoi(yytext);
    return VALUE;
}
[']		{return CUT;}
[ \t\n]+                 {;}
[a-zA-Z0-9\/.+-]+				{
									strcpy(yylval.string,strdup(yytext));
									return FILENAME;
								}
.   {
        yyerror("SYNTAX_ERROR Expression not recognized\n");
        exit(EXIT_FAILURE);
    }
%%
