%{
	#include "gpp_lexer.h"
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
				return (OP_OC + countForCC%2) ; }
				
[,]            { return OP_COMMA; }

[;][;]                  { 
						int continueToRead = 0;
						while(continueToRead != NEWLINE) continueToRead = yylex();
						return COMMENT ;}


[a-zA-Z_][a-zA-Z0-9_]*    { return IDENTIFIER ; }


[0]|[1-9][0-9]*      { return VALUE ; }
[0][0-9]*			{return UNKNOWN;}
[0-9]*[a-zA-Z]*				{return UNKNOWN;}


^\n		{printf("The file \"parsed_cpp.txt\" was filled \n");
		exit(EXIT_SUCCESS);}
[\n]        {  return NEWLINE;}

[ \t]+      { return WHITESPACE ;}
. 			{return UNKNOWN ;}
%%




 

char* printToFile(int lexCodeOfCharacter);
int main(int argc, char **argv)
{
	char tempLexerBuffer[100];
	int errorCounter = 0;
	int flag = 0;
	FILE* fp = NULL;
	FILE* fpForWrite = NULL;
    if(argc >= 2) {
        fp = fopen(argv[1], "r");
        if(fp == NULL ){
        	printf("The filename wasn't found!\n");
        }
        else{
        	yyin = fp;
        	flag = 1;
        }
    }
    else{
    	flag = 2;
    	yyin = stdin;
    }

    fpForWrite = fopen("parsed_cpp.txt","w");
    int lexerValue = 1;
    while(flag != 0 && lexerValue != 0){


    	lexerValue = yylex();


		if(lexerValue > 0 && lexerValue < 32)
		{		
			printf("%s",printToFile(lexerValue));
			fprintf(fpForWrite,"%s",printToFile(lexerValue));
			
		}
		else if(lexerValue == 34){
			printf(" ERROR %s was not recognized!\n",yytext);
			fprintf(fpForWrite," ERROR %s was not recognized!\n",yytext);
			errorCounter++;
		}
	}


    if(flag != 0){
    	if(errorCounter > 0) printf("The file \"parsed_cpp.txt\" was created and found some errors\n");
    	else printf("The file \"parsed_cpp.txt\" was filled succesfully ! \n");

    }
    return 0;
}

char* printToFile(int lexCodeOfCharacter){
	switch(lexCodeOfCharacter){
		case 1: 	return("KW_AND\n");
					break;
		case 2: 	return("KW_OR\n");
					break;
		case 3: 	return("KW_NOT\n");
					break;
		case 4: 	return("KW_EQUAL\n");
					break;
		case 5: 	return("KW_LESS\n");
					break;
		case 6: 	return("KW_NIL\n");
					break;
		case 7: 	return("KW_LIST\n");
					break;
		case 8: 	return("KW_APPEND\n");
					break;
		case 9: 	return("KW_CONCAT\n");
					break;
		case 10: 	return("KW_SET\n");
					break;
		case 11: 	return("KW_DEFFUN\n");
					break;
		case 12: 	return("KW_FOR\n");
					break;
		case 13: 	return("KW_IF\n");
					break;
		case 14: 	return("KW_EXIT\n");
					break;		
		case 15: 	return("KW_LOAD\n");
					break;
		case 16: 	return("KW_DISP\n");
					break;
		case 17: 	return("KW_TRUE\n");
					break;
		case 18: 	return("KW_FALSE\n");
					break;		
		case 19: 	return("OP_PLUS\n");
					break;
		case 20: 	return("OP_MINUS\n");
					break;
		case 21: 	return("OP_DIV\n");
					break;		
		case 22: 	return("OP_DBLMULT\n");
					break;
		case 23: 	return("OP_MULT\n");
					break;
		case 24: 	return("OP_OP\n");
					break;
		case 25: 	return("OP_CP\n");
					break;
		case 26: 	return("OP_OC\n");
					break;
		case 27: 	return("OP_CC\n");
					break;
		case 28: 	return("OP_COMMA\n");
					break;
		case 29: 	return("COMMENT\n");
					break;		
		case 30: 	return("IDENTIFIER\n");
					break;
		case 31: 	return("VALUE\n");
					break;
		default:
					break;
		}
}



