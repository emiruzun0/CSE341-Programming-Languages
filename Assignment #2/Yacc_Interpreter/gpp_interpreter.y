%{
    #include <stdio.h>
    #include <string.h>
    #include <ctype.h>

    int searchInMyList(char list[100][50],int listCounter, char *str){
    	int i;
    	for(int i = 0;i < listCounter ;++i)	if(strcmp(str,list[i]) == 0) return i;
    	return -1;
    }
    void arrCopy(int toArray[],int fromArray[],int arrSize ){
    	int i;
    	for(int i = 0;i< arrSize && i< 99;++i) toArray[i] = fromArray[i];
    }
    int toLower(char *p){for ( ; *p; ++p) *p = tolower(*p);}

    int holdValue1 = 0;
    int arrSize = 0;
    int holdValue3 = 0;

    struct myIdentifiers{
    	char list[100][50];
    	int listCounter;
    	int values[100];
    };

    struct myIdentifiers myIdentifier = {"",0,0};
    extern  FILE *yyin;
    FILE *fp;



%}


%token KW_AND
%token KW_OR
%token KW_NOT
%token KW_EQUAL
%token KW_LESS
%token KW_NIL
%token KW_LIST
%token KW_APPEND
%token KW_CONCAT
%token KW_SET
%token KW_DEFFUN
%token KW_DEFVAR
%token KW_FOR
%token KW_IF
%token KW_EXIT
%token KW_LOAD
%token KW_DISP
%token KW_TRUE
%token KW_FALSE
%token OP_PLUS
%token OP_MINUS
%token OP_DIV
%token OP_DBLMULT
%token OP_MULT
%token OP_OP
%token OP_CP
%token OP_OC
%token OP_CC
%token OP_COMMA
%token COMMENT
%token IDENTIFIER
%token VALUE
%token CUT
%token FILENAME

%union 
{
	struct {
		int selection;
        float value;
        char string[100];
        int values[100];
    };
}

%%
START: | START INPUT {printf("$ "); holdValue1 = arrSize = holdValue3 = 0;};

INPUT: 
	EXPI{
		if($<selection>$ == 0) 	{printf("%.2f\n", $<value>1); $<value>$ = $<value>1;}
		else if($<selection>$ == 1) {printf("%s\n", $<string>1); strcpy($<string>$,$<string>1);}
		else {
			int k;
			printf("(");
			for(k = 0; k < arrSize; ++k)
				{printf("%d", $<values>1[k]);
				if(k != arrSize-1) printf(" ");}
			printf(")\n");
			arrCopy($<values>$, $<values>1, arrSize);
		}
	}
	| EXPB{ if($<value>$ == 1) printf("TRUE\n");
		else printf("FALSE\n");}
	| EXPLISTI {
		int k;
		printf("(");
		for(k = 0; k < arrSize; ++k)
			{printf("%d", $<values>1[k]);
			if(k != arrSize-1) printf(" ");}
		printf(")\n");
		arrCopy($<values>$, $<values>1, arrSize);
		}
	| COMMENT {};



EXPI: 
		VALUE {$<value>$ = $<value>1; }
		| IDENTIFIER {
			toLower($<string>1);
			int select = searchInMyList(myIdentifier.list, myIdentifier.listCounter,$<string>1);
			if(select != -1)
			{
				$<value>$ = myIdentifier.values[select];
				$<selection>$ = 0;
			}
			else
			{
				printf("%s doesn't have a value!!\n", $<string>1);
				exit(0);
			}
			};
		| OP_OP OP_PLUS EXPI EXPI OP_CP {$<value>$ = $<value>3 + $<value>4;$<selection>$ = 0;}
		| OP_OP OP_MINUS EXPI EXPI OP_CP {$<value>$ = $<value>3 - $<value>4;$<selection>$ = 0;}
		| OP_OP OP_MULT EXPI EXPI OP_CP {$<value>$ = $<value>3 * $<value>4;$<selection>$ = 0;}
		| OP_OP OP_DIV EXPI EXPI OP_CP {
			if($<value>4 == 0)
				{printf("Denumerator couldn't be zero!\n");
				exit(0);}
			else
				{$<value>$ = $<value>3 / $<value>4;
				$<selection>$ = 0;}
			}
		| OP_OP OP_DBLMULT EXPI EXPI OP_CP{
			int result = $<value>3;
			int counter = 1;
			while(counter < $<value>4){
				result *= $<value>3;
				counter++;
			}
			$<value>$ = result;
			$<selection>$ = 0;
			}
		| OP_OP KW_SET IDENTIFIER EXPI OP_CP{
			toLower($<string>3);
			int select = searchInMyList(myIdentifier.list, myIdentifier.listCounter,$<string>1);
			if(select == -1)
			{
				strcpy(myIdentifier.list[myIdentifier.listCounter], $<string>3);
				myIdentifier.values[myIdentifier.listCounter] = $<value>4;
				myIdentifier.listCounter += 1;
			}
			else	myIdentifier.values[select] = $<value>4;
			$<value>$ = $<value>4;
			$<selection>$ = 0;
			}
		| OP_OP KW_SET IDENTIFIER EXPLISTI OP_CP{
			arrCopy($<values>$, $<values>4, arrSize);
			$<selection>$ = 3;
			}
		| OP_OP KW_DEFVAR IDENTIFIER EXPI OP_CP{
			toLower($<string>3);
			strcpy(myIdentifier.list[myIdentifier.listCounter], $<string>3);
			myIdentifier.values[myIdentifier.listCounter] = $<value>4;
			myIdentifier.listCounter += 1;
			strcpy($<string>$,$<string>3);
			$<selection>$ = 1;
	  		}
		| OP_OP KW_DEFVAR IDENTIFIER EXPLISTI OP_CP{
			toLower($<string>3);
			strcpy($<string>$, $<string>3);
			$<selection>$ = 1;
	  		}										  		
		| OP_OP KW_FOR OP_OP IDENTIFIER EXPI EXPI OP_CP EXPLISTI OP_CP {
			arrCopy($<values>$, $<values>8, arrSize);
			$<selection>$ = 3;
		 }
		| OP_OP KW_DISP EXPI OP_CP {
			if($<selection>3 == 0)	{$<value>$ = $<value>3; $<selection>$ = 0;}
			else if($<selection>3 == 1){strcpy($<string>$, $<string>3); $<selection>$ = 1;}
			else if($<selection>3 == 2)	{$<value>$ = $<value>3; $<selection>$ = 2;}
			else if($<selection>3 == 3)	{arrCopy($<values>$, $<values>3, arrSize); $<selection>$ = 3;}
			}
		| OP_OP KW_DISP EXPLISTI OP_CP {
			arrCopy($<values>$, $<values>3, arrSize); $<selection>$ = 3;
			}
		| OP_OP KW_DEFFUN IDENTIFIER OP_OP IDENTIFIERS OP_CP EXPI OP_CP {
			toLower($<string>3);
			strcpy(myIdentifier.list[myIdentifier.listCounter], $<string>3);
			myIdentifier.values[myIdentifier.listCounter] = -1;
			myIdentifier.listCounter += 1;
			strcpy($<string>$ , $<string>3);
			$<selection>$ = 1;
			}
		| OP_OP KW_DEFFUN IDENTIFIER OP_OP IDENTIFIERS OP_CP EXPLISTI OP_CP {
			toLower($<string>3);
			strcpy(myIdentifier.list[myIdentifier.listCounter], $<string>3);
			myIdentifier.values[myIdentifier.listCounter] = -1;
			myIdentifier.listCounter += 1;
			strcpy($<string>$ , $<string>3);
			$<selection>$ = 1;
			}
		| OP_OP KW_DEFFUN IDENTIFIER EXPLISTI OP_CP {
			toLower($<string>3);
			strcpy(myIdentifier.list[myIdentifier.listCounter], $<string>3);
			myIdentifier.values[myIdentifier.listCounter] = -1;
			myIdentifier.listCounter += 1;
			strcpy($<string>$ , $<string>3);
			$<selection>$ = 1;
			}
		| OP_OP KW_IF EXPB EXPLISTI OP_CP{
			if($<value>3 == 1){arrCopy($<values>$, $<values>4, arrSize);$<selection>$ = 3;}
			else{$<value>$ = 0;$<selection>$ = 3;}
		 }
		| OP_OP KW_IF EXPB EXPLISTI EXPLISTI OP_CP{
			if($<value>3 == 1)
			{arrCopy($<values>$, $<values>4, arrSize);$<selection>$ = 3;}
			else{arrCopy($<values>$, $<values>5, arrSize);$<selection>$ = 3;}
		 }
		| OP_OP KW_EXIT OP_CP {printf("Program closed.\n"); exit(1);}
		;
EXPB:
	OP_OP KW_AND EXPB EXPB OP_CP{$<value>$ = $<value>3 && $<value>4;}
	| OP_OP KW_OR EXPB EXPB OP_CP{$<value>$ = $<value>3 || $<value>4;}
	| OP_OP KW_NOT EXPB OP_CP{$<value>$ = !($<value>3);}
	| OP_OP KW_EQUAL EXPB EXPB OP_CP{if($<value>3 == $<value>4) $<value>$ = 1; else $<value>$ = 0;} 
	| OP_OP KW_EQUAL EXPI EXPI OP_CP{if($<value>3 == $<value>4) $<value>$ = 1; else $<value>$ = 0;} 
	| OP_OP KW_LESS EXPI EXPI OP_CP{if($<value>3 <= $<value>4) $<value>$ = 1; else $<value>$ = 0;}
	| KW_TRUE {$<value>$ = 1;}
	| KW_FALSE{$<value>$ = 0;} 
	| KW_NIL {$<value>$ = 0;}
	| OP_OP KW_DISP EXPB OP_CP {$<value>$ = $<value>3; $<selection>$ = 2;}
	;


EXPLISTI:
	OP_OP KW_CONCAT EXPLISTI EXPLISTI OP_CP {
		int counter, k = 0;
		counter = holdValue3 - arrSize;
		while(counter < holdValue3){
			$<values>3[counter] = $<values>4[k];
			counter++;
			k++;
		}
		arrCopy($<values>$, $<values>3, holdValue3);
		arrSize = holdValue3;
	}
	| OP_OP KW_APPEND EXPLISTI EXPLISTI OP_CP{
		int counter, k = 0;
		counter = holdValue3 - arrSize;
		while(counter < holdValue3){
			$<values>3[counter] = $<values>4[k];
			counter++;
			k++;
		}
		arrCopy($<values>$, $<values>3, holdValue3);
		arrSize = holdValue3;
	}
	| OP_OP KW_APPEND EXPI EXPLISTI OP_CP{
		int counter= 0;
		while(counter < arrSize){
			$<values>$[counter+1] = $<values>4[counter];
			counter++;
		}
		$<values>$[0] = $<value>3;
		arrSize = arrSize +1;
	}
	| LISTVALUE {
		arrCopy($<values>$, $<values>1, holdValue1);
		$<selection>$ = 3;
		arrSize = holdValue1;
		holdValue3 += holdValue1;
		holdValue1 = 0;
	}
	;


LISTVALUE:
	OP_OP KW_LIST VALUES OP_CP {
		arrCopy($<values>$, $<values>3, holdValue1);
		}
	| CUT OP_OP VALUES OP_CP {
		arrCopy($<values>$, $<values>3, holdValue1);
		};

VALUES:
	VALUES VALUE{
		$<values>$[holdValue1] = $<value>2;
		holdValue1 += 1;
			}
	    | VALUES IDENTIFIER{
			toLower($<string>2);
			int select = searchInMyList(myIdentifier.list, myIdentifier.listCounter,$<string>2);
			if(select != -1)	$<value>2 = myIdentifier.values[select];
			else
			{
				printf("No value for %s\n", $<string>2);
				exit(0);
			}
			$<values>$[holdValue1] = $<value>2;
			holdValue1 += 1;
		}
		| VALUE {$<values>$[holdValue1] = $<value>1; holdValue1 += 1;};

IDENTIFIERS:
		IDENTIFIERS IDENTIFIER{
			toLower($<string>1);
			strcpy($<string>$ , $<string>1);
			strcat($<string>$, " ");
			strcat($<string>$, $<string>2);
		  }
		| IDENTIFIER {
			toLower($<string>1);
			int select = searchInMyList(myIdentifier.list, myIdentifier.listCounter,$<string>1);
			if(select == -1)
			{
				strcpy(myIdentifier.list[myIdentifier.listCounter], $<string>1);
				myIdentifier.values[myIdentifier.listCounter] = -1;
				myIdentifier.listCounter += 1;
			}
			strcpy($<string>$ , $<string>1);
	};


%%

int yyerror(char *msg) 
{ 
  printf("SYNTAX_ERROR Expression not recognized\n");
  exit(0); 
}

int yywrap(){
	return 1;
} 

int main(int argc, char **argv)
 {
 	char tempLexerBuffer[100];
 	FILE* fp = NULL;
 	printf("$ ");

 	if(argc >= 2) {
        yyin = fopen(argv[1], "r");
        yyparse();
        
    }
	   
	yyparse() ;    	

 } 