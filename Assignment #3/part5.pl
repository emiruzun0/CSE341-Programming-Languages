sendForEquations(L) :-   solve(L,LS,RS),        %Seperate left side and right side
                    open('output.txt',append,Stream),    %Print them
                    write(Stream,LS),  
                    write(Stream,' = '),
                    write(Stream,RS),
                    write(Stream,'\n'),
                    close(Stream),
                    fail.
sendForEquations(_).

convert(NUMString, Res) :- atom_codes(NUM, NUMString),         
                                   atom_number(NUM, Res).

createList(I, L) :-  read_line_to_codes(I, Line),        %Reads the file and create a list
                   ( Line == end_of_file
                   ->L = [];
                     convert(Line, FinalLine),
                     L = [FinalLine | FurtherLines],
                     createList(I, FurtherLines) ).


solve(L,LS,RS) :-		%First, take the list and sepeate them
   seperate(L,LL,RL),     
   statement(LL,LS),    %Send them to the statement method.     	      
   statement(RL,RS),                 
   LS =:= RS.  

seperate(L,L1,L2) :- append(L1,L2,L),	 %(append) List L1 and L2 concatenation of L
					 L1 = [_|_],
					  L2 = [_|_].  

statement([X],X).                    
statement(L,T) :-     		%Call like recursively.               
   seperate(L,LL,RL),              
   statement(LL,LS),                 
   statement(RL,RS),                 
   operators(LS,RS,T).          

operators(LS,RS,LS+RS).		%Check the operators
operators(LS,RS,LS-RS).
operators(LS,RS,LS*RS).
operators(LS,RS,LS/RS) :- RS =\= 0.   % if the RS is 0, then prevent this probability because number cannot divided by zero

main  :-open('input.txt',read,I),         %You can use main method for read input.txt and write to the output.txt
        createList(I,L),
        close(I),
        write(L),
        sendForEquations(L).




test1 :- sendForEquations([2,3,5,7,11]).       %You can write "test1. " etc
test2 :- sendForEquations([5,3,5,7,49]).
test3 :- sendForEquations([5,8,4,2,78]).
test4 :- sendForEquations([7,14,10,9,98]).

                 
% You can copy and paste to the terminal  sendForEquations([2,3,5,7,11])
% sendForEquations([5,3,5,7,49])
% sendForEquations([5,8,4,2,78])
% sendForEquations([7,14,10,9,98])
