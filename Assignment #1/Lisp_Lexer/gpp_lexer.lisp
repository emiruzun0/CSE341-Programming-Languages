
(defun readFromFile (filename)
  (with-open-file (stream filename)
        (setf contents (make-string (file-length stream)))
        (read-sequence contents stream)
        contents
    )

)

(defun writeToFile (content)                            ;;Write to file function.
(with-open-file (stream "parsed_lisp.txt"
    :direction :output
    :if-exists :append
    :if-does-not-exist :create)
	(format stream "~a ~%" content))
)


(defun checkIsKeyword (evaledString)                            ;;Controls the given string, returns the strings which is matched.
    (setf caselessString (string-downcase evaledString))
    (cond
        ((string= caselessString "and") "KW_AND")
        ((string= caselessString "or") "KW_OR")
        ((string= caselessString "not") "KW_NOT")
        ((string= caselessString "equal") "KW_EQUAL")
        ((string= caselessString "less") "KW_LESS")
        ((string= caselessString "nil") "KW_NIL")
        ((string= caselessString "list") "KW_LIST")
        ((string= caselessString "append") "KW_APPEND")
        ((string= caselessString "concat") "KW_CONCAT")
        ((string= caselessString "set") "KW_SET")
        ((string= caselessString "deffun") "KW_DEFFUN")
        ((string= caselessString "for") "KW_FOR")
        ((string= caselessString "if") "KW_IF")
        ((string= caselessString "exit") "KW_EXIT")
        ((string= caselessString "load") "KW_LOAD")
        ((string= caselessString "disp") "KW_DISP")
        ((string= caselessString "true") "KW_TRUE")
        ((string= caselessString "false") "KW_FALSE")
        (T NIL)
    )
)

(defun checkIsAlphaCharacter (evaledString)
	(setf tempCharacter (char evaledString 0))			;Take the first character of string
	(alpha-char-p tempCharacter))						;Controls the character is alpha or not

(defun checkIsNumCharacter (evaledString)				;;Check the string is num or not
	(setf tempCharacter (char evaledString 0))
	(not (null (digit-char-p tempCharacter))))
	
(defun checkIsAlphaNum (evaledString)					;Check the string is alphanumeric or not
	(or (checkIsAlphaCharacter evaledString) (checkIsNumCharacter evaledString)))

(defun checkIsSpaceCharacter (evaledString)						;Check the string is tab or newline or space or return
	(setf tempCharacter (char evaledString 0))
	(or (char= tempCharacter #\TAB) (char= tempCharacter #\NEWLINE) (char= tempCharacter #\SPACE) (char= tempCharacter #\RETURN)))

(defun checkIsOperator (evaledString nextCharacter)             ;Check the string is operator or nor.
    (cond
        ((string= evaledString "+") "OP_PLUS")
        ((string= evaledString "-") "OP_MINUS")
        ((string= evaledString "/") "OP_DIV")
        ((string= evaledString "(") "OP_OP")
        ((string= evaledString ")") "OP_CP")
        ((string= evaledString "\"") "OP_OC")
        ((string= evaledString ",") "OP_COMMA")
        ((string= evaledString "**") "OP_DBMULT")
        ((and (string= evaledString "*") (string/= nextCharacter "*")) "OP_MULT")       ;;If the * sign number is 2, then print dblmult, else ğrint mult
        (T NIL)
    )
)

(defun deterministicFiniteAutomation (evaledString nextCharacter currentState)          ;;Dfa function for lexer
	(setf myLength2 (length evaledString))                                             ;;Take the length of string
	(setf lastEvaledString (subseq evaledString (1- myLength2) myLength2))             

	(case currentState
		('startState                                                                  ;;Everytime start with start state when the program is started.
			(cond 
				((checkIsSpaceCharacter evaledString)                                  ;;if the string is space characters (like newline,space etc.), then evaluated string is ""
					(setf evaledString ""))
				((string= evaledString ";")                                         ;;If the character is comma, then state is changed to comma state. Because comma state control next character 
					(setf currentState 'commaState))
				((string= evaledString "0")                                          ;;If the character is zero, then check the next character or alphanum character.If this cond true, then state is changed to error.
                    (cond
                        ((and (not (null nextCharacter)) (checkIsAlphaNum nextCharacter))
                            (setf currentState 'errorState)
                        )
                        (T                                                             ;;Print value to file and terminal
                            (write-line "VALUE")
                            (writeToFile "VALUE")
                            (setf evaledString "")
                        )
                    )
                )
                ((checkIsNumCharacter evaledString)                                 ;;If the current state is start and the character is number, then state is changed to value state.
                	(setf currentState 'valueState))
                ((checkIsAlphaCharacter evaledString)                                ;;If the current state is start and the character is alpha character, then state is changed to identifier state.
                	(setf currentState 'identifierState))
                ((checkIsOperator evaledString nextCharacter)                         ;;If the current state is start and the character is operator, then goes to the operator function. And write it .
                	(write-line (checkIsOperator evaledString nextCharacter))
                	(writeToFile (checkIsOperator evaledString nextCharacter))
                	(setf evaledString ""))                                            
                ((string= evaledString "*")                                         ;;;;If the current state is start and the character is "*", then state is changed to multiply state.
                	(setf currentState 'multiplyState))
                (T(setf currentState 'errorState))))
		('multiplyState                                                           ;;This state checks the next character and print according to this condition.
			(when (checkIsOperator evaledString nextCharacter)       
				(setf currentState 'startState)
				(write-line (checkIsOperator evaledString nextCharacter))
				(writeToFile (checkIsOperator evaledString nextCharacter))
				(setf evaledString "")))
		('commaState                                                              ;;Comma state check if this string ";;" or not, then it determines the state according the check result.
			(if (string= ";;" evaledString)  
				(setf currentState 'commentState)
				(setf currentState 'errorState)))
		('commentState                                                            ;;Comment state checks the last character of this string and if it is newline,space or tab, then print comment
			(setf lastCharacter (char lastEvaledString 0))
			(when (or (char= lastCharacter #\NEWLINE) (char= lastCharacter #\SPACE) (char= lastCharacter #\TAB))
                (setf currentState 'startState)
                (write-line "COMMENT")
                (writeToFile "COMMENT")
                (setf evaledString "")))
		('valueState                                                              
			(cond 
				((checkIsSpaceCharacter lastEvaledString)                           ;;value state checks the space character or not, then changes the state.
					(setf currentState 'startState)
					(write-line "VALUE")
					(writeToFile "VALUE")                                              ;;print value if the last string is space character.
					(setf evaledString ""))
				((checkIsOperator lastEvaledString NIL)                              ;;checks the last evaluated string is operator or not.
                    (setf currentState 'startState)                                 ;;If it is operator, then changes the current state
                    (write-line "VALUE")
                    (writeToFile "VALUE")
                    (setf tempTokens (deterministicFiniteAutomation lastEvaledString nextCharacter currentState))    ;;Now the tokens is set to the dfa function again. But now the string is last evaluated string
                    (setf evaledString (car tempTokens))                                          ;;evaled string is set to the temptokens first element.
                    (setf currentState (nth 1 tempTokens)))                                        ;;Current state is set to the temptokens second element.
                ((not (checkIsNumCharacter lastEvaledString))
                    (setf currentState 'errorState))))
        ('identifierState                                                           ;;If the state is identifier, then functions comes to there.
			(setf tempString (subseq evaledString 0 (1- myLength2)))                 ;;This is my tempstring which is subsequent of evaled string.
            (cond
                ((checkIsSpaceCharacter lastEvaledString)                           ;;This condition checks the lastevaluated string is space or not.
                    (setf currentState 'startState)
                    (if (checkIsKeyword tempString)                                 ;;Now, the tempstring is checked for it is keyword or not. If it is, then prints the keyword otherwise print identifier.
                    	(previousString
                    		(write-line (checkIsKeyword tempString))
	                        (writeToFile (checkIsKeyword tempString)))
	                    (progn
	                    	(write-line "IDENTIFIER")
	                        (writeToFile "IDENTIFIER")

	                    )
                    )
                    (setf evaledString lastEvaledString)                            
                )
                ((checkIsOperator lastEvaledString NIL)                             ;;If the last evaluated string is operator, then check lastevaluated string now. If it is keyword prints keyword otherwise print identifier.
                    (setf currentState 'startState)
                    (if (checkIsKeyword tempString)
                    	(progn
                    		(write-line (checkIsKeyword tempString))
	                        (writeToFile (checkIsKeyword tempString)))
	                    (progn
	                    	(write-line "IDENTIFIER")
	                        (writeToFile "IDENTIFIER")

	                    ) 
                    )
                    (setf tempTokens (deterministicFiniteAutomation lastEvaledString nextCharacter currentState))               ;;Now again sends to function for tokens.
                    (setf evaledString (car tempTokens))
                    (setf currentState (nth 1 tempTokens))
                )
                ((not (checkIsAlphaNum lastEvaledString))
                    (setf currentState 'errorState)
                )
            ))
        )
	(list evaledString currentState)
)




(defun lexerAnalyzer(contentForLexer &optional previousString currentState)
	(if (null previousString) (setf previousString ""))
	(if (null currentState)	(setf currentState 'startState))

	(setf myLength (length contentForLexer))	  ;This is my content length.

	(when (= 0 myLength)						  ;If the length is 0, then return NIL
		(deterministicFiniteAutomation (concatenate 'string previousString " ") NIL currentState)
		(return-from lexerAnalyzer NIL)
	)
	(setf currentCharacter (subseq contentForLexer 0 1))	;I take the first character of content for every time. 
	(if (= 1 myLength)
		(setf nextCharacter NIL)							;If the length is 0, then ext character will be NIl, otherwise it will be the second character of content
		(setf nextCharacter (subseq contentForLexer 1 2))
	)

	(setf evaledString (concatenate 'string previousString currentCharacter))		;This is the concatenate for previous string and current character. There will assigned to evalutead string
	(setf tempTokens (deterministicFiniteAutomation evaledString nextCharacter currentState))					;This is actually my tokens which is determined from deterministicFinateAutomation. This holds the previous string and current state.

	(setf currentState (nth 1 tempTokens))											;It takes the current state from tokens
	(when (eq currentState 'errorState)													;If the state is error, then print it
        (when (string= (subseq evaledString 0 1) "0")
            (setf evaledString (concatenate 'string evaledString (subseq contentForLexer 1 2)))
        )
        (format T "ERROR ~A was not recognized." evaledString)
        (writeToFile (format nil "ERROR ~A was not recognized." evaledString))
       	(terpri)
       	(return-from lexerAnalyzer)
        
    )
	(setf evaledString (string-left-trim " " (car tempTokens)))	;shift to left
	(lexerAnalyzer (subseq contentForLexer 1 myLength) evaledString currentState)		;Now content is passed to the lexerAnalyzer function with first character removed.
)

(defun gppinterpreter (&optional filename)                          
    (when (not (null filename))                                     ;;If the filename is not empty, then fills the content to lexed.
        (setf contentForLexer (readFromFile filename))
        (lexerAnalyzer contentForLexer)
        (bye)
    )
    (write-line "Enter string for token :")                         ;;If the filename is empty, then passed to repl mode. 
    (setf givenInput (read-line))
    (if (string= givenInput "")                                     ;;If the given input is empty string, then terminates the program.
    	(bye)) 
    (lexerAnalyzer givenInput)
    (gppinterpreter)
    
)

(defun main()                                                   ;;My main function. I opened output file first because may be its inside is not empty (For multi time run)
	(with-open-file (stream "parsed_lisp.txt"
    :direction :output
    :if-exists :rename-and-delete
    :if-does-not-exist :create)
    (close stream)
	)


    (if (null *args*)                                           ;;This is my command line. If the arguments is null then repl mode opens, otherwise takes the first file name 
        (gppinterpreter )
        (gppinterpreter (nth 0 *args*))
    )
)

(main)