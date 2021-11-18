
(defun main ()
    (if (null *args*)
        (gppinterpreter)
        (gppinterpreter (nth 0 *args*))
    )
)


(defun gppinterpreter (&optional (filename -1))		;;Start function. I use in previous homework
	(if (equal filename -1)
		(let ((line) (check))
			(loop
			   (format t "$ " )
			   (setq line (read-line))
			   (setq check (controlForOneLine line))
			   (terpri)
			   (when (= check -1) (return))
			)
		)
		(let ((in (open filename :if-does-not-exist nil)))
   			(when in
      			(loop for line = (read-line in nil)
      
      			while line do (progn (controlForOneLine line) (terpri)))
      			(close in)
   			)
		)
	)
)


(defun controlForOneLine (givenString)		;;This function takes a string and then string-trim method will be applied.
	(let ((words) (controlCheck 0) (tempword) (check 0))
		(setq tokensline (list))
		(setq tokens (list))
		(setq givenString (string-trim '(#\Space #\Tab #\Newline) givenString))
		(setq words (StringSplit givenString))
		(loop for word in words 			;;Check the given word or whatever that comes, and then print result
			do
			(progn
			(setq tempword (string-trim '(#\Space #\Tab #\Newline) word))
			(setq controlCheck (controlForOneWord tempword))
			(if (or (equal controlCheck 2) (equal controlCheck -1)) (return controlCheck))
			)
		)
		(if (equal controlCheck -1)
			(write "SYNTAX_ERROR Expression not recognized")
			(progn
			(if (equal controlCheck 2) ()
				(progn
				(setq check (ControlForTokens))
				(if (equal check nil) (write "SYNTAX_ERROR Expression not recognized"))
			)
			)
			)
		)
		controlCheck			
	)
)



(defun StringSplit (string &optional (wSpace " "))		;; Split string
  (StringSplitHelper string wSpace))
(defun StringSplitHelper (string &optional (wSpace " ") (r nil))
  (let ((n (position wSpace string
		     :from-end t
		     :test #'(lambda (x y)
			       (find y x :test #'string=)))))
    (if n
	(StringSplitHelper (subseq string 0 n) wSpace (cons (subseq string (1+ n)) r))
      (cons string r))))

(defun StringFind (word complist &optional (i 0))
	(if (null complist)
		nil
		(if (string= word (car complist))
			i
			(StringFind word (cdr complist) (+ i 1))
		)
	)
)
(defun checkIsIdentifier (word)				;;Controls that it is an identifier or not
	(let ((lengthOfWord (- (length word) 1)) (chars) (resultCase t))
		(loop for i from 0 to lengthOfWord
		do
		(progn
		(setq chars (char word i))
		(if (= i 0)
			(if (or (alpha-char-p chars) (char= chars #\_) (char= chars #\.) (char= chars #\+)) (setq resultCase t) (setq resultCase nil))
			(if (or (alpha-char-p chars) (digit-char-p chars) (char= chars #\_) (char= chars #\.) (char= chars #\+)) () (setq resultCase nil))
		)
		(if (equal resultCase nil) (return resultCase))
		)
		)
		resultCase
	)
)

(defun checkIsValue (word)		;;Controls that it is a value or not
	(let ((chr) (res t))
		(if (equal (every #'digit-char-p word) nil)
		(setq res nil)
		(progn
		(if (= (length word) 1)
		(setq res t)
		(progn
		(setq chr (char word 0))
		(if (equal (digit-char-p chr) 0) (setq res nil) (setq res t))
		)
		)		
		)
	)
	res	
	)
)

(defun findTheExp (givenExpression)   ;; It counts the paranthesis for match the open and close paranthes.
	(let ((counter 0) (str) (j 0) (res nil))
		(if (string= (nth 0 givenExpression) "(")
			(progn
			(loop for i in givenExpression
			do (progn
			(setq str (string-downcase i))
			(if (string= str "(") (setq counter (+ counter 1)))
			(if (string= str ")") (setq counter (- counter 1)))
			(setq j (+ j 1))
			(if (= counter 0) (return j))
			)
			)
			(setq res j)
			)
		)
		res
	)
)





(defun controlForOneWord (givenWordFromLine)					; This function, takes the word and check which token is aproppriate for this word
	(let ((lengthOfWord (length givenWordFromLine)) (newWord) (j 0) (resultCase) (temp) (check 0) (id 0))
		(loop for i from 1 to lengthOfWord
			do
			(progn
				(if (= check 1) (setq check 0) )
				(setq newWord (string-downcase (subseq givenWordFromLine j i)))		;;Takes the characters oand create a new word from that
				(if (= check 0)											;;Controls the new word that is matches any token or not.
					(progn												;;Check the new word is operator
					(setq resultCase (StringFind newWord Operator))
					(if (not (equal resultCase nil))
					(progn
					(if (equal resultCase 4)
					(if (and (< i lengthOfWord) (string= (subseq givenWordFromLine i (+ i 1)) "*")) (progn (setq i (+ i 1)) (setq resultCase 3))))  ;;Check for dblmult
					(if (equal resultCase 7) (progn (setq resultCase (+ resultCase (mod opoc 2))) (setq opoc (+ opoc 1))))  ;;Check match with "\"", then check wheter it is OP_OC or OP_CP
					(if (or (equal resultCase 5) (equal resultCase 6) (equal resultCase 7) (equal resultCase 9) (equal resultCase 10)) ;; If the token is "(" ,")" ,"\"",",","'" then different any token could be.
						(progn (setq tokens (append tokens (list newWord))) (setq tokensline (append tokensline (list (nth resultCase OP)))) (setq j i) (setq check 1))
						(if (>= i lengthOfWord)
						(progn (setq tokens (append tokens (list newWord))) (setq tokensline (append tokensline (list (nth resultCase OP)))) (setq check 1))
						(progn
					 	(setq temp (subseq givenWordFromLine i (+ i 1)))
					 	(if (equal (StringFind temp Possible) nil)
					 		(progn (setq check -1))
					 		(progn (setq tokens (append tokens (list newWord))) (setq tokensline (append tokensline (list (nth resultCase OP)))) (setq j i) (setq check 1))
					 	)
					)
					)	
					)
					)	
					)
					)	
				)
				(if (= check 0) ;;The new word is keyword or not
					(progn
					(setq resultCase (StringFind newWord KeyWords))
					(if (not (equal resultCase nil))
					(if (>= i lengthOfWord)
					(progn (setq tokens (append tokens (list newWord))) (setq tokensline (append tokensline (list (nth resultCase KW)))) (setq check 1))
					(progn
				 	(setq temp (subseq givenWordFromLine i (+ i 1)))
				 	(if (and (equal (StringFind temp Possible) nil))
				 		(if (equal (checkIsIdentifier (concatenate 'string newWord temp)) nil) 
				 		(progn (setq check -1))
				 		)
					 (progn (setq tokens (append tokens (list newWord))) (setq tokensline (append tokensline (list (nth resultCase KW)))) (setq j i) (setq check 1))
					 )
					)
					)
					)
					)	
				)
				(if (= check 0)		;;The new word is value or not
				(progn
					(setq resultCase (checkIsValue newWord))
					(if (not (equal resultCase nil))
					(progn
					(loop
						(setq temp (string-downcase (subseq givenWordFromLine j i)))
						(setq i (+ i 1))
						(when (or (equal (checkIsValue temp) nil) (> i lengthOfWord)) (return))
					)
					(setq i (- i 1))
					(if (equal (checkIsValue temp) nil) (setq i (- i 1)))								
					(if (>= i lengthOfWord)
						(progn (setq tokens (append tokens (list newWord))) (setq tokensline (append tokensline (list "VALUE"))) (setq check 1))
						(progn
					 	(setq temp (subseq givenWordFromLine i (+ i 1)))
					 	(if (equal (StringFind temp Possible) nil)
					 		(progn (setq check -1))
					 		(progn (setq tokens (append tokens (list newWord))) (setq tokensline (append tokensline (list "VALUE"))) (setq j i) (setq check 1))
					 	)
						)
					)
					)	
					)
					)
				)

				(if (= check 2) (return check))

			)
		)
		check			
	)
)



(defun ControlForTokens (&optional(tempTokens tokens) (tempTokensForOneLine tokensline)(flag2 0))
	(let ((lengthOfWord (list-length tempTokens)) (resultCase 0) (flag 0) (value1 0) (value2 0) (value3 0) (temp 2) (temp2) (value4) (kw) (temp3) (temp4))
		
		(if (and (string= (nth 0 tempTokensForOneLine) "OP_OP") (string= (nth (- lengthOfWord 1) tempTokensForOneLine) "OP_CP" ))
			(progn
				(setq kw (nth 1 tempTokensForOneLine))
				(if (or (string= kw "OP_PLUS") (string= kw "OP_MINUS") (string= kw "OP_MULT") (string= kw "OP_DIV") (string= kw "OP_DBLMULT"))
				(progn
					(setq flag 1)
					(if (or (string= (nth 2 tempTokensForOneLine) "VALUE"))
					(progn
						(if (string= (nth 2 tempTokensForOneLine) "VALUE") (progn (setq value1 (parse-integer (nth 2 tempTokens))) (setq temp 3)))
					)
					(progn
					(setq temp4 (findTheExp (subseq tempTokens 2)))
					(if (equal temp4 nil) (progn (setq resultCase nil) (setq temp nil)) (setq temp (+ temp temp4)))
					(if (and (not (equal temp nil)) (< temp lengthOfWord)) (setq value1 (ControlForTokens (subseq tempTokens 2 temp) (subseq tempTokensForOneLine 2 temp) 1)) (setq resultCase nil))
					(if (equal value1 nil) (setq resultCase nil))
					)
					)
					(if (equal temp nil) (setq resultCase nil) (setq temp2 (+ temp 2)))
					(if (not (equal resultCase nil))
						(if (or (string= (nth temp tempTokensForOneLine) "VALUE") )
						(progn
						(if (string= (nth temp tempTokensForOneLine) "VALUE") (progn (setq value2 (parse-integer (nth temp tempTokens)))))
						)
						(progn
						(setq temp4 (findTheExp (subseq tempTokens temp)))
						(if (equal temp4 nil) (progn (setq resultCase nil) (setq temp2 nil)) (setq temp2 (+ temp temp4)))
						(if (and (not (equal temp2 nil)) (< temp2 lengthOfWord)) (progn (setq value2 (ControlForTokens (subseq tempTokens temp temp2) (subseq tempTokensForOneLine temp temp2) 1)) (setq temp2 (+ temp2 1))) (setq resultCase nil))
						(if (equal value2 nil) (setq resultCase nil))
						)
						)
					)
					(if (and (not (equal resultCase nil)) (equal temp2 lengthOfWord) (not (equal value1 nil)) (not (equal value2 nil)))	;; If the string is operator, then do this operation
						(progn
						(if (string= kw "OP_PLUS") (setq resultCase (+ value1 value2)))
						(if (string= kw "OP_MINUS") (setq resultCase (- value1 value2)))
						(if (string= kw "OP_MULT") (setq resultCase (* value1 value2)))
						(if (string= kw "OP_DIV") (setq resultCase (/ value1 value2)))
						(if (string= kw "OP_DBLMULT") (setq resultCase (expt value1 value2)))
						)
						(setq resultCase nil)
					)

				)
				)
				(if (and (string= kw "KW_EXIT") (equal lengthOfWord 3))		;; If the string is exit, then exit,
					(progn (setq flag 1) (write "Program Closed.")(terpri) (exit))
				)

				(if (string= kw "KW_DISP")  ;; If the string is disp,display whatever is given
				(progn
					(setq flag 1)
					(setq value1 (ControlForTokens (subseq tempTokens 2 (- lengthOfWord 1)) (subseq tempTokensForOneLine 2 (- lengthOfWord 1)) 1))
					(if (not (equal value1 nil))
					(progn (write value1)(terpri) (setq resultCase value1))
					(setq resultCase nil)
					)
					)
				)
				(if (equal flag 0)
					(progn
						(setq resultCase (EXPB tempTokens tempTokensForOneLine 0))
					)
				)
			)
			(progn					;; If the string is comment, then return comment
				(if (string= (nth 0 tokens) "COMMENT")
				(setq flag2 1)
				(progn
				(setq temp3 (nth 0 tempTokensForOneLine))
				(if (equal lengthOfWord 1)
				(progn
				(if (string= temp3 "VALUE") (setq value1 (parse-integer (nth 0 tempTokens))) (setq temp 3))
				(setq resultCase value1)
				)
				(setq resultCase nil)
				)
				)
				)
			)
		)
		(if (and (not (equal resultCase nil)) (not (equal resultCase -200)) (= flag2 0)) (write resultCase))
		resultCase
	)
)

(defun EXPB (&optional(tempTokens tokens) (tempTokensForOneLine tokensline)(flag2 0))
	(let ((lengthOfWord (list-length tempTokens)) (resultCase 0) (flag 0) (value1 0) (value2 0) (temp 2) (temp2) (kw) (temp3)(temp4) (flag3 0))
		
		(if (and (string= (nth 0 tempTokensForOneLine) "OP_OP") (string= (nth (- lengthOfWord 1) tempTokensForOneLine) "OP_CP" ))
			(progn
			(setq kw (nth 1 tempTokensForOneLine))
			(if (or (string= kw "KW_AND") (string= kw "KW_OR") (string= kw "KW_EQUAL") (string= kw "KW_LESS"))
			(progn
				(setq flag 1)
				(setq temp3 (nth 2 tempTokensForOneLine))
				(if (or (string= temp3 "VALUE") (string= temp3 "KW_TRUE") (string= temp3 "KW_FALSE") )
					(progn
					(if (string= temp3 "VALUE") (setq value1 (parse-integer (nth 2 tempTokens))) (setq temp 3))
					(if (string= temp3 "KW_TRUE") (setq value1 t) (setq temp 3))
					(if (string= temp3 "KW_FALSE") (setq value1 -2) (setq temp 3))
					)
					(progn
					(setq temp4 (findTheExp (subseq tempTokens 2)))
					(if (equal temp4 nil) (progn (setq resultCase nil) (setq temp nil)) (setq temp (+ temp temp4)))
					(if (and (not (equal temp nil)) (< temp lengthOfWord)) (progn (setq value1 (EXPB (subseq tempTokens 2 temp) (subseq tempTokensForOneLine 2 temp) 1)) (setq temp2 (+ temp 2))) (setq resultCase nil))
					)
				)
				(if (equal temp nil) (setq resultCase nil) (setq temp3 (nth temp tempTokensForOneLine)))
				(if (not (equal resultCase nil))
					(if (or (string= temp3 "VALUE") (string= temp3 "KW_TRUE") (string= temp3 "KW_FALSE") )
					(progn
					(if (string= temp3 "VALUE") (setq value2 (parse-integer (nth temp tempTokens))))
					(if (string= temp3 "KW_TRUE") (setq value2 t))
					(if (string= temp3 "KW_FALSE") (setq value2 -2))
					(setq temp2 5)	
					)
					(progn
					(setq temp4 (findTheExp (subseq tempTokens temp)))
					(if (equal temp4 nil) (progn (setq resultCase nil) (setq temp2 nil)) (setq temp2 (+ temp temp4)))
					(if (and (not (equal temp2 nil)) (< temp2 lengthOfWord)) (progn (setq value2 (EXPB (subseq tempTokens temp temp2) (subseq tempTokensForOneLine temp temp2) 1)) (setq temp2 (+ temp2 1))) (setq resultCase nil))
					)
					)
				)
				(if (and (not (equal resultCase nil)) (equal temp2 lengthOfWord) (not (equal value1 nil)) (not (equal value2 nil)))
					(progn
						(if (equal value1 -2) (setq value1 nil))
						(if (equal value2 -2) (setq value2 nil))
						(if (string= kw "KW_AND") (setq resultCase (and value1 value2)))
						(if (string= kw "KW_OR") (setq resultCase (or value1 value2)))
						(if (string= kw "KW_EQUAL") (setq resultCase (equal value1 value2)))
						(if (string= kw "KW_LESS") (setq resultCase (< value1 value2)))
						(if (= flag2 0) (setq flag3 2)
							(progn (if (equal resultCase nil) (setq resultCase -2))))
					)
					(setq resultCase nil)
				)

			)
			)
			(if (string= kw "KW_NOT")
				(progn
				(setq flag 1)
				(setq temp3 (nth 2 tempTokensForOneLine))
				(if (or (string= temp3 "VALUE") (string= temp3 "KW_TRUE") (string= temp3 "KW_FALSE") )
				(progn
				(if (string= temp3 "VALUE") (setq value1 (parse-integer (nth 2 tempTokens))) (setq temp 3))
				(if (string= temp3 "KW_TRUE") (setq value1 t) (setq temp 3))
				(if (string= temp3 "KW_FALSE") (setq value1 nil) (setq temp 3))
				(setq resultCase (not value1))
				(if (= flag2 0) (setq flag3 2)
					(progn (if (equal resultCase nil) (setq resultCase -2))))
				)
				(progn
				(setq temp4 (findTheExp (subseq tempTokens 2)))
				(if (equal temp4 nil) (progn (setq resultCase nil) (setq temp nil)) (setq temp (+ temp temp4)))
				(if (and (not (equal temp nil)) (< temp lengthOfWord)) (progn (setq value1 (EXPB (subseq tempTokens 2 temp) (subseq tempTokensForOneLine 2 temp) 1)) (setq temp (+ temp 1))) (setq resultCase nil))
				(if (and (not (equal resultCase nil)) (equal temp lengthOfWord) (not (equal value1 nil)))
					(progn
						(setq resultCase (not value1))
					)
					(setq resultCase nil)
				)
				)
				)
				)
			)

				(if (equal flag 0) (setq resultCase nil))

			)
			(progn
				(setq temp3 (nth 0 tempTokensForOneLine))
				(if (equal lengthOfWord 1)
				(progn
				(if (string= temp3 "VALUE") (setq value1 (parse-integer (nth 0 tempTokens))) (setq temp 3))
				(if (string= temp3 "KW_TRUE") (setq value1 t) (setq temp 3))
				(if (string= temp3 "KW_FALSE") (setq value1 -2) (setq temp 3))
					(setq resultCase value1)
				)
				(setq resultCase nil)
				)
			)
		)
		(if (or (and (not (equal resultCase nil)) (= flag2 0)) (= flag3 2)) (write resultCase))
		(if (and (= flag2 0) (= flag3 2)) (setq resultCase -200))
		resultCase
	)
)

(defvar KeyWords (list "and" "or" "not" "equal" "less" "nil" "exit"  "disp" "true" "false"))
(defvar KW (list "KW_AND" "KW_OR" "KW_NOT" "KW_EQUAL" "KW_LESS" "KW_NIL" "KW_EXIT" "KW_DISP" "KW_TRUE" "KW_FALSE"))
(defvar Operator (list "+" "-" "/" "**" "*" "(" ")" "\"" "\"" "," "'"))
(defvar OP (list "OP_PLUS" "OP_MINUS" "OP_DIV" "OP_DBLMULT" "OP_MULT" "OP_OP" "OP_CP" "OP_OC" "OP_CC" "OP_COMMA" "CUT"))
(defvar Space (list "\n" "\t" " "))
(defvar Comment ";")
(defvar Possible (list "(" ")" "\""))
(defvar opoc 0)
(defvar tokens (list))
(defvar tokensline (list))


(main)
