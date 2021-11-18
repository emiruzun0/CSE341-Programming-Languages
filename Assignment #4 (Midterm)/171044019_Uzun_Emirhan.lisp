(setq propositions 	;;This prepositions is from the homework pdf but I just changed the mammal list and also query. 
	(list 
		(list (list "legs" (list "X" 2)) (list (list "mammal" (list "X")) (list "arms" (list "X" 2))))
		(list (list "legs" (list "X" 4)) (list (list "mammal" (list "X")) (list "arms" (list "X" 0)))) 
		(list (list "animal" (list "horse")) () ) 
		(list (list "mammal" (list "horse" "lion" "tiger" "mouse" "cat")) () ) ;;fact
		(list (list "arms" (list "horse" 0) () ))    
		(list () (list "animal" (list "X")))  ;;First query
		(list () (list "mammal" (list "horse" "lion" "X" "Y" "cat")) ))) ;Second query
;; The fact list is horse,lion, tiger,mouse, cat
;; And the query is horse,lion, X , Y, cat
;; So the X will be tiger and the Y will be mouse. 
;; (tiger,mouse) will be printed.

(defun specify (propositions c)
	(dolist (elements propositions)
		(if (and (string/= (nth 0 (nth 0 elements)) nil) (string= (nth 0 c) (nth 0 (nth 0 elements))) (= (length (nth 1 elements)) 0))
			(return-from specify 'f) ;If it is a fact, then return string f 
		)
	)
	(return-from specify 'p) ; Otherwise return string p
)

(defun writeToFile (result)		;;Write to the file
	(with-open-file (stream "output.txt"
    :direction :output
    :if-exists :append
    :if-does-not-exist :create)
	(format stream "~a ~%" result)))

(defun begin (propositions)
	(dolist (elements propositions)
		(if (string= nil (nth 0 (nth 0 elements))) ;If the first element of list is nil, then it is a query.
			(progn
				(setq flag (specify propositions (nth 1 elements)))		;;Send to the specify function and assign to flag this result.
				(if (string= flag 'f)			;;If the flag is 'f which is fact, then send to the analyze function and print the result of that function.
					(progn
						(setq result (analyze propositions (nth 1 elements)))
						(writeToFile result)
						(write result) ;;Print the result
						(terpri))
				)
			))))

(defun analyze (propositions f)
	(setq result '())		;;Result list
	(setq pointers'())		;;Shows the i.th element of pointers
	(setq unknownCounter 0)	;;If the upper case letters given, (like X,Y) then it will be 2 end of the loop
	(dolist (givenElement (nth 1 f))	;;Turns f times which is given fact number
    	(if (upper-case-p (char givenElement 0))	;;If the given elements is upper case, then increment the counter
    		(progn
    			(setq unknownCounter (+ unknownCounter 1))
    			(push 1 pointers)	;;Push the 1, to the pointers list
    		)
    		(push 0 pointers)) ;;If it is not upper case then push 0 to the pointers list
  	)
  	(setq pointers (reverse pointers))
	(dolist (i propositions)
		(if (and (string/= (nth 0 (nth 0 i)) nil) (string= (nth 0 f) (nth 0 (nth 0 i))) ;;If the 0th element of i is not equal to nil and 0th element of i is equal to 0th element of fact
			(= (length (nth 1 i)) 0) (= (length (nth 1 (nth 0 i))) (length (nth 1 f)))) ; then fact is found
			(progn
				(if (= unknownCounter 0)	;;If the counter is 0 which means no upper case element like X,Y then result will be determined in this block. True or false
					(progn 
						(setq result (list 'true))
						(dotimes (j (length (nth 1 f)))
							(if (string/= (nth j (nth 1 (nth 0 i))) (nth j (nth 1 f)))
								(setq result (list 'false))))
					)
					(progn  ;;If the counter is not 0 , then the flag will be 1 and this block tries to find the unknowns.
						(setq flag 1)
						(dotimes (j (length (nth 1 f)))
							(if (and (= (nth j pointers) 0) (string/= (nth j (nth 1 (nth 0 i))) (nth j (nth 1 f))))
								(setq flag 0)))
						(if (= flag 1)
							(progn
								(setq secondList '())	;;create a second list for the actual result list.		
								(dotimes (j (length (nth 1 f)))	;;J turns the length of the first element of fact.
									(if (= (nth j pointers) 1)	;;If the jth index of pointers is equal to 1, then push the specified element to the second list.
										(push (nth j (nth 1 (nth 0 i))) secondList)))
								(setq secondList (reverse secondList))
								(push secondList result))	;;Push the second list to the result list.
						)
					)
				)
			)
		)
	)
	result 	;;Return the result list.
)

(defun main ()
	(with-open-file (stream "output.txt"
    :direction :output
    :if-exists :supersede
    :if-does-not-exist :create)
    (close stream))

(setq result (begin propositions))
)


(main)


