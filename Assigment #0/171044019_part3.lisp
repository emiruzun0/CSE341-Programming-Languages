(defun parse-string-to-int (line)  ;parse string to int in list
  (with-input-from-string (s line)
    (loop
      :for num := (read s nil nil)
      :while num
      :collect num)))




(setq myString (open "integer_inputs.txt"))  
(setq myString (read-line myString))   ;read line and put to string variable
(setq myList (parse-string-to-int myString))  ;parse string to integers
(setq myLength (- (list-length myList) 1))   ;find the number of inputs

(with-open-file (stream "collatz_outputs.txt" :direction :output)  ;write to file
	(defun collatz-conjecture (n)  ;collatz recursive function
		(progn
		  (format stream "~a " n) 
		  (cond
		    ((= n 1) n)
		    ((= (mod n 2) 0) (collatz-conjecture (/ n 2)))
		    ((= (mod n 2) 1) (collatz-conjecture (+ 1 (* n 3)))))))


	(if (> myLength 5)		;at most 5 integers
		(setq myLength 4))
	(loop for a from 0 to myLength  ;for first 5 numbers in text
	   	do 
			 (format stream "~a : " (nth a myList))
			 (collatz-conjecture (nth a myList))
			 (format stream " ~%")	   		
	)

	(close stream)  ;close file
)



