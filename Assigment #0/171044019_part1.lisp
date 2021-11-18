(defvar myList nil)  ;list for numbers
(defvar tempList nil)  ;list for temporary



(defun parenthesisp (c)   ;find the paranthesis
  (find c "()"))

(defun next-token (string start token-stack)
  (let ((search (position-if #'parenthesisp string :start start)))
    (typecase search
      (number
       ;; token from start to parenthesis
       (when (> search start)
         (push (subseq string start search) token-stack))
       ;; parenthesis
       (push (subseq string search (1+ search)) token-stack)
       ;; next state
       (values string (1+ search) token-stack))
      (null
       ;; token from start to end of string
       (when (< start (1- (length string)))
         (push (subseq string start) token-stack))
       ;; next-state
       (values string nil token-stack)))))



(defun all-tokens (string)  ;gets the line(s) and computes them
  (do (;; initial start value is 0
       (start 0)
       ;; initial token stack is nil
       (tokens))

      ;; loop until start is nil, then return the reverse of tokens
      ((not start) (nreverse tokens))

    ;; advance state
    (multiple-value-setq (string start tokens)
      (next-token string start tokens))))


(defun get-file (filename) 		;take the file name and returns the lines of file
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line)))


(setq tempList (get-file "nested_list.txt"))   ;collect lines to tempList
(setq tempList (mapcan #'all-tokens tempList))  ;takes arguments as there are lists
(setq myLength (list-length tempList))  ;set my length to list length

(loop for a from 0 to (- myLength 1) 	;loop to push my actual list 
	   	do 
	   		(setq myStr (nth a tempList))
	   		(if (and (eq NIL (search "(" myStr)) (eq nil (search ")" myStr)) (not(eq 0 (search " " myStr))))
             (push (nth a tempList) myList) 
      		)
	)

(setq myList (reverse myList))  ;reverse the list because I pushed it and push puts the first place instead of last

(with-open-file (stream "flattened_list.txt" :direction :output)  ;open file for writing

	(loop for x from 0 to (- (list-length myList) 1) 
		   	do 
		   		(format stream "~a " (nth x  myList))  ;write to file
	)
	(close stream)
)