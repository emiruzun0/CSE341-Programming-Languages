(defvar myString nil)


(defun parse-string-to-int (line)   ;parse string to integers
  (with-input-from-string (s line)
    (loop
      :for num := (read s nil nil)
      :while num
      :collect num)))  ;put into a list which is num
 

(defun isprime (n)  ;prime control function
    (setq counter 0)  ;counter is set to 0
    (loop for i from 1 to n
        do(
            cond
                ((= 0 (mod n i))
                    (setq counter (+ counter 1))
                )
        )
    )
 
    (if (= 2 counter)  ; only divided into one and itself which is counter 2
        (return-from isprime 1))
    (return-from isprime 0)
)

(defun isSemiPrime (n)  ;semi prime control function
    (setq counter 0)  
    (setq loopCounter (- n 1)) 
    (setq temp 2)


    (loop for i from 2 to loopCounter  ;loop from 2 to number-1 
        do
        (cond
            ((= 0 (mod n temp))     ;if modula equals 0, then 
                 (setq counter (+ counter 1))   ;counter increases 1 and 
                 (setq n (/ n temp))  ;number is divided by the temp
                 )

        )
        (cond
            ((/= 0 (mod n temp))    ;if modula is not equal to 0, then
                 (setq temp (+ temp 1))    ; increase temp by 1
        ))
    
    )


    (if (= counter 2)   ;if the counter is 2 which is 2 prime number,
        (return-from isSemiPrime 1))    ;then return 1
    (return-from isSemiPrime 0)
)

  
(setq myString (open "boundries.txt"))    ;open txt
(setq myString (read-line myString))    ;read line
(setq myList (parse-string-to-int myString))  ;parse string to integer
(setq num1 (car myList))   ;initialize the first element to the number1
(setq num2 (nth 1 myList))  ;;initialize the first element to the number2


(with-open-file (stream "primedistribution.txt" :direction :output)  ;open file for writing

  (loop for a from num1 to num2   ;loop between two numbers
      do
      (cond ((= 1 (isprime a))    
          (format stream "~a is Prime ~%" a))   ;write to the file
      )
      (cond ((/= 1 (isprime a))
          (if (= 1 (isSemiPrime a))
             (format stream "~a is Semi-prime ~%" a)) ;write to the file
      ))
  )
  (close stream)  ;close file
)

