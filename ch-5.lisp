;; macro recipie
;; The steps to writing a macro are as follows:
;;   1 Write a sample call to the macro and the code it should expand into, or vice versa.
;;   2 Write code that generates the handwritten expansion from the arguments in the sample call.
;;   3 Make sure the macro abstraction doesn't "leak."

(defun primep (number)
  "Checks if the given number is a prime number"
  (when (> number 1)
        (loop for fac from 2 to (isqrt number) never (zerop (mod number fac)))))
(defun next-prime (number)
  "Returns the prime number to closest to given number"
  (loop for n from number when (primep n) return n))
(next-prime 9)

;; code to be converted to a macro
(do ((p (next-prime 0) (next-prime (1+ p))))
    ((> p 19))
  (format t "~d " p))

;; macro defination
(defmacro do-primes (num-and-range &rest body)
  (let ((num (first num-and-range))
        (start (second num-and-range))
        (end (third num-and-range))) ; binds the values from the list
    `(do ((,num (next-prime ,start) (next-prime (1+ ,num))))
         ((> ,num ,end)) ; this is the code we wanted to macro. putting things in place
       ,@body))) ; as usual body part is the instructions to be run

;; same macro above
(defmacro do-primes ((num start end) &body body) ; notice destructuring
  `(do ((,num (next-prime ,start) (next-prime (1+ ,num)))) ; the rest is the same
     ((> ,num ,end))
     ,@body))

;; macro use
(do-primes (p 0 19) ; first list passed to macro becomes variables
  (print "p is now =>") ; just for show
  (format t "~d " p)) ; rest of the lambdas are instructions

(macroexpand-1 '(do-primes (p 0 19) (format t "~d " p)))

;; Backquote Examples
;; Backquote Syntax Equivalent List-Building Code Result
;; `(a (+ 1 2) c)          (list 'a '(+ 1 2) 'c)                      => (a (+ 1 2) c)
;; `(a ,(+ 1 2) c)         (list 'a (+ 1 2) 'c)                       => (a 3 c)
;; `(a (list 1 2) c)       (list 'a '(list 1 2) 'c)                   => (a (list 1 2) c)
;; `(a ,(list 1 2) c)      (list 'a (list 1 2) 'c)                    => (a (1 2) c)
;; `(a ,@(list 1 2) c)     (append (list 'a) (list 1 2) (list 'c))    => (a 1 2 c)

;; leaky abstractions

(macroexpand-1 '(do-primes (p 0 (random 10)) (format t "~d " p)))
; (DO ((P (NEXT-PRIME 0) (NEXT-PRIME (1+ P))))
;     ((> P (RANDOM 100))) <====== HERE: (RANDOM 100) is passed and it will be called on each loop
;   (FORMAT T "~d " P))

;; improved version
(defmacro do-primes ((var start end) &body body)
  `(do ((ending-value ,end) ; here we bind the end value to a new variable.
                           (,var (next-prime ,start) (next-prime (1+ ,var))))
       ((> ,var ending-value))
     ,@body))

(macroexpand-1 '(do-primes (p 0 (random 10)) (format t "~d " p)))
; (DO ((ENDING-VALUE (RANDOM 10)) <====== HERE: now we bind it to a value and it will be called once. now there is another issue. initialization forms for variables in a DO loops need to be in the same order
;      (P (NEXT-PRIME 0) (NEXT-PRIME (1+ P))))
;     ((> P ENDING-VALUE))
;   (FORMAT T ~d P))

;; improved version
(defmacro do-primes ((var start end) &body body)
  `(do ((,var (next-prime ,start) (next-prime (1+ ,var)))
        (ending-value ,end)) ; here we changed the order. now end is bound after start is bound
     ((> ,var ending-value))
     ,@body))

(macroexpand-1 '(do-primes (p 0 (random 10)) (format t "~d " p)))
; (DO ((P (NEXT-PRIME 0) (NEXT-PRIME (1+ P)))
;      (ENDING-VALUE (RANDOM 10))) <====== HERE: now the issue with the order is fixed but there is yet another issue. the variable name ending-value defined in macro can cause issues since the programmer may use it within the same scope the macro is called 
;     ((> P ENDING-VALUE))
;   (FORMAT T ~d P))

;; examples of issue above
(macroexpand-1 '(do-primes (ending-value 0 10) (print ending-value)))
; (DO ((ENDING-VALUE (NEXT-PRIME 0) (NEXT-PRIME (1+ ENDING-VALUE))) <====== HERE
;      (ENDING-VALUE 10)) <====== HERE
;     ((> ENDING-VALUE ENDING-VALUE)) <====== HERE
;   (PRINT ENDING-VALUE))

;; another example
(macroexpand-1 '(let ((ending-value 0))
                  (do-primes (p 0 10)
                    (incf ending-value p))
                  ending-value))
; (DO ((ENDING-VALUE (NEXT-PRIME 0) (NEXT-PRIME (1+ ENDING-VALUE))) <====== HERE
;      (ENDING-VALUE 10)) <====== HERE
;     ((> ENDING-VALUE ENDING-VALUE)) <====== HERE
;   (PRINT ENDING-VALUE))
; (LET ((ENDING-VALUE 0))
;   (DO-PRIMES (P 0 10)
;     (INCF ENDING-VALUE P))
;   ENDING-VALUE)

;; improved version, solution to the problem above
(defmacro do-primes ((var start end) &body body)
  (let ((ending-value-name (gensym))) ; here gensym function will generate a unique symbol for the variable. notice do loop is wrapped in a let scope with bindging
    `(do ((,var (next-prime ,start) (next-prime (1+ ,var)))
          (,ending-value-name ,end)) ; here ending-value-name will turn into a unique symbol and end value will be assigned to it
       ((> ,var ,ending-value-name))
       ,@body)))

(macroexpand-1 '(do-primes (ending-value 0 10) (print ending-value)))
; (DO ((ENDING-VALUE (NEXT-PRIME 0) (NEXT-PRIME (1+ ENDING-VALUE)))
;      (G286 10)) <====== HERE : actual end value of macro
;     ((> ENDING-VALUE G286)) <====== HERE
;   (PRINT ENDING-VALUE))

;; some rule of thumbs
;;  1 Unless there's a particular reason to do otherwise, include any subforms in the expansion in positions that will be evaluated in the same order as the subforms appear in the macro call.
;;  2 Unless there's a particular reason to do otherwise, make sure subforms are evaluated only once by creating a variable in the expansion to hold the value of evaluating the argument form and then using that variable anywhere else the value is needed in the expansion.
;;  3 Use GENSYM at macro expansion time to create variable names used in the expansion.

(defmacro with-gensyms ((&rest names) &body body)
  "Generates a unique symbol for names"
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))
(loop for n in '(a b c) collect `(,n (gensym)))

(defmacro do-primes ((var start end) &body body)
  (with-gensyms (ending-value-name) ; usign macro above to prevent the last leak. another way of solving
    `(do ((,var (next-prime ,start) (next-prime (1+ ,var)))
          (,ending-value-name ,end))
         ((> ,var ,ending-value-name))
       ,@body)))
(macroexpand-1 '(do-primes (ending-value 0 10) (print ending-value)))
; (DO ((ENDING-VALUE (NEXT-PRIME 0) (NEXT-PRIME (1+ ENDING-VALUE)))
;      (G287 10))
;     ((> ENDING-VALUE G287))
;   (PRINT ENDING-VALUE))

;; some macro example in the wild
(defmacro once-only ((&rest names) &body body)
  (let ((gensyms (loop for n in names collect (gensym))))
    `(let (,@(loop for g in gensyms collect `(,g (gensym))))
       `(let (,,@(loop for g in gensyms for n in names collect ``(,,g ,,n)))
          ,(let (,@(loop for n in names for g in gensyms collect `(,n ,g)))
             ,@body)))))