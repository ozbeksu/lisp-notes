;;
;; S-expressions
;;

;; numbers

; 123     ; the integer one hundred twenty-three
; 3/7     ; the ratio three-sevenths
; 1.0     ; the floating-point number one in default precision
; 1.0e0   ; another way to write the same floating-point number
; 1.0d0   ; the floating-point number one in "double" precision
; 1.0e-4  ; the floating-point equivalent to one-ten-thousandth
; +42     ; the integer forty-two
; -42     ; the integer negative forty-two
; -1/4    ; the ratio negative one-quarter
; -2/8    ; another way to write negative one-quarter
; 246/2   ; another way to write the integer one hundred twenty-three

;; strings

; "foo"   ; the string containing the characters f, o, and o.
; "fo\o"  ; the same string
; "fo\\o" ; the string containing the characters f, o, \, and o.
; "fo\"o" ; the string containing the characters f, o, ", and o.

;; symbols

; x               ; the symbol X
; ()              ; the empty list
; (1 2 3)         ; a list of three numbers
; ("foo" "bar")   ; a list of two strings
; (x y z)         ; a list of three symbols
; (x 1 "foo")     ; a list of a symbol, a number, and a string
; (+ (* 2 3) 4)    ; a list of a symbol, a list, and a number.
; (defun hello-world () (format t "hello, world"))

;; equal checks

(eq 3 3)            ; t
(eq 3 3.0)          ; nil
(eq 1 "1")          ; nil
(eq "a" "a")        ; nil
(eq "a" "A")        ; nil
(eq () ())          ; t
(eq '(1 2) '(1 2))  ; nil

(eql 3 3)           ; t
(eql 3 3.0)         ; nil
(eql 1 "1")         ; nil
(eql "a" "a")       ; nil
(eql "a" "A")       ; nil
(eql () ())         ; t
(eql '(1 2) '(1 2)) ; nil

(equal 3 3)             ; t
(equal 3 3.0)           ; nil
(equal 1 "1")           ; nil
(equal "a" "a")         ; t
(equal "a" "A")         ; nil
(equal () ())           ; t
(equal '(1 2) '(1 2))   ; t

(equalp 3 3)            ; t
(equalp 3 3.0)          ; t
(equalp 1 "1")          ; nil
(equalp "a" "a")        ; t
(equalp "a" "A")        ; t
(equalp () ())          ; t
(equalp '(1 2) '(1 2))  ; t

;; functions

(defun verbose-sum (x y) 
    "Sum any two numbers after printing a message."
    (format t "Summing ~d and ~d.~%" x y)
    (+ x y))
(verbose-sum 10 20)

;; optional arguments

(defun zoo (a b &optional c d) (list a b c d))
(zoo 1 2 3 4) ; (1 2 3 4)
(zoo 1 2 3) ; (1 2 3 NIL)
(zoo 1 2) ; (1 2 NIL NIL)

;; optional arguments with default values

(defun boo (a b &optional (c 0) (d 1)) (list a b c d)) ; optional parameters have default values
(boo 1 2 3 4) ; (1 2 3 4)
(boo 1 2 3) ; (1 2 3 1)
(boo 1 2) ; (1 2 0 1)

(defun size-array (&optional (n 10)) (make-array n))
(size-array) ; #(0 0 0 0 0 0 0 0 0 0)
(size-array 3) ; #(0 0 0)

;; optional arguments with its default value passed from arguments occurred earlier

(defun make-rectangle (width &optional (height width)) ; here width is re-assigned to height as default 
    (format t "rectangle with: ~a height: ~a" width height))
(make-rectangle 5 10)
(make-rectangle 5)

;; optional arguments with supplied-p 
; supplied-p (supplied parameter) is true when caller passes an optional argument
; false when default value defined in function body is used for optional argument

(defun coo (a b &optional (c 3 c-supplied-p))
    (list a b c c-supplied-p))
(coo 1 2) ; (1 2 3 NIL) 3 is default value from function body
(coo 1 2 3) ; (1 2 3 T) 3 is passed by the caller. it is not default value from function body. notice c-supplied-p is t
(coo 1 2 4) ; (1 2 4 T) 4 is passed by the caller. notice c-supplied-p is t

;; rest paramters

(defun print-all (&rest nums)
    (format t "~a ~a" (car nums) (cdr nums)))
(print-all 1 2 3 4) ; 1 (2 3 4)

(defun sum-all (&rest args)
    (if (null (car args)) 
        0 
        (+ (car args) (apply 'sum-all (cdr args)))))
(sum-all 1 2 3 4)

;; keyword paramters

(defun make-rgba-color (&key (r 0) (g 0) (b 0) (a 1 a-supplied-p))
    (format t "rgb(~d, ~d, ~d, ~d) ~a" r g b a a-supplied-p))
(make-rgba-color :r 255 :g 255 :b 255) ; white
(make-rgba-color :r 255) ; red
(make-rgba-color :g 255) ; green
(make-rgba-color :b 255) ; blue
(make-rgba-color :a 0.5) ; semi-transparent black

;; keyword paramters with named atoms

(defun foo (&key ((:apple a)) ((:box b) 0) ((:charlie c) 0 c-supplied-p))
    (list a b c c-supplied-p))
(foo :apple 10 :box 20 :charlie 30)

;; mixing different parameter types

; (defun moo (x &optional y &key z) (list x y z)) ; &OPTIONAL and &KEY found in the same lambda list:
; (moo 1 2 :z 3) ; (1 2 3)
; (moo 1) ; (1 nil nil)
; (moo 1 :z 3) ; ERROR

(defun goo (&rest rest &key a b c) 
    (list rest a b c))
(goo :a 1 :b 2 :c 3)

;; return values

(defun format-fn () 
    #'(lambda (i j) (format t "~d * ~d = ~d~%" i j (* i j))))
(defun yoo (n) 
    (dotimes (i 10) 
        (dotimes (j 10)
            ; (apply (format-fn) (list i j)) 
            (when (> (* i j) n) 
                  (return-from yoo (list i j)))))) ; breaks the loop and returns the value
(apply (format-fn) (yoo 20))

;; higher order functions

(defun multiply-by-two (x) (* 2 x))
(multiply-by-two 10)
(funcall 'multiply-by-two 15)
(funcall #'multiply-by-two 20)

(defun plot (fn min max step) 
    (loop for i from min to max by step do 
              (loop repeat (funcall fn i) do (format t "*")) 
              (format t "~%")))
(plot #'multiply-by-two 0 4 1/2)
(plot #'exp 0 4 1/2)

(apply #'plot #'multiply-by-two '(0 4 1/2))
(apply #'plot #'(lambda (x) (* 2 x)) '(0 10 1))
(apply #'plot #'exp '(0 4 1/2))