;;
;; variables
;;

(defun foo (x)
  (format t "parameter ~a~%" x)
  (let ((x (+ x 2)))
    (format t "outer let ~a~%" x)
    (let ((x (* x 3)))
      (format t "inner let ~a~%" x))
    (format t "outer let ~a~%" x))
  (format t "parameter ~a~%" x))
(foo 1)

(dotimes (x 10) (format t "~d " x)) ; notice x is incremented each time

; (let ((x 10)
;       (y (+ x 20))) ; notice x is unbound. initial value of x is not available
;   (list x y))

(let* ((x 10)
       (y (+ x 20))) ; initial value of x is available to be assigned to y
  (list x y))

(defun zoo (x)
  (let ((x 10)
        (y (+ x 20))) ; notice x is bound within a function scope. no need for let*
    (list x y)))
(zoo 2)

(let ((x 10))
  (let ((y (+ x 10))) ; binding can also be achieved with nested lets
    (list x y)))

(defparameter *count*
              (let ((x 0)) #'(lambda () (incf x)))) ; lambda is closing over the x variable
(funcall *count*) ; each time counter is called x variable is incremented

(defparameter *counter* (let ((x 0)) ; encapsulation example with named list
                          (list
                           :incr #'(lambda () (incf x)) ; keywords are used as function names
                           :decr #'(lambda () (decf x))
                           :count #'(lambda () x))))

(funcall (getf *counter* :incr)) ; getting lamdas back from counter and calling them
(funcall (getf *counter* :decr))
(funcall (getf *counter* :count))

(defvar *count* 0
        "Count of widgets made so far.") ; defvar assigns only if the variable is undefined
(print *count*)
(defparameter *gap-tolerance* 0.001
              "Tolerance to be allowed in widget gaps.") ; defparameter always assigns the initial value to the named variable
(print *gap-tolerance*)

;; global and local binding

(defvar *x* 10)
(defun print-x ()
  (format t "X: ~d~%" *x*)) ; accesses to a global variable

(defun test-print-x ()
  (print-x) ; has initial value
  (let ((*x* 20))
    (progn
     (print-x) ; has the value defined in let scope
     (incf *x*) ; modifications wont affect global value
     (print-x)
     (decf *x*)
     (print-x)))
  (print-x))
(test-print-x)

;; constants

(defconstant z-port 8080)
(print z-port)

;; assignment

(defun set-x-ten (x)
  (setf x 10))

(let ((y 20))
  (set-x-ten y) ; here y is set to 10 inside set-x-ten but y is still 20 outside
  (print y))

(let ((x 0)
      (y 0))
  (setf x 1 y 2) ; can set multiple at one go
  (list x y))

(let ((x 0)
      (y 0))
  (setf x (setf y (random 10)))) ; returns the new value for chaining

; Simple variable:    (setf x 10)
; Array:              (setf (aref a 0) 10)
; Hash table:         (setf (gethash 'key hash) 10)
; Slot named 'field': (setf (field o) 10)

(defparameter *array*
              (make-array 3
                :element-type 'integer
                :initial-contents '(1 2 3)))
*array*
(setf (aref *array* 0) -1) ; set value of array place
(incf (aref *array* 0)) ; in place increment
(incf (aref *array* 0)) ; in place increment
*array*

(let ((x 1) (y 2)) (rotatef x y) (list x y)) ; swaps values
(let ((x 1) (y 2)) (shiftf x y 10) (list x y)) ; y -> x; 10 -> y