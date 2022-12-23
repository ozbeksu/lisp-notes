;;
;; macros
;;

;; WHEN and UNLESS

(if (> 2 3) "yep" "nope") ; "nope"
(if (> 2 3) "yep") ; NIL
(if (> 3 2) "yep" "nope") ; "yep"

(defmacro my-when (condition &rest body) ; macro exaple of when
  `(if ,condition (progn ,@body)))
(my-when (> 3 2) ; when some condition is met run commands
  (print "yo")
  (print "go"))

(defmacro my-unless (condition &rest body)
  `(if (not ,condition) (progn ,@body)))
(my-unless (> 2 3) ; when some condition is met run commands
  (print "yo")
  (print "go"))

;; COND

(defun fizz-buzz (x)
  (cond ; similar to switch case
       ((equal (mod x 3) 0) "fizz") ; condition and clause
       ((equal (mod x 5) 0) "buzz")
       (x))) ; last branch is default branch
(fizz-buzz 3)
(fizz-buzz 5)
(fizz-buzz 7)

;; AND, OR, and NOT

(not nil)
(not (= 1 1))
(and (when (= 1 2) "nope") (when (= 3 3) "yea"))
(or (when (= 1 2) "nope") (when (= 3 3) "yea"))

;; DOLIST and DOTIMES

(dolist (x '(1 2 3)) (print x)) ; returns nil at the end

(dolist (x '(3 4 5)) (print x) (when (evenp x) 'return)) ; use of return as break

(dotimes (i (length '(9 8 7))) (print i)) ; similar to a for loop

(let ((alist '(9 8 7)))
  (dotimes (i (length alist))
    (let ((el (nth i alist)))
      (when (eq el 8)
            (print "found")
            (return el)))))

(dotimes (x 5)
  (dotimes (y 5)
    (format t "~3d " (* (1+ x) (1+ y))))
  (format t "~%"))

;; do

(do ((i 0 (1+ i))) ; (dotimes (i 4) (print i))
  ((>= i 4))
  (print i))

(do ((n 0 (1+ n))
     (cur 0 next)
     (next 1 (+ cur next)))
    ((= 10 n) cur))


(defparameter *two-mins-from-now* (local-time:timestamp+ (local-time:now) 2 :minute))
(do () ; infinite loop
  ((> (get-universal-time) *two-mins-from-now*))
  (format t "Waiting~%")
  (sleep 60))

(do ((nums nil) (i 1 (1+ i)))
    ((> i 10) (nreverse nums))
  (push i nums))

;; loop

(loop ; same as do loop above. simple form infinite loop
     ((> (get-universal-time) *two-mins-from-now*))
     (format t "Waiting~%")
     (sleep 60))

(loop for i from 1 to 10 collecting i) ; same as do above

(loop for x from 1 to 10 summing (expt x 2))

(loop for x across "the quick brown fox jumps over the lazy dog"
        counting (find x "aeiou"))

(loop for i below 10
        and a = 0 then b
        and b = 1 then (+ b a)
      finally (return a))