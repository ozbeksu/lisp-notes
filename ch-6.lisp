;;
;; practice: unit format
;;

(defun test-+ ()
  (and
   (= (+ 1 2) 3)
   (= (+ 1 2 3) 6)
   (= (+ -1 -3) -4))) ; chaining cases with AND. test will return T when all tests passes.
(test-+)

(defun test-+ ()
  (format t "~:[FAIL~;pass~] ... ~a~%" (= (+ 1 2) 3) '(= (+ 1 2) 3))
  (format t "~:[FAIL~;pass~] ... ~a~%" (= (+ 1 2 3) 6) '(= (+ 1 2 3) 6))
  (format t "~:[FAIL~;pass~] ... ~a~%" (= (+ -1 -3) -4) '(= (+ -1 -3) -4))) ; a slightly better approach. now we can see result and test function on  a single line.
(test-+)

;; refactoring

(defun report-result (result form)
  (format t "~:[FAIL~;pass~] ... ~a~%" result form)) ; now reporting part is refactored.
;
(defun test-+ ()
  (report-result (= (+ 1 2) 3) '(= (+ 1 2) 3))
  (report-result (= (+ 1 2 3) 6) '(= (1 2 3) 6))
  (report-result (= (+ -1 -3) -4) '(= (+ -1 -3) -4))) ; but still some code duplication.
(test-+)

;; refactoring with macro

(defun report-result (result form)
  (format t "~:[FAIL~;pass~] ... ~a~%" result form)) ; same report function

(defmacro check (form)
  `(report-result ,form ',form)) ; now reporting is expanded with macro

(defun test-+ ()
  (check (= (+ 1 2) 3))
  (check (= (+ 1 2 3) 6))
  (check (= (+ -1 -3) -4))) ; now function is simplified to just an assertion
(test-+)

;; refactoring check macro

(defmacro check (&body forms)
  `(progn
    ,@(loop for f in forms collect `(report-result ,f ',f))))

(defun test-+ ()
  (check
    (= (+ 1 2) 3)
    (= (+ 1 2 3) 6)
    (= (+ -1 -3) -4)))
(test-+)

;; refactoring report function to return result

(defun report-result (result form)
  (format t "~:[FAIL~;pass~] ... ~a~%" result form)
  result) ; report now returns the test result

(defmacro with-gensyms ((&rest names) &body body)
  "Generates a unique symbol for names"
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body)) ; symbol generator for macro

(defmacro combine-results (&body forms)
  (with-gensyms (result)
    `(let ((,result t)) ; set result true
       ,@(loop for f in forms collect `(unless ,f (setf ,result nil))) ; set result to false if test fails
       ,result))) ; return result

(defmacro check (&body forms)
  `(combine-results ; combine results and pass to report function with results
     ,@(loop for f in forms collect `(report-result ,f ',f))))

(defun test-+ ()
  (check
    (= (+ 1 2) 3)
    (= (+ 1 2 3) 6)
    (= (+ -1 -3) -4))) ; all pass
(test-+) ; t

(defun test-+ ()
  (check
    (= (+ 1 2) 3)
    (= (+ 1 2 3) 6)
    (= (+ -1 -3) -5))) ; fails
(test-+) ; nil

;; better result reporting

(defun test-* ()
  (check
    (= (* 2 2) 4)
    (= (* 3 5) 15)))
(test-*)

(defun test-arithmetic ()
  (combine-results
    (test-+)
    (test-*)))
(test-arithmetic) ; runs all tests

;; finding failed test functions

(defvar *test-name* nil)

(defun report-result (result form)
  (format t "~:[FAIL~;pass~] ... ~a: ~a~%" result *test-name* form)
  result)

(defun test-+ ()
  (let ((*test-name* 'test-+)) ; bind the function name
    (check
      (= (+ 1 2) 3)
      (= (+ 1 2 3) 6)
      (= (+ -1 -3) -4))))

(defun test-* ()
  (let ((*test-name* 'test-*)) ; bind the function name
    (check
      (= (* 2 2) 4)
      (= (* 3 5) 15))))

(defun test-arithmetic ()
  (combine-results
    (test-+)
    (test-*)))
(test-arithmetic) ; now we can see the function names

;; abstracting test functions

(defmacro deftest (name parameters &body body)
  `(defun ,name ,parameters
     (let ((*test-name* (append *test-name* (list ',name))))
       ,@body)))

(deftest test-+ () ; now we use deftest
  (check
    (= (+ 1 2) 3)
    (= (+ 1 2 3) 6)
    (= (+ -1 -3) -4)))

(deftest test-* () ; now we use deftest
  (check
    (= (* 2 2) 4)
    (= (* 3 5) 15)))

(deftest test-arithmetic ()
  (combine-results
    (test-+)
    (test-*)))
(test-arithmetic)

(deftest test-math ()
  (test-arithmetic))
(test-math)

;; summary

(defvar *test-name* nil)

(defmacro deftest (name parameters &body body)
  "Define a test function. Within a test function we can call
   other test functions or use 'check' to run individual test
   cases."
  `(defun ,name ,parameters
     (let ((*test-name* (append *test-name* (list ',name))))
       ,@body)))

(defmacro check (&body forms)
  "Run each expression in 'forms' as a test case."
  `(combine-results
     ,@(loop for f in forms collect `(report-result ,f ',f))))

(defmacro combine-results (&body forms)
  "Combine the results (as booleans) of evaluating 'forms' in order."
  (with-gensyms (result)
    `(let ((,result t))
       ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
       ,result)))

(defun report-result (result form)
  "Report the results of a single test case. Called by 'check'."
  (format t "~:[FAIL~;pass~] ... ~a: ~a~%" result *test-name* form)
  result)

(deftest test-+ ()
  (check
    (= (+ 1 2) 3)
    (= (+ 1 2 3) 6)
    (= (+ -1 -3) -4)))

(deftest test-* ()
  (check
    (= (* 2 2) 4)
    (= (* 3 5) 15)))

(deftest test-arithmetic ()
  (combine-results
    (test-+)
    (test-*)))

(deftest test-math ()
  (test-arithmetic))
(test-math)
