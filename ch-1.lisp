(format t "Hello, World!") ; format like printf in python

(defun greet () (format t "Hello, World!")) ; function definition
(greet) ; function call

(list 1 2 3) ; list
(list :a 1 :b 2 :c 3) ; plist or paired list

(defvar some-list (list :a 1 :b 2 :c 3)) ; variable declaration
(getf some-list :b) ; getf takes a symbol and returns its pair

(make-cd "Roses" "Kathy Mattea" 7 t)

(defvar *db* nil) ; creates a global variable * implies global
(defvar *db-filename* "./my-cds.db")
(defun add-record (cd) (push cd *db*))

(defun make-cd (title artist rating ripped)
  (list :title title :artist artist :rating rating :ripped ripped))

(add-record (make-cd "Roses" "Kathy Mattea" 7 t))
(add-record (make-cd "Fly" "Dixie Chicks" 8 t))
(add-record (make-cd "Home" "Dixie Chicks" 9 t))

(defun dump-db ()
  (dolist (cd *db*) ; items in the db assigned to cd
    (format t "~{~a:~10t~a~%~}~%" cd))) ; list format string
(dump-db)

(defun dump-db-nested ()
  (format t "~{~{~a:~10t~a~%~}~%~}" *db*)) ; format extracts the list and loops
(dump-db-nested)

(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*) ; required by some implementations to ensure that Lisp doesn't wait for a newline before it prints the prompt 
  (read-line *query-io*)) ; reads from console

(defun prompt-for-cd ()
  (make-cd
   (prompt-read "Title")
   (prompt-read "Artist")
   (or
    (parse-integer (prompt-read "Rating") :junk-allowed t) ; tries to parse the string (permissive with :junk-allowed)
    0)
   (y-or-n-p "Ripped?") ; casts to t or nil
        ))

(defun add-cds ()
  (loop (add-record (prompt-for-cd))
        (when (not (y-or-n-p "Another?")) (return))))
(add-cds)

(defun save-db (filename)
  (with-open-file (out filename ; pass content from file to out
                       :direction :output ; configures for write
                       :if-exists :supersede) ; allows override
    (with-standard-io-syntax (print *db* out)))) ; prints the out to *db* with io safe syntax
(save-db *db-filename*)

(defun load-db (filename)
  (with-open-file (in filename) ; reading doesn't require extra config
    (with-standard-io-syntax
      (setf *db* (read in))))) ; sets result of execution to variable
(load-db *db-filename*)


(remove-if-not #'evenp '(1 2 3 4 5 6 7 8 9 10))
(remove-if-not #'(lambda (x) (= 0 (mod x 2))) '(1 2 3 4 5 6 7 8 9 10))

(remove-if-not #'(lambda (cd) (equal (getf cd :artist) "Dixie Chicks")) *db*)

; (defun select (key value)
;     (remove-if-not #'(lambda (cd) (equal (getf cd key) value)) *db*))
; (select :artist "Dixie Chicks")

(defun select (selector-fn) (remove-if-not selector-fn *db*))

(defun select-by-artist (artist) #'(lambda (cd) (equal (getf cd :artist) artist)))
(select (select-by-artist "Dixie Chicks"))

(defun select-by-title (title) #'(lambda (cd) (equal (getf cd :title) title)))
(select (select-by-title "Roses"))

(defun foo (a b c) (list a b c)) ; regular funtion
(foo 1 2 3)

(defun bar (&key a b c) (list a b c)) ; keyword parameter funtion
(bar :a 1 :b 2 :c 3)
(bar :c 1 :b 2 :a 3)
(bar :a 1 :c 3)
(bar)

(defun baz (&key a (b 20) (c 30 c-p)) (list a b c c-p)) ; parameters with default values
(baz :a 1 :b 2 :c 3)
(baz :c 1 :b 2 :a 3)
(baz :a 1 :c 3)
(baz)

(defun where (&key title artist rating (ripped nil ripped-p))
  #'(lambda (cd)
      (and
       (if title (equal (getf cd :title) title) t)
       (if artist (equal (getf cd :artist) artist) t)
       (if rating (equal (getf cd :rating) rating) t)
       (if ripped-p (equal (getf cd :ripped) ripped) t))))

(select (where :title "Fly"))

(defun update (selector-fn &key title artist rating (ripped nil ripped-p))
  (setf *db*
    (mapcar #'(lambda (row)
                (when (funcall selector-fn row)
                      (if title (setf (getf row :title) title))
                      (if artist (setf (getf row :artist) artist))
                      (if rating (setf (getf row :rating) rating))
                      (if ripped-p (setf (getf row :ripped) ripped)))
                row) *db*)))
(update (where :artist "Dixie Chicks") :rating 11)

(defun delete-rows (selector-fn)
  (setf *db* (remove-if selector-fn *db*)))
(delete-rows (where :artist "Limpopo"))

(select (where :title "Give Us a Break" :ripped t)) ; short for below lambda
(select #'(lambda (cd)
            (and (equal (getf cd :title) "Give Us a Break")
                 (equal (getf cd :ripped) t))))

;; lisp macros

(defmacro backwards (expr) (reverse expr)) ; expr is taken as is first, then processed by reverse, then evaluated as lisp code
(backwards ("hello, world" t format))

(defun make-comparison-expr-wr (field value) ; wrong
  (list equal (list getf cd field) value))

(defun make-comparison-expr (field value)
  (list 'equal (list 'getf 'cd field) value))
(make-comparison-expr :rating 10)
(make-comparison-expr :title "Give Us a Break")

;; notice quotes
`(1 2 3) ; backquote works the same with forward quote
'(1 2 3)
'(1 2 3)

;; notice quotes
'(1 2 (+ 1 2)) ; backquote works the same with forward quote
`(1 2 (+ 1 2))

;; notice quotes
`(1 2 ,(+ 1 2)) ; anything after comma is evaluated insdie the expression
'(1 2 ,(+ 1 2)) ; invalid syntax

(defun make-comparison-expr (field value)
  `(equal (getf cd ,field) ,value))
(make-comparison-expr :rating 10)
(make-comparison-expr :title "Give Us a Break")

(defun make-comparison-list (fields)
  (loop while fields collecting
          (make-comparison-expr (pop fields) (pop fields))))
(make-comparison-list (list :title "Give Us a Break" :rating 10))

;; notice quotes
`(and ,(list 1 2 3) 4) ; list expression is evaluated
`(and ,@(list 1 2 3) 4) ; list expression is evaluated and spliced

(defmacro where (&rest clauses) ; rest is for arbitrary num of args
  `#'(lambda (cd) (and ,@(make-comparison-list clauses))))
(where :title "Give Us a Break" :rating 10)
(macroexpand-1 '(where :title "Give Us a Break" :rating 10))

(select (where :title "Give Us a Break" :rating 10))
(*db*)