; (mapc #'ql:quickload '(:cl-fad :cl-who :hunchentoot :parenscript))

; (defpackage "PS-TUTORIAL"
;   (:use "COMMON-LISP" "HUNCHENTOOT" "CL-WHO" "PARENSCRIPT" "CL-FAD"))

; (in-package "PS-TUTORIAL")

; (setq cl-who:*attribute-quote-char* #\")
; (start (make-instance 'easy-acceptor :port 8080))

; (define-easy-handler (app :uri "/app.js") ()
;   (setf (content-type*) "text/javascript")
;   (ps
;     (defun handle-click () ((@ window console log) "hello"))
;     (defun app () ((@ window console log) "init"))
;                       (app)))

; (define-easy-handler (home :uri "/") ()
;   (with-html-output-to-string (s)
;     (:html
;      (:head 
;         (:title "Parenscript::App")
;         (:script :type "text/javascript" :src "/app.js"))
;      (:body (:h2 "App")
;             (:button :onclick (ps (handle-click)) "Click")))))
