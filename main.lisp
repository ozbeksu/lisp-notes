(defparameter *counter*
              (let ((c 0))
                (list
                 :incr #'(lambda () (incf c))
                 :decr #'(lambda () (decf c))
                 :count #'(lambda () c))))

(funcall (getf *counter* :count))
(funcall (getf *counter* :incr))
(funcall (getf *counter* :decr))
