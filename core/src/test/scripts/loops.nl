(defun loop (n d)
    (cond
        ((<= n 0) 0)
        (true (progn (loop (- n 1) d) (d n)))))

(defun some (n) (print n))

(loop 10 #'some)
