(defun intmap (fun max) (let* ((plrec (lambda (k l)
    (cond
        ((< k 1) nil)
        ((fun k) (cons k (plrec (- k 1) l)))
         (true  (plrec (- k 1) l))))))
            (plrec max '())))

(defun prim? (p)
    (let* (
        (dividable?
            (lambda (p v) (== (% p v) 0)))
        (primrec?
            (lambda (p v)
                (cond
                    ((>= v p) true)
                    ((dividable? p v) false)
                    (true (primrec? p (+ 1 v)))))))
     (primrec? p 2)))

(defun primpair? (p1) (or (and (prim? p1) (prim? (+ p1 2))) (and (prim? p1)(prim? (- p1 2))) ))

(defun primlist (max) (intmap prim? max))

(defun primspace (max) (eval (append - (primlist max))))

(defun primsum (max) (eval (append + (primlist max))))
