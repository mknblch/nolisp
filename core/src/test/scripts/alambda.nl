
; resulting function keeps the reference to the underlying context
(defun FO (x)
    (progn
        (define ix x)
        (lambda () ix)))

; paul grahams anamorphic lambda macro, rewritten using let* rather then labels
(defmacro alambda (params body)
    `(let* ((self (lambda (,@params) ,body))) self))

(defmacro defuna (name params body)
    `(setq ,name (alambda ,params ,body)))


(defuna fact (n)
    (if (eq? n 0) 1 (* n (self (- n 1))))) ; bam
    