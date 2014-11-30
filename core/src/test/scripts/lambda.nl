
; resulting function keeps the reference to the underlying context
(defun FO (x)
    (progn
        (define ix x)
        (lambda () ix)))
