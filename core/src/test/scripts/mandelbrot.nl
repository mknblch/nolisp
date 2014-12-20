(load "./core/src/test/scripts/draw.nl")

(defun exImg (size func)
    (progn
        (setq size size)
        (exportImage (mapImage (makeImage size size) func) "p.png")))


(defun limit (x)
    (cond
        ((>= x MAXITER) 0)
        (true (* x (/ 255 MAXITER)))))

(defun red (x)
    (cond
        ((>= x MAXITER) 0)
        (true (* x (/ 255 11)))))

(defun green (x)
    (cond
        ((>= x MAXITER) 0)
        (true (* x (/ 255 22)))))

(defun blue (x)
    (cond
        ((>= x MAXITER) 0)
        (true (* x (/ 255 33)))))

(setq MAXITER 150)

(defun mandelbrot (px py)
    (progn
        (local  half (/ size 2.0)
                n 0
                q 0.0
                x0 (/ (- px (* half 1.5)) half)
                y0 (/ (- py half) half)
                x x0
                y y0
                x1 x0
                y1 y0)
        (while (and (<= q 2.0)(< n MAXITER))
            ((local x1 (+ (- (* x x) (* y y)) x0))
             (local y1 (+ (* 2.0 (* x y)) y0))
             (local q (sqrt (+ (* x1 x1) (* 1.2 y1 y1))))
             (local x x1 y y1)
             (local n (+ n 1))))
        (local l (limit n))
        (color (red l) (green l) (blue l))))

(exImg 512 mandelbrot)