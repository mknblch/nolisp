(load "./core/src/test/scripts/draw.nl")

(defun exImg (w h func)
    (exportImage (mapImage (makeImage w h) func) "p.png"))

(defun dcos (x y)
    (progn
        (local g (sqrt (% (* x x) (* y y))))
        (color g g g)))

(defun noise (x y)
    (progn
        (local g (rint 255))
        (color g g g)))


(exImg 512 512 noise)