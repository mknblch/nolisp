(setq RGB (java-const java.awt.image.BufferedImage:TYPE_INT_RGB))

(defun makeImage (w h)
    (new java.awt.image.BufferedImage (int int int) (w h RGB)))

(defun color (r g b)
    (ior (<< 255 24) (<< r 16) (<< g 8) b))

(defun setPixel (image x y color)
    (progn
        (call setRGB (int int int) (x y color) image)))

(defun getWidth (image)
    (call getWidth image))

(defun getHeight (image)
    (call getHeight image))

(defun getLength (image)
    (- (* (getWidth image) (getHeight image)) 1))

(defun mapImage (image func)
    (progn
        (local width (getWidth image))
        (local height (getHeight image))
        (local y 0)
        (fori (0 (- height 1))
            (progn
                (local x 0)
                (fori (0 (- width 1))
                    (progn
                        (setPixel image x y (func x y))
                        (local x (+ x 1))))
                (local y (+ y 1))))
                image))

(defun exportImage (image path)
    (call-static javax.imageio.ImageIO:write
        ((class java.awt.image.RenderedImage) string (class java.io.File))
        (image "png" (new java.io.File (path)))))

(defun black (x y)
    (color 0 0 0))

(defun white (x y)
    (color 0xFF 0xFF 0xFF))

(defun mod-grid (x y)
    (color (% (* x y) 255) (% (* x y) 255)(% (* x y) 255)))

;  (exportImage (mapImage (makeImage 255 255) mod-grid) "img.png")
