
(in-package :cl-user)
(use-package :cl-gd)
(use-package :cl-interpol)
(enable-interpol-syntax)

(defun make-tiles (input-filename &key (coord-width 5) (x-offset 0) (y-offset 0) (tile-size 100) (output-prefix "tiles/tile-"))
  (with-image-from-file (input-image input-filename)
    (loop for x from 0 below (image-width input-image) by tile-size
	  do (format t "~&~a: " x)
	  do (loop for y from 0 below (image-height input-image) by tile-size
		   for tile-filename = (format nil #?"~a~$(coord-width),'0D-~$(coord-width),'0D.png"
					       output-prefix (+ x-offset x) (+ y-offset y))
		   do (princ #\.) (finish-output)
		   do (with-image (tile tile-size tile-size)
			(copy-image input-image tile x y 0 0 tile-size tile-size)
			(write-image-to-file tile-filename
					     :image tile
					     :if-exists :supersede))))))