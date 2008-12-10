(in-package :bos.m2)

(defun draw-coordinate (x y coord)
  (let ((font (pdf:get-font "Helvetica")))
    (dolist (part (nreverse (cl-ppcre:split ", "
                                            (with-output-to-string (s)
                                              (apply #'geometry:format-lon-lat s coord)))))
      (pdf:draw-left-text x y part font 8 300)
      (incf y 10))))

(defun make-m2-pdf (contract &key print template)
  (flet ((render-m2s ()
           (pdf:in-text-mode
             (destructuring-bind (bb-x bb-y bb-width bb-height) (contract-bounding-box contract)
               (let* ((m2s (sort (copy-list (contract-m2s contract))
                                 (lambda (a b)
                                   (if (= (m2-y a) (m2-y b))
                                       (- (m2-x a) (m2-x b))
                                       (- (m2-y b) (m2-y b))))))
                      (first-m2 (first m2s))
                      (last-m2 (first (last m2s)))
                      (scale (/ 80 (max bb-width bb-height))))

                 (draw-coordinate 140 240 (m2-lon-lat first-m2))

                 (unless (eq first-m2 last-m2)
                   (draw-coordinate 220 120 (m2-lon-lat last-m2)))

                 (pdf:translate (+ 95.0 (if (>= bb-width bb-height) 0
                                            (* 0.5 (abs (- bb-width bb-height)) scale)))
                                (+ 145.0 (if (>= bb-height bb-width) 0
                                            (* 0.5 (abs (- bb-width bb-height)) scale))))

                 (pdf:scale scale scale)

                 (pdf:set-line-width 0.05)
                 (pdf:set-gray-stroke 0.6)
                 (pdf:move-to 0 0)
                 (pdf:line-to 0 bb-height)
                 (pdf:line-to bb-width bb-height)
                 (pdf:line-to bb-width 0)
                 (pdf:close-and-stroke)
                 (pdf:stroke)

                 (pdf:set-line-width 0.1)
                 (pdf:set-gray-stroke 0)
                 (pdf:set-gray-fill 0.6)
                 (pdf:set-line-join 2)

                 (dolist (m2 (contract-m2s contract))
                   (let ((x (- (m2-x m2) bb-x))
                         (y (- (m2-y m2) bb-y)))
                     (pdf:move-to x y)
                     (pdf:line-to x (1+ y))
                     (pdf:line-to (1+ x) (1+ y))
                     (pdf:line-to (1+ x) y)
                     (pdf:line-to x y)
                     (pdf:close-fill-and-stroke)))))))
         (save-pdf ()
           (pdf:write-document (contract-m2-pdf-pathname contract :print print))))
    (if template
        (if print
            (pdf:with-existing-document (template)
              (pdf:with-existing-page (0)
                (pdf:insert-original-page-content))
              (pdf:with-existing-page (1)
                (pdf:insert-original-page-content)
                (render-m2s))
              (save-pdf))
            (pdf:with-existing-document (template)
              (pdf:with-existing-page (0)
                (pdf:insert-original-page-content)
                (render-m2s))
              (save-pdf)))
        (pdf:with-document ()
          (pdf:with-page ()
            (render-m2s))
          (save-pdf)))
    t))

#+(or)
(make-m2-pdf (random-elt (class-instances 'contract)))