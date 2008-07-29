                                        ; please don't read this code, it is not pretty

(in-package :bos.web)

(enable-interpol-syntax)

;; map-browser-handler

;; Dient zur Auswahl eines Punktes auf dem Projektgelände.  Zunächst
;; wird die Übersichtskarte angezeigt (360x360 Pixel).  Bei Klick auf
;; die Karte wird die unverkleinerte Overview-Ansicht angezeigt.
;; Durch einen weiteren Klick wird die Position ausgewählt.  Bei
;; Auswahl des Punktes wird zu der im url-Parameter festgelegten URL
;; verzweigt, die Koordinaten des Punktes werden als 'x'- und
;; 'y'-Parameter an diese URL übergeben.

(defclass map-browser-handler (prefix-handler)
  ())

(defun decode-coords-in-handler-path (handler)
  (labels ((ensure-valid-coordinates (x y)
             (setq x (parse-integer x))
             (setq y (parse-integer y))
             (when (and (<= 491698 x 502498)
                        (<= 9879300 y 9890100))
               (decf x 491698)
               (decf y 9879300))
             (unless (and (<= 0 x 10800)
                          (<= 0 y 10800))
               (error "invalid coordinates ~A/~A" x y))
             (list x y)))
    (with-query-params (xcoord ycoord)
      (when (and xcoord ycoord)
        (return-from decode-coords-in-handler-path (ensure-valid-coordinates xcoord ycoord))))
    (let ((handler-arguments (decoded-handler-path handler)))
      (when (and handler-arguments
                 (< 1 (length handler-arguments)))
        (apply #'ensure-valid-coordinates handler-arguments)))))

(defmethod handle ((handler map-browser-handler))
  (with-query-params (chosen-url)
    (when chosen-url
      (setf (hunchentoot:session-value :chosen-url) chosen-url)))
  (with-query-params (view-x view-y)
    (destructuring-bind (&optional click-x click-y) (decode-ismap-query-string)
      (destructuring-bind (&optional point-x point-y) (decode-coords-in-handler-path handler)
        (with-query-params (action)
          (when (equal action "save")
            (if (hunchentoot:session-value :chosen-url)
                (redirect (format nil "~Ax=~D&y=~D"
                                  (hunchentoot:session-value :chosen-url)
                                  point-x
                                  point-y))
                (with-bos-cms-page (:title "Map Point Chooser")
                  (html (:princ-safe "You chose " point-x " / " point-y))))
            (return-from handle t)))
        (cond
          ((and view-x view-y)
           (setq view-x (parse-integer view-x)
                 view-y (parse-integer view-y)))
          (t
           (setq view-x point-x
                 view-y point-y)))
        (let ((start-tile (and point-y
                               (ensure-map-tile (max 0 (- view-x 180))
                                                (max 0 (- view-y 180))))))
          (when (and point-y click-y)
            (let ((click-coord-x (+ (tile-nw-x start-tile) click-x))
                  (click-coord-y (+ (tile-nw-y start-tile) click-y)))
              (setq point-x click-coord-x
                    point-y click-coord-y)
              (redirect (format nil "/map-browser/~D/~D" click-coord-x click-coord-y))
              (return-from handle t)))
          (cond
            ((and click-y (not point-y))
             (redirect (format nil "/map-browser/~D/~D" (* 30 click-x) (* 30 click-y))))
            (point-y
             (with-bos-cms-page (:title "Map Point Chooser")
               (with-query-params (heading)
                 (when heading
                   (html (:h2 (:princ-safe heading)))))
               (html
                ((:script :language "JavaScript")
                 "
function updateCoords() {
        var new_x = document.mapnavigator.xcoord.value;
        var new_y = document.mapnavigator.ycoord.value;
        document.location.href = '/map-browser/' + new_x + '/' + new_y;

        return false;
}
"
                 ))
               (html ((:div :style "position:relative; height:400px;")
                      ((:div :id "overview"
                             :style "position:absolute; left:0px; top:0px;")
                       ((:a :href "/map-browser/")
                        ((:img :ismap "ismap" :width "360" :height "360" :border "0" :src "/images/sl_all.jpg"))))
                      (let ((view-cursor-x (- (round (tile-nw-x start-tile) 30) 2))
                            (view-cursor-y (- (round (tile-nw-y start-tile) 30) 2)))
                        (html ((:div :id "overview-cursor-tile"
                                     :style #?"position:absolute; left:$(view-cursor-x)px; top:$(view-cursor-y)px")
                               ((:img :src "/images/overview-cursor.png")))))
                      (let ((point-x (- (round point-x 30) 2))
                            (point-y (- (round point-y 30) 2)))
                        (html ((:div :id "overview-cursor"
                                     :style #?"position:absolute; left:$(point-x)px; top:$(point-y)px")
                               ((:img :src "/images/map-cursor.png" :width 5 :height 5)))))
                      (loop for y from 0 upto 270 by 90
                         for tile-index-y from 0 by 1
                         for map-y = (+ (tile-nw-y start-tile) y)
                         for screen-y = y
                         do (loop for x from 0 upto 270 by 90
                               for map-x = (+ (tile-nw-x start-tile) x)
                               for tile-index-x from 0 by 1
                               for screen-x = (+ x 380)
                               do (html ((:div :id #?"tile-$(tile-index-x)-$(tile-index-y)"
                                               :style #?"position:absolute; left:$(screen-x)px; top:$(screen-y)px;")
                                         ((:img :width "90" :height "90"
                                                            :border "0"
                                                            :src #?"/overview/$(map-x)/$(map-y)"))))))
                      ((:div :id "overlay"
                             :style #?"position:absolute; left:380px; top:0px; width:360px; height:360px;")
                       ((:a :href #?"/map-browser/$(point-x)/$(point-y)")
                        ((:img :src "/images/trans.gif" :ismap "ismap" :border "0" :width "360" :height "360"))))
                      (let* ((cursor-x (- (+ 380 (- point-x (tile-nw-x start-tile))) 8)) ; 380 -> horizontal offset for tiled map
                             (cursor-y (- point-y (tile-nw-y start-tile) 8)))
                        (html
                         ((:div :id "cursor"
                                :style #?"position:absolute; left:$(cursor-x)px; top:$(cursor-y)px; visibility:visible")
                          ((:img :src "/images/map-cursor.png")))))))
               (map-navigator point-x point-y "/map-browser/" :formcheck "return updateCoords();")))
            (t
             (with-bos-cms-page (:title "Map Point Chooser")
               (html
                ((:a :href "/map-browser/")
                 ((:img :ismap "ismap" :src "/image/sl_all"))))))))))))