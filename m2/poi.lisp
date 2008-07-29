;; poi.lisp

;; Klassen und Funktione für die "Points of Information", die für die
;; Quadratmeter-Datenbank gespeichert werden.

(in-package :bos.m2)

;;; POI-Anwendungsklassen und Konstruktoren

;;; poi-image
(define-persistent-class poi-image (store-image)
  ((poi :read)
   (title :update :initform (make-string-hash-table))
   (subtitle :update :initform (make-string-hash-table))
   (description :update :initform (make-string-hash-table))))

(defmethod print-object ((object poi-image) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (format stream "~D" (store-object-id object))))

(deftransaction make-poi-image (language &key title subtitle description poi)
  (let ((poi-image (make-object 'poi-image :poi poi)))
    (setf (slot-string poi-image 'title language) title)
    (setf (slot-string poi-image 'subtitle language) subtitle)
    (setf (slot-string poi-image 'description language) description)
    poi-image))

(defmethod destroy-object :before ((poi-image poi-image))
  (with-slots (poi) poi-image
    (when poi
      (setf (poi-images poi) (remove poi-image (poi-images poi))))))

(defmethod initialize-persistent-instance :after ((poi-image poi-image))
  (setf (poi-images (poi-image-poi poi-image)) (append (poi-images (poi-image-poi poi-image)) (list poi-image))))

(deftransaction update-poi-image (poi-image language
                                            &key title subtitle description)
  (when title
    (setf (slot-string poi-image 'title language) title))
  (when subtitle
    (setf (slot-string poi-image 'subtitle language) subtitle))
  (when description
    (setf (slot-string poi-image 'description language) description)))

;;; poi-movie
(define-persistent-class poi-movie ()
  ((poi :read)
   (url :update :initform nil)))

;;; poi
(define-persistent-class poi ()
  ((published :update :initform nil)
   (name :read :index-type string-unique-index
               :index-reader find-poi :index-values all-pois
               :documentation "Symbolischer Name")
   (title :update :initform (make-string-hash-table) :documentation "Angezeigter Name")
   (subtitle :update :initform (make-string-hash-table) :documentation "Unterschrift")
   (description :update :initform (make-string-hash-table) :documentation "Beschreibungstext")
   (area :update :initform nil :documentation "Polygon mit den POI-Koordinaten")
   (icon :update :initform "palme" :documentation "Name des Icons")   
   (medias :update :initform nil)))

(defmethod poi-movies :before ((poi poi))
  "Lazily update the db schema. Method can be removed later."
  (macrolet ((movie (tail) `(car ,tail)))
    (mapl (lambda (tail)
            (when (stringp (movie tail))
              (setf (movie tail)
                    (make-object 'poi-movie :poi poi :url (movie tail)))))
          (slot-value poi 'movies))))

(deftransaction make-poi (language name &key title description area)
  (let ((poi (make-object 'poi :name name :area area)))
    (setf (slot-string poi 'title language) title)
    (setf (slot-string poi 'description language) description)
    poi))

(defmethod destroy-object :before ((poi poi))
  (mapc #'delete-object (poi-images poi)))

(defmethod poi-complete ((poi poi) language)
  (and (every #'(lambda (slot-name) (slot-string poi slot-name language nil)) '(title subtitle description))
       (poi-area poi)
       (poi-images poi)
       t))

(defun update-poi (poi language &key title subtitle description area icon published (images :not-set) (movies :not-set))
  (with-transaction ()
    (setf (slot-value poi 'published) published)
    (when title
      (setf (slot-string poi 'title language) title))
    (when subtitle
      (setf (slot-string poi 'subtitle language) subtitle))
    (when description
      (setf (slot-string poi 'description language) description))
    (when area
      (setf (poi-area poi) area))
    (when icon
      (setf (poi-icon poi) icon))
    (when (listp images)
      (setf (poi-images poi) images))
    (when (listp movies)
      (setf (poi-movies poi) movies))))

(defmethod poi-center-x ((poi poi))
  (first (poi-area poi)))

(defmethod poi-center-y ((poi poi))
  (second (poi-area poi)))

(defun poi-center-lon-lat (poi)
  (geo-utm:utm-x-y-to-lon-lat (+ +nw-utm-x+ (poi-center-x poi)) (- +nw-utm-y+ (poi-center-y poi)) +utm-zone+ t))

(defun make-poi-javascript (language)
  "Erzeugt das POI-Javascript für das Infosystem"
  (with-output-to-string (*standard-output*)
    (format t "var anzahlSponsoren = ~D;~%" (number-of-paying-sponsors))
    (format t "var anzahlVerkauft = ~D;~%" (number-of-sold-sqm))
    (format t "var pois = new Array;~%")
    (dolist (poi (sort (remove-if #'(lambda (poi) (or (not (poi-complete poi language))
                                                      (not (poi-published poi))))
                                  (store-objects-with-class 'poi))
                       #'(lambda (poi-1 poi-2) (string-lessp (slot-string poi-1 'title language) (slot-string poi-2 'title language)))))
      (format t "
var poi = { symbol: ~S,
            icon: ~S,
            name: ~S,
            untertitel: ~S,
            text: ~S,
            x: ~D,
            y: ~D,
            thumbnail: ~D
};
"
              (poi-name poi)
              (poi-icon poi)
              (slot-string poi 'title language)
              (slot-string poi 'subtitle language)
              (escape-nl (slot-string poi 'description language))
              (poi-center-x poi)
              (poi-center-y poi)
              (length (poi-images poi)))
      (format t "poi.thumbnail = ~D;~%" (length (poi-images poi)))
      (when (poi-airals poi)

        (format t "poi.luftbild = ~D;~%" (store-object-id (first (poi-airals poi)))))
      (when (poi-panoramas poi)
        (format t "poi.panoramas = [ ~{~D~^, ~} ];~%" (mapcar #'store-object-id (poi-panoramas poi))))
      (when (poi-movies poi)
        (format t "poi.movies = [ ~{~S~^, ~} ];~%" (mapcar #'poi-movie-url (poi-movies poi))))
      (loop for slot-name in '(title subtitle description)
         for javascript-name in '("imageueberschrift" "imageuntertitel" "imagetext")
         for slot-values = (mapcar (lambda (image)
                                     (escape-nl (slot-string image slot-name language)))
                                   (poi-images poi))
         when slot-values
         do (format t "poi.~A = [ ~{~S~^, ~} ];~%" javascript-name slot-values))
      (format t "pois.push(poi);~%"))
    (dolist (allocation-area (remove-if (complement #'allocation-area-active-p) (class-instances 'allocation-area)))
      (destructuring-bind (x y) (allocation-area-center allocation-area)
        (format t "poi = [];~%")
        (format t "poi['icon'] = ~S;~%" "sale")
        (format t "poi['name'] = ~S;~%" "Zu Verkaufen")
        (format t "poi['x'] = ~D;~%" x)
        (format t "poi['y'] = ~D;~%" y)
        (format t "poi['thumbnail'] = 0;~%")
        (format t "pois.push(poi);~%")))))
