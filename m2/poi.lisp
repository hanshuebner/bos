;; poi.lisp

;; Klassen und Funktione für die "Points of Information", die für die
;; Quadratmeter-Datenbank gespeichert werden.

(in-package :bos.m2)

;;; POI-Anwendungsklassen und Konstruktoren

;;; textual-attributes-mixin
(defpersistent-class textual-attributes-mixin ()
  ((title :initform (make-string-hash-table)
          :documentation "angezeigter name")
   (subtitle :initform (make-string-hash-table)
             :documentation "unterschrift")
   (description :initform (make-string-hash-table)
                :documentation "beschreibungstext")))

(defmethod initialize-persistent-instance :after ((obj textual-attributes-mixin)
                                                  &key language title subtitle description)
  (update-textual-attributes obj language
                             :title title
                             :subtitle subtitle
                             :description description))

(deftransaction update-textual-attributes (obj language &key title subtitle description)
  (when title
    (setf (slot-string obj 'title language) title))
  (when subtitle
    (setf (slot-string obj 'subtitle language) subtitle))
  (when description
    (setf (slot-string obj 'description language) description))
  obj)

;;; poi-medium
(defpersistent-class poi-medium (textual-attributes-mixin)
  ((poi :reader poi-medium-poi :initarg :poi)))

(deftransaction make-poi-medium (class-name &rest rest &key language title subtitle description poi initargs)
  (declare (ignore poi initargs))
  (assert (if (or title subtitle description) language t) nil
          "language needs to be specified, if any of title, subtitle
           or description is given")
  (apply #'make-object class-name rest))

(defmethod initialize-persistent-instance :after ((poi-medium poi-medium) &key poi)
  (when poi
    (push poi-medium (poi-media poi))))

(defmethod print-object ((object poi-medium) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (format stream "~D" (store-object-id object))))

(defmethod destroy-object :before ((poi-medium poi-medium))
  (with-slots (poi) poi-medium
    (when poi
      (setf (poi-media poi) (remove poi-medium (poi-media poi))))))

;;; poi-image
(defpersistent-class poi-image (store-image poi-medium)
  ())

;;; poi-airal
(defpersistent-class poi-airal (store-image poi-medium)
  ())

;;; poi-panorama
(defpersistent-class poi-panorama (store-image poi-medium)
  ())

;;; poi-movie
(defpersistent-class poi-movie (poi-medium)
  ((url :accessor poi-movie-url :initarg :url :initform nil)))

;;; poi
(defpersistent-class poi (textual-attributes-mixin)
  ((name
    :reader poi-name :initarg :name
    :index-type string-unique-index
    :index-reader find-poi :index-values all-pois
    :documentation "symbolischer name")
   (published
    :accessor poi-published :initarg :published :initform nil
    :documentation "wenn dieses flag nil ist, wird der poi in den uis nicht angezeigt")
   (area
    :accessor poi-area :initarg :area :initform nil
    :documentation "polygon mit den poi-koordinaten")
   (icon
    :accessor poi-icon :initarg :icon :initform "palme"
    :documentation "name des icons")
   (media
    :accessor poi-media :initarg :media :initform nil
    :documentation "liste aller poi-medien, wie poi-image, poi-airal ...")))

(deftransaction make-poi (name &rest rest &key area language title subtitle description)
  (declare (ignore area))
  (assert (if (or title subtitle description) language t) nil
          "language needs to be specified, if any of title, subtitle
           or description is given")
  (apply #'make-object 'poi :name name rest))

(defmethod destroy-object :before ((poi poi))
  (mapc #'delete-object (poi-media poi)))

(defmethod poi-complete ((poi poi) language)
  (and (every #'(lambda (slot-name) (slot-string poi slot-name language nil)) '(title subtitle description))
       (poi-area poi)
       (<= 6 (count-if (lambda (medium) (typep medium 'poi-image)) (poi-media poi)))
       t))

(defmethod poi-center-x ((poi poi))
  (first (poi-area poi)))

(defmethod poi-center-y ((poi poi))
  (second (poi-area poi)))

(defun poi-center-lon-lat (poi)
  (geo-utm:utm-x-y-to-lon-lat (+ +nw-utm-x+ (poi-center-x poi)) (- +nw-utm-y+ (poi-center-y poi)) +utm-zone+ t))

;;; POI media are stored in one list - for convenience we provide
;;; accessors by type. POI-IMAGES e.g. returns a list of all
;;; POI-IMAGES in the same order as they appear in the media list. The
;;; second value is a list of corresponding positions in that list.
(macrolet ((define-poi-medium-reader (name)
             (let ((type (find-symbol (subseq (symbol-name name) 0 (1- (length (symbol-name name)))))))
               (assert type)
               `(defun ,name (poi)
                  ;; this surely could be optimized
                  (let ((media-of-type (remove-if-not (lambda (medium) (typep medium ',type)) (poi-media poi))))
                    (values media-of-type
                            (mapcar (lambda (medium) (position medium (poi-media poi))) media-of-type)))))))
  (define-poi-medium-reader poi-images)
  (define-poi-medium-reader poi-airals)
  (define-poi-medium-reader poi-panoramas)
  (define-poi-medium-reader poi-movies))

(defun poi-sat-images (poi)
  "We use the 6 last (oldest) images of POI as images for the
  satellite application."
  (multiple-value-bind (images positions)
      (poi-images poi)
    (let* ((length (length images))
           (start (max 0 (- length 6))))
      (values (subseq images start length)
              (subseq positions start length)))))

;;; Provides for the shifting of images in the edit-poi handler.
;;; Exchanges (nth index (poi-sat-images poi)) with
;;; (nth (1+ index) (poi-sat-images poi)).
(deftransaction poi-sat-images-exchange-neighbours (poi index)  
  (check-type index (integer 0 4))
  (multiple-value-bind (images positions)
      (poi-images poi)
    (declare (ignore images))
    (let ((media-index-a (nth index positions))
          (media-index-b (nth (1+ index) positions)))
      (rotatef (nth media-index-a (poi-media poi))
               (nth media-index-b (poi-media poi))))))

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
              (length (poi-sat-images poi)))
      (format t "poi.thumbnail = ~D;~%" (length (poi-sat-images poi)))
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
                                   (poi-sat-images poi))
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

;;; poi schema evolution aids

(define-modify-macro appendf (&rest args) append)

(defmethod convert-slot-value-while-restoring ((poi poi) (slot-name (eql 'airals)) value)
  (appendf (poi-media poi) (mapcar (lambda (obj) (change-class obj 'poi-airal :poi poi)) value)))

(defmethod convert-slot-value-while-restoring ((poi poi) (slot-name (eql 'images)) value)
  (appendf (poi-media poi) (mapcar (lambda (obj) (change-class obj 'poi-image :poi poi)) value)))

(defmethod convert-slot-value-while-restoring ((poi poi) (slot-name (eql 'movies)) value)
  (appendf (poi-media poi) (mapcar (lambda (url) (make-instance 'poi-movie :url url :poi poi)) value)))

(defmethod convert-slot-value-while-restoring ((poi poi) (slot-name (eql 'panoramas)) value)
  (appendf (poi-media poi) (mapcar (lambda (obj) (change-class obj 'poi-panorama :poi poi)) value)))

(defun pois-sanity-check ()
  (labels ((poi-sanity-check (poi)
             (dolist (medium (poi-media poi))
               (unless (eq poi (poi-medium-poi medium))
                 (warn "~s does not point to ~s" medium poi)))
             (dolist (movie (poi-movies poi))
               (unless (stringp (poi-movie-url movie))
                 (warn "~s has a url of ~s" movie (poi-movie-url movie))))))
    (mapc #'poi-sanity-check (class-instances 'poi))
    (values)))

