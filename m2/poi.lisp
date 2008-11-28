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

(defmethod initialize-instance :after ((obj textual-attributes-mixin)
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

(deftransaction very-shallow-copy-textual-attributes (from to)
  "Useful for making the TEXTUAL-ATTRIBUTES of FROM available to TO,
before FROM is deleted. Please note that copying is so shallow that
FROM and TO must not both continue to exist."
  (setf (slot-value to 'title) (slot-value from 'title)
        (slot-value to 'subtitle) (slot-value from 'subtitle)
        (slot-value to 'description) (slot-value from 'description))
  to)

;;; poi-medium
(defpersistent-class poi-medium (textual-attributes-mixin)
  ((poi :reader poi-medium-poi :initarg :poi)))

(deftransaction make-poi-medium (class-name &rest rest &key language title subtitle description poi initargs)
  (declare (ignore poi initargs))
  (assert (if (or title subtitle description) language t) nil
          "language needs to be specified, if any of title, subtitle
           or description is given")
  (apply #'make-instance class-name (remove-keys '(:initargs) rest)))

(defmethod initialize-instance :after ((poi-medium poi-medium) &key poi)
  (when poi
    (push poi-medium (poi-media poi))))

(defmethod print-object ((object poi-medium) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (format stream "~D" (store-object-id object))))

(defgeneric poi-medium-creation-time (medium)
  (:method ((medium blob))
    (blob-timestamp medium)))

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
  ((url :accessor poi-movie-url :initarg :url :initform nil)
   (created :initform (error "need :created initarg when creating poi-medium")
            :initarg :created
            :reader poi-medium-creation-time)))

;;; poi
(defpersistent-class poi (textual-attributes-mixin)
  ((name
    :reader poi-name :initarg :name
    :index-type string-unique-index
    :index-reader find-poi :index-values all-pois
    :documentation "symbolischer name")
   (published-web
    :accessor poi-published-web :initarg :published-web :initform nil
    :documentation "wenn dieses flag nil ist, wird der poi auf der Website nicht angezeigt")
   (published-earth
    :accessor poi-published-earth :initarg :published-earth :initform nil
    :documentation "wenn dieses flag nil ist, wird der poi in Google Earth nicht angezeigt")
   (area
    :accessor poi-area :initarg :area :initform nil
    :documentation "polygon mit den poi-koordinaten")
   (icon
    :accessor poi-icon :initarg :icon :initform "palme"
    :documentation "name des icons")
   (media
    :accessor poi-media :initarg :media :initform nil
    :documentation "liste aller poi-medien, wie poi-image, poi-airal ...")
   (lod-min
    :accessor poi-lod-min :initarg :poi-lod-min :initform 600
    :documentation "the lod minimum used in Google Earth")))


(defmethod convert-slot-value-while-restoring ((object poi) (slot-name (eql 'published))
                                               published)
  (setf (slot-value object 'published-web) published))

(deftransaction make-poi (name &rest rest &key area language title subtitle description)
  (declare (ignore area))
  (assert (if (or title subtitle description) language t) nil
          "language needs to be specified, if any of title, subtitle
           or description is given")
  (apply #'make-instance 'poi :name name rest))

(defmethod destroy-object :before ((poi poi))
  (mapc #'delete-object (poi-media poi)))

(deftransaction update-poi (poi &key published-web published-earth icon area lod-min)
  (check-type published-web boolean)
  (check-type published-earth boolean)
  (check-type area list)
  (setf (poi-published-web poi) published-web
        (poi-published-earth poi) published-earth)
  (when icon
    (setf (poi-icon poi) icon))
  (when area
    (setf (poi-area poi) area))
  (when lod-min
    (setf (poi-lod-min poi) (abs lod-min)))
  poi)

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

(defmethod (setf poi-media) :after (value (poi poi))
  (setf (slot-value poi 'media) (sort (slot-value poi 'media) #'> :key #'poi-medium-creation-time)))

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
  (check-type index (mod 6))
  (multiple-value-bind (images positions)
      (poi-images poi)
    (declare (ignore images))
    (let ((media-index-a (nth index positions))
          (media-index-b (nth (mod (1+ index) 6) positions)))
      (rotatef (nth media-index-a (poi-media poi))
               (nth media-index-b (poi-media poi))))))

(defun make-poi-javascript (language)
  "Erzeugt das POI-Javascript für das Infosystem"
  (with-output-to-string (*standard-output*)
    (format t "var anzahlSponsoren = ~D;~%" (number-of-paying-sponsors))
    (format t "var anzahlVerkauft = ~D;~%" (number-of-sold-sqm))
    (format t "var pois = new Array;~%")
    (dolist (poi (sort (remove-if #'(lambda (poi) (or (not (poi-complete poi language))
                                                      (not (poi-published-web poi))))
                                  (store-objects-with-class 'poi))
                       #'(lambda (poi-1 poi-2) (string-lessp (slot-string poi-1 'title language) (slot-string poi-2 'title language)))))
      (format t "
var poi = { id: ~S,
            symbol: ~S,
            icon: ~S,
            name: ~S,
            untertitel: ~S,
            text: ~S,
            x: ~D,
            y: ~D,
            thumbnail: ~D,
            published_earth: ~:[false~;true~]
};
"
              (store-object-id poi)
              (poi-name poi)
              (poi-icon poi)
              (slot-string poi 'title language)
              (slot-string poi 'subtitle language)
              (escape-nl (slot-string poi 'description language))
              (poi-center-x poi)
              (poi-center-y poi)
              (length (poi-sat-images poi))
              (poi-published-earth poi))
      (format t "poi.thumbnail = ~D;~%" (length (poi-sat-images poi)))
      (when (poi-airals poi)

        (format t "poi.luftbild = ~D;~%" (store-object-id (first (poi-airals poi)))))
      (when (poi-panoramas poi)
        (format t "poi.panoramas = [ ~{~D~^, ~} ];~%" (mapcar #'store-object-id (poi-panoramas poi))))
      (when (poi-movies poi)
        (format t "poi.movies = [ ~{~S~^, ~} ];~%"
                (mapcar #'(lambda (movie)
                            (assert (stringp (poi-movie-url movie)) nil
                                    "POI-MOVIE-URL of ~S is ~S, but should be a string"
                                    movie (poi-movie-url movie))
                            (poi-movie-url movie))
                        (poi-movies poi))))
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
  (unless (slot-boundp poi 'media) (setf (slot-value poi 'media) nil))
  (appendf (slot-value poi 'media) (mapcar (lambda (obj) (change-class obj 'poi-airal :poi poi)) value)))

(defmethod convert-slot-value-while-restoring ((poi poi) (slot-name (eql 'images)) value)
  (unless (slot-boundp poi 'media) (setf (slot-value poi 'media) nil))
  (appendf (slot-value poi 'media) (mapcar (lambda (obj) (change-class obj 'poi-image :poi poi)) value)))

(defmethod convert-slot-value-while-restoring ((poi poi) (slot-name (eql 'movies)) value)
  (unless (slot-boundp poi 'media) (setf (slot-value poi 'media) nil))
  (appendf (slot-value poi 'media) (mapcar (lambda (url) `(poi-movie :url ,url :poi ,poi)) value)))

(defmethod convert-slot-value-while-restoring ((poi poi) (slot-name (eql 'panoramas)) value)
  (unless (slot-boundp poi 'media) (setf (slot-value poi 'media) nil))
  (appendf (slot-value poi 'media) (mapcar (lambda (obj) (change-class obj 'poi-panorama :poi poi)) value)))

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

(defvar *language* "en"
  "Current language for JSON encoding")

(defmethod json:encode ((object symbol) &optional stream)
  (json:encode (string-downcase (symbol-name object)) stream))

(defgeneric json-encode (object)
  (:method-combination progn))

(defmethod json-encode progn ((object store-object))
  (json:encode-object-element "id" (store-object-id object)))

(defmethod json-encode progn ((poi poi))
  (json:encode-object-elements
   "name" (poi-name poi)
   "icon" (poi-icon poi)
   "x" (poi-center-x poi)
   "y" (poi-center-y poi)))

(defmethod json-encode progn ((blob blob))
  (json:encode-object-elements
   "type" (blob-type blob)
   "timestamp" (format-date-time (blob-timestamp blob) :mail-style t)))

(defmethod json-encode progn ((image store-image))
  (json:encode-object-elements
   "name" (store-image-name image)
   "width" (store-image-width image)
   "height" (store-image-height image)))

(defmethod json-encode progn ((object bos.m2::textual-attributes-mixin))
  (dolist (field '(title subtitle description))
    (let ((string (slot-string object field *language*)))
      (unless (equal "" string)
        (json:encode-object-element field string)))))

(defmethod json-encode progn ((medium poi-medium))
  (json:encode-object-element
   "mediumType"
   (cl-ppcre:regex-replace "^poi-" (string-downcase (class-name (class-of medium))) "")))

(defmethod json-encode progn ((movie poi-movie))
  (json:encode-object-elements
   "url" (poi-movie-url movie)
   "timestamp" (format-date-time (poi-medium-creation-time movie) :mail-style t)))

(defun poi-as-json (poi language)
  (let ((*language* language))
    (json:with-object ()
      (json-encode poi)
      (json:with-object-element ("media")
        (json:with-array ()
          (dolist (medium (poi-media poi))
            (json:with-object ()
              (json-encode medium))))))))

(defun pois-as-json (language)
  (json:with-array ()
    (dolist (poi (class-instances 'poi))
      (when (poi-complete poi language)
        (poi-as-json poi language)))))
