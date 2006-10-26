;; poi.lisp

;; Klassen und Funktione für die "Points of Information", die für die
;; Quadratmeter-Datenbank gespeichert werden.

;; Die Implementation kurvt ein bisschen um den aktuellen Datastore
;; herum, da eine ästhetische Implementation der mehrsprachigen
;; Strings MOP erforderlich machen würde, die Umstellung des Datastore
;; auf MOP jedoch noch nicht fertig ist.

(in-package :bos.m2)

;; Multilinguale Strings als Slots, werden als Hashes im Objekt
;; gespeichert und über slot-string bzw. (setf slot-string)
;; angesprochen.

(defun make-string-hash-table ()
  (make-hash-table :test #'equal))

(defun slot-string (object slot-name language &optional (not-found-value ""))
  (or (gethash language (slot-value object slot-name)) not-found-value))

(defun set-slot-string (object slot-name language new-value)
  (unless (in-transaction-p)
    (error "attempt to set string in multi-language string slot ~a of object ~a outside of transaction" slot-name object))
  (setf (gethash language (slot-value object slot-name)) new-value))

(defsetf slot-string set-slot-string)

(deftransaction set-slot-string-values (object language &rest args)
  (loop for (slot-name value) on args by #'cddr
	do (setf (slot-string object slot-name language) value)))

;; POI-Anwendungsklassen und Konstruktoren

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

(define-persistent-class poi ()
  ((name :read :index-type string-unique-index
	 :index-reader find-poi :index-values all-pois
	 :documentation "Symbolischer Name")
   (title :update :initform (make-string-hash-table) :documentation "Angezeigter Name")
   (subtitle :update :initform (make-string-hash-table) :documentation "Unterschrift")
   (description :update :initform (make-string-hash-table) :documentation "Beschreibungstext")
   (area :update :initform nil :documentation "Polygon mit den POI-Koordinaten")
   (icon :update :initform "palme" :documentation "Name des Icons")
   (images :update :initform nil)
   (airals :update :initform nil)
   (panoramas :update :initform nil)
   (published :update :initform nil)))

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

(defun update-poi (poi language &key title subtitle description area icon published (images :not-set))
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
      (setf (poi-images poi) images))))

(defmethod poi-center-x ((poi poi))
  (first (poi-area poi)))

(defmethod poi-center-y ((poi poi))
  (second (poi-area poi)))

(defun make-poi-javascript (language)
  "Erzeugt das POI-Javascript für das Infosystem"
  (with-output-to-string (*standard-output*)
    (format t "var anzahlSponsoren = ~D;~%" (length (remove-if-not #'(lambda (sponsor) (some #'contract-paidp (sponsor-contracts sponsor)))
							       (class-instances 'sponsor))))
    (format t "var anzahlVerkauft = ~D;~%" (bos.m2::number-of-sold-sqm))
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
	(let ((panorama-ids (mapcar #'store-object-id (poi-panoramas poi))))
	  (format t "poi.panoramas = [ ~D~{, ~D~} ];~%" (first panorama-ids) (rest panorama-ids))))
      (loop for slot-name in '(title subtitle description)
	    for javascript-name in '("imageueberschrift" "imageuntertitel" "imagetext")
	    for slot-values = (mapcar #'(lambda (image)
					  (escape-nl (slot-string image slot-name language)))
				      (poi-images poi))
	    when slot-values
	    do (format t "poi.~A = [ ~S~{, ~S~} ];~%" javascript-name (car slot-values) (cdr slot-values)))
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
