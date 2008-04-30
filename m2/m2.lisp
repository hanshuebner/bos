(in-package :bos.m2)

;;;; M2-STORE

(defvar *m2-store* nil)

(defclass m2-store (mp-store)
  ((tile-index :reader m2-store-tile-index)))

(defmethod initialize-instance :before ((store m2-store) &key &allow-other-keys)
  (when *m2-store*
    (warn "reinitializing m2-store object"))
  (setq *m2-store* store)
  (setf (slot-value store 'tile-index)
	(indexed-class-index-named (find-class 'm2) 'm2-index)))

(defvar *store-transient-init-functions* nil)

(defun register-store-transient-init-function (function-name)
  (check-type function-name symbol)
  (pushnew function-name *store-transient-init-functions*))

(defmethod bknr.datastore::restore-store :after ((store m2-store) &key until)
  (declare (ignore store until))
  (dolist (function-name *store-transient-init-functions*)
    (funcall function-name)))

(defun get-map-tile (x y)
  (get-tile (m2-store-tile-index *m2-store*) x y))

(defun ensure-map-tile (x y)
  (ensure-tile (m2-store-tile-index *m2-store*) x y))

;;;; M2

;;; Exportierte Funktionen:
;;;
;;; M2-CONTRACT (m2) => contract or NIL
;;; M2-NUM (m2) => integer
;;; M2-PRINTABLE (m2) => string
;;; M2-X (m2) => integer
;;; M2-Y (m2) => integer
;;; M2-UTM-X (m2) => double-float
;;; M2-UTM-y (m2) => double-float
;;;
;;; GET-M2 (x y) => m2 or NIL
;;; ENSURE-M2 (x y) => m2
;;; GET-M2-WITH-NUM (sqm-num) => m2 or nil
;;; ENSURE-M2-WITH-NUM (sqm-num) => m2

(define-persistent-class m2 ()
  ((x :read)
   (y :read)
   (contract :update :relaxed-object-reference t)
   (my-slot :read))
  (:default-initargs :contract nil)
  (:class-indices (m2-index :index-type tiled-index
			    :slots (x y)
			    :index-reader m2-at
			    :index-initargs (:width +width+
                                             :height +width+
                                             :tile-size +m2tile-width+
                                             :tile-class 'image-tile))))

(defmethod print-object ((object m2) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (format stream "at (~D,~D), ~A"
            (m2-x object)
            (m2-y object)
            (if (m2-contract object) "sold" "free"))))

(defun get-m2 (&rest coords)
  (m2-at coords))

(defun ensure-m2 (&rest coords)
  (or (m2-at coords)
      (destructuring-bind (x y) coords
	(make-instance 'm2 :x x :y y))))

(defmethod get-m2-with-num ((num integer))
  (multiple-value-bind (y x) (truncate num +width+)
    (get-m2 x y)))

(defmethod get-m2-with-num ((num string))
  (get-m2-with-num (parse-integer num :radix 36)))

(defmethod ensure-m2-with-num ((num integer))
  (multiple-value-bind (y x) (truncate num +width+)
    (ensure-m2 x y)))

(defmethod ensure-m2-with-num ((num string))
  (ensure-m2-with-num (parse-integer num :radix 36)))

(defun m2-num (m2)
  "Fortlaufende Quadratmeternummer in row-major-order."
  (+ (* (m2-y m2) +width+) (m2-x m2)))

(defun m2-num-string (m2)
  "Quadratmeternummer im druckbaren Format (Radix 36, 6 Zeichen lang)"
  (format nil "~36,6,'0R" (m2-num m2)))

;; UTM laeuft von links nach rechts und von UNTEN NACH OBEN.
(defun m2-utm-x (m2) (+ +nw-utm-x+ (m2-x m2)))
(defun m2-utm-y (m2) (- +nw-utm-y+ (m2-y m2)))
(defun m2-utm (m2) (list (m2-utm-x m2) (m2-utm-y m2)))

(defun m2-lon-lat (m2)
  (geo-utm:utm-x-y-to-lon-lat (m2-utm-x m2) (m2-utm-y m2) +utm-zone+ t))

(defmethod m2-num-to-utm ((num integer))
  (multiple-value-bind (y x) (truncate num +width+)
    (+ +nw-utm-x+ x)
    (- +nw-utm-y+ y)))

(defmethod m2-num-to-utm ((num string))
  (m2-num-to-utm (parse-integer num :radix 36)))

(defmethod m2-allocation-area ((m2 m2))
  (find-if #'(lambda (allocation-area) (point-in-polygon-p (m2-x m2) (m2-y m2) (allocation-area-vertices allocation-area)))
	   (class-instances 'allocation-area)))

(defun m2s-polygon (m2s)
  (let* ((m2 (first m2s))
	 (contract (m2-contract m2)))
    (region-to-polygon (list (m2-x m2) (m2-y m2))
		       (lambda (p)
			 (let ((m2 (apply #'get-m2 p)))
			   (and m2 (eql contract (m2-contract m2))))))))

(defun m2s-polygon-lon-lat (m2s)
  (let ((polygon (m2s-polygon m2s)))
    (mapcar (lambda (point)
	      (destructuring-bind (x y) point
		(geo-utm:utm-x-y-to-lon-lat (+ +nw-utm-x+ x) (- +nw-utm-y+ y) +utm-zone+ t)))
	    polygon)))

;;;; SPONSOR

;;; Exportierte Funktionen:
;;;
;;; MAKE-SPONSOR (&rest initargs) => sponsor
;;; (Automatisch Zuweisung eines Login-Namens.)
;;;
;;; SPONSOR-PASSWORD-QUESTION (sponsor) => string
;;; SPONSOR-PASSWORD-ANSWER (sponsor) => string
;;; SPONSOR-INFO-TEXT (sponsor) => string
;;; SPONSOR-COUNTRY (sponsor) => string
;;; SPONSOR-LANGUAGE (sponsor) => string (preferred language)
;;; SPONSOR-CONTRACTS (sponsor) => list of contract
;;;
;;; Sowie Funktionen von USER.

(define-persistent-class sponsor (user)
  ((master-code :read :initform nil)
   (info-text :update :initform nil)
   (country :update :initform nil)
   (contracts :update :initform nil)
   (language :update :initform nil))
  (:default-initargs :full-name nil :email nil))

(defmethod user-editable-p ((sponsor sponsor))
  nil)

(defun sponsor-p (object)
  (equal (class-of object) (find-class 'sponsor)))

(deftransaction sponsor-set-info-text (sponsor newval)
  (setf (sponsor-info-text sponsor) newval))

(deftransaction sponsor-set-country (sponsor newval)
  (setf (sponsor-country sponsor) newval))

(deftransaction sponsor-set-language (sponsor newval)
  (setf (sponsor-language sponsor) newval))

(defmethod sponsor-language :around ((sponsor sponsor))
  (or (call-next-method)
      "en"))

(defvar *sponsor-counter* 0)

(defun make-sponsor (&rest initargs &key login &allow-other-keys)
  (apply #'make-object 'sponsor
         :login (or login (format nil "s-~36R-~36R" (incf *sponsor-counter*) (get-universal-time)))
	 :master-code (mod (+ (get-universal-time) (random 1000000)) 1000000)
         initargs))

(defun sponsor-consistent-p (sponsor)
  (labels ((contract-points-to-sponsor (contract)
             (eq sponsor (contract-sponsor contract))))
    (let ((consistent t))
      (unless (every #'contract-points-to-sponsor (sponsor-contracts sponsor))        
        (let ((*print-length* 5))
          (warn "~s of ~s dont point to it by CONTRACT-SPONSOR~
                 ~%the wrongly pointed to objs with duplicates removed are: ~s"
                (remove-if #'contract-points-to-sponsor (sponsor-contracts sponsor))
                sponsor
                (remove-duplicates (remove sponsor (mapcar #'contract-sponsor (sponsor-contracts sponsor))))))
        (setq consistent nil))
      consistent)))

(defmethod destroy-object :before ((sponsor sponsor))
  (mapc #'delete-object (sponsor-contracts sponsor)))

(defmethod sponsor-id ((sponsor sponsor))
  (store-object-id sponsor))

(define-user-flag :editor)

(defmethod editor-p ((user user))
  (or (admin-p user)
      (user-has-flag user :editor)))

(defmethod editor-p ((user null))
  nil)

(defclass editor-only-handler ()
  ())

(defmethod bknr.web:authorized-p ((handler editor-only-handler))
  (editor-p (bknr.web:bknr-session-user)))

;;;; CONTRACT

;;; Exportierte Funktionen:
;;;
;;; MAKE-CONTRACT (sponsor m2s) => contract
;;;
;;; GET-CONTRACT (id) => contract
;;;
;;; CONTRACT-SPONSOR (contract) => sponsor
;;; CONTRACT-PAIDP (contract) => boolean
;;; CONTRACT-DATE (contract) => Universal-Timestamp
;;; CONTRACT-M2S (contract) => list of m2
;;; CONTRACT-BOUNDING-BOX (contract) => (list left top width height)
;;;
;;; CONTRACT-SET-PAIDP (contract newval) => newval

(defvar *claim-colors* '((0 0 128)
			 (0 128 0)
			 (0 128 128)
			 (128 0 0)
			 (128 0 128)
			 (128 128 0)
			 (0 0 255)
			 (0 255 0)
			 (0 255 255)
			 (255 0 0)
			 (255 0 255)
			 (255 255 0)))

(define-persistent-class contract ()
  ((sponsor :read :relaxed-object-reference t)
   (date :read)
   (paidp :update)
   (m2s :read)
   (color :read)
   (download-only :update)
   (cert-issued :read)
   (worldpay-trans-id :update :initform nil)
   (expires :read :documentation "universal time which specifies the time the contract expires (is deleted) when it has not been paid for" :initform nil)
   (largest-rectangle :update))
  (:default-initargs
      :m2s nil
    :paidp nil
    :download-only nil
    :color (random-elt *claim-colors*)
    :cert-issued nil
    :expires (+ (get-universal-time) *manual-contract-expiry-time*)))

(defmethod print-object ((object contract) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (format stream "ID: ~D, ~A"
            (store-object-id object)
            (if (contract-paidp object) "paid" "unpaid"))))

(defun contract-p (object)
  (equal (class-of object) (find-class 'contract)))

(defmethod initialize-persistent-instance :after ((contract contract))
  (pushnew contract (sponsor-contracts (contract-sponsor contract)))
  (publish-contract-change contract)
  (dolist (m2 (contract-m2s contract))
    (setf (m2-contract m2) contract))
  (setf (contract-largest-rectangle contract)
        (contract-compute-largest-rectangle contract)))

(defmethod destroy-object :before ((contract contract))
  (let ((sponsor (contract-sponsor contract)))
    (when sponsor
      (setf (sponsor-contracts sponsor) (remove contract (sponsor-contracts sponsor)))))
  (publish-contract-change contract)
  (dolist (m2 (contract-m2s contract))
    (setf (m2-contract m2) nil))
  (return-contract-m2s (contract-m2s contract)))

(defun get-contract (id)
  (let ((contract (store-object-with-id id)))
    (prog1
	contract
      (unless (subtypep (type-of contract) 'contract)
	(error "invalid contract id (wrong type) ~A" id)))))

(defun publish-contract-change (contract)  
  "Note: This routine is (ab)used to reinitialize the transient contract tree
and thus may be called more than once."
  (publish-rect-change *rect-publisher* (contract-bounding-box contract) contract))

(defmethod contract-is-expired ((contract contract))
  (and (contract-expires contract)
       (> (get-universal-time) (contract-expires contract))))

(deftransaction contract-set-paidp (contract newval)
  (setf (contract-paidp contract) newval)
  (publish-contract-change contract)
  (add-contract-to-cache contract)
  (bknr.rss::add-item "news" contract))

(defmethod contract-price ((contract contract))
  (* (length (contract-m2s contract)) +price-per-m2+))

(defmethod contract-download-only-p ((contract contract))
  (or (contract-download-only contract)
      (< (contract-price contract) *mail-amount*)))

(deftransaction contract-set-download-only-p (contract newval)
  (setf (contract-download-only contract) newval))

(defmethod contract-fdf-pathname ((contract contract) &key language print)
  (when (and print
	     (contract-download-only-p contract))
    (error "no print fdf for download-only contract ~A" contract))
  (merge-pathnames (make-pathname :name (format nil "~D-~(~A~)"
                                                (store-object-id contract)
                                                language)
				  :type "fdf")
		   (if print *cert-mail-directory* *cert-download-directory*)))

(defmethod contract-pdf-pathname ((contract contract) &key print)
  (merge-pathnames (make-pathname :name (format nil "~D" (store-object-id contract))
				  :type "pdf")
		   (if print bos.m2::*cert-mail-directory* bos.m2::*cert-download-directory*)))

(defmethod contract-pdf-url ((contract contract))
  (format nil "/certificate/~A" (store-object-id contract)))

(defmethod contract-certificates-generated-p (contract)
  (and (probe-file (contract-pdf-pathname contract))
       (or (contract-download-only-p contract)
	   (probe-file (contract-pdf-pathname contract :print t)))))

(defmethod contract-delete-certificate-files (contract)
  (ignore-errors
    (delete-file (contract-pdf-pathname contract))
    (delete-file (contract-pdf-pathname contract :print t))))

(defun wait-for-certificates (contract)
  "Wait until the PDF generating process has generated the certificates"
  (dotimes (i 10)
    (when (contract-certificates-generated-p contract)
      (return))
    (sleep 1))
  (unless (contract-certificates-generated-p contract)
    (error "Cannot generate certificate")))

(defmethod contract-issue-cert ((contract contract) name &key address language)
  (when (contract-cert-issued contract)
    (warn "re-issuing cert for ~A" contract))
  (contract-delete-certificate-files contract)
  (make-certificate contract name :address address :language language)
  (unless (contract-download-only-p contract)
    (make-certificate contract name :address address :language language :print t))
  (wait-for-certificates contract)
  (change-slot-values contract 'cert-issued t))

(defmethod contract-image-tiles ((contract contract))
  (let (image-tiles)
    (dolist (m2 (contract-m2s contract))
      (pushnew (get-map-tile (m2-x m2) (m2-y m2))
	       image-tiles))
    image-tiles))

(defmethod contract-bounding-box ((contract contract))
  (geometry:with-bounding-box-collect (collect)
    (dolist (m2 (contract-m2s contract))
      (collect (list (m2-x m2) (m2-y m2))))))

(defun contracts-bounding-box (&optional (contracts (all-contracts)))  
  (geometry:with-bounding-box-collect (collect)
    (dolist (contract contracts)
      (dolist (m2 (contract-m2s contract))
        (collect (list (m2-x m2) (m2-y m2)))))))

(defun contract-area (contract)
  (length (contract-m2s contract)))

(defun contract-polygon (contract)
  (m2s-polygon (contract-m2s contract)))

(defun contract-compute-largest-rectangle (contract)
  (macrolet ((when-scaling-needed (arg &body body)
               `(if (= scaler 1)
                    ,arg
                    (progn ,@body))))
    (let* ((m2s (contract-m2s contract))
           (area (length m2s))
           (scaler (ceiling area 1000.0))
           (bounding-box (contract-bounding-box contract)))
      (geometry:with-rectangle bounding-box
        (declare (ignore width height))
        (labels ( ;; to-orig
                 (distance-to-orig (d)
                   (when-scaling-needed d
                     (round (* d scaler))))
                 (x-coordinate-to-orig (x)
                   (when-scaling-needed x
                     (+ left (round (* (- x left) scaler)))))
                 (y-coordinate-to-orig (y)
                   (when-scaling-needed y
                     (+ top (round (* (- y top) scaler)))))
                 (rectangle-to-orig (r)
                   (when-scaling-needed r
                     (geometry:with-rectangle r
                       (list (x-coordinate-to-orig left)
                             (y-coordinate-to-orig top)
                             (distance-to-orig width)
                             (distance-to-orig height)))))
                 ;; from-orig
                 (distance-from-orig (d)
                   (when-scaling-needed d
                     (floor d scaler)))
                 (x-coordinate-from-orig (x)
                   (when-scaling-needed x
                     (+ left (floor (- x left) scaler))))
                 (y-coordinate-from-orig (y)
                   (when-scaling-needed y
                     (+ top (floor (- y top) scaler))))
                 (rectangle-from-orig (r)
                   (when-scaling-needed r
                     (geometry:with-rectangle r
                       (list (x-coordinate-from-orig left)
                             (y-coordinate-from-orig top)
                             (distance-from-orig width)
                             (distance-from-orig height))))))
          (rectangle-to-orig
           (screamer-user:largest-rectangle
            (rectangle-from-orig bounding-box)
            (lambda (x y)
              (let ((m2 (get-m2 (x-coordinate-to-orig x) (y-coordinate-to-orig y))))
                (and m2 (eql contract (m2-contract m2))))))))))))

(defun contract-neighbours (contract)
  "Return all contracts that have an adjacent m2 to one of CONTRACT's
  m2s."
  (let (contracts)
    (flet ((push-neighbour (x y)
             (let ((m2 (get-m2 x y)))
               (when (and m2
                          (m2-contract m2)
                          (not (eq (m2-contract m2) contract))
                          (pushnew (m2-contract m2) contracts))))))
      (dolist (m2 (contract-m2s contract) contracts)
        (let ((x (m2-x m2))
              (y (m2-y m2)))
          (push-neighbour (1- x) (1- y))
          (push-neighbour (1- x) (1+ y))
          (push-neighbour (1+ x) (1- y))
          (push-neighbour (1+ x) (1+ y)))))))

(defun contract-center (contract)
  (destructuring-bind (left top width height)
      (contract-largest-rectangle contract)
    (rectangle-center (list left top width height) :roundp nil)))

(defun contract-center-lon-lat (contract)
  (error "this function is deprecated")
  (let ((center (contract-center contract)))
    (with-points (center)
      (geo-utm:utm-x-y-to-lon-lat (+ +nw-utm-x+ center-x) (- +nw-utm-y+ center-y) +utm-zone+ t))))

(defun tx-make-contract (sponsor m2-count &key date paidp expires)
  (warn "Old tx-make-contract transaction used, contract dates may be wrong")
  (tx-do-make-contract sponsor m2-count :date date :paidp paidp :expires expires))

(deftransaction do-make-contract (sponsor m2-count &key date paidp expires download-only)
  (let ((m2s (allocate-m2s-for-sale  m2-count)))
    (if m2s
	(let ((contract (make-object 'contract
				     :sponsor sponsor
				     :date date
				     :m2s m2s
				     :expires expires
				     :download-only download-only)))
	  (when paidp
	    (contract-set-paidp contract paidp))
	  contract)
	(warn "can't create contract, ~A square meters for ~A could not be allocated" m2-count sponsor))))

(define-condition allocation-areas-exhausted (simple-error)
  ((numsqm :initarg :numsqm :reader numsqm))
  (:report (lambda (condition stream)
	     (format stream "Could not satisfy your request for ~A sqms, please contact the BOS office"
		     (numsqm condition)))))

(defun make-contract (sponsor m2-count
                      &key (date (get-universal-time))
                      paidp
                      (expires (+ (get-universal-time) *manual-contract-expiry-time*))
                      download-only)
  (unless (and (integerp m2-count)
	       (plusp m2-count))
    (error "number of square meters must be a positive integer"))
  (let ((contract (do-make-contract sponsor m2-count
				    :date date
				    :paidp paidp
				    :expires expires
				    :download-only download-only)))
    (unless contract
      (send-system-mail :subject "Contact creation failed - Allocation areas exhaused"
			:text (format nil "A contract for ~A square meters could not be created, presumably because no
suitable allocation area was found.  Please check the free allocation
areas and add more space.

Sponsor-ID: ~A
"
				      m2-count (store-object-id sponsor)))
      (error 'allocation-areas-exhausted :numsqm m2-count))
    contract))

(deftransaction recolorize-contracts (&optional colors)
  "Assigns a new color to each contract choosing from COLORS, so
that CONTRACTS-WELL-COLORED-P holds."
  (let ((contracts (class-instances 'contract)))
    (loop for contract in contracts
       for color in (screamer-user:colorize colors contracts #'contract-neighbours)
       do (setf (slot-value contract 'color) color)
       do (publish-contract-change contract))))

(defun contracts-well-colored-p ()
  "Checks if all contracts have a different color than all their
neighbours."
  (loop for contract in (class-instances 'contract)
     do (when (member (contract-color contract) (contract-neighbours contract)
                      :key #'contract-color :test #'equal)
          (return nil))
     finally (return t)))

(defun contract-consistent-p (contract)
  (labels ((m2-points-to-contract (m2)
             (eq contract (m2-contract m2))))
    (let ((consistent t))
      (unless (every #'m2-points-to-contract (contract-m2s contract))        
        (let ((*print-length* 5))
          (warn "~s of ~s dont point to it by M2-CONTRACT~
                 ~%either those m2s are free or point to another contract~
                 ~%the wrongly pointed to objs with duplicates removed are: ~s"
                (remove-if #'m2-points-to-contract (contract-m2s contract))
                contract
                (remove-duplicates (remove contract (mapcar #'m2-contract (contract-m2s contract))))))
        (setq consistent nil))
      (when (null (contract-m2s contract))
        (warn "~s has no m2s" contract)
        (setq consistent nil))
      consistent)))

(defvar *last-contracts-cache* nil)
(defconstant +last-contracts-cache-size+ 20)

(defun last-paid-contracts ()
  (unless *last-contracts-cache*
    (setf *last-contracts-cache* (subseq (append (sort (remove-if-not #'contract-paidp (class-instances 'contract))
						       #'> :key #'contract-date)
						 (make-list +last-contracts-cache-size+))
					 0 +last-contracts-cache-size+)))
  (remove-if #'object-destroyed-p *last-contracts-cache*))

(defun add-contract-to-cache (contract)
  (last-paid-contracts) ; force cache initialization, should really be done by a eval-when
  (push contract *last-contracts-cache*)
  (setf (cdr (nthcdr (1- +last-contracts-cache-size+) *last-contracts-cache*)) nil))

(defun number-of-sold-sqm ()
  (let ((retval 0))
    (dolist (contract (remove-if-not #'contract-paidp (class-instances 'contract)))
      (incf retval (length (contract-m2s contract))))
    retval))

(defun string-safe (string)
  (if string
      (escape-nl (arnesi:escape-as-html string))
      ""))

(defun make-m2-javascript (sponsor)
  "Erzeugt das Quadratmeter-Javascript für die angegebenen Contracts"
  (with-output-to-string (*standard-output*)
    (let ((paid-contracts (remove nil (sponsor-contracts sponsor) :key #'contract-paidp)))
      (format t "profil = {};~%")
      (format t "profil.id = ~D;~%" (store-object-id sponsor))
      (format t "profil.name = ~S;~%" (string-safe (or (user-full-name sponsor) "[anonym]")))
      (format t "profil.country = ~S;~%" (or (sponsor-country sponsor) "[unbekannt]"))
      (format t "profil.anzahl = ~D;~%" (loop for contract in paid-contracts
					   sum (length (contract-m2s contract))))
      (format t "profil.nachricht = '~A';~%" (string-safe (sponsor-info-text sponsor)))
      (format t "profil.contracts = [ ];~%")
      (loop for contract in paid-contracts
	 do (destructuring-bind (left top width height) (contract-bounding-box contract)
	      (format t "profil.contracts.push({ id: ~A, left: ~A, top: ~A, width: ~A, height: ~A, date: ~S });~%"
		      (store-object-id contract)
		      left top width height
		      (format-date-time (contract-date contract) :show-time nil)))))))

(defun delete-directory (pathname)
  (cl-fad:delete-directory-and-files pathname :if-does-not-exist :ignore))

(defun reinit (&key delete directory website-url enable-mails)
  (format t "~&; Startup Quadratmeterdatenbank...~%")
  (force-output)
  (setf *enable-mails* enable-mails)
  (setf *website-url* website-url)
  (setf *rect-publisher* (make-rect-publisher))
  (unless directory
    (error ":DIRECTORY parameter not set in m2.rc"))
  (assert (and (null (pathname-name directory))
	       (null (pathname-type directory)))
	  (directory)
	  ":DIRECTORY parameter is ~s (not a directory pathname)" directory)
  (when delete
    (delete-directory directory)
    (assert (not (probe-file directory))))
  (close-store)
  (make-instance 'm2-store
		 :directory directory
		 :subsystems (list (make-instance 'store-object-subsystem)
				   (make-instance 'blob-subsystem
						  :n-blobs-per-directory 1000)
				   (make-instance 'bos.m2.allocation-cache:allocation-cache-subsystem)))
  (format t "~&; Startup der Quadratmeterdatenbank done.~%")
  (force-output))

(defun consistent-p ()
  (let ((inconsistent-objs
         (list
          (remove-if #'sponsor-consistent-p (class-instances 'sponsor))
          (remove-if #'contract-consistent-p (class-instances 'contract))
          (remove-if #'allocation-area-consistent-p (class-instances 'allocation-area)))))
    (values (every #'null inconsistent-objs)
            inconsistent-objs)))

;; testing

(defun fill-with-random-contracts (&optional percentage)
  (loop for sponsor = (make-sponsor)
     while (and (or (null percentage)
		    (< (allocation-area-percent-used (first (class-instances 'allocation-area))) percentage))
		(make-contract sponsor
			       (random-elt (cons (1+ (random 300))
						 '(1 1 1 1 1 5 5 10 10 10 10 10 10 10 10
						   10 10 10 10 10 30 30 30)))
			       :paidp t))))


;;; for quick visualization
#+ltk
(defun show-m2s-polygon (m2s &aux (points (m2s-polygon m2s)))
  (labels ((compute-bounding-box (m2s)
	     (let* ((left (m2-x (elt m2s 0)))
		    (top (m2-y (elt m2s 0)))
		    (right left)
		    (bottom top))
	       (loop for i from 1 below (length m2s) do
		    (let* ((v (elt m2s i))
			   (x (m2-x v))
			   (y (m2-y v)))
		      (setf left (min left x)
			    right (max right x)
			    top (min top y)
			    bottom (max bottom y))))
	       (values left top (- right left) (- bottom top)))))	      
    (multiple-value-bind (left top width height)
	(compute-bounding-box m2s)
      (declare (ignore width height))
      (finish-output)
      (flet ((transform-x (x)
	       (+ 30 (* 30 (- x left))))
	     (transform-y (y)
	       (+ 30 (* 30 (- y top)))))	
	(ltk:with-ltk ()
	  (let ((canvas (make-instance 'ltk:canvas :width 700 :height 700)))	  
	    ;; draw m2s
	    (loop for m2 in m2s
	       for x = (transform-x (m2-x m2))
	       for y = (transform-y (m2-y m2))
	       do (ltk:create-text canvas (+ 10 x) (+ 10 y) "x"))
	    ;; draw polygon
	    (loop for a in points
	       for b in (cdr points)
	       while (and a b)
	       do (ltk:create-line* canvas
				    (transform-x (first a)) (transform-y (second a))
				    (transform-x (first b)) (transform-y (second b))))
	    (let ((a (first points)))
	      (ltk:create-text canvas (transform-x (first a)) (transform-y (second a)) "o")) 
	    (ltk:pack canvas)))))))

#+ltk
(defun show-contract-center (contract)
  (labels ((compute-bounding-box (m2s)
	     (let* ((left (m2-x (elt m2s 0)))
		    (top (m2-y (elt m2s 0)))
		    (right left)
		    (bottom top))
	       (loop for i from 1 below (length m2s) do
		    (let* ((v (elt m2s i))
			   (x (m2-x v))
			   (y (m2-y v)))
		      (setf left (min left x)
			    right (max right x)
			    top (min top y)
			    bottom (max bottom y))))
	       (values left top (- right left) (- bottom top)))))	      
    (let* ((m2s (contract-m2s contract))
           (rectangle (contract-largest-rectangle contract))
           (center (geometry:rectangle-center rectangle)))
      (multiple-value-bind (left top width height)
          (compute-bounding-box m2s)
        (declare (ignore width height))
        (finish-output)
        (flet ((transform-x (x)
                 (+ 30 (* 30 (- x left))))
               (transform-y (y)
                 (+ 30 (* 30 (- y top)))))	
          (ltk:with-ltk ()
            (let ((canvas (make-instance 'ltk:canvas :width 700 :height 700)))	  
              ;; draw m2s
              (loop for m2 in m2s
                 for x = (transform-x (m2-x m2))
                 for y = (transform-y (m2-y m2))
                 do (ltk:create-text canvas (+ 10 x) (+ 10 y) "x"))
              (geometry:with-rectangle rectangle
                (ltk:create-rectangle canvas (transform-x left) (transform-y top)
                                      (transform-x (+ left width)) (transform-y (+ top height))))
              (destructuring-bind (x y)
                  center
                (geometry:with-rectangle ((list (- x 0.1) (- y 0.1) 0.2 0.2))
                  (ltk:create-rectangle canvas (transform-x left) (transform-y top)
                                        (transform-x (+ left width)) (transform-y (+ top height)))))
              (ltk:pack canvas))))))))

