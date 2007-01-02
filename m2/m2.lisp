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
			    :index-initargs (:width +width+ :height +width+ :tile-size +m2tile-width+ :tile-class 'image-tile))))

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

(defmethod m2-num-to-utm ((num integer))
  (multiple-value-bind (y x) (truncate num +width+)
    (+ +nw-utm-x+ x)
    (- +nw-utm-y+ y)))

(defmethod m2-num-to-utm ((num string))
  (m2-num-to-utm (parse-integer num :radix 36)))

(defmethod m2-allocation-area ((m2 m2))
  (find-if #'(lambda (allocation-area) (point-in-polygon-p (m2-x m2) (m2-y m2) (allocation-area-vertices allocation-area)))
	   (class-instances 'allocation-area)))

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
;;; SPONSOR-CONTRACTS (sponsor) => list of contract
;;;
;;; Sowie Funktionen von USER.

(define-persistent-class sponsor (user)
  ((master-code :read          :initform nil)
   (info-text :update	       :initform nil)
   (country :update	       :initform nil)
   (contracts :update          :initform nil))
  (:default-initargs :full-name nil :email nil))

(defun sponsor-p (object)
  (equal (class-of object) (find-class 'sponsor)))

(deftransaction sponsor-set-info-text (sponsor newval)
  (setf (sponsor-info-text sponsor) newval))

(deftransaction sponsor-set-country (sponsor newval)
  (setf (sponsor-country sponsor) newval))

(defvar *sponsor-counter* 0)

(defun make-sponsor (&rest initargs &key login &allow-other-keys)
  (apply #'make-object 'sponsor
         :login (or login (format nil "s-~36R-~36R" (incf *sponsor-counter*) (get-universal-time)))
	 :master-code (mod (+ (get-universal-time) (random 1000000)) 1000000)
         initargs))

(defmethod destroy-object :before ((sponsor sponsor))
  (mapc #'delete-object (sponsor-contracts sponsor)))

(defmethod sponsor-id ((sponsor sponsor))
  (store-object-id sponsor))

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
   (expires :read :documentation "universal time which specifies the time the contract expires (is deleted) when it has not been paid for" :initform nil))
  (:default-initargs
      :m2s nil
    :paidp nil
    :download-only nil
    :color (random-elt *claim-colors*)
    :cert-issued nil
    :expires (+ (get-universal-time) *manual-contract-expiry-time*)))

(defun contract-p (object)
  (equal (class-of object) (find-class 'contract)))

(defmethod initialize-persistent-instance :after ((contract contract))
  (pushnew contract (sponsor-contracts (contract-sponsor contract)))
  (contract-changed contract)
  (dolist (m2 (contract-m2s contract))
    (setf (m2-contract m2) contract)))

(defmethod destroy-object :before ((contract contract))
  (let ((sponsor (contract-sponsor contract)))
    (when sponsor
      (setf (sponsor-contracts sponsor) (remove contract (sponsor-contracts sponsor)))))
  (contract-changed contract)
  (dolist (m2 (contract-m2s contract))
    (setf (m2-contract m2) nil))
  (return-m2s (contract-m2s contract)))

(defun get-contract (id)
  (let ((contract (store-object-with-id id)))
    (prog1
	contract
      (unless (subtypep (type-of contract) 'contract)
	(error "invalid contract id (wrong type) ~A" id)))))

(defmethod contract-changed ((contract contract))
  (mapc #'(lambda (tile) (image-tile-changed tile)) (contract-image-tiles contract)))

(defmethod contract-is-expired ((contract contract))
  (and (contract-expires contract)
       (> (get-universal-time) (contract-expires contract))))

(deftransaction contract-set-paidp (contract newval)
  (setf (contract-paidp contract) newval)
  (contract-changed contract)
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

(defmethod contract-issue-cert ((contract contract) name &key address language)
  (if (contract-cert-issued contract)
      (warn "can't re-issue cert for ~A" contract)
      (progn
	(make-certificate contract name :address address :language language)
	(unless (contract-download-only-p contract)
	  (make-certificate contract name :address address :language language :print t))
	(dotimes (i 10)
	  (when (probe-file (contract-pdf-pathname contract))
	    (return))
	  (sleep 1))
	(if (probe-file (contract-pdf-pathname contract))
	    (change-slot-values contract 'cert-issued t)
	    (error "Cannot generate certificate")))))

(defmethod contract-image-tiles ((contract contract))
  (let (image-tiles)
    (dolist (m2 (contract-m2s contract))
      (pushnew (get-map-tile (m2-x m2) (m2-y m2))
	       image-tiles))
    image-tiles))

(defmethod contract-bounding-box ((contract contract))
  (let (min-x min-y max-x max-y)
    (dolist (m2 (contract-m2s contract))
      (setf min-x (min (m2-x m2) (or min-x (m2-x m2))))
      (setf min-y (min (m2-y m2) (or min-y (m2-y m2))))
      (setf max-x (max (m2-x m2) (or max-x (m2-x m2))))
      (setf max-y (max (m2-y m2) (or max-y (m2-y m2)))))
    (list min-x min-y (1+ (- max-x min-x)) (1+ (- max-y min-y)))))

(defun tx-make-contract (sponsor m2-count &key date paidp expires)
  (warn "Old tx-make-contract transaction used, contract dates may be wrong")
  (tx-do-make-contract sponsor m2-count :date date :paidp paidp :expires expires))

(deftransaction do-make-contract (sponsor m2-count &key date paidp expires download-only)
  (let ((m2s (find-free-m2s m2-count)))
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

(defun make-contract (sponsor m2-count
                      &key (date (get-universal-time))
                      paidp
                      (expires (+ (get-universal-time) *manual-contract-expiry-time*))
                      download-only)
  (unless (and (integerp m2-count)
	       (plusp m2-count))
    (error "number of square meters must be a positive integer"))
  (let ((contract (do-make-contract sponsor m2-count :date date :paidp paidp :expires expires :download-only download-only)))
    (unless contract
      (send-system-mail :subject "Contact creation failed - Allocation areas exhaused"
			:text (format nil "A contract for ~A square meters could not be created, presumably because no
suitable allocation area was found.  Please check the free allocation
areas and add more space.

Sponsor-ID: ~A
"
				      m2-count (store-object-id sponsor)))
      (error "could not create contract, allocation areas exhausted?"))
    contract))

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
      (escape-nl (with-output-to-string (s)
		   (net.html.generator::emit-safe s string)))
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
  (when (probe-file pathname)
    ;; XXX Achtung, auf #-cmu folgt das Symlinks.
    (loop for file in (directory pathname #+cmu :truenamep #+cmu nil)
       when (pathname-name file)
       do (delete-file file)
       unless (pathname-name file)
       do (delete-directory file))
    #+allegro
    ;; Das loescht doch eh schon die unterverzeichnisse mit?
    (excl:delete-directory-and-files pathname)
    #+cmu
    (unix:unix-rmdir (ext:unix-namestring pathname))
    #+sbcl
    (sb-posix:rmdir (namestring pathname))
    #-(or allegro cmu sbcl)
    ...))

(defun reinit (&key delete directory website-url)
  (format t "~&; Startup Quadratmeterdatenbank...~%")
  (force-output)
  (setf *website-url* website-url)
  (unless directory
    (error ":DIRECTORY parameter not set in m2.rc"))
  (when delete
    (delete-directory directory)
    (assert (not (probe-file directory))))
  (make-instance 'm2-store
		 :directory directory
		 :subsystems (list (make-instance 'store-object-subsystem)
				   (make-instance 'blob-subsystem
						  :n-blobs-per-directory 1000)))
  (format t "~&; Startup der Quadratmeterdatenbank done.~%")
  (force-output))

;; testing

(defun fill-with-random-contracts (&optional percentage)
  (loop for sponsor = (make-sponsor)
     while (and (or (null percentage)
		    (< (allocation-area-percent-used (first (class-instances 'allocation-area))) percentage))
		(make-contract sponsor
			       (random-elt (cons (1+ (random 300)) '(1 1 1 1 1 5 5 10 10 10 10 10 10 10 10 10 10 10 10 10 30 30 30)))
			       :paidp t))))