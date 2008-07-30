(in-package :bos.test)
(in-suite :bos.test.poi)

(test make-poi-medium.without-poi
  (with-fixture initial-bos-store ()
    (let ((medium (make-poi-medium 'poi-medium :language "de"
                                               :title "a title")))
      (is (string= "a title" (slot-string medium 'title "de"))))
    (signals (error) (make-poi-medium 'poi-medium :title "a title"))))

(test make-poi-medium.with-poi
  (with-fixture initial-bos-store ()
    (let* ((poi (make-poi "turm"))
           (medium (make-poi-medium 'poi-medium :language "de"
                                                :title "a title"
                                                :poi poi)))      
      (is (eq poi (poi-medium-poi medium)))
      (is (member medium (poi-media poi))))))

(test make-poi
  (with-fixture initial-bos-store ()
    (let ((poi (make-poi "turm" :area (list 50 60))))
      (is (string= "turm" (poi-name poi)))
      (is (= 50 (poi-center-x poi)))
      (is (= 60 (poi-center-y poi)))
      (is (string= "" (slot-string poi 'title "de")))
      (is (string= "" (slot-string poi 'subtitle "de")))
      (is (string= "" (slot-string poi 'description "de")))
      (is (null (poi-images poi)))
      (is (null (poi-airals poi)))
      (is (null (poi-panoramas poi)))
      (is (null (poi-movies poi))))
    (signals (error) (make-poi "brunnen" :title "title"))
    (let ((poi2 (make-poi "brunnen" :language "de"
                                    :title "a title"
                                    :subtitle "a subtitle"
                                    :description "a description")))
      (is (string= "brunnen" (poi-name poi2)))      
      (is (string= "a title" (slot-string poi2 'title "de")))
      (is (string= "a subtitle" (slot-string poi2 'subtitle "de")))
      (is (string= "a description" (slot-string poi2 'description "de"))))))

(defun finishes-make-poi-javascript ()
  (dolist (language '("de" "en" "da"))
    (finishes (make-poi-javascript language))))

(test make-poi-javascript
  (with-fixture initial-bos-store ()
    (finishes-make-poi-javascript)
    (make-poi "turm" :area (list 50 60))
    (finishes-make-poi-javascript)
    (make-poi "brunnen" :language "de"
              :title "a title"
              :subtitle "a subtitle"
              :description "a description")
    (finishes-make-poi-javascript)))

(test make-poi-image
  (with-fixture initial-bos-store ()
    
    (let ((test-image-path (merge-pathnames "test.png" (bknr.datastore::store-directory *store*)))
          (poi (make-poi "turm")))      
      (cl-gd:with-image* (100 120 t)
        (cl-gd:write-image-to-file test-image-path))
      (is (null (poi-media poi)))
      (import-image test-image-path :class-name 'poi-image
                    :initargs `(:poi ,poi :language "de" :title "a title"))
      (is (poi-media poi))
      (is (string= "a title" (slot-string (first (poi-media poi)) 'title "de")))
      (is (= 100 (store-image-width (first (poi-media poi)))))
      (is (= 120 (store-image-height (first (poi-media poi)))))
      (let ((medium (first (poi-media poi))))
        (is (eq poi (poi-medium-poi medium))))
      (finishes-make-poi-javascript))))
