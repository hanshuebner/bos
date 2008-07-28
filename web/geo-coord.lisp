(cl-interpol:enable-interpol-syntax)

(defun get-geo-coord (pathname)
  (let ((data (bknr.utils:file-contents pathname :element-type 'character)))
    (cl-ppcre:regex-replace
     #?r"Geographic coordinates:.*\n(?:.*\n){10,15}\s*(\d{1,3} \d\d [NS]), (\d{1,3} \d\d [EW])(?:.*\n)*.*Internet country code:.*\n(?:.*\n){10}\s*\.(..)"
     data
     (lambda (target-string start end match-start match-end reg-starts reg-ends)
       (declare (ignore start end match-start match-end))
       (labels ((reg (n)
                  (copy-seq (subseq target-string
                                    (aref reg-starts n)
                                    (aref reg-ends n)))))
         (return-from get-geo-coord (list (intern (string-upcase (reg 2)) :keyword)
                                          (reg 0)
                                          (reg 1))))))
    (format t "Unmatched: ~A~@[ (~A)~]~%"
            pathname (ignore-errors (aref (nth-value 1 (cl-ppcre:scan-to-strings "The World Factbook -- (.*)</title>" data)) 0)))
    nil))

(defun get-geo-coords (directory-pathname)
  (remove nil (mapcar #'get-geo-coord (directory (merge-pathnames #P"*.html" directory-pathname)))))