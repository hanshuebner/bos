(in-package :cl-user)

(defpackage :geometry
  (:use :cl :iterate :arnesi)
  (:export #:with-points
           #:with-rectangle
           #:distance
           #:dorect
           #:rectangle-center
           #:rectangle-intersects-p
           #:point-in-polygon-p
           #:point-in-circle-p
           #:point-in-rect-p
           #:bounding-box
           #:with-bounding-box-collect
           #:find-boundary-point
           #:region-to-polygon
           #:format-lon-lat
           #:nodes-connected-p
           ;;
           #:*rect-publisher*
           #:make-rect-publisher
           #:register-rect-subscriber
           #:remove-rect-subscriber
           #:publish-rect-change))

(defpackage :geo-utm
  (:use :cl)
  (:export #:lon-lat-to-utm-x-y
           #:utm-x-y-to-lon-lat
           #:make-float-pair
           #:lon-lat-to-utm-x-y*
           #:utm-x-y-to-lon-lat*))

(defpackage :bos.m2.config
  (:export #:+width+
           #:+nw-utm-x+
           #:+nw-utm-y+
           #:+utm-zone+
           #:+m2tile-width+
           #:+price-per-m2+

           #:*office-mail-address*
           #:*mail-amount*
           #:*pdf-base-directory*
           #:*cert-mail-directory*
           #:*cert-download-directory*
           #:*receipt-mail-directory*
           #:*receipt-download-directory*
           #:*cert-mail-template*
           #:*cert-download-template*
           #:*receipt-mail-template*
           #:*receipt-download-template*
           #:*num-coords-per-line*
           #:*manual-contract-expiry-time*
           #:*online-contract-expiry-time*))

(defpackage :bos.m2
  (:use :cl
        :cl-ppcre
        :cl-interpol
        :geometry
        :bknr.utils
        :bknr.indices
        :bknr.datastore
        :bknr.user
        :bknr.web
        :bknr.images
        :bknr.statistics
        :bknr.rss
        :bos.m2.config
        :cl-smtp
        :kmrcl
        :cxml
        :cl-mime
        :cl-gd)
  (:shadowing-import-from :cl-interpol #:quote-meta-chars)
  (:shadowing-import-from :bknr.utils #:run-shell-command)
  (:export #:m2-store
           #:*m2-store*
           #:register-transient-init-function

           #:get-tile
           #:ensure-tile
           #:get-map-tile
           #:ensure-map-tile
           #:tile-nw-x
           #:tile-nw-y
           #:tile-absolute-x
           #:tile-absolute-y
           #:tile-height
           #:tile-width
           #:image-tile-original-image
           #:image-tile-current-image
           #:image-tile-image
           #:image-tile-image-generated-time
           #:image-tile-changed-time
           #:image-tile-layers
           #:current-image

           #:background #:areas #:contracts #:palette

           #:m2
           #:get-m2
           #:ensure-m2
           #:m2-contract
           #:m2-num
           #:m2-num-string
           #:m2-num-to-utm
           #:m2-x
           #:m2-y
           #:m2-utm-x
           #:m2-utm-y
           #:m2-utm
           #:m2-lon-lat
           #:m2s-polygon
           #:m2s-polygon-lon-lat
           #:escape-nl
           #:return-contract-m2s

           #:sponsor
           #:make-sponsor
           #:sponsor-p
           #:sponsor-master-code
           #:sponsor-info-text
           #:sponsor-country
           #:sponsor-contracts
           #:sponsor-id
           #:sponsor-language
           #:sponsor-set-info-text
           #:sponsor-set-country
           #:sponsor-set-language
           #:country
           #:info-text
           #:language

           #:editor-only-handler
           #:editor-p

           #:contract
           #:make-contract
           #:all-contracts
           #:contract-p
           #:get-contract
           #:contract-sponsor
           #:contract-paidp
           #:contract-download-only
           #:contract-date
           #:contract-m2s
           #:contract-bounding-box
           #:contract-cert-name
           #:contracts-bounding-box
           #:contract-area
           #:contract-polygon
           #:contract-largest-rectangle
           #:contract-neighbours
           #:contract-center
           #:contract-center-lon-lat
           #:contract-color
           #:contract-cert-issued
           #:contract-set-paidp
           #:contract-set-download-only-p
           #:contract-price
           #:contract-issue-cert
           #:contract-certificates-generated-p
           #:contract-worldpay-trans-id
           #:contract-spendino-status-log
           #:contract-pdf-pathname
           #:contract-pdf-url
           #:contract-download-only-p
           ;; contract-stats
           #:number-of-sold-sqm
           #:number-of-paying-sponsors
           #:contract-stats-for-country
           #:last-paid-contracts
           #:do-sponsor-countries
           #:sponsors-as-json

           #:make-m2-javascript
           #:recolorize-contracts
           #:contracts-well-colored-p
           #:contract-published-p
           #:paidp

           #:allocation-area
           #:make-allocation-area
           #:make-allocation-rectangle
           #:all-allocation-areas
           #:allocation-area-bounding-box
           #:allocation-area-bounding-box2
           #:allocation-areas-bounding-box
           #:allocation-areas-plus-contracts-bounding-box
           #:allocation-area-active-p
           #:allocation-area-top
           #:allocation-area-left
           #:allocation-area-height
           #:allocation-area-width
           #:gauge
           #:allocation-area-contracts
           #:allocation-area-total-m2s
           #:allocation-area-free-m2s
           #:allocation-area-vertices
           #:allocation-area-percent-used
           #:left #:top #:width #:height #:active-p

           ;; slot-strings
           #:slot-string
           #:set-slot-string-values

           ;; pois
           #:title #:subtitle #:description ; for slot-string access
           #:update-textual-attributes
           #:very-shallow-copy-textual-attributes
           #:poi-medium
           #:poi-medium-poi
           #:poi-medium-creation-time
           #:make-poi-medium
           #:poi-image
           #:poi-airal
           #:poi-panorama
           #:poi-movie
           #:poi-movie-url
           #:poi
           #:poi-name
           #:find-poi
           #:all-pois
           #:poi-published-web
           #:poi-published-earth
           #:poi-area
           #:poi-icon
           #:poi-media
           #:poi-lod-min
           #:make-poi
           #:update-poi
           #:poi-complete
           #:poi-center-x
           #:poi-center-y
           #:poi-center-lon-lat
           #:poi-images
           #:poi-sat-images
           #:poi-sat-images-exchange-neighbours
           #:poi-airals
           #:poi-panoramas
           #:poi-movies
           #:make-poi-javascript
           #:poi-as-json
           #:pois-as-json

           ;; news
           #:news-item
           #:make-news-item
           #:update-news-item
           #:all-news-items
           #:news-item-time
           #:news-item-title
           #:news-item-text

           #:send-to-postmaster
           #:mail-fiscal-certificate-to-office
           #:mail-instructions-to-sponsor
           #:mail-info-request
           #:mail-manual-sponsor-data
           #:mail-backoffice-sponsor-data
           #:mail-worldpay-sponsor-data
           #:mail-spendino-sponsor-data
           #:mail-print-pdf
           #:send-instructions-to-sponsor

           #:*cert-download-directory*

           #:make-queue
           #:queue-empty-p
           #:enqueue
           #:dequeue
           #:queue-elements
           #:peek-queue
           ))

(defpackage :bos.m2.allocation-cache
  (:use :cl
        :geometry
        :bknr.indices
        :bknr.datastore
        :bknr.user
        :bknr.images
        :bknr.statistics
        :bknr.rss
        :bos.m2
        :bos.m2.config
        :iterate
        :arnesi)
  (:shadowing-import-from :bos.m2
                          #:enqueue
                          #:dequeue
                          #:peek-queue
                          #:queue-empty-p)
  (:export #:find-exact-match
           #:add-area
           #:count-cache-entries
           #:pprint-cache
           #:allocation-cache-subsystem))