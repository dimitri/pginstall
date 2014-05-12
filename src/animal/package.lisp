;;;
;;; The build animal is responsible for building extensions against a set of
;;; PostgreSQL installations.
;;;
(defpackage #:pginstall.client
  (:use #:cl #:pginstall.config)
  (:export #:build-api-uri
           #:server-error
           #:server-error-uri
           #:server-error-status-code
           #:server-error-reason
           #:server-error-body
           #:query-repo-server
           #:post-repo-server))

(defpackage #:pginstall.animal
  (:use #:cl
        #:pginstall.client
        #:pginstall.common
        #:pginstall.config
        #:pginstall.repo
        #:postmodern)
  (:import-from #:alexandria
                #:read-file-into-string)
  (:export #:discover-animal-setup-and-register-on-server
           #:register-animal-on-server
           #:list-pgconfigs-on-server
           #:add-pgconfig-on-server
           #:get-extension-to-build
           #:upload-archive
           #:get-animal-uri
           #:build-extension
           #:build-extension-for-server))

