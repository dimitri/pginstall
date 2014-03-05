;;;
;;; The build animal is responsible for building extensions against a set of
;;; PostgreSQL installations.
;;;
(defpackage #:pginstall.animal
  (:use #:cl
        #:pginstall.common
        #:pginstall.config
        #:pginstall.repo
        #:postmodern
        #:iolib.os
        #:iolib.pathnames)
  (:import-from #:iolib.base
                #:read-file-into-string)
  (:export #:list-pg-configs))
