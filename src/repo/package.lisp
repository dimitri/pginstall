;;;
;;; The build animal is responsible for building extensions against a set of
;;; PostgreSQL installations.
;;;
(defpackage #:pginstall.repo
  (:use #:cl
        #:pginstall.common
        #:pginstall.config
        #:postmodern
        #:iolib.os
        #:iolib.pathnames)
  (:export #:extension                  ; the extension class
           #:ext-id
           #:short-name
           #:full-name
           #:uri
           #:desc
           #:extension-short-name

           #:platform                   ; the platform class
           #:platform-id
           #:os-name
           #:os-version
           #:arch

           #:animal                     ; the animal class
           #:animal-id
           #:name
           #:uri
           #:platform

           #:pgconfig                   ; the pgconfig class
           #:pconfig-id
           #:animal-name
           #:pg-config
           #:pg-version
           #:pg-configure
           #:pg-cc
           #:pg-cflags))
