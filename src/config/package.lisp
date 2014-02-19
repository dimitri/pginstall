;;;
;;; pginstall Configuration management
;;;

(defpackage #:pginstall.config
  (:use #:cl
        #:pginstall.common
        #:py-configparser
        #:iolib.os
        #:iolib.pathnames
        #:puri)
  (:export #:*dburi*
           #:*listen-port*

           ;; repository server
           #:*archive-path*

           ;; build animal
           #:*gmake*
           #:*build-root*
           #:*repo-server*

           #:read-config
           #:save-config
           #:set-option-by-name))

