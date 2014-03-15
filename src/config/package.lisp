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
  (:export #:*config-filename*

           ;; repository server
           #:*dburi*
           #:*listen-port*
           #:*archive-path*
           #:*repo-logfile*
           #:*http-logfile*

           ;; build animal
           #:*repo-server*
           #:*animal-name*
           #:*git*
           #:*gmake*
           #:*build-root*
           #:*repo-server*

           #:read-config
           #:save-config
           #:write-current-config
           #:get-option-by-name
           #:set-option-by-name))

