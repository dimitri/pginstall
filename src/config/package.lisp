;;;
;;; pginstall Configuration management
;;;

(defpackage #:pginstall.config
  (:use #:cl
        #:pginstall.common
        #:py-configparser
        #:puri)
  (:export #:*config-filename*
           #:set-config-filename
           #:expand-user-homedir-pathname

           ;; repository server
           #:*dburi*
           #:*listen-port*
           #:*archive-path*
           #:*repo-logfile*
           #:*http-logfile*
           #:*upstream-server*

           ;; build animal
           #:*repo-server*
           #:*animal-name*
           #:*git*
           #:*gmake*
           #:*build-root*

           #:read-config
           #:save-config
           #:write-current-config
           #:get-option-by-name
           #:set-option-by-name))

