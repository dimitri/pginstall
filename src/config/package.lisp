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

           #:*pidfile*
           #:read-pid
           #:kill-pid

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

           ;; S3
           #:*s3-bucket*
           #:*s3-access-key*
           #:*s3-secret-key*

           #:read-config
           #:save-config
           #:write-current-config
           #:get-option-by-name
           #:set-option-by-name))

