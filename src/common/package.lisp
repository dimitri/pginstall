;;;
;;; Common code needed in several parts of the rest of the code.
;;;
(defpackage #:pginstall.common
  (:use #:cl #:esrap)
  (:import-from #:postmodern #:with-connection #:query)
  (:export #:parse-extension-uri

           ;; dburi
           #:parse-pgsql-connection-string
           #:with-pgsql-connection
           #:validate-dburi

           ;; pgconfig
           #:*pg-versions*
           #:*pg-bin-paths*
           #:*pg-config-keys*
           #:run-pg-config
           #:find-pgconfig-paths
           #:uname
           #:os-name-and-version
           #:parse-properties-output

           ;; run command
           #:*verbose*
           #:*log-stream*
           #:run-command))
