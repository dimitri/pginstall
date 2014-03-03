;;;
;;; Common code needed in several parts of the rest of the code.
;;;
(defpackage #:pginstall.common
  (:use #:cl #:esrap #:iolib.os)
  (:import-from #:postmodern #:with-connection #:query)
  (:export #:parse-pgsql-connection-string
           #:with-pgsql-connection
           #:validate-dburi

           #:*pg-config-keys*
           #:run-pg-config
           #:os-name-and-version
           #:parse-properties-output))
