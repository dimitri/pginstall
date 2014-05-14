;;;
;;; The main entry point, manages the command line.
;;;
(defpackage #:pginstall
  (:use #:cl
        #:pginstall.animal
        #:pginstall.client
        #:pginstall.common
        #:pginstall.config
        #:pginstall.repo
        #:pginstall.server)

  (:import-from #:alexandria
                #:read-file-into-string
                #:read-file-into-byte-vector)

  (:export #:*listen-port*
           #:*repo-logfile*
           #:*http-logfile*
           #:*dburi*
           #:*archive-path*
           #:read-config
           #:save-config

           ;; controlling the server
           #:*acceptor*
           #:start-server
           #:stop-server
           #:restart-server
           #:query-repo-server
           #:post-repo-server))
