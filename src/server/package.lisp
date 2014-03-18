;;;
;;; The repository server is publishing information via an HTTP API.
;;;
;;; Main features:
;;;   - publishing available extensions, with binary archives
;;;   - registering build animals
;;;   - queueing builds
;;;   - when a build node, fetch build commands from the queue
;;;   - upload an extension's archive
;;;

(defpackage #:pginstall.server
  (:use #:cl
        #:pginstall.common
        #:pginstall.config
        #:pginstall.repo
        #:pginstall.animal
        #:postmodern
        #:simple-routes
        #:cl-who
        #:iolib.pathnames)
  (:import-from #:iolib.base
                #:read-file-into-string
                #:read-file-into-byte-vector)
  (:import-from #:split-sequence
                #:split-sequence)
  (:export #:*acceptor*
           #:*server-is-running*
           #:start-server
           #:stop-server
           #:restart-server))

