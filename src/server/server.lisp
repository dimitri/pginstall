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

(in-package #:pginstall.server)

(setf *routeslist*
      (compile-routes
       ;; User website
       (:GET  "/" 'home)

       ;; Extension API
       (:GET  "/api/list/extensions"                  'api-list-extensions)
       (:GET  "/api/get/extension/:os/:version/:arch" 'api-get-extension)

       ;; Buildfarm animal API
       (:GET  "/api/list/plafforms"   'api-list-platforms)
       (:GET  "/api/list/animals"     'api-list-animals)
       (:GET  "/api/list/pg"          'api-list-pg)

       (:GET  "/api/register/:animal" 'api-register-animal)
       (:GET  "/api/build/:extension" 'api-build-extension)))

(defvar *acceptor* nil "The Web Server")

(defun start-server ()
  "Start the web server"
  (when *acceptor*
    (error "The web server is already running."))

  (setf *acceptor* (make-instance simpleroutes-acceptor
                                  :port *listen-port*
                                  :document-root *archive-path*
                                  :access-log-destination *terminal-io*
                                  :message-log-destination *terminal-io*))
  (hunchentoot:start *acceptor*))

(defun stop-server ()
  "Stop the web server"
  (unless *acceptor*
    (error "The web server isn't running."))

  (hunchentoot:stop *acceptor*)
  (setf *acceptor* nil))

(defun restart-server ()
  "Restart the web server."
  (stop-server)
  (start-server))


;;;
;;; Main entry points for the web server.
;;;
(defun api-list-extensions ()
  "Return a JSON list of extensions."
  ;; (setf (hunchentoot:content-type*) "text/plain")
  (yason:encode (select-star 'extension)))

(defun list-platforms ()
  "Return a JSON formated list of platforms."
  ;; (setf (hunchentoot:content-type*) "text/plain")
  (yason:encode (select-star 'platform)))

(defun list-animals ()
  "Return a JSON formated list of platforms."
  ;; (setf (hunchentoot:content-type*) "text/plain")
  (yason:encode (select-star 'animal)))

(defun list-pgconfigs (animal-name)
  "Return a JSON formated list of platforms."
  ;; (setf (hunchentoot:content-type*) "text/plain")
  (yason:encode (list-pg-configs :animal-name animal-name)))
