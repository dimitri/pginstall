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
       (:GET  "/api/list/extension"                  'api-list-extension)
       (:GET  "/api/get/extension/:os/:version/:arch" 'api-get-extension)

       ;; Buildfarm animal API
       (:GET  "/api/list/plafform"   'api-list-platform)
       (:GET  "/api/list/animal"     'api-list-animal)
       (:GET  "/api/list/pgconfig"   'api-list-pgconfig)

       (:GET  "/api/get/pgconfig/:animal" 'api-get-pgconfig)

       (:GET  "/api/register/animal/:name/:os/:version/:arch" 'api-register-animal)
       (:GET  "/api/build/:extension" 'api-build-extension)))

(defvar *acceptor* nil "The Web Server")

(defun start-server ()
  "Start the web server"
  (when *acceptor*
    (error "The web server is already running."))

  (setf *acceptor* (make-instance 'simpleroutes-acceptor
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
(defun home ()
  "Hello, world?")

(defmacro define-api-list (class-name)
  "Define a function that outputs a listing of instances of CLASS-NAME."
  (declare (type symbol class-name))
  (let ((fun-name (intern (format nil "API-LIST-~a" class-name))))
    `(defun ,fun-name ()
       (setf (hunchentoot:content-type*) "text/plain")
       (with-output-to-string (*standard-output*)
         (yason:encode (select-star ',class-name))))))

;; enable all our straight-to-json API
(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-api-list extension)
  (define-api-list platform)
  (define-api-list animal)
  (define-api-list pgconfig))

(defun api-get-pgconfig (animal-name)
  "Get the list of pgconfig known to given ANIMAL-NAME."
  (setf (hunchentoot:content-type*) "text/plain")
  (with-output-to-string (*standard-output*)
    (yason:encode (list-pg-configs :animal-name animal-name))))

(defun api-register-animal (name os version arch)
  "Register a new animal, with details about its platform."
  (setf (hunchentoot:content-type*) "text/plain")
  (with-output-to-string (*standard-output*)
    (yason:encode (register-animal name os version arch))))
