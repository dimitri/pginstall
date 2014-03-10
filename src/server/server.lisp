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
       (:GET  "/api/list/extension"                    'api-list-extension)
       (:GET  "/api/list/extension/:os/:version/:arch" 'api-list-available-extensions)
       (:GET  "/api/fetch/:extension/:pgversion/:os/:version/:arch" 'api-fetch-archive)

       ;; Buildfarm animal API
       (:GET  "/api/list/plafform"   'api-list-platform)
       (:GET  "/api/list/animal"     'api-list-animal)
       (:GET  "/api/list/pgconfig"   'api-list-pgconfig)

       (:GET  "/api/get/pgconfig/:animal" 'api-get-pgconfig)
       (:GET  "/api/add/pgconfig/:animal" 'api-add-pgconfig)

       (:GET  "/api/get/work/for/:animal" 'api-get-work)
       (:POST "/api/upload/archive"       'api-upload-archive)

       ;; Repository server API
       (:GET  "/api/register/animal/:name/:os/:version/:arch" 'api-register-animal)
       (:GET  "/api/build/:extension" 'api-queue-extension-build)
       ))

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

;;;
;;; elisp: (put 'with-prefixed-accessors 'lisp-indent-function 'defun)
;;;
(defmacro define-api-function (fun args &body body)
  "Define the function FUN to return the result of BODY as a json encoded
   data in text-plain"
  (declare (list args))
  `(defun ,fun ,args
     (let ,(loop :for arg :in args
              :collect (list arg `(hunchentoot:url-decode ,arg)))
       (with-output-to-string (*standard-output*)
         (handler-case
             (let ((result (yason:encode ,@body)))
               (setf (hunchentoot:content-type*) "text/plain")
               result)
           (condition (c)
             (setf (hunchentoot:return-code*)
                   hunchentoot:+http-internal-server-error+)
             (format t "~a" c)))))))

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


;;;
;;; API entries for Build Animals
;;;
(define-api-function api-get-pgconfig (animal-name)
  (list-pg-configs :animal-name animal-name))

(define-api-function api-register-animal (name os version arch)
  (register-animal name os version arch))

(define-api-function api-get-work (animal)
  (queue-get-work animal))

;;;
;;; Repository server API
;;;
(define-api-function api-queue-extension-build (extension)
  (queue-extension-build extension))

(defun api-upload-archive ()
  "Handle the upload of a new archive."
  (setf (hunchentoot:content-type*) "text/plain")
  (let* ((extension   (hunchentoot:post-parameter "extension"))
         (pgversion   (hunchentoot:post-parameter "pgversion"))
         (animal-name (hunchentoot:post-parameter "animal"))
         (os-name     (hunchentoot:post-parameter "os-name"))
         (os-version  (hunchentoot:post-parameter "os-version"))
         (arch        (hunchentoot:post-parameter "arch"))
         (buildlog    (hunchentoot:post-parameter "buildlog")))
    (destructuring-bind (archive archive-filename archive-mime-type)
        (hunchentoot:post-parameter "archive")
      (declare (ignore archive-mime-type))
      (with-output-to-string (*standard-output*)
        (yason:encode
         (receive-archive extension pgversion
                          animal-name os-name os-version arch
                          buildlog archive-filename archive))))))

;;;
;;; API entries for the PostgreSQL embedded client
;;;
(define-api-function api-list-available-extensions (os version arch)
  (select-extensions-available-on-platform os version arch))

(defun api-fetch-archive (extension pgversion os version arch)
  "Return the archive file."
  (let* ((extension (hunchentoot:url-decode extension))
         (pgversion (hunchentoot:url-decode pgversion))
         (os        (hunchentoot:url-decode os))
         (version   (hunchentoot:url-decode version))
         (arch      (hunchentoot:url-decode arch))
         (pathname  (archive-pathname extension pgversion os version arch)))
    (hunchentoot:handle-static-file pathname "application/octet-stream")))
