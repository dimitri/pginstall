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
       (:GET  "/"          'home)
       (:GET  "/config"    'config)
       (:GET  "/help/.*"   'render-doc-page)
       (:GET  "/dist/.*"   'serve-bootstrap-file)
       (:GET  "/pict/.*"   'serve-pict-file)

       (:GET  "/extension" 'front-list-extensions)
       (:GET  "/animal"    'front-list-animals)
       (:GET  "/build"     'front-list-builds)
       (:GET  "/archive"   'front-list-archives)

       (:GET  "/build/:id"       'front-display-build)
       (:GET  "/animal/:name"    'front-display-animal)
       (:GET  "/extension/:name" 'front-display-extension)

       (:GET  "/queue/:name"     'front-queue-build)

       ;; Server remote control
       (:GET  "/api/config"             'api-server-config)
       (:GET  "/api/status"             'api-server-status)
       (:GET  "/api/reload"             'api-server-reload)
       (:GET  "/api/terminate/yourself" 'api-server-stop)

       ;; Extension API
       (:GET  "/api/list/extension" 'api-list-extension)
       (:GET  "/api/list/extension/:os/:version/:arch"
              'api-list-available-extensions)

       (:GET  "/api/fetch/:extension/:pgversion/:os/:version/:arch"
              'api-fetch-archive)

       (:POST "/api/add/extension" 'api-add-extension)

       ;; Buildfarm animal API
       (:GET  "/api/list/platform"         'api-list-platform)
       (:GET  "/api/list/animal"           'api-list-animal)

       (:GET  "/api/list/pgconfig/:animal" 'api-list-pgconfig)
       (:POST "/api/add/pgconfig/:animal"  'api-add-pgconfig)

       (:GET  "/api/get/work/for/:animal" 'api-get-work)
       (:POST "/api/upload/archive"       'api-upload-archive)

       ;; Repository server API
       (:GET  "/api/pick/my/name/:os/:version/:arch" 'api-pick-my-name)
       (:GET  "/api/register/animal/:name/:os/:version/:arch"
              'api-register-animal)
       (:GET  "/api/build/:extension"
              'api-queue-extension-build)
       ))

(defvar *acceptor* nil "The Web Server")
(defvar *server-is-running* nil)

(defun start-server (&optional interactive
                     &aux
                       (access-log (if interactive *terminal-io* *http-logfile*))
                       (mesg-log   (if interactive *terminal-io* *repo-logfile*)))
  "Start the web server"
  (when *acceptor*
    (error "The web server is already running."))

  ;; read the config file to know which port to listen on
  (read-config)

  (setf *acceptor* (make-instance 'simpleroutes-acceptor
                                  :port *listen-port*
                                  :document-root *archive-path*
                                  :access-log-destination access-log
                                  :message-log-destination mesg-log))
  (hunchentoot:start *acceptor*)
  (setf *server-is-running* t))

(defun stop-server ()
  "Stop the web server"
  (unless *acceptor*
    (error "The web server isn't running."))

  (hunchentoot:stop *acceptor*)
  (setf *acceptor* nil *server-is-running* nil))

(defun restart-server ()
  "Restart the web server."
  (stop-server)
  (start-server))

(defun api-server-stop ()
  "Stop the server, from remote, just set *server-is-running* to nil"
  (setf *server-is-running* nil)
  (setf (hunchentoot:content-type*) "text/plain")
  "OK, I'm stopping now...")

(defun api-server-status ()
  "Return OK when the server is OK."
  (setf (hunchentoot:content-type*) "text/plain")
  "OK")

(defun api-server-config ()
  "Return current server's configuration, in INI format."
  (setf (hunchentoot:content-type*) "text/plain")
  (with-output-to-string (s)
    (write-current-config s)))

(defun api-server-reload ()
  "Reload the configuration from disk, then return it."
  (read-config)
  (api-server-config))


;;;
;;; elisp: (put 'with-prefixed-accessors 'lisp-indent-function 'defun)
;;;
(defmacro with-condition-handling ((&key json content-type) &body body)
  "Handle any condition signaled by BODY properly for a web server."
  (let ((result (gensym)))
   `(handler-case
        (let* ((,result ,@body)
               (,result (if ,json (yason:encode ,result) ,result)))
          (when ,content-type
            (setf (hunchentoot:content-type*) ,content-type))
          ,result)
      (condition (c)
        (setf (hunchentoot:return-code*)
              hunchentoot:+http-internal-server-error+)
        (format t "~a" c)))))

(defmacro define-api-function (fun args &body body)
  "Define the function FUN to return the result of BODY as a json encoded
   data in text-plain"
  (declare (list args))
  `(defun ,fun ,args
     (with-output-to-string (*standard-output*)
       (with-condition-handling (:json t :content-type "text/plain")
         ,@body))))

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
  (define-api-list animal))


;;;
;;; API for extensions
;;;
(defun api-add-extension ()
  "Handle a :POST query with pg-config object's parameters."
  (setf (hunchentoot:content-type*) "text/plain")
  (let* ((full-name    (hunchentoot:post-parameter "fullname"))
         (uri          (hunchentoot:post-parameter "uri"))
         (description  (hunchentoot:post-parameter "description")))
    (with-output-to-string (*standard-output*)
      (with-condition-handling (:json t :content-type "text/plain")
        (with-pgsql-connection (*dburi*)
          (make-dao 'extension
                    :full-name full-name
                    :uri       uri
                    :desc      description))))))


;;;
;;; API entries for Build Animals
;;;
(define-api-function api-pick-my-name (os version arch)
  (let ((*animal-name* (pick-animal-name)))
    (register-animal *animal-name* os version arch)))

(define-api-function api-register-animal (name os version arch)
  (register-animal name os version arch))

(define-api-function api-get-work (animal)
  (queue-get-work animal))

(define-api-function api-list-pgconfig (animal)
  (list-pg-configs :animal-name animal))

(defun api-add-pgconfig (animal-name)
  "Handle a :POST query with pg-config object's parameters."
  (setf (hunchentoot:content-type*) "text/plain")
  (let* ((animal-name  animal-name)
         (pg-config    (hunchentoot:post-parameter "pg-config"))
         (version      (hunchentoot:post-parameter "version"))
         (configure    (hunchentoot:post-parameter "configure"))
         (cc           (hunchentoot:post-parameter "cc"))
         (cflags       (hunchentoot:post-parameter "cflags")))
    (with-output-to-string (*standard-output*)
      (with-condition-handling (:json t :content-type "text/plain")
        (with-pgsql-connection (*dburi*)
          (make-dao 'pgconfig
                    :animal-name animal-name
                    :pg-config   pg-config
                    :version     version
                    :configure   configure
                    :cc          cc
                    :cflags      cflags))))))

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
  (let* ((extension extension)
         (pgversion pgversion)
         (os        os)
         (version   version)
         (arch      arch)
         (pathname  (archive-pathname extension pgversion os version arch)))
    (when (and pathname (probe-file pathname))
      (hunchentoot:handle-static-file pathname "application/octet-stream"))))
