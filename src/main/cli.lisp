;;;
;;; Handle the CLI invocations of the pginstall binary.
;;;

(in-package #:pginstall)

;;;
;;; First, our very own command line facility.
;;;
(eval-when (:load-toplevel :compile-toplevel :execute)
  (defstruct command verbs bindings help lambda)

  (defvar *commands* (make-array 0
                                 :element-type 'command
                                 :adjustable t
                                 :fill-pointer t)
    "Host commands defined with the DEFINE-COMMAND macro.")

  (defmethod same-command ((a command) (b command))
    "Return non-nil when a and b are commands with the same verbs"
    (equal (command-verbs a) (command-verbs b))))

(defmethod command-matches ((command command) args)
  "When the given COMMAND matches given command line ARGS, then return it
   and the argument to apply to it."
  (declare (type list args))
  (when (<= (length (command-verbs command)) (length args))
    (let ((matches-p (loop :for verb :in (command-verbs command)
                        :for arg in args
                        :for matches-p := (string-equal verb arg)
                        :while matches-p
                        :finally (return matches-p))))
      (when matches-p
        (let ((fun-args (nthcdr (length (command-verbs command)) args)))
          (when (= (length (command-bindings command)) (length fun-args))
            (list (command-lambda command) fun-args)))))))

(defmacro define-command ((verbs bindings) help-string &body body)
  "Define a command that is to be fired when VERBS are found at the
   beginning of the command, assigning remaining arguments to given
   bindings.

   The help-string is used when displaying the program usage."
  (let ((fun      (gensym))
        (command  (gensym))
        (position (gensym))
        (output   (gensym)))
   `(eval-when (:load-toplevel :compile-toplevel :execute)
      (let* ((,fun      (lambda ,bindings
                          (read-config)
                          (let ((,output (progn ,@body)))
                            (typecase ,output
                              (string (format t "~a~%" ,output))
                              (t      nil)))))
             (,command  (make-command :verbs ',verbs
                                      :bindings ',bindings
                                      :help ,help-string
                                      :lambda (compile nil ,fun)))
             (,position (position-if (lambda (c) (same-command c ,command))
                                     *commands*)))
        (if ,position
            (setf (aref *commands* ,position) ,command)
            (vector-push-extend ,command *commands*))))))

(defun find-command-function (argv)
  "Loop through *COMMANDS* to find the code to execute given ARGV."
  (let ((args (rest argv)))
    (loop :for command :across *commands*
       :for match := (command-matches command args)
       :until match
       :finally (return match))))

(defun usage (args)
  "Loop over all the commands and output the usage of the main program"
  (format t "~a: command line parse error.~%" (first args) )
  (format t "~@[Error parsing args: ~{~s~^ ~}~%~]~%" (rest args))
  (loop :for command :across *commands*
     :do (with-slots (verbs bindings help) command
           (format t "~{~a~^ ~} ~{~a~^ ~}~28T~a~%" verbs bindings help))))

;;;
;;; Who doesn't like the advanced and detailed error reporting offered by
;;; PostgreSQL? Let's have a try at it ourselves.
;;;
(define-condition cli-error ()
  ((mesg   :initarg :mesg   :reader cli-error-message)
   (detail :initarg :detail :reader cli-error-detail)
   (hint   :initarg :hint   :reader cli-error-hint)))

(defun main (argv)
  "The main entry point for the command-line interface."
  (let ((match (find-command-function argv)))
    (if match
        (destructuring-bind (fun args) match
          (handler-case
              (handler-bind ((warning
                              #'(lambda (c)
                                  (format t "WARNING: ~a~%" c)
                                  (muffle-warning))))
                (apply fun args))
            (cli-error (e)
              (format t
                      "ERROR: ~a~%~@[DETAIL: ~a~%~]~@[HINT: ~a~%~]"
                      (cli-error-message e)
                      (cli-error-detail e)
                      (cli-error-hint e)))
            (server-error (e)
              (format t
                      "ERROR ~d ON ~a~%~@[REASON: ~a~%~]~@[BODY: ~a~%~]"
                      (server-error-status-code e)
                      (server-error-uri e)
                      (server-error-reason e)
                      (server-error-body e)))
            (condition (c)
              (format t "ERROR: ~a~%" c))))
        (usage argv))))


;;;
;;; And now the commands, starting with the setup
;;;
(define-command (("config" "set") (name value))
    "set config variable NAME to VALUE"
  (set-option-by-name name value))

(define-command (("config" "get") (name))
    "get config value for variable NAME"
  (get-option-by-name name))


;;;
;;; Controlling the server
;;;
(define-command (("server" "start") ())
    "start the repository server"
  (let ((status (ignore-errors (query-repo-server 'status)))
        (*repo-logfile* *terminal-io*))
    (unless (and status (string= "OK" status))
      (daemon:daemonize :output *repo-logfile*
                        :error  *repo-logfile*
                        :exit-parent t
                        :sigterm (lambda (sig)
                                   (declare (ignore sig))
                                   (stop-server)))
      (format t "PLOP~%")
      (format *terminal-io* "PLIIP~%")
      (format *repo-logfile* "AAAH~%")
      (start-server)
      (loop :while *server-is-running*
         :do (format t "still wow!~%")
         :do (sleep 1)))))

(define-command (("server" "stop") ())
    "stop the currently running server"
  (query-repo-server 'terminate 'yourself))

(define-command (("server" "status") ())
    "get the status of the currently running server"
  (query-repo-server 'status))

(define-command (("server" "reload") ())
    "have the currently running server reload its config file"
  (query-repo-server 'reload))

(define-command (("server" "config") ())
    "display the config currently in use in the running server"
  (query-repo-server 'config))

(define-command (("server" "setup") (dburi))
    "prepare the database model and install meta-data"
  (setup dburi))


;;;
;;; The buildfarm animal CLI
;;;
(define-command (("animal" "register") ())
    "ask the server for a name for the current animal"
  (discover-animal-setup-and-register-on-server)
  (format t "Welcome aboard ~a!~%" *animal-name*)
  (format t "See yourself at ~a~%" (get-animal-uri)))

(define-command (("animal" "find" "pgconfig") ())
    "list the pgconfig setups  registered on the server"
  (loop :for pgconfig-path :in (find-pgconfig-paths)
     :for pgconfig := (make-instance 'pgconfig :pg-config pgconfig-path)
     :do (format t "Found ~s for ~a~%"
                 pgconfig-path (pg-version pgconfig))))

(define-command (("animal" "list" "pgconfig") ())
    "list the pgconfig setups registered on the server"
  (list-pgconfigs-on-server))

(define-command (("animal" "add" "pgconfig") (path))
    "register a new pgconfig path on the server"
  (add-pgconfig-on-server path))

(define-command (("animal" "config") ())
    "ask the server for a name for the current animal"
  (write-current-config *standard-output*))

(define-command (("animal" "build") ())
    "build all extension queued for our platform"
  (loop :while (build-extension-for-server)))

(define-command (("build") (fullname))
    "build extension given by FULLNAME"
  (let ((logdir      (merge-pathnames "logs/" *build-root*))
        (github-uri (format nil "https://~a.git" fullname)))

    (ensure-directories-exist logdir)

    (loop :for (filename . log) :in (build-extension fullname github-uri)
       :for logfile := (merge-pathnames
                        (make-pathname :name (extension-short-name fullname)
                                       :type "txt")
                        logdir)
       :do (with-open-file (s logfile
                              :direction :output
                              :if-does-not-exist :create
                              :if-exists :supersede)
             (write-sequence log s))
       :do (format t "~%Built: ~a ~% logs: ~a~%" filename logfile))))

(define-command (("whoami") ())
    "display information about this local animal"
  (let ((platform (make-instance 'platform)))
    (format t "Animal Name: ~a~%" *animal-name*)
    (format t "        URI: ~a~%" (get-animal-uri))
    (format t "         OS: ~a~%" (os-name platform))
    (format t "    Version: ~a~%" (os-version platform))
    (format t "       Arch: ~a~%" (arch platform))))


;;;
;;; The extension CLI
;;;
(define-command (("extension" "list") ())
    "list all known extensions on the server"
  (query-repo-server 'list 'extension))

(define-command (("extension" "add") (name uri desc))
    "add a new extension on the repository server"
  (post-repo-server 'add 'extension :fullname name :uri uri :description desc))

(define-command (("extension" "queue") (name))
    "queue a build for extension NAME"
  (query-repo-server 'build name))

(define-command (("queue") (name))
    "queue a build for extension NAME"
  (query-repo-server 'build name))

