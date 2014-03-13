;;;
;;; Handle the CLI invocations of the pginstall binary.
;;;

(in-package #:pginstall)

;;;
;;; First, our very own command line facility.
;;;
(defstruct command verbs bindings help lambda)

(defvar *commands* (make-array 0
                               :element-type 'command
                               :adjustable t
                               :fill-pointer t)
  "Host commands defined with the DEFINE-COMMAND macro.")

(defmethod same-command ((a command) (b command))
  "Return non-nil when a and b are commands with the same verbs"
  (equal (command-verbs a) (command-verbs b)))

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
        (position (gensym)))
   `(eval-when (:load-toplevel :compile-toplevel :execute)
      (let* ((,fun      (lambda ,bindings ,@body))
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
              (apply fun args)
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
                      (server-error-body e)))))
        (usage argv))))

(defvar *old-commands*
  '(("set"
     ("dburi"        . set-config-variable)
     ("listen-port"  . set-config-variable)
     ("archive-path" . set-config-variable)
     ("name"         . set-config-variable)
     ("build-root"   . set-config-variable)
     ("gmake"        . set-config-variable)
     ("git"          . set-config-variable)
     ("server"       . set-config-variable))

    ("server"
     ("start"   . server-start)
     ("stop"    . server-stop)
     ("status"  . server-status)
     ("config"  . server-config)
     ("reload"  . server-reload)
     ("restart" . server-restart))

    ("animal"
     ("name"       . animal-name)
     ("register"   . animal-register)
     (("pg" "ls")  . animal-list-pgconfigs)
     (("pg" "add") . animal-add-pgconfig)
     ;; (("pg" "rm")  . animal-remove-pgconfig)
     ("build"      . animal-build))

    ("extension"
     ("ls"              . extension-list)
     ("add"             . extension-add)
     ;; ("rm"              . extension-remove)
     (("queue" "build") . extension-queue-build))))


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
(define-command (("server" "status") ())
    "get the status of the currently running server"
  (query-repo-server 'status))

(define-command (("server" "reload") ())
    "have the currently running server reload its config file"
  (query-repo-server 'reload))

(define-command (("server" "config") ())
    "display the config currently in use in the running server"
  (query-repo-server 'config))


;;;
;;; The buildfarm animal CLI
;;;
(define-command (("animal" "name") ())
    "ask the server for a name for the current animal"
  (let ((*animal-name* (yason:parse (query-repo-server 'pick 'my 'name))))
    (save-config)
    (format t "Welcome aboard ~a!~%" *animal-name*)
    (format t "See yourself at ~a/animal/pict/~a~%" *repo-server* *animal-name*)
    *animal-name*))

(define-command (("animal" "register") ())
    "register the current animal name and pgconfigs on the server"
  (read-config)                         ; that sets *animal-name*
  (if *animal-name*
      (register-animal-on-server)
      (error 'cli-error
             :mesg "no animal name"
             :detail "To register this animal, you need to name it."
             :hint "use the command: pginstall set name <name>")))

(define-command (("animal" "list" "pgconfig") ())
    "list the pgconfig setups currently registered on the server"
  (list-pgconfigs-on-server))

(define-command (("animal" "add" "pgconfig") (path))
    "register a new pgconfig path on the server"
  (add-pgconfig-on-server path))

(define-command (("animal" "build") ())
    "build all extension queued for our platform"
  (loop :while (build-extension-for-server)))


;;;
;;; The extension CLI
;;;
(define-command (("extension" "list") ())
    "list all known extensions on the server"
  (query-repo-server 'list 'extension))

(define-command (("extension" "add") (name uri desc))
    "list all known extensions on the server"
  (post-repo-server 'add 'extension :fullname name :uri uri :description desc))

(define-command (("extension" "queue" "build") (name))
    "queue a build for extension NAME"
  (query-repo-server 'build name))

