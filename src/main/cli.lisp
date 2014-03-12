;;;
;;; Handle the CLI invocations of the pginstall binary.
;;;

(in-package #:pginstall)

(defvar *commands*
  '(("set"
     ("dburi"        . set-config-variable)
     ("listen-port"  . set-config-variable)
     ("uri"          . set-config-variable)
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
     ("reload"  . server-reload)
     ("restart" . server-restart))

    ("animal"
     ("name"       . set-config-variable)
     ("register"   . animal-register)
     (("pg" "ls")  . animal-list-pgconfigs)
     (("pg" "add") . animal-add-pgconfig)
     (("pg" "rm")  . animal-remove-pgconfig)
     ("register"   . animal-register)
     ("build"      . animal-build)
     ("upload"     . animal-upload))

    ("extension"
     ("ls"              . extension-list)
     ("add"             . extension-add)
     ("rm"              . extension-remove)
     (("queue" "build") . extension-queue-build))))

(defun ensure-list (l)
  (typecase l
    (list l)
    (t    (list l))))

(defun find-command-function (argv)
  "Return the function to run for given command, and its arguments"
  (destructuring-bind (program command &rest args) argv
    (declare (ignore program))
    (loop :for (section . funs ) :in *commands*
       :when (string-equal section command)
       :return (loop :for (cmd . fun) :in funs
                  :when (notany #'null (mapcar #'string-equal (ensure-list cmd) args))
                  :return `(,fun ,@args)))))

(defmacro with-arguments (specs args &body body)
  "Destructure argument list ARGS against SPECS and run BODY within a
   lexical context that defines each SPECS entry as a binding to the
   matching ARGS value.

   SPECS is a list of:

     - either a symbol name, which is the name of the binding
     - a list of a symbol name followed by a type specification.

   A type specification allows checking that the argument is of the proper
   type and converts the string form of the argument to its proper
   type unless a string is wanted (it's then a no-op).

     - :type string
     - :type integer"
  (let ((bindings (mapcar #'car (mapcar #'ensure-list specs))))
    `(destructuring-bind ,bindings
         ',(loop :for spec :in specs
              :for arg in args
              :collect (destructuring-bind (symbol &key (type 'string))
                           (ensure-list spec)
                         (declare (ignore symbol))
                         (case type
                           (string  arg)
                           (integer (parse-integer arg)))))
       ,@body)))

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
  (destructuring-bind (fun &rest args)
      (find-command-function argv)
    (handler-case
        (apply fun args)
      (cli-error (e)
        (format t
                "ERROR: ~a~%~@[DETAIL: ~a~%~]~@[HINT: ~a~%~]"
                (cli-error-message e)
                (cli-error-detail e)
                (cli-error-hint e))))))


;;;
;;; Now for implementing the functions.
;;;
(defun set-config-variable (name value)
  "Set configuration variable by name NAME to given VALUE."
  (set-option-by-name name value))


;;;
;;; Controlling the repository server process.
;;;
(defun write-pidfile ())
(defun read-pidfile ())

(defun server-start ())
(defun server-stop ())

(defun server-status ())


;;;
;;; The buildfarm animal CLI
;;;
(defun animal-register (command &rest args)
  (declare (ignore command args))
  (read-config)                         ; that sets *animal-name*
  (if *animal-name*
      (register-animal-on-server)
      (error 'cli-error
             :mesg "no animal name"
             :detail "To register this animal, you need to name it."
             :hint "use the command: pginstall set name <name>")))
