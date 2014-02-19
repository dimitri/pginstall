;;;
;;; pginstall Configuration management
;;;

(in-package #:pginstall.config)

(defvar *config-filename* "~/.pginstall.ini"
  "Where to store pginstall configuration.")

(defparameter *dburi* "postgresql:///pginstall"
  "PostgreSQL database connection.")

(defparameter *listen-port* 8042
  "Port bound by the repository server, exposing the HTTP protocol.")

(defparameter *archive-path* "/var/cache/pginstall/"
  "Where to store the extensions archives.")

(defparameter *gmake* "/usr/bin/make"
  "Path to the GNU Make command-line tool.")

(defparameter *build-root* "/tmp/pginstall/"
  "Path where to build the PostgreSQL extensions on the buildfarm animal.")

(defparameter *repo-server* "http://localhost:8042/"
  "HTTP URI of the repository server this animal should register against.")


;;;
;;; Defaults, organized in sections, with proper use facing option names
;;;
(defvar *sections-variables*
  '(("common"
     ("dburi"        *dburi*        validate-dburi)
     ("listen-port"  *listen-port*  parse-integer))
    ("repo"
     ("archive-path" *archive-path* check-and-make-directory))
    ("animal"
     ("build-root"   *build-root*   check-and-make-directory)
     ("gmake"        *gmake*        check-executable)
     ("repo-server"  *repo-server*  check-uri)))
  "Association list between configuration option names and special variables.")

(defun read-config (&optional (filename *config-filename*))
  "Read the FILENAME INI file and set the special variables accordingly."
  (let* ((ini  (make-config))
         (conf (read-files ini (list filename))))
    (loop for (section . options) in *sections-variables*
       do (loop for (option var check-fun) in options
             do (let ((value (get-option conf section option)))
                  (funcall check-fun value)
                  (setf (symbol-value var) value))))
    conf))

(defun write-current-config (stream)
  "Write the current configuration of pginstall in STREAM."
  (let ((config (make-config)))
    (loop for (section . options) in *sections-variables*
       do (progn
            (add-section config section)
            (loop for (option var check-fun) in options
               do (set-option config section option (symbol-value var)))))
    (write-stream config stream)
    config))

(defun save-config (&optional (filename (file-path *config-filename*)))
  "Save the current configuration of pginstall in FILENAME."
  (with-open-file (s (file-path-namestring filename)
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create
                     :external-format :utf8)
    (write-current-config s)))

(defun set-option-by-name (option-name value &optional (save t))
  "Set the current special variable for OPTION-NAME to given value, and
   optionally save the new setup to *config-filename*."
  (loop for (section . options) in *sections-variables*
     do (loop for (option var check-fun) in options
           when (string-equal option-name option)
           do (progn
                (funcall check-fun value)
                (setf (symbol-value var) value)
                (when save (save-config)))))
  ;; return the new value
  value)


;;
;; Validation functions
;;
(defun check-and-make-directory (value)
  "Check that VALUE is a valid pathname and create a directory if it doesn't
   already exists."
  (ensure-directories-exist
   ;; we want the last component of the filepath in value to be created as
   ;; a directory too, so we play a little trick here:
   (file-path-namestring (merge-file-paths "foo" value))))

(defun check-executable (value)
  "Check that VALUE is the pathname of a valid executable file."
  (unless (and (member (file-kind value :follow-symlinks t)
                       ;; OSX bug in iolib?
                       '(:regular-file :pipe))
               (absolute-file-path-p (parse-file-path value))
               (member :user-exec (file-permissions value)))
    (error "Not an executable file: ~s" value))
  value)

(defun check-uri (value)
  "Check that VALUE is a valid URI to use as a remote pginstall repository
   server URI."
  (let ((uri (parse-uri value)))
    ;; we want a usable URI here
    (unless (and (uri-scheme uri)
                 (uri-host uri))
      (error "Could not parse the URI: ~s" value))
    value))
