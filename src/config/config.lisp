;;;
;;; pginstall Configuration management
;;;

(in-package #:pginstall.config)

(defvar *config-filename* "~/.pginstall.ini"
  "Where to store pginstall configuration.")

(defparameter *repo-logfile* "/tmp/pginstall-repo.log")
(defparameter *http-logfile* "/tmp/pginstall-http.log")

(defparameter *dburi* "postgresql:///pginstall"
  "PostgreSQL database connection.")

(defparameter *listen-port* 8042
  "Port bound by the repository server, exposing the HTTP protocol.")

(defparameter *archive-path* "/var/cache/pginstall/"
  "Where to store the extensions archives.")

(defparameter *gmake* "make"
  "Path to the GNU Make command-line tool.")

(defparameter *git* "git"
  "Path to the git command-line tool.")

(defparameter *build-root* "/tmp/pginstall/"
  "Path where to build the PostgreSQL extensions on the buildfarm animal.")

(defparameter *repo-server* "http://localhost:8042/"
  "HTTP URI of the repository server this animal should register against.")

(defparameter *animal-name* nil
  "Name of the animal that runs on this local instance.")


;;;
;;; Defaults, organized in sections, with proper use facing option names
;;;
(defvar *sections-variables*
  '(("server"
     ("dburi"        *dburi*        validate-dburi)
     ("listen-port"  *listen-port*  parse-integer)
     ("repo-logfile" *repo-logfile* check-file-path)
     ("http-logfile" *http-logfile* check-file-path)
     ("archive-path" *archive-path* check-and-make-directory))
    ("animal"
     ("name"         *animal-name*  identity)
     ("build-root"   *build-root*   check-and-make-directory)
     ("gmake"        *gmake*        check-executable)
     ("git"          *git*          check-executable)
     ("server"       *repo-server*  check-uri)))
  "Association list between configuration option names and special variables.")

(defun read-config (&optional (filename *config-filename*))
  "Read the FILENAME INI file and set the special variables accordingly."
  (when (probe-file filename)
    (let* ((ini  (make-config))
           (conf (read-files ini (list filename))))
      (loop :for (section . options) :in *sections-variables*
         :do (loop :for (option var check-fun) :in options
                :when (has-option-p conf section option)
                :do (let ((value (get-option conf section option)))
                      (setf (symbol-value var)
                            (handler-case
                                (funcall check-fun value)
                              ;; allow reading broken config
                              (condition (c)
                                (warn "~a" c)
                                value))))))
      conf)))

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

(defun get-option-by-name (option-name)
  "Get the current value of the given OPTION-NAME."
  (loop :for (section . options) :in *sections-variables*
     :for value := (loop :for (option var check-fun) :in options
                      :when (string-equal option-name option)
                      :return (symbol-value var))
     :when value
     :return value))

(defun set-option-by-name (option-name value &optional (save t))
  "Set the current special variable for OPTION-NAME to given value, and
   optionally save the new setup to *config-filename*."
  (loop :for (section . options) :in *sections-variables*
     :do (loop for (option var check-fun) :in options
            :when (string-equal option-name option)
            :do (progn
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
   (file-path-namestring (merge-file-paths "foo" value)))

  ;; and return value, rather than value/foo
  value)

(defun check-executable (value)
  "Check that VALUE is the pathname of a valid executable file."
  (when (absolute-file-path-p (parse-file-path value))
    (unless (and (member (file-kind value :follow-symlinks t)
                         ;; OSX bug in iolib?
                         '(:regular-file :pipe))
                 (member :user-exec (file-permissions value)))
      (error "Not an executable file: ~s" value)))
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

(defun check-file-path (path)
  "Check that we can open a file at given PATH."
  (ensure-directories-exist
   (directory-namestring (file-path-namestring (parse-file-path path))))
  ;; then return path itself
  path)
