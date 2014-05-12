;;;
;;; Interface with the PostgreSQL pg_config tool
;;;
(in-package #:pginstall.common)

(defvar *pg-config-keys* '(:CONFIGURE :CC :VERSION :CFLAGS)
  "Set of pg_config keys we are interested into.")

(defun run-pg-config (path &optional (keys *pg-config-keys*))
  "Run the pg_config utility given at PATH and fetch interesing values."
  (let (ret)
    (multiple-value-bind (code stdout stderr)
        (run-command `(,path) :capture-output nil)
      (declare (ignore code stderr))
      (with-input-from-string (s stdout)
        (loop for line = (read-line s nil nil)
           while line
           do (cl-ppcre:register-groups-bind (key value)
                  ("([A-Z-]+) = (.*)" line)
                (when (member key keys
                              :test (lambda (key e)
                                      (string-equal key (symbol-name e))))
                  (push (cons key value) ret))))))
    ret))


;;;
;;; Support auto-discovery of pgconfig binaries available on the system
;;;
(defparameter *pg-versions*
  '("9.1" "9.2" "9.3" "9.4")
  "Candidate PostgreSQL versions.")

(defparameter *pg-bin-paths*
  '("/usr/lib/postgresql/X.Y/bin"
    "/usr/pgsql-X.Y/bin")
  "A list of PATHs where we usually find PostgreSQL binaries.")

(defun expand-pg-path (path &optional (versions *pg-versions*))
  "Expand a PATH for PostgreSQL version, replacing X.Y with the major
   version. Always returns a list."
  (mapcar (lambda (path) (format nil"~a/" path)) ; yeah it's ugly.
          (if (cl-ppcre:scan "X\\.Y" path)
              (loop :for version :in versions
                 :collect (cl-ppcre:regex-replace-all "X\\.Y" path version))
              (list path))))

(defun find-pgconfig-paths ()
  "Returns a list of `pg_config` binaries found in $PATH"
  (let ((paths (split-sequence:split-sequence #\: (environment-variable "PATH"))))
    (loop :for path :in (append *pg-bin-paths* paths)
       :append (loop :for p :in (expand-pg-path path)
                  :for filepath := (uiop:merge-pathnames* "pg_config" p)
                  :for filename := (uiop:native-namestring filepath)
                  :when (probe-file filename)
                  :collect filename))))

