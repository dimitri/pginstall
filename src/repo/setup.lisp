;;;
;;; The build animal is responsible for building extensions against a set of
;;; PostgreSQL installations.
;;;
;;; This file containst server side implementation of management routines
;;; for buildfarm animals.

(in-package #:pginstall.repo)

(defparameter *default-extension-list*
  '((:full-name "github.com/dimitri/prefix"
     :uri       "https://github.com/dimitri/prefix"
     :desc      "prefix")
    (:full-name "github.com/markokr/plproxy"
     :uri       "https://github.com/markokr/plproxy-dev.git"
     :desc      "PL/Proxy"))
  "A list of Extension. Each extension is a list of its full name, uri and
   description.")

(defparameter *pg-versions*
  '("8.4" "9.0" "9.1" "9.2" "9.3" "9.4")
  "Candidate PostgreSQL versions.")

(defparameter *pg-bin-paths*
  '("/usr/lib/postgresql/X.Y/bin"
    "/usr/pgsql-X.Y/bin")
  "A list of PATHs where we usually find PostgreSQL binaries.")

(defun expand-pg-path (path &optional (versions *pg-versions*))
  "Expand a PATH for PostgreSQL version, replacing X.Y with the major
   version. Always returns a list."
  (if (cl-ppcre:scan "X\\.Y" path)
      (loop :for version :in versions
         :collect (cl-ppcre:regex-replace-all "X\\.Y" path version))
      (list path)))

(defun list-pg-config-in-path ()
  "Returns a list of `pg_config` binaries found in $PATH"
  (let ((paths (split-sequence:split-sequence #\: (environment-variable "PATH"))))
    (loop :for path :in (append *pg-bin-paths* paths)
       :append (loop :for p :in (expand-pg-path path)
                  :for filename := (make-pathname :directory p :name "pg_config")
                  :when (probe-file filename)
                  :collect (namestring filename)))))

(defun setup ()
  "Bootstrap an automated setup so that we can play with a repository server
   and a local animal."
  (read-config)

  (with-pgsql-connection (*dburi*)
    (with-transaction ()
     (loop for extension in *default-extension-list*
        do (apply #'make-dao 'extension extension))

     (let* ((animal (make-dao 'animal :name *animal-name*)))
       (loop :for pgconfig :in (list-pg-config-in-path)
          :collect (make-dao 'pgconfig
                             :animal-name *animal-name*
                             :animal-id   (animal-id animal)
                             :pg-config   pgconfig))))))
