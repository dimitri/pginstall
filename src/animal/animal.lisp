;;;
;;; The build animal is responsible for building extensions against a set of
;;; PostgreSQL installations.
;;;

(in-package #:pginstall.animal)

(defparameter *build-root* "/tmp/")
(defparameter *gmake* "/usr/bin/make")

(defparameter *pg-server-dev-list*
  '("/Users/dim/pgsql/ddl/bin/pg_config"))

(defparameter *extension-list*
  '((:full-name "github.com/dimitri/prefix"
     :uri       "https://github.com/dimitri/prefix"
     :desc      "prefix")
    (:full-name "github.com/markokr/plproxy"
     :uri       "https://github.com/markokr/plproxy-dev.git"
     :desc      "PL/Proxy"))
  "A list of Extension. Each extension is a list of its full name, uri and
   description.")

(defclass extension ()
    ((id          :col-type integer  :reader ext-id)
     (shortname   :col-type string :accessor short-name)
     (fullname    :col-type string :accessor full-name :initarg :full-name)
     (uri         :col-type string :accessor uri       :initarg :uri)
     (description :col-type string :accessor desc      :initarg :desc))
  (:documentation
   "a PostgreSQL extension. The full name must be a unique partial URI,
    where the scheme name, query and fragment parts are omitted.")
  (:metaclass dao-class)
  (:keys id))

(defun extension-short-name (extension-full-name)
  "Return the extension short-name when given a short name."
  (pathname-name extension-full-name))

(defmethod initialize-instance :after ((extension extension) &key)
  "Automatically compute the short name of the extension."
  (setf (slot-value extension 'shortname)
        (extension-short-name (slot-value extension 'fullname))))

(defmethod print-object ((extension extension) stream)
  (print-unreadable-object (extension stream :type t :identity t)
    (with-slots (id shortname fullname) extension
      (format stream "~d ~a [~a]" id shortname fullname))))

(defun build-extension (name)
  "Build extension matching NAME, which can be either a full name or a short
   name."
  ;; TODO: implement with-psql-transaction and co in config module
  (with-psql-transaction ()
    (let* ((slot      'shortname)     ; TODO: compute 'shortname or 'fullname
           (extension (select-dao 'extension (:= slot name))))
      (uiop:run-program *gmake*))))
