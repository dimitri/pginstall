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
  "A default list of Extension, to start with something.")


;;
;; Read our model queries
;;
(defparameter *model*
  (read-queries (asdf:system-relative-pathname :pginstall "src/repo/model.sql"))
  "List of SQL queries to run to prepare our data model.")

(defparameter *model-table-list*
  (sort
   (remove-if #'null
              (mapcar (lambda (sql) (cl-ppcre:register-groups-bind (table-name)
                                        ("create table ([^ ]+)" sql)
                                      table-name))
                      *model*))
   #'string<)
  "List of table names expected to be created by *model*, to allow for
   checking if the setup has been made.")

(defparameter *api*
  (read-queries (asdf:system-relative-pathname :pginstall "src/repo/api.sql"))
  "List of SQL queries to run to prepare our data API.")

(defun setup (&optional dburi)
  "Bootstrap an automated setup so that we can play with a repository server
   and a local animal."
  (let ((*dburi* (or dburi *dburi*)))
    (with-pgsql-connection (*dburi*)
      (with-transaction ()
        (loop :for sql :in *model* :do (query sql))
        (loop :for sql :in *api* :do (query sql))

        (loop :for extension :in *default-extension-list*
           :do (apply #'make-dao 'extension extension))

        (loop :for (name . pict) :in *animal-name-registry*
           :do (query "insert into registry(name, pict) values($1, $2)"
                      name pict))))))
