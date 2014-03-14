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

(defun setup ()
  "Bootstrap an automated setup so that we can play with a repository server
   and a local animal."
  (read-config)

  (with-pgsql-connection (*dburi*)
    (with-transaction ()
     (loop for extension :in *default-extension-list*
        :do (apply #'make-dao 'extension extension))

     (loop :for (name . pict) :in *animal-name-registry*
        :do (query "insert into registry(name, pict) values($1, $2)" name pict))

     (let* ((animal (make-dao 'animal :name *animal-name*)))
       (loop :for pgconfig :in (find-pgconfig-paths)
          :collect (make-dao 'pgconfig
                             :animal-name *animal-name*
                             :animal-id   (animal-id animal)
                             :pg-config   pgconfig))))))
