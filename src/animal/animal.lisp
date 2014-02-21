;;;
;;; The build animal is responsible for building extensions against a set of
;;; PostgreSQL installations.
;;;
;;; This file containst server side implementation of management routines
;;; for buildfarm animals.


(in-package #:pginstall.animal)

(defparameter *default-extension-list*
  '((:full-name "github.com/dimitri/prefix"
     :uri       "https://github.com/dimitri/prefix"
     :desc      "prefix")
    (:full-name "github.com/markokr/plproxy"
     :uri       "https://github.com/markokr/plproxy-dev.git"
     :desc      "PL/Proxy"))
  "A list of Extension. Each extension is a list of its full name, uri and
   description.")

;;;
;;; TODO: Implement:
;;;
;;;   (defun register-animal (name server-uri animal-uri))
;;;   (defun add-pg-config (pg-config-path))
;;;   (defun rm-pg-config (pg-config-path))
;;;   (defun list-animals ())
;;;   (defun list-pg-configs ())
;;;   (defun build-extension (name &rest pg-config))
;;;

(defun add-pg-config (pg-config-pathspec
                      &key ((:animal-name *animal-name*) *animal-name*))
  "Add a pg_config path to the build enironment list of this animal."
  (with-pgsql-connection (*dburi*)
    (let ((exists (car (query-dao 'pgconfig
                                  "select pgc.*
                               from pgconfig pgc
                                    join animal a on a.id = pgc.animal
                              where a.name = $1 and pg_config = $2"
                              *animal-name* pg-config-pathspec))))
      (or exists
          (make-dao 'pgconfig
                    :animal-name *animal-name*
                    :pg-config pg-config-pathspec)))))

(defun list-pg-configs (&key ((:animal-name *animal-name*) *animal-name*))
  "List the pgconfigs associated to given :ANIMAL-NAME, which defaults to
   the special variable *ANIMAL-NAME*, as initialized by reading the local
   configuration file."
  (with-pgsql-connection (*dburi*)
    (query-dao 'pgconfig "select pgc.*
                            from pgconfig pgc
                                 join animal a on a.id = pgc.animal
                           where a.name = $1"
               *animal-name*)))


