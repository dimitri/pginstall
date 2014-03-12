;;;
;;; The build animal is responsible for building extensions against a set of
;;; PostgreSQL installations.
;;;
;;; This file containst server side implementation of management routines
;;; for buildfarm animals.

(in-package #:pginstall.repo)

(defun register-animal (name os version arch)
  "Register a new animal, with its plaftorm details."
  ;; conflict avoidance and resolution?
  (with-pgsql-connection (*dburi*)
    (let ((platform (or
                     (car (select-dao 'platform (:and (:= 'os_name os)
                                                      (:= 'os_version version)
                                                      (:= 'arch arch))))
                     (make-dao 'platform
                               :os-name os
                               :os-version version
                               :arch arch))))
      (make-dao 'animal :name name :platform (platform-id platform)))))

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


