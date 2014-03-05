;;;
;;; The implementation of the API exposed through the server.
;;;

(in-package #:pginstall.repo)

(defun select-star (type)
  "Return a list of all extensions known to the system."
  (with-pgsql-connection (*dburi*)
    (select-dao type)))

