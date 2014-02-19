;;;
;;; Common code needed in several parts of the rest of the code.
;;;
;;; PostgreSQL transaction and connection management, etc.

(in-package #:pginstall.common)

(defmacro with-pgsql-connection ((connection-string) &body body)
  "Runs BODY within an established PostgreSQL connection."
  `(with-connection (parse-pgsql-connection-string ,connection-string)
     ,@body))

;;;
;;; Validating a connection string
;;;
(defun validate-dburi (connection-string)
  "Signal an error when CONNECTION-STRING either can't be parsed or if we
   can't connect to a PostgreSQL server when using it."
  (with-pgsql-connection (connection-string)
    (query "SELECT 1" :single))
  ;; make sure to return the valid connection-string
  connection-string)


