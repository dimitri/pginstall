;;;
;;; The implementation of the API exposed through the server.
;;;

(in-package #:pginstall.repo)

(defun queue-extension-build (extension-name)
  "Queue a build request for EXTENSION-NAME."
  (with-pgsql-connection (*dburi*)
    (query "select * from pginstall.queue_build($1)"
           extension-name
           (:dao build-queue :single))))

(defun queue-get-work (animal-name)
  "Return an Extension object from the build queue"
  (with-pgsql-connection (*dburi*)
    (query "select * from pginstall.pick_from_queue($1)"
           animal-name
           (:dao extension :single))))

(defun receive-archive (extension pgversion
                        animal-name os version arch
                        buildlog archive-filename archive)
  "Register a new extension's archive and move the file at the expected place."
  (let ((archive-full-name (merge-pathnames archive-filename *archive-path*)))
    (uiop:copy-file archive archive-full-name)

    (with-pgsql-connection (*dburi*)
      (query "select * from pginstall.register_build($1, $2, $3, $4, $5, $6, $7, $8)"
             extension pgversion animal-name os version arch
             buildlog
             (namestring archive-full-name)
             (:dao archive :single)))))

(defun select-extensions-available-on-platform (os version arch)
  "Return the list of available extensions on a given platform."
  (with-pgsql-connection (*dburi*)
    (query-dao 'extension
               "select * from pginstall.list_extensions_for($1, $2, $3)"
               os version arch)))

(defun archive-pathname (extension pgversion os version arch)
  "Return the pathname to the extension's archive file for given version of
   PostgreSQL and OS specifications."
  (with-pgsql-connection (*dburi*)
    (query "select * from pginstall.archive_pathname($1, $2, $3, $4, $5)"
           extension pgversion os version arch
           :single)))
