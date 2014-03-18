;;;
;;; The repository server is also offering a web frontend to manage the
;;; information found in the database, read logs, etc.
;;;
;;; Setup some PATHs that we need to cache the filesystem content as static
;;; files.

(in-package #:pginstall.server)

(defvar *root*
  (asdf:system-relative-pathname :pginstall "web/")
  "Where to find our static resources")

(defvar *noconf-path* (merge-pathnames "noconf.html" *root*))
(defvar *header-path* (merge-pathnames "header.html" *root*))
(defvar *footer-path* (merge-pathnames "footer.html" *root*))

(defvar *readme-path* (asdf:system-relative-pathname :pginstall "README.md"))

(defvar *dist*   (merge-pathnames
                  (make-pathname :directory '(:relative "bootstrap-3.1.1-dist"))
                  *root*))
(defvar *pict*   (merge-pathnames
                  (make-pathname :directory '(:relative "images")) *root*))

(defvar *docroot* (asdf:system-relative-pathname :pginstall "doc/"))


;;;
;;; Now load all the static files in memory, at load time.
;;;
(defparameter *fs*
  (let ((fs (make-hash-table :test #'equal)))
    (loop :for (root . url-path) in `((,*dist*        . "/dist")
                                      (,*pict*        . "/pict")
                                      (,*docroot*     . "/help")
                                      (,*readme-path* . "/readme"))
       :do (case (iolib.os:file-kind root)
             (:directory    (load-static-directory fs root url-path))
             (:regular-file (load-static-file fs root url-path))))
    fs)
  "File system as an hash-table in memory.")

(defparameter *noconf* (read-file-into-string *noconf-path*))
(defparameter *header* (read-file-into-string *header-path*))
(defparameter *footer* (read-file-into-string *footer-path*))
(defparameter *readme* (read-file-into-string *readme-path*))
