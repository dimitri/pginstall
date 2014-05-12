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
;;; Tools to load our in-memory cache.
;;;
(defun markdown-to-html (path)
  "Produce some HTML output from the Markdown document found at PATH."
  (with-html-output-to-string (s)
    (htm
     (:div :class "col-sm-9 col-sm-offset-3 col-md-9 col-md-offset-2 main"
           (str
            (multiple-value-bind (md-object html-string)
                (cl-markdown:markdown path :stream nil)
              (declare (ignore md-object))
              html-string))))))

(defun load-static-file (fs pathname url-path)
  "Load given PATHNAME contents at URL-PATH in FS."
  (cond
    ((string= "md" (pathname-type pathname))
     (setf (gethash (uiop:split-name-type url-path) fs)
           (markdown-to-html (read-file-into-string pathname))))
    (t
     (setf (gethash url-path fs)
           (read-file-into-byte-vector pathname)))))

(defun pathname-to-url (pathname url-path)
  "Transform given PATHNAME into an URL at which to serve it within URL-PATH."
  (multiple-value-bind (flag path-list last-component file-namestring-p)
      (uiop:split-unix-namestring-directory-components
       (uiop:native-namestring pathname))
    (declare (ignore flag file-namestring-p))
    (format nil "~a~{/~a~}/~a" url-path path-list last-component)))

(defun load-static-directory (fs root url-path)
  "Walk PATH and load all files found in there as binary sequence, FS being
   an hash table referencing the full path against the bytes."
  (flet ((collectp  (dir) (declare (ignore dir)) t)
         (recursep  (dir) (declare (ignore dir)) t)
         (collector (dir)
           (loop :for pathname :in (uiop:directory-files dir)
              :unless (or (uiop:directory-pathname-p pathname)
                          (string= "zip" (pathname-type pathname)))
              :do (let ((url (pathname-to-url
                              (uiop:enough-pathname pathname root) url-path)))
                    (load-static-file fs pathname url)))))
    (uiop:collect-sub*directories root #'collectp #'recursep #'collector)))


;;;
;;; Now load all the static files in memory, at load time.
;;;
(defparameter *fs*
  (let ((fs (make-hash-table :test #'equal)))
    (loop :for (root . url-path) in `((,*dist*        . "/dist")
                                      (,*pict*        . "/pict")
                                      (,*docroot*     . "/help")
                                      (,*readme-path* . "/readme"))
       :do (if (uiop:directory-pathname-p root)
               (load-static-directory fs root url-path)
               (load-static-file fs root url-path)))
    fs)
  "File system as an hash-table in memory.")

(defparameter *noconf* (read-file-into-string *noconf-path*))
(defparameter *header* (read-file-into-string *header-path*))
(defparameter *footer* (read-file-into-string *footer-path*))
(defparameter *readme* (read-file-into-string *readme-path*))
