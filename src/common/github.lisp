;;;
;;; Parse github repositories information
;;;

(in-package #:pginstall.common)

(defun parse-extension-uri (uri)
  "Parse given URI as a github http project uri"
  (let ((uri (puri:parse-uri uri)))
    (when (string= "github.com" (puri:uri-host uri))
      (destructuring-bind (abs user repo) (puri:uri-parsed-path uri)
        (declare (ignore abs))
        (let ((metadata
               (github:api-command (format nil "/repos/~a/~a" user repo))))
          (values (format nil "github.com/~a" (getf metadata :full-name))
                  (getf metadata :clone-url )
                  (getf metadata :description)))))))

