;;;
;;; The upstream feature allows to fetch archive files from the upstream
;;; server when one is setup: it's a proxy.
;;;

(in-package #:pginstall.server)

(defun fetch-archive-from-upstream (archive-filename)
  "Go fetch an archive file on *upstream-server*."
  (let ((uri (format nil "~a~a" *upstream-server* archive-filename)))
    (multiple-value-bind (body status-code headers uri stream must-close reason)
        (drakma:http-request uri)
      (declare (ignore headers stream must-close))
      (if (= status-code 200)
          (values body status-code)
          (values
           (format nil "Couldn't fetch archive from upstream server ~s~%~A~%"
                   uri reason)
           hunchentoot::+http-internal-server-error+)))))


