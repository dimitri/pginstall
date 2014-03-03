;;;
;;; Interface with the PostgreSQL pg_config tool
;;;
(in-package #:pginstall.common)

(defvar *pg-config-keys* '(:CONFIGURE :CC :VERSION :CFLAGS)
  "Set of pg_config keys we are interested into.")

(defun run-pg-config (path &optional (keys *pg-config-keys*))
  "Run the pg_config utility given at PATH and fetch interesing values."
  (let (ret)
    (multiple-value-bind (code stdout stderr)
        (run-program path)
      (declare (ignore stderr))
      (when (= 0 code)
        (with-input-from-string (s stdout)
          (loop for line = (read-line s nil nil)
             while line
             do (cl-ppcre:register-groups-bind (key value)
                    ("([A-Z-]+) = (.*)" line)
                  (when (member key keys
                                :test (lambda (key e)
                                        (string-equal key (symbol-name e))))
                    (push (cons key value) ret)))))))
    ret))
