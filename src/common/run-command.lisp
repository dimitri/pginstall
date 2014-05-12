;;;
;;; Call into the OS / Shell to run a command.
;;;

(in-package #:pginstall.common)

(defvar *verbose* nil)
(defvar *log-stream* *standard-output*)

(defun environment-variable (name)
  "Return the value of the environement variable NAME."
  #+sbcl (sb-posix:getenv name)
  #+ccl  (ccl:getenv name))

(defun (setf environment-variable) (newvalue name)
  "Set the environment variable NAME to VALUE."
  (progn
    #+sbcl (sb-posix:setenv name newvalue 1)
    #+ccl  (ccl:setenv name newvalue t)
    newvalue))

(defun set-environment (environment)
  "ENVIRONMENT is expected to be an alist of environement variable names and
  values."
  (loop :for (name . value) :in environment
     :do (setf (environment-variable name) value)))

(defun ensure-simple-base-strings (command)
  "Return a list of simple-base-strings."
  (declare (type list command))
  (mapcar (lambda (s) (typecase s
                        (simple-base-string s)
                        (t                  (coerce s 'simple-base-string))))
          command))

(defun run-command (command
                    &key
                      cwd
                      ignore-error-status
                      (log-stream *log-stream*)
                      (capture-output t)
                      environment)
  "Run specified COMMAND (a list of strings) within CWD."
  (flet ((format-command (stream command)
           (format stream "~{~a~^ ~}~%" (mapcar (lambda (arg)
                                                  (if (find #\Space arg)
                                                      (format nil "~s" arg)
                                                      arg))
                                                command))))
    (when *verbose*
      (format-command t command))
    (format-command log-stream command)

    (let* ((outs   (make-string-output-stream))
           (out    (if capture-output
                       (make-broadcast-stream log-stream outs)
                       (make-broadcast-stream outs)))
           (errors (make-string-output-stream))
           (err    (if capture-output
                       (make-broadcast-stream log-stream errors)
                       (make-broadcast-stream errors))))
      (uiop:with-current-directory (cwd)
        ;; set environment variables
        (set-environment environment)
        (multiple-value-bind (output error code)
            (uiop:run-program (ensure-simple-base-strings command)
                              :output out
                              :error-output err
                              :ignore-error-status t)
          (declare (ignore output error))
          (let ((output-string (get-output-stream-string outs))
                (error-string  (get-output-stream-string errors)))
           (unless ignore-error-status
             (unless (= 0 code)
               (format t "~%Command:  ~a" (format-command nil command))
               (format t "Status: ~a~%" code)
               (format t "Error: ~a: ~a~%" (car command) error-string)
               (error "Command ~s failed with status ~a." (car command) code)))

           ;; return the error code, as we don't have output/error anymore
           (values code output-string error-string)))))))
