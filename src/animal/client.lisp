;;;
;;; The Buildfarm Animal gets is orders from the Repository Server
;;;

(in-package #:pginstall.animal)

;;;
;;; TODO: Implement:
;;;
;;;   (defun add-pg-config (pg-config-path))
;;;   (defun rm-pg-config (pg-config-path))
;;;


;;;
;;; First, some tooling
;;;
(defun build-api-uri (&rest query)
  "Build the API HTTP URI to use for the API described in API."
  (format nil "~aapi/~{~a~^/~}"
          *repo-server*
          (mapcar (lambda (element)
                    (typecase element
                      (symbol (string-downcase (symbol-name element)))
                      (t      (drakma:url-encode element :utf8))))
                  query)))

(define-condition server-error ()
  ((uri         :initarg :uri    :reader server-error-uri)
   (status-code :initarg :status :reader server-error-status-code)
   (reason      :initarg :reason :reader server-error-reason)
   (body        :initarg :body   :reader server-error-body)))

(defun query-repo-server (&rest query)
  "Query the repository server and signal a condition when it returns any
   kind of error."
  (multiple-value-bind (body status-code headers uri stream must-close reason)
      (drakma:http-request (apply #'build-api-uri query))
    (declare (ignore headers stream must-close))
    (if (= status-code 200)
        body
        (error 'server-error
               :uri uri :status status-code :reason reason :body body))))


;;;
;;; Now the client side implementation of the API
;;;
(defun register-animal-on-server ()
  "Register *ANIMAL-NAME* on *REPO-SERVER* for current platform."
  (let ((platform (make-instance 'platform)))
    (query-repo-server 'register 'animal
                       *animal-name*
                       (os-name platform)
                       (os-version platform)
                       (arch platform) )))

(defun get-extension-to-build ()
  "Get an extension to build from the *REPO-SERVER*, or nil."
  (yason:parse (query-repo-server 'get 'work 'for *animal-name*)))

(defun build-extension-for-server ()
  "Connect to the *REPO-SERVER* and ask for any extension to build for
   registered *ANIMAL-NAME*."
  (let ((extension-hash (get-extension-to-build)))
    (when extension-hash
      (build-extension (gethash "FULLNAME" extension-hash)
                       (gethash "URI"      extension-hash)))))
