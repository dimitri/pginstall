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
                      (number (format nil "~a" element))
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
                       (arch platform))))

(defun get-extension-to-build ()
  "Get an extension to build from the *REPO-SERVER*, or nil."
  (yason:parse (query-repo-server 'get 'work 'for *animal-name*)))

(defun upload-archive (extension-full-name archive-filename buildlog)
  "Upload an archive file."
  (let ((archive                        ; the multi-part POST data
         (list (pathname archive-filename)
               :content-type "application/octet-stream"
               :filename (file-path-file archive-filename))))

   (cl-ppcre:register-groups-bind (pgversion os-name os-version arch)
       ("^.*--(.*)--(.*)--(.*)--(.*).tar.gz" (file-path-file archive-filename))
     (drakma:http-request (build-api-uri 'upload 'archive)
                          :method :post
                          :form-data t
                          :content-length t
                          :parameters `(("archive"    . ,archive)
                                        ("buildlog"   . ,buildlog)
                                        ("extension"  . ,extension-full-name)
                                        ("pgversion"  . ,pgversion)
                                        ("animal"     . ,*animal-name*)
                                        ("os-name"    . ,(substitute #\Space
                                                                     #\_
                                                                     os-name))
                                        ("os-version" . ,os-version)
                                        ("arch"       . ,arch))))))

(defun build-extension-for-server ()
  "Connect to the *REPO-SERVER* and ask for any extension to build for
   registered *ANIMAL-NAME*."
  (let ((extension-hash (get-extension-to-build)))
    (when extension-hash
      (let* ((extension-full-name (gethash "FULLNAME" extension-hash))
             (extension-uri       (gethash "URI" extension-hash))
             (archives
              (build-extension extension-full-name extension-uri)))
        (loop :for (archive-filename . log) :in archives
           :collect (parse-archive
                     (upload-archive extension-full-name archive-filename log)))))))
