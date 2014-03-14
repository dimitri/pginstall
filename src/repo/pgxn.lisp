;;;
;;; The PGXN module implements grabbing all information about extensions
;;; known to PGXN, so that it's easier to get started with pginstall.
;;;

(defpackage #:pginstall.pgxn
  (:use #:cl
        #:pginstall.common
        #:pginstall.config
        #:pginstall.repo
        #:postmodern))

(in-package #:pginstall.pgxn)

(defvar *pgxn-mirror* "http://api.pgxn.org")
(defvar *pgxn-index* (format nil "~a/~a" *pgxn-mirror* "index.json"))

(defvar *pgxn-stream* nil)
(defvar *pgxn-api* nil)
(defvar *dists* nil
  "The whole list of PGXN available extensions.")

(defun get-all-dist-info ()
  "Get details about all the extensions known at PGXN."
  (let ((*pgxn-stream* (get-pgxn-api)))
    (setf *dists*
          (loop :for username :in (get-user-list)
             :append (mapcar #'get-dist-info (get-user-release-list username))))
    (close *pgxn-stream*)))


;;;
;;; Integration of the PGXN API
;;;
(defun get-pgxn-api ()
  "The whole PGXN API is based around publishing the meta-api information in
   its index.json file, so that we can pretend to auto-discover the
   features."
  (multiple-value-bind (body status-code headers uri stream must-close reason)
      (drakma:http-request *pgxn-index* :close nil)
    (declare (ignore status-code headers uri must-close reason))
    (setf *pgxn-api*
          (yason:parse
           (babel:octets-to-string body :encoding :utf-8)))
    ;; and return the stream
    stream))

(defun fill-uri-template (tmpl-name &rest keyword-pairs &key &allow-other-keys)
  "Return a string to use an a PGXN API URI, given a TEMPLATE containing
   {letter} style entries that are replaced with the :letter keyword."
  (let ((template
         (format nil "~a~a" *pgxn-mirror* (gethash tmpl-name *pgxn-api*))))
    (cl-ppcre:regex-replace-all "{([a-z]+)}"
                                template
                                #'(lambda (match first &rest registers)
                                    (declare (ignore match registers))
                                    (getf keyword-pairs
                                          (intern (string-upcase first)
                                                  (find-package "KEYWORD"))))
                                :simple-calls t)))

(define-condition pgxn-error ()
  ((uri         :initarg :uri    :reader server-error-uri)
   (status-code :initarg :status :reader server-error-status-code)
   (reason      :initarg :reason :reader server-error-reason)
   (body        :initarg :body   :reader server-error-body)))

(defun query-pgxn (api &rest keyword-pairs &key &allow-other-keys)
  "Query api.pgxn.org for the api parts, with given parameters."
  (let* ((api-uri (apply #'fill-uri-template
                         api
                         :allow-other-keys t
                         keyword-pairs)))
    (multiple-value-bind (body status-code headers uri stream must-close reason)
        (handler-case
            (drakma:http-request api-uri :stream *pgxn-stream* :close nil)
          (stream-error (c)
            (declare (ignore c))
            (drakma:http-request api-uri :close nil)))
      (declare (ignore headers must-close))
      (if (= status-code 200)
          (progn
            (setf *pgxn-stream* stream) ; keep it
            (yason:parse
             (babel:octets-to-string body :encoding :utf-8)))
          (progn
            (setf *pgxn-stream* nil)    ; take a new one next time
            (error 'pgxn-error
                   :uri uri
                   :status-code status-code
                   :reason reason
                   :body body))))))


;;;
;;; Use the API to grab extension related information: we have to fetch all
;;; usernames and we have them organized by first letter, then for each user
;;; we may fetch a list of "releases", and each release is also a "dist"
;;; object offering the information we need for a pginstall extension:
;;;
;;;   Extension Name, Repository URL and description.
;;;
(defun get-user-list (&optional (letters "abcdefghijklmnopqrstuvwxyz"))
  "Get the whole user list of PGXN."
  (loop :for letter :across letters
     :append (mapcar (lambda (user-hash)
                       (gethash "user" user-hash))
                     (handler-case
                         (query-pgxn "userlist" :letter (string letter))
                       (pgxn-error (e)
                         (declare (ignore e))
                         nil)))))

(defun get-user-release-list (username &key (sorted t))
  "Get the list of all known \"releases\" for USERNAME."
  (let ((extensions
         (alexandria:hash-table-keys
          (gethash "releases" (query-pgxn "user" :user username)))))
    (if sorted (sort extensions #'string<) extensions)))

(defun get-dist-info (dist-name)
  "Get details about given DIST-NAME."
  (let* ((dist-hash   (query-pgxn "dist" :dist (string-downcase dist-name)))
         (description (gethash "description" dist-hash))
         (uri         (let ((resources (gethash "resources" dist-hash)))
                        (when resources
                          (let ((repo (gethash "repository" resources)))
                            (when repo
                             (string-trim " /" (gethash "web" repo)))))))
         (parsed-uri  (when uri (puri:parse-uri uri)))
         (full-name   (if parsed-uri
                          (format nil "~a~a"
                                  (puri:uri-host parsed-uri)
                                  (puri:uri-path parsed-uri))
                          dist-name)))
    (make-instance 'extension
                   :full-name full-name
                   :uri uri
                   :desc description)))


