;;;
;;; Some tooling to talk to the repository server.
;;;
(in-package #:pginstall.client)

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

(defun parse-query-and-parameters (&rest query-and-parameters)
  "Parse the &rest arguments as query API elements until we get our first
   keyword parameter then as a parameters plist."
  (loop
     :for query-or-parameter :in query-and-parameters
     :for done-with-query := nil :then (or done-with-query
                                           (keywordp query-or-parameter))

     :when done-with-query
     :collect (string-downcase (string query-or-parameter)) :into parameters

     :else :collect query-or-parameter :into query

     :finally (return (list :query query
                            :parameters (alexandria:plist-alist parameters)))))

(defun post-repo-server (&rest query-and-parameters)
  "Send a :POST query to the repository server with KEY-VALUE-PAIRS
   parameters and signal a condition when it returns any kind of error."
  (destructuring-bind (&key query parameters)
      ;; given our strange specs on the parameter list, we get to parse it
      ;; ourselves.
      (apply #' parse-query-and-parameters query-and-parameters)
    (multiple-value-bind (body status-code headers uri stream must-close reason)
        (drakma:http-request (apply #'build-api-uri query)
                             :method :post
                             :form-data t
                             :parameters parameters)
      (declare (ignore headers stream must-close))
      (if (= status-code 200)
          body
          (error 'server-error
                 :uri uri :status status-code :reason reason :body body)))))
