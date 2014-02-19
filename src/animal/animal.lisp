;;;
;;; The build animal is responsible for building extensions against a set of
;;; PostgreSQL installations.
;;;

(in-package #:pginstall.animal)

(defparameter *default-extension-list*
  '((:full-name "github.com/dimitri/prefix"
     :uri       "https://github.com/dimitri/prefix"
     :desc      "prefix")
    (:full-name "github.com/markokr/plproxy"
     :uri       "https://github.com/markokr/plproxy-dev.git"
     :desc      "PL/Proxy"))
  "A list of Extension. Each extension is a list of its full name, uri and
   description.")

