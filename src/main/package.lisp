;;;
;;; The main entry point, manages the command line.
;;;
(defpackage #:pginstall
  (:use #:cl
        #:pginstall.common
        #:pginstall.config
        #:pginstall.repo))
