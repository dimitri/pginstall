;;;
;;; The main entry point, manages the command line.
;;;
(defpackage #:pginstall
  (:use #:cl
        #:pginstall.animal
        #:pginstall.common
        #:pginstall.config
        #:pginstall.repo))
