;;;
;;; The build animal is responsible for building extensions against a set of
;;; PostgreSQL installations.
;;;
(defpackage #:pginstall.repo
  (:use #:cl
        #:pginstall.common
        #:pginstall.config
        #:postmodern
        #:iolib.os
        #:iolib.pathnames)
  (:export #:extension                  ; the extension class
           #:ext-id
           #:short-name
           #:full-name
           #:uri
           #:desc
           #:extension-short-name

           #:platform                   ; the platform class
           #:platform-id
           #:os-name
           #:os-version
           #:arch

           #:animal                     ; the animal class
           #:animal-id
           #:name
           #:uri
           #:platform

           #:pgconfig                   ; the pgconfig class
           #:pconfig-id
           #:animal-name
           #:pg-config
           #:pg-version
           #:pg-configure
           #:pg-cc
           #:pg-cflags

           #:queue                      ; the queue class
           #:queue-id
           #:queue-ext-id

           #:running                    ; the running class
           #:running-id
           #:running-ext-id
           #:running-animal-id
           #:running-started

           #:build-log                  ; the build-log class
           #:build-log-id
           #:build-log-ext-id
           #:build-log-animal-id
           #:build-log-stamp
           #:build-log-result
           #:build-log-log

           #:build-queue                ;the build-queue class
           #:build-queue-id
           #:build-queue-ext-id
           #:build-queue-fullname
           #:build-queue-uri
           #:build-queue-desc

           #:archive                    ; the archive class
           #:archive-id
           #:archive-ext-id
           #:archive-platform-id
           #:archive-pgversion
           #:archive-filename

           ;; the Main API
           #:select-star

           ;; the Repository Setup API
           #:*default-extension-list*
           #:setup

           ;; the Animal API
           #:*animal-name-registry*
           #:pick-animal-name
           #:register-animal
           #:add-pg-config
           #:list-pg-configs

           ;; the Extension API
           #:queue-extension-build
           #:queue-get-work
           #:select-extensions-available-on-platform
           #:receive-archive
           #:archive-pathname
           ))
