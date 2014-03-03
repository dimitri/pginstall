;;;
;;; The build animal is responsible for building extensions against a set of
;;; PostgreSQL installations.
;;;
;;; This file contains client-side implementation of the animal itself, that
;;; is the local building of extensions and preparing of the binary archive.
;;;

(in-package #:pginstall.animal)

(defun extension-build-root (extension)
  "Returns the directory where to `git clone` and build an extension."
  (let ((build-root
         (merge-pathnames
          (make-pathname :directory `(:relative ,(full-name extension)))
          *build-root*)))
    (directory-namestring (ensure-directories-exist (namestring build-root)))))

(defun extension-install-prefix (extension vers)
  "Returns the directory where to `make install` an EXTENSION for given
   PostgreSQL version VERS.."
  (let ((install-prefix
         (merge-pathnames
          (make-pathname :directory `(:relative ,(short-name extension) ,vers))
          (merge-pathnames (make-pathname :directory `(:relative "install"))
                           (make-pathname :directory *build-root*)))))
    (directory-namestring (ensure-directories-exist install-prefix))))

(defun make-install (extension pgconfig build-root prefix)
  "Build the extension in the *build-root* directory."
  (check-type extension extension)
  (check-type pgconfig pgconfig)

  (format t "cd ~a" build-root)
  (format t "make DESTDIR=~a install~%" prefix)

  ;; we copy the environment so that we can scribble on the copy
  (let* ((environment (environment)))
    (with-current-directory build-root
      (setf (environment-variable "CC") (pg-cc pgconfig))
      (multiple-value-bind (code stdout stderr)
          (run-program `(,*gmake*
                         ,(format nil "PGCONFIG=~a" (pg-config pgconfig))
                         ,(format nil "DESTDIR=~a" prefix)
                         "install")
                       :environment environment)
        (unless (= 0 code)
          (error "Error during make install: ~a~%" stderr))
        (format t "~a~%" stdout)
        stdout))))

(defun git-clone (extension directory)
  "Fetch the current version of the code for given extension."
  (check-type extension extension)
  (format t "git clone ~a ~a~%" (short-name extension) directory)
  (multiple-value-bind (code stdout stderr)
      (run-program `(,*git*
                     "clone"
                     ,(uri extension)
                     ,directory))
    (unless (= 0 code)
      (error "Error during git clone: ~a~%" stderr))
    (format t "~a~%" stdout)
    stdout))

(defun git-clean-fdx (directory)
  "Cleanup the DIRECTORY before building again an extension."
  (format t "git clean -fdx ~a~%" directory)
  (with-current-directory directory
    (multiple-value-bind (code stdout stderr)
        (run-program `(,*git* "clean" "-fdx"))
      (unless (= 0 code)
        (error "Error during git clean -fdx: ~a~%" stderr))
      (format t "~a~%" stdout)
      stdout)))


;;;
;;; The main build code.
;;;
;;; On the animal, we don't have access to the main repository database, so
;;; the arguments we get here are all we are going to be able to work with.
;;;
(defun build-extension (extension-full-name extension-uri
                        &key
                          (pgconfig-path-list (list-pg-config-in-path))
                          ((animal-name *animal-name*) *animal-name*))
  "Build extension EXTENSION-FULL-NAME, on the build animal."
  (let* ((extension      (make-instance 'extension
                                        :full-name extension-full-name
                                        :uri extension-uri))
         (root           (extension-build-root extension))
         (pg-config-keys '(:CONFIGURE :CC :VERSION :CFLAGS
                           :DOCDIR :PKGLIBDIR :SHAREDIR)))

    ;; git clone the extension sources
    (format t "rm -rf ~a~%" root)
    (delete-files root :recursive t)
    (git-clone extension root)

    (loop for pgconfig-path in pgconfig-path-list
       for config-values = (run-pg-config pgconfig-path pg-config-keys)
       collect
       ;; we want the symbols in the destructuring-bind list here to be read in
       ;; the PGINSTALL.ANIMAL package, not the KEYWORD package.
       ;; http://www.lispworks.com/documentation/HyperSpec/Body/03_dad.htm
         (destructuring-bind (&key version
                                   configure cc cflags
                                   docdir sharedir pkglibdir)
             ;; and run-pg-config is returning an ALIST, we want a PLIST here.
             (alexandria:alist-plist
              (loop for (key . value) in config-values
                 collect (cons (intern key (find-package "KEYWORD")) value)))

           (let* ((pgconfig (make-instance 'pgconfig
                                           :pg-config pgconfig-path
                                           :version   version
                                           :configure configure
                                           :cc        cc
                                           :cflags    cflags))
                  (vers     (cl-ppcre:register-groups-bind (short-version)
                                ("PostgreSQL (.*)" version)
                              short-version))
                  (prefix   (extension-install-prefix extension vers)))

             ;; now git close extension sources then build the extension
             ;; in our per-major-version build directory
             (git-clean-fdx root)
             (make-install extension pgconfig root prefix)

             ;; then pack an archive of what we just built
             (flet ((expand-pg-config-dir (path)
                      (merge-pathnames (make-pathname :directory `(:relative ,path))
                                       (make-pathname :directory prefix))))
               (pack-archive extension vers
                             :docdir    (expand-pg-config-dir docdir)
                             :pkglibdir (expand-pg-config-dir pkglibdir)
                             :sharedir  (expand-pg-config-dir sharedir))))))))
