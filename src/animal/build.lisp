;;;
;;; The build animal is responsible for building extensions against a set of
;;; PostgreSQL installations.
;;;
;;; This file contains client-side implementation of the animal itself, that
;;; is the local building of extensions and preparing of the binary archive.
;;;

(in-package #:pginstall.animal)

(defun extension-build-root (extension &key (clean t))
  "Returns the directory where to `git clone` and build an extension."
  (let ((build-root
         (merge-pathnames
          (make-pathname :directory `(:relative ,(short-name extension)))
          *build-root*)))

    (when (and clean (uiop:probe-file* build-root))
      (uiop:delete-directory-tree build-root
                                  :validate t
                                  :if-does-not-exist :ignore))

    (directory-namestring (ensure-directories-exist (namestring build-root)))))

(defun extension-install-prefix (extension vers)
  "Returns the directory where to `make install` an EXTENSION for given
   PostgreSQL version VERS.."
  (multiple-value-bind (flag path-list last-component file-namestring-p)
      (uiop:split-unix-namestring-directory-components
       (uiop:native-namestring *build-root*))
    (declare (ignore last-component file-namestring-p))

    (let ((install-prefix
           (make-pathname :directory `(,flag ,@path-list
                                             "install"
                                             ,(short-name extension)
                                             ,vers))))
      (directory-namestring (ensure-directories-exist install-prefix)))))

(defun make-install (extension pgconfig build-root prefix)
  "Build the extension in the *build-root* directory."
  (check-type extension extension)
  (check-type pgconfig pgconfig)

  ;; we copy the environment so that we can scribble on the copy
  (let* ((environment `(("CC" . ,(pg-cc pgconfig))))
         (gmake       `(,*gmake*
                        ,(format nil "PGCONFIG=~a" (pg-config pgconfig))
                        ,(format nil "DESTDIR=~a" prefix)
                        "install")))
    (run-command gmake :cwd build-root :environment environment)))

(defun git-clone (extension directory)
  "Fetch the current version of the code for given extension."
  (check-type extension extension)
  (let ((git-clone `(,*git* "clone" "--depth" "1" ,(uri extension) ,directory)))
    (run-command git-clone :cwd directory)))

(defun git-clean-fdx (directory)
  "Cleanup the DIRECTORY before building again an extension."
  (let ((git-clean `(,*git* "clean" "-fdx")))
    (run-command git-clean :cwd directory)))


;;;
;;; The main build code.
;;;
;;; On the animal, we don't have access to the main repository database, so
;;; the arguments we get here are all we are going to be able to work with.
;;;
(defun build-extension-with-pgconfig (extension extension-root pgconfig-path
                                      &key
                                        (log (make-array '(0)
                                                         :element-type 'base-char
                                                         :fill-pointer 0
                                                         :adjustable t))
                                        (pg-config-keys
                                            '(:CONFIGURE :CC :VERSION :CFLAGS
                                              :DOCDIR :PKGLIBDIR :SHAREDIR)))
  "Build extension in given EXTENSION-ROOT path with PGCONFIG-PATH."
  (format t "~10tbuilding with ~s~%" pgconfig-path)
  (let ((archive-filename))
   (with-output-to-string (*log-stream* log)
     (destructuring-bind (&key version
                               configure cc cflags
                               docdir sharedir pkglibdir)
         ;; and run-pg-config is returning an ALIST, we want a PLIST here.
         (alexandria:alist-plist
          (loop :for (key . value) :in (run-pg-config pgconfig-path pg-config-keys)
             :collect (cons (intern key (find-package "KEYWORD")) value)))

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
         (git-clean-fdx extension-root)
         (make-install extension pgconfig extension-root prefix)

         ;; then pack an archive of what we just built
         (flet ((expand-pg-config-dir (path)
                  (merge-pathnames (make-pathname :directory `(:relative ,path))
                                   (make-pathname :directory prefix))))
           (setf archive-filename
                 (pack-archive extension vers
                               :docdir    (expand-pg-config-dir docdir)
                               :pkglibdir (expand-pg-config-dir pkglibdir)
                               :sharedir  (expand-pg-config-dir sharedir)))))))
   (cons archive-filename log)))

(defun build-extension (extension-full-name extension-uri
                        &key
                          (pgconfig-path-list (find-pgconfig-paths))
                          ((animal-name *animal-name*) *animal-name*))
  "Build extension EXTENSION-FULL-NAME, on the build animal."
  (format t "Building extension ~a~%" extension-full-name)
  (format t "~10tgit clone ~s~%" extension-uri)
  (let* ((extension      (make-instance 'extension
                                        :full-name extension-full-name
                                        :uri extension-uri))
         (root           (extension-build-root extension))
         (log            (make-array '(0)
                                     :element-type 'base-char
                                     :fill-pointer 0
                                     :adjustable t)))

    ;; git clone the extension sources
    (with-output-to-string (*log-stream* log)
      (git-clone extension root))

    (loop :for pgconfig-path :in pgconfig-path-list
       :collect (build-extension-with-pgconfig extension
                                               root
                                               pgconfig-path
                                               :log log))))
