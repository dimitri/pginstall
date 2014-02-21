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
          (make-pathname :directory *build-root*))))
    (directory-namestring (ensure-directories-exist build-root))))

(defun extension-install-prefix (extension)
  "Returns the directory where to `make install` an extension."
  (let ((install-prefix
         (merge-pathnames
          (make-pathname :directory `(:relative ,(short-name extension)))
          (merge-pathnames (make-pathname :directory '(:relative "install"))
                           (make-pathname :directory *build-root*)))))
    (directory-namestring (ensure-directories-exist install-prefix))))

(defun pack-archive (extension version
                     &key docdir libdir pkglibdir sharedir)
  "Pack an archive for given EXTENSION and PostgreSQL version."
  (declare (ignore version))
  (check-type extension extension)
  (let ((platform  (make-instance 'platform))
        (manifest  (compute-manifest docdir libdir pkglibdir sharedir)))
    (declare (ignore platform manifest))))

(defun compute-manifest (stream docdir libdir pkglibdir sharedir)
  "Prepare our archive manifest."
  (declare (ignore stream docdir libdir pkglibdir sharedir))
  ;; (let ((control-files (walk-directory sharedir) ))
  ;;  (format stream "control: ~{~a~^~&~9T~}" control-files))
  )

(defun libdir-to-module-pathname ())

(defun make-install (extension pgconfig-path)
  "Build the extension in the *build-root* directory."
  (check-type extension extension)
  (check-type pgconfig-path (or string pathname))
  (let* ((environment    (environment)) ; so that we can scribble on the copy
         (pg-config-keys '(:CONFIGURE :CC :VERSION :CFLAGS
                           :DOCDIR :LIBDIR :PKGLIBDIR :SHAREDIR))
         (config-values  (run-pg-config pgconfig-path pg-config-keys)))
    ;; we want the symbols in the destructuring-bind list here to be read in
    ;; the PGINSTALL.ANIMAL package, not the KEYWORD package.
    ;; http://www.lispworks.com/documentation/HyperSpec/Body/03_dad.htm
    (destructuring-bind (&key ((configure configure)) ((version version))
                              ((cc cc)) ((cflags cflags))
                              ((docdir docdir)) ((sharedir sharedir))
                              ((libdir libdir)) ((pkglibdir pkglibdir)))
        ;; and run-pg-config is returning an ALIST, we want a PLIST here.
        (alexandria:alist-plist config-values)
      (let ((pgconfig (make-instance 'pgconfig
                                     :pg-config pgconfig-path
                                     :version   version
                                     :configure configure
                                     :cc        cc
                                     :cflags    cflags))
            (prefix   (extension-install-prefix extension))
            (root     (extension-build-root extension)))

        (with-current-directory root
          (setf (environment-variable "CC") (pg-cc pgconfig))
          (run-program *gmake*
                       `(,(format nil "PGCONFIG=~s" pgconfig-path)
                          ,(format nil "DESTDIR=~s" prefix))
                       :environment environment))

        (flet ((expand-pg-config-dir (path)
                 (merge-pathnames (make-pathname :directory path) root)))
         (pack-archive extension version
                       :docdir    (expand-pg-config-dir docdir)
                       :libdir    (expand-pg-config-dir libdir)
                       :pkglibdir (expand-pg-config-dir pkglibdir)
                       :sharedir  (expand-pg-config-dir sharedir)))))))

(defun git-clone (extension)
  "Fetch the current version of the code for given extension."
  (check-type extension extension)
  (run-program *git* `("clone"
                       ,(uri extension)
                        ,(extension-build-root extension))))

(defun build-extension (name)
  "Build extension matching NAME, which can be either a full name or a short
   name."
  ;; TODO: implement with-psql-transaction and co in config module
  (with-psql-connection (*dburi*)
    (let* ((slot      'shortname)    ; TODO: compute 'shortname or 'fullname
           (extension (select-dao 'extension (:= slot name))))
      (uiop:run-program *gmake*))))
