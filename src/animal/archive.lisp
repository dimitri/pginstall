;;;
;;; The build animal is responsible for building extensions against a set of
;;; PostgreSQL installations.
;;;
;;; This file contains the code to prepare an extension archive.
;;;

(in-package #:pginstall.animal)

(defun $libdir-to-module-pathname (source target)
  "Parse given SCRIPT and replace strings \"AS '$libdir/...'\" to \"AS
   'MODULE_PATHNAME/...'\"."
  (let ((content (iolib.base:read-file-into-string source)))
    (with-open-file (newscript target
                               :direction :output
                               :if-exists :supersede
                               :if-does-not-exist :error
                               :external-format :utf-8)
      (write-string (cl-ppcre:regex-replace-all "([$]libdir/)"
                                                content
                                                "MODULE_PATHNAME/")
                    newscript)))
  target)

;;;
;;; Yes, the name has been picked to "work" with default emacs indenting.
;;;
(defmacro with-filter-list-directory (dir &body filter)
  "Walk DIR and evaluates BODY, then return FILES."
  (let ((push-files (gensym)))
    `(let (files)
       (let ((,push-files
              (lambda (name kind parent depth)
                (declare (ignorable name kind parent depth))
                (when ,@filter
                  (let ((filename
                         (merge-file-paths name (merge-file-paths parent ,dir))))
                    (push filename files))))))
         (walk-directory ,dir ,push-files))
       files)))

(defun copy-files-into (directory files)
  "Copy FILES into DIRECTORY and return the list of files copied."
  (loop for file-path in files
     for target = (file-path-namestring
                   (merge-file-paths
                    (file-path-namestring (file-path-file file-path))
                    directory))
     for source = (file-path-namestring file-path)
     do (iolib.base:copy-file source target)
     collect target))

(defun prepare-archive-files (extension archive-dir docdir pkglibdir sharedir)
  "Prepare files installed with PGXS command `make install` to take part of
   our EXTENSION archive, and copy them into ARCHIVE-DIR.

   Returns the FILELIST of pathnames that made it to the archive directory."
  (declare (type extension extension)
           (type pathname docdir pkglibdir sharedir))
  (let* ((control-files  (with-filter-list-directory sharedir
                           (string= (file-path-file-type name) "control")))

         (module-files   (with-filter-list-directory pkglibdir
                           (string= (file-path-file-type name) "so")))

         (script-files   (with-filter-list-directory sharedir
                           (string= (file-path-file-type name) "sql")))

         (doc-files      (with-filter-list-directory docdir
                           (eq kind :regular-file)))

         (archive-libdir (merge-pathnames
                          (make-pathname :directory `(:relative "lib"))
                          archive-dir))

         (archive-extdir (merge-pathnames
                          (make-pathname :directory
                                         `(:relative ,(short-name extension)))
                          archive-dir)))

    ;; prepare the archive directories layout
    (loop for dir in (list archive-dir archive-libdir archive-extdir)
       do (ensure-directories-exist dir))

    ;; put file at their right place for the archive
    (let ((ctrl      (copy-files-into archive-dir control-files))
          (libs      (copy-files-into archive-libdir module-files))
          (scripts   (copy-files-into archive-extdir script-files))
          (docs      (copy-files-into archive-extdir doc-files))
          ;; normalize the filename
          (manifest
           (file-path-namestring
            (parse-file-path
             (namestring
              (make-pathname :directory (directory-namestring archive-dir)
                             :name "MANIFEST"
                             :type "TXT"))))))
      ;; and prepare the MANIFEST.TXT file
      (iolib.base:with-output-to-file (stream manifest
                                              :if-exists :supersede
                                              :external-format :utf-8)
        (format stream "~&control: ~{~a~^~&~9T~}" ctrl)
        (format stream "~&module:  ~{~a~^~&~9T~}"  libs)
        (format stream "~&scripts: ~{~a~^~&~9T~}" scripts)
        (format stream "~&docs:    ~{~a~^~&~9T~}"    docs))

      ;; and return the filelist
      (mapcar (lambda (pathname)
                (enough-namestring pathname archive-dir))
              (append (list manifest) ctrl libs scripts docs)))))

(defun pack-archive (extension version &key docdir pkglibdir sharedir)
  "Pack an archive for given EXTENSION and PostgreSQL version."
  (check-type extension extension)
  (let* ((platform         (make-instance 'platform))
         (archive-name     (format nil "~a-~a-~a-~a"
                                   (short-name extension) version
                                   (os-name platform)
                                   (os-version platform)))
         (archive-filename (merge-pathnames (make-pathname :name archive-name
                                                           :type "tar")
                                            *build-root*))
         (archive-dir
          (merge-pathnames (make-pathname :directory `(:relative ,archive-name))
                           *build-root*))
         (filelist
          (prepare-archive-files extension archive-dir docdir pkglibdir sharedir)))

    ;; build the archive, it's all ready
    (let ((*default-pathname-defaults*
           (make-pathname :directory (directory-namestring archive-dir))))

      (archive:with-open-archive (archive archive-filename
                                          :direction :output
                                          :if-exists :supersede)
        (dolist (file filelist (archive:finalize-archive archive))
          (let ((entry (archive:create-entry-from-pathname archive file)))
            (archive:write-entry-to-archive archive entry)))))

    ;; return the archive filename
    archive-filename))

