;;;
;;; The build animal is responsible for building extensions against a set of
;;; PostgreSQL installations.
;;;
;;; This file contains the code to prepare an extension archive.
;;;

(in-package #:pginstall.animal)

;;;
;;; First, some tooling
;;;
(defun parse-default-version (control-file)
  "Return the `default_version` property of the CONTROL-FILE."
  (let ((props
         (parse-properties-output (read-file-into-string control-file))))
    (string-trim "'" (cdr (assoc "version" props :test #'string=)))))

(defun $libdir-to-module-pathname (source target)
  "Parse given SCRIPT and replace strings \"AS '$libdir/...'\" to \"AS
   'MODULE_PATHNAME/...'\"."
  (let ((content (read-file-into-string source)))
    (with-open-file (newscript target
                               :direction :output
                               :if-exists :supersede
                               :if-does-not-exist :create
                               :external-format :utf-8)
      (write-string (cl-ppcre:regex-replace-all "AS '[^']+'"
                                                content
                                                "AS 'MODULE_PATHNAME'")
                    newscript)))
  target)

;;;
;;; Even lower-level tooling, files and directories.
;;;
(defun copy-files-into (directory files
                        &key
                          (base-dir directory)
                          rewrite-$libdir)
  "Copy FILES into DIRECTORY and return the list of files copied."
  (loop for file-path in files
     for target = (file-path-namestring
                   (merge-file-paths
                    (file-path-namestring (file-path-file file-path))
                    directory))
     for source = (file-path-namestring file-path)
     do (if rewrite-$libdir
            ($libdir-to-module-pathname source target)
            (iolib.base:copy-file source target))
     collect (enough-namestring target base-dir)))

(defun archive-extdir (extension archive-dir)
  "Return the ARCHIVE-DIR/{EXTENSION-SHORT-NAME}/ path where we want to
   store scripts, auxilliary control files, and docs."
  (merge-pathnames
   (make-pathname :directory `(:relative ,(short-name extension))) archive-dir))

(defun make-archive-dir (extension basename)
  "Ensure *archive-path*/BASENAME exists, removing a possibly existing old copy."
  (declare (type extension extension))
  (let ((archive-dir
         (merge-pathnames
          (make-pathname :directory `(:relative ,basename)) *archive-path*)))

    ;; first, cleanup
    (when (probe-file archive-dir)
      (delete-files archive-dir :recursive t))

    ;; now, prepare the target directories we need, and return the archive-dir
    (prog1
        (ensure-directories-exist archive-dir)
      (ensure-directories-exist (archive-extdir extension archive-dir)))))

;;;
;;; Of course we want a compressed tarball.
;;;
(defun gzip (filename)
  "Run the `gzip -9` command on given filename"
  (let ((target (format nil "~a.gz" filename)))
    (format t "gzip -9 ~a~%" filename)

    (when (probe-file target)
      (delete-file target))

    (multiple-value-bind (code stdout stderr)
        (run-program `("gzip" "-9" ,filename))
      (declare (ignore stdout))
      (unless (= 0 code)
        (error "Error during gzip -9: ~a~%" stderr)))

    ;; return the new filename
    target))


;;;
;;; The main functions are there.
;;;
;;; Yes, the name has been picked to "work" with default emacs indenting.
;;;
(defmacro with-filter-list-directory (dir &body filter)
  "Walk DIR and evaluates BODY, then return FILES."
  (let ((push-files (gensym)))
    `(when (directory-exists-p ,dir)
       (let (files)
         (let ((,push-files
                (lambda (name kind parent depth)
                  (declare (ignorable name kind parent depth))
                  (when ,@filter
                    (let ((filename
                           (merge-file-paths name (merge-file-paths parent ,dir))))
                      (push filename files))))))
           (walk-directory ,dir ,push-files))
         files))))

(defun prepare-archive-files (extension archive-dir docdir pkglibdir sharedir)
  "Prepare files installed with PGXS command `make install` to take part of
   our EXTENSION archive, and copy them into ARCHIVE-DIR.

   Returns the FILELIST of pathnames that made it to the archive directory."
  (declare (type extension extension)
           (type pathname docdir pkglibdir sharedir))
  (let* ((extname        (short-name extension))
         (control-files  (with-filter-list-directory sharedir
                           (and
                            (string= (file-path-file-name name) extname)
                            (string= (file-path-file-type name) "control"))))

         (module-files   (with-filter-list-directory pkglibdir
                           (string= (file-path-file-type name) "so")))

         (script-files   (with-filter-list-directory sharedir
                           (or
                            (string= (file-path-file-type name) "sql")
                            (and
                             (string/= (file-path-file-name name) extname)
                             (string= (file-path-file-type name) "control")))))

         (doc-files      (with-filter-list-directory docdir
                           (eq kind :regular-file)))

         (archive-extdir (archive-extdir extension archive-dir)))

    ;; put file at their right place for the archive
    (let ((ctrl      (copy-files-into archive-dir control-files))
          (libs      (copy-files-into archive-extdir module-files
                                      :base-dir archive-dir))
          (scripts   (copy-files-into archive-extdir script-files
                                      :base-dir archive-dir
                                      :rewrite-$libdir t))
          (docs      (copy-files-into archive-extdir doc-files
                                      :base-dir archive-dir)))

      ;; and return the filelist
      (append ctrl libs scripts docs))))

(defun pack-archive (extension version &key docdir pkglibdir sharedir)
  "Pack an archive for given EXTENSION and PostgreSQL version."
  (declare (type extension extension))
  (let* ((archive-basename (format nil "~a--~a" (short-name extension) version))
         (archive-dir      (make-archive-dir extension archive-basename))
         (filelist         (prepare-archive-files extension
                                                  archive-dir
                                                  docdir
                                                  pkglibdir
                                                  sharedir))
         (platform         (make-instance 'platform))
         (archive-name     (format nil "~a--~a--~a--~a--~a"
                                   (short-name extension)
                                   version
                                   (substitute #\_ #\Space (os-name platform))
                                   (os-version platform)
                                   (arch platform)))
         (archive-filename (merge-pathnames (make-pathname :name archive-name
                                                           :type "tar")
                                            *archive-path*)))

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
    (gzip (namestring archive-filename))))

