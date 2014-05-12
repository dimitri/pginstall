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
  (format *log-stream* "rewrite-libdir ~s ~s~%"
          source
          (uiop:native-namestring target))
  (let ((content (uiop:read-file-string source)))
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
     for target = (make-pathname :directory (directory-namestring directory)
                                 :name (pathname-name file-path)
                                 :type (pathname-type file-path))
     for source = (uiop:native-namestring file-path)
     do (if rewrite-$libdir
            ($libdir-to-module-pathname source target)
            (progn
              (format *log-stream* "cp ~s ~s~%"
                      source
                      (uiop:native-namestring target))
              (uiop:copy-file source target)))
     collect (enough-namestring target base-dir)))

(defun archive-extdir (extension archive-dir)
  "Return the ARCHIVE-DIR/{EXTENSION-SHORT-NAME}/ path where we want to
   store scripts, auxilliary control files, and docs."
  (merge-pathnames
   (make-pathname :directory `(:relative ,(short-name extension))) archive-dir))

(defun make-archive-dir (extension basename)
  "Ensure *build-root*/BASENAME exists, removing a possibly existing old copy."
  (declare (type extension extension))
  (let ((archive-dir
         (merge-pathnames
          (make-pathname :directory `(:relative ,basename)) *build-root*)))

    ;; first, cleanup
    (when (probe-file archive-dir)
      (uiop:delete-directory-tree archive-dir :validate t))

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

    (when (probe-file target)
      (delete-file target))

    (run-command `("gzip" "-9" ,filename) :cwd (directory-namestring filename))

    ;; return the new filename
    target))


;;;
;;; The main functions are there.
;;;
;;; Yes, the name has been picked to "work" with default emacs indenting.
;;;
(defmacro with-filter-list-directory (dir &body filter)
  "Walk DIR and evaluates BODY, then return FILES."
  `(when (uiop:directory-exists-p ,dir)
     (let ((files))
       (flet ((collectp  (directory) (declare (ignore directory)) t)
              (recursep  (directory) (declare (ignore directory)) t)
              (collector (directory)
                (loop :for name :in (uiop:directory-files directory)
                   :when ,@ (progn filter)
                   :do (push name files))))
         (uiop:collect-sub*directories ,dir #'collectp #'recursep #'collector))
       files)))

(defun prepare-archive-files (extension archive-dir docdir pkglibdir sharedir)
  "Prepare files installed with PGXS command `make install` to take part of
   our EXTENSION archive, and copy them into ARCHIVE-DIR.

   Returns the FILELIST of pathnames that made it to the archive directory."
  (declare (type extension extension)
           (type pathname docdir pkglibdir sharedir))
  (let* ((extname        (short-name extension))
         (control-files  (with-filter-list-directory sharedir
                           (and
                            (string= (pathname-name name) extname)
                            (string= (pathname-type name) "control"))))

         (module-files   (with-filter-list-directory pkglibdir
                           (string= (pathname-type name) "so")))

         (script-files   (with-filter-list-directory sharedir
                           (or
                            (string= (pathname-type name) "sql")
                            (and
                             (string/= (pathname-name name) extname)
                             (string= (pathname-type name) "control")))))

         (doc-files      (with-filter-list-directory docdir
                           (not (uiop:directory-pathname-p name))))

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
                                            *build-root*)))

    ;; build the archive, it's all ready
    (let ((*default-pathname-defaults*
           (make-pathname :directory (directory-namestring archive-dir))))

      (format *log-stream* "tar cf ~s ~{~s~^ ~}~%" archive-filename filelist)
      (archive:with-open-archive (archive archive-filename
                                          :direction :output
                                          :if-exists :supersede)
        (dolist (file filelist (archive:finalize-archive archive))
          (let ((entry (archive:create-entry-from-pathname archive file)))
            (archive:write-entry-to-archive archive entry)))))

    ;; return the archive filename
    (gzip (uiop:native-namestring archive-filename))))

