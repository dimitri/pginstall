;;;
;;; The Buildfarm Animal gets is orders from the Repository Server
;;;

(in-package #:pginstall.animal)

;;;
;;; Facilities
;;;
(defun get-animal-uri (&optional (animal-name *animal-name*))
  "Return the URI where to browse for the animal specs on the server."
  (when *repo-server*
    (puri:merge-uris
     (format nil "/animal/~a" (drakma:url-encode animal-name :utf-8))
     (puri:parse-uri *repo-server*))))


;;;
;;; Client side implementation of the API.
;;;
(defun discover-animal-setup-and-register-on-server ()
  "Do the whole animal naming and registering dance with the Build Farm Server:

     - Ask the server for a name,
     - Find the pgconfigs path on this system,
     - Register against the server with our name and pgconfigs,
     - Save the local animal configuration file."
  ;; first change the *animal-name* globally
  (let* ((platform (make-instance 'platform))
         (animal   (parse-animal
                    (query-repo-server 'pick 'my 'name
                                       (os-name platform)
                                       (os-version platform)
                                       (arch platform)))))
    (setf *animal-name* (name animal))

    (loop :for pgconfig-path :in (find-pgconfig-paths)
       :do (add-pgconfig-on-server pgconfig-path))

    ;; serialize current configuration to ini file.
    (save-config)))

(defun register-animal-on-server ()
  "Register *ANIMAL-NAME* on *REPO-SERVER* for current platform."
  (let ((platform (make-instance 'platform)))
    (query-repo-server 'register 'animal
                       *animal-name*
                       (os-name platform)
                       (os-version platform)
                       (arch platform))))

(defun list-pgconfigs-on-server ()
  "Query for known pgconfigs on the server."
  (query-repo-server 'list 'pgconfig *animal-name*))

(defun add-pgconfig-on-server (pgconfig-path)
  "Send a :POST query to the server with the details of the pgconfig entry
   as parameters."
  (let ((pgconfig (make-instance 'pgconfig :pg-config pgconfig-path)))
    (post-repo-server 'add 'pgconfig *animal-name*
                      :pg-config (pg-config pgconfig)
                      :version   (pg-version pgconfig)
                      :configure (pg-configure pgconfig)
                      :cc        (pg-cc pgconfig)
                      :cflags    (pg-cflags pgconfig))))


;;;
;;; Building Extensions
;;;
(defun get-extension-to-build ()
  "Get an extension to build from the *REPO-SERVER*, or nil."
  (yason:parse (query-repo-server 'get 'work 'for *animal-name*)))

(defun upload-archive (extension-full-name archive-filename buildlog)
  "Upload an archive file."
  (let* ((filename (nth 2
                        (multiple-value-list
                         (uiop:split-unix-namestring-directory-components
                          (uiop:native-namestring
                           (pathname archive-filename))))))
         (archive                       ; the multi-part POST data
          (list (pathname archive-filename)
                :content-type "application/octet-stream"
                :filename filename)))

    (cl-ppcre:register-groups-bind (pgversion os-name os-version arch)
        ("^.*--(.*)--(.*)--(.*)--(.*).tar.gz" filename)
      (multiple-value-bind (body status-code headers uri stream must-close reason)
          (drakma:http-request (build-api-uri 'upload 'archive)
                               :method :post
                               :form-data t
                               :content-length t
                               :parameters `(("archive"    . ,archive)
                                             ("buildlog"   . ,buildlog)
                                             ("extension"  . ,extension-full-name)
                                             ("pgversion"  . ,pgversion)
                                             ("animal"     . ,*animal-name*)
                                             ("os-name"    . ,(substitute #\Space
                                                                          #\_
                                                                          os-name))
                                             ("os-version" . ,os-version)
                                             ("arch"       . ,arch)))
        (declare (ignore headers stream must-close))
        (if (= status-code 200)
            body
            (error 'server-error
                   :uri uri :status status-code :reason reason :body body))))))

(defun build-extension-for-server ()
  "Connect to the *REPO-SERVER* and ask for any extension to build for
   registered *ANIMAL-NAME*."
  (let ((extension-hash (get-extension-to-build)))
    (when extension-hash
      (let* ((extension-full-name (gethash "FULLNAME" extension-hash))
             (extension-uri       (gethash "URI" extension-hash))
             (archives
              (build-extension extension-full-name extension-uri)))
        (loop :for (archive-filename . log) :in archives
           :for archive := (parse-archive
                            (upload-archive extension-full-name
                                            archive-filename
                                            log))
           :collect archive)))))
