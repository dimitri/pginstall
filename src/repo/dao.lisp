;;;
;;; The Database Access Objects, as proposed by our PostgreSQL client
;;; library, allows to hydrate CLOS objects from SQL queries.
;;;
;;;   http://marijnhaverbeke.nl/postmodern/postmodern.html#daos
;;;

(in-package #:pginstall.repo)


;;;
;;; We are building extensions.
;;;
(defclass extension ()
    ((id          :col-type integer  :reader ext-id)
     (shortname   :col-type string :accessor short-name)
     (fullname    :col-type string :accessor full-name :initarg :full-name)
     (uri         :col-type string :accessor uri       :initarg :uri)
     (description :col-type string :accessor desc      :initarg :desc))
  (:documentation
   "a PostgreSQL extension. The full name must be a unique partial URI,
    where the scheme name, query and fragment parts are omitted.")
  (:metaclass dao-class)
  (:keys id))

(defun extension-short-name (extension-full-name)
  "Return the extension short-name when given a short name."
  (pathname-name extension-full-name))

(defmethod initialize-instance :after ((extension extension) &key)
  "Automatically compute the short name of the extension."
  (setf (slot-value extension 'shortname)
        (extension-short-name (slot-value extension 'fullname))))

(defmethod print-object ((extension extension) stream)
  (print-unreadable-object (extension stream :type t :identity t)
    (let ((id (when (slot-boundp extension 'id)
                (ext-id 'id))))
      (with-slots (shortname fullname) extension
        (format stream "~d ~a [~a]" id shortname fullname)))))


(defclass platform ()
    ((id          :col-type integer  :reader platform-id)
     (os-name     :col-type string :col-name os_name    :reader os-name)
     (os-version  :col-type string :col-name os_version :reader os-version)
     (arch        :col-type string :reader arch))
  (:documentation
   "the Platform an animal is running.")
  (:metaclass dao-class)
  (:keys id))

(defmethod initialize-instance :after ((platform platform) &key)
  "Automatically compute os-name, os-version and arch."
  (destructuring-bind (os-name os-version) (os-name-and-version)
    (setf (slot-value platform 'os-name)    os-name
          (slot-value platform 'os-version) os-version
          (slot-value platform 'arch)       (uname "-m"))))

(defmethod print-object ((platform platform) stream)
  (print-unreadable-object (platform stream :type t :identity t)
    (let ((id (when (slot-boundp platform 'id)
                (platform-id platform))))
     (with-slots (os-name os-version arch) platform
       (format stream "~a ~a ~a [~a]" id os-name os-version arch)))))


;;;
;;; The animal
;;;
(defclass animal ()
    ((id       :col-type integer   :reader animal-id)
     (name     :col-type string  :accessor name     :initarg :name)
     (uri      :col-type string  :accessor uri      :initarg :uri )
     (platform :col-type integer :reader platform))
  (:documentation "a Build Animal, its job is to build extensions.")
  (:metaclass dao-class)
  (:keys id))

(defmethod initialize-instance :after ((animal animal) &key)
  "Automatically fetch platform."
  (unless (and (slot-boundp animal 'platform)
               (slot-value animal 'platform))
    (let* ((p    (make-instance 'platform))
           ;; we don't use get-dao because we declared that the platform key
           ;; is id and we search by name, version, arch here.
           (pid  (or (car (select-dao 'platform (:and
                                                 (:= 'os_name (os-name p))
                                                 (:= 'os_version (os-version p))
                                                 (:= 'arch (arch p)))))
                     ;; race conditions?
                     (make-dao 'platform))))
      (setf (slot-value animal 'platform) (platform-id pid)))))

(defmethod print-object ((animal animal) stream)
  (print-unreadable-object (animal stream :type t :identity t)
    (let ((id (when (slot-boundp animal 'id)
                (animal-id animal))))
      (with-slots (name uri) animal
        (format stream "~a ~a [~a]" id name uri)))))


;;;
;;; The pgconfig entries
;;;
(defclass pgconfig ()
    ((id          :col-type integer     :reader pgconfig-id)
     (animal-name :accessor animal-name :initarg :animal-name)
     (animal      :col-type integer     :reader animal)
     (pg-config   :col-type string      :col-name pg_config
                  :accessor pg-config   :initarg :pg-config)
     (version     :col-type string      :reader pg-version)
     (configure   :col-type string      :reader pg-configure)
     (cc          :col-type string      :reader pg-cc)
     (cflags      :col-type string      :reader pg-cflags))
  (:documentation "a PostgreSQL Server Dev Environment")
  (:metaclass dao-class)
  (:keys id))

(defmethod initialize-pgconfig ((pgconfig pgconfig))
  "Use the run-pg-config function to grab values from the pg_config command
   and fill-in the PGCONFIG object."
  (let ((config (run-pg-config (slot-value pgconfig 'pg-config))))
    (loop for (key . value) in config
       for keysym = (find-symbol key #. *package*)
       when keysym
       do (setf (slot-value pgconfig keysym) value))))

(defmethod initialize-instance :after ((pgconfig pgconfig) &key)
  "Automatically fetch platform."
  (when (and (not (slot-boundp pgconfig 'animal))
             (slot-boundp pgconfig 'animal-name))
    (let* ((animal-name (slot-value pgconfig 'animal-name))
           (animal      (car (select-dao 'animal (:= 'name animal-name)))))
      (setf (slot-value pgconfig 'animal) (animal-id animal))))

  (when (slot-boundp pgconfig 'pg-config)
    (unless (slot-boundp pgconfig 'configure)
      (initialize-pgconfig pgconfig)))

  (unless (slot-boundp pgconfig 'animal-name)
    (setf (slot-value pgconfig 'animal-name) *animal-name*)))

(defmethod print-object ((pgconfig pgconfig) stream)
  (print-unreadable-object (pgconfig stream :type t :identity t)
    (let ((id (when (slot-boundp pgconfig 'id)
                (pgconfig-id pgconfig))))
      (with-slots (animal-name pg-config version) pgconfig
        (format stream "~a ~a: ~a [~a]" id animal-name pg-config version)))))
