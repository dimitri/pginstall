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
                (ext-id extension))))
      (with-slots (shortname fullname) extension
        (format stream "~d ~a [~a]" id shortname fullname)))))


(defclass platform ()
    ((id          :col-type integer  :reader platform-id)
     (os-name     :col-type string :col-name os_name
                  :initarg :os-name :reader os-name)
     (os-version  :col-type string :col-name os_version
                  :initarg :os-version :reader os-version)
     (arch        :col-type string :reader arch
                  :initarg :arch :reader arch))
  (:documentation
   "the Platform an animal is running.")
  (:metaclass dao-class)
  (:keys id))

(defmethod initialize-instance :after ((platform platform) &key)
  "Automatically compute os-name, os-version and arch."
  (unless (slot-boundp platform 'os-name)
    (destructuring-bind (os-name os-version) (os-name-and-version)
      (setf (slot-value platform 'os-name)    os-name
            (slot-value platform 'os-version) os-version
            (slot-value platform 'arch)       (uname "-m")))))

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
    ((id       :col-type integer :reader animal-id  :initarg :id)
     (name     :col-type string  :accessor name     :initarg :name)
     (platform :col-type integer :reader platform   :initarg :platform))
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
      (with-slots (name) animal
        (format stream "~a ~a" id name)))))


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


;;;
;;; Getting the build done
;;;
(defclass queue ()
    ((id        :col-type integer :reader queue-id)
     (extension :col-type integer :accessor queue-ext-id :initarg :extension))
  (:documentation "a Build Queue Job Entry.")
  (:metaclass dao-class)
  (:keys id))

(defmethod print-object ((queue queue) stream)
  (print-unreadable-object (queue stream :type t :identity t)
    (let ((id (when (slot-boundp queue 'id) (queue-id queue))))
      (with-slots (extension) queue
        (format stream "~a ~a" id extension)))))


(defclass running ()
    ((id        :col-type integer :reader running-id)
     (queue     :col-type integer :accessor running-queue-id :initarg :extension)
     (animal    :col-type integer :accessor running-animal-id
                :initarg :animal)
     (started   :col-type local-time :accessor running-started
                :initarg :started)
     (done      :col-type local-time :accessor running-done
                :initarg :done))
  (:documentation "a Build Job Instance while it's running.")
  (:metaclass dao-class)
  (:keys id))

(defmethod print-object ((running running) stream)
  (print-unreadable-object (running stream :type t :identity t)
    (let ((id (when (slot-boundp running 'id) (running-id running))))
      (with-slots (extension animal started) running
        (format stream "~a ~a ~a <~a>" id extension animal started)))))

(defclass build-log ()
    ((id          :col-type integer :reader build-log-id)
     (extension   :col-type integer :accessor build-log-ext-id
                  :initarg :extension)
     (animal      :col-type integer :accessor build-log-animal-id
                  :initarg :animal)
     (build-stamp :col-type local-time      :col-name buildstamp
                  :accessor build-log-stamp :initarg :build-log-stamp)
     (log         :col-type text    :accessor build-log-log :initarg :log))
  (:documentation "a Build Job Log Entry.")
  (:metaclass dao-class)
  (:table-name buildlog)
  (:keys id))

(defmethod print-object ((buildlog build-log) stream)
  (print-unreadable-object (buildlog stream :type t :identity t)
    (let ((id (when (slot-boundp buildlog 'id) (build-log-id buildlog))))
      (with-slots (extension animal result when) buildlog
        (format stream "~a ~a ~a [~a] <~a>" id extension animal result when)))))


;;;
;;; Build Queue higher level objects, with JOIN results
;;;
(defclass build-queue ()
    ((id          :col-type integer :reader build-queue-id)
     (ext-id      :col-type integer :reader build-queue-ext-id)
     (fullname    :col-type string  :reader build-queue-ext-full-name)
     (uri         :col-type string  :reader build-queue-ext-uri)
     (description :col-type string  :reader build-queue-ext-desc))
  (:documentation "a Build Queue Job Entry.")
  (:metaclass dao-class)
  (:keys id))

(defmethod print-object ((build-queue build-queue) stream)
  (print-unreadable-object (build-queue stream :type t :identity t)
    (let ((id (when (slot-boundp build-queue 'id)
                (build-queue-id build-queue))))
      (with-slots (ext-id fullname) build-queue
        (format stream "~a ~a ~a" id ext-id fullname)))))


;;;
;;; The build animals upload "archives" of extensions for a specific platform
;;;
(defclass archive ()
    ((id        :col-type integer :reader archive-id :initarg :id)
     (extension :col-type integer :accessor archive-ext-id :initarg :extension)
     (platform  :col-type integer :accessor archive-platform-id
                :initarg :platform)
     (pgversion :col-type string  :accessor archive-pgversion
                :initarg :pgversion)
     (log       :col-type integer :accessor archive-log :initarg :log)
     (archive   :col-type string :accessor archive-filename
                :initarg :filename))
  (:documentation "The result of Building an Extension is an Archive.")
  (:metaclass dao-class)
  (:keys id))

(defmethod print-object ((archive archive) stream)
  (print-unreadable-object (archive stream :type t :identity t)
    (let ((id (when (slot-boundp archive 'id) (archive-id archive))))
      (with-slots (archive) archive
        (format stream "~a <~a>" id archive)))))
