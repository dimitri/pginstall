;;;
;;; The Buildfarm Animal gets is orders from the Repository Server
;;;

(in-package #:pginstall.animal)

(defun parse-animal (json)
  "Given a JSON string representing an archive DAO object, returns a proper
   CLOS object."
  (let* ((hash       (yason:parse json))
         (id         (gethash "ID" hash))
         (name       (gethash "NAME" hash))
         (platform   (gethash "PLATFORM" hash)))
    (make-instance 'animal :id id :name name :platform platform)))

(defun parse-archive (json)
  "Given a JSON string representing an archive DAO object, returns a proper
   CLOS object."
  (let* ((hash       (yason:parse json))
         (id         (gethash "ID" hash))
         (ext-id     (gethash "EXTENSION" hash))
         (platform   (gethash "PLATFORM" hash))
         (pgversion  (gethash "PGVERSION" hash))
         (log        (gethash "LOG" hash))
         (filename   (gethash "ARCHIVE" hash)))
    (make-instance 'archive
                   :id id
                   :extention ext-id
                   :platform platform
                   :pgversion pgversion
                   :log log
                   :filename filename)))
