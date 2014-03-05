;;;
;;; The repository server is publishing information via an HTTP API.
;;;
;;; Encode our objects in JSON for HTTP API publishing
;;;

(in-package #:pginstall.server)

(defmacro define-yason-encoder (class)
  "Define a new yason:encode method for CLASS."
  `(defmethod yason:encode ((instance ,class)
                            &optional (stream *standard-output*))
     "Encode an EXTENSION object into JSON."
     (yason:with-output (stream)
       (yason:with-object ()
         (loop :for slot :in (closer-mop:compute-slots ,class)
            :for slot-name := (closer-mop:slot-definition-name slot)
            :do (yason:encode-object-element
                 (string slot-name)
                 (slot-value instance slot-name)))))))

(define-yason-encoder #. (find-class 'extension))
(define-yason-encoder #. (find-class 'platform))
(define-yason-encoder #. (find-class 'animal))
(define-yason-encoder #. (find-class 'pgconfig))
