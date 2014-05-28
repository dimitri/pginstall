;;;
;;; Implement S3 upload of archive files, to be able to serve them directly
;;; from an Amazon storage backed service (CloudFront or something else).
;;;

(in-package #:pginstall.server)

(defun upload-archive-to-s3 (archive-filename)
  "Upload given ARCHIVE-FILENAME to the S3 *s3-bucket*"
  (let ((archive-full-name (merge-pathnames archive-filename *archive-path*))
        (zs3:*credentials* (list *s3-access-key* *s3-secret-key*)))
    (when (and *s3-bucket* (zs3:bucket-exists-p *s3-bucket*))
      (zs3:put-file archive-full-name *s3-bucket* archive-filename))))
