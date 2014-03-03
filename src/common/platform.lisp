;;;
;;; Get OS specific information about the platform.
;;;
(in-package #:pginstall.common)

(defun os-name-and-version ()
  "Return os-name and os-version where we're currently running, as a list."
  (let ((system (uname "-s")))
   (cond ((string= system "Darwin")   (macosx-name-and-version))
         ((string= system "Linux")    (lsb-name-and-version))
         (t                           (list system (uname "-r"))))))

;;
;; That's pretty universal on Unix Systems and allows to dispatch to
;; specific code.
;;
(defun uname (option)
  "Gets the output of `uname -option`."
  (multiple-value-bind (code stdout stderr)
      (run-program `("uname" ,option))
    (declare (ignore stderr))
    (when (= 0 code)
      (with-input-from-string (s stdout)
        (read-line s)))))


;;
;; Under Linux distributions, you get to install a package that gives you
;; the command `lsb_release`, that is then portable enough.
;;
;;   debian:  apt-get install lsb_release
;;   centos:  yum install lsb
;;
(defun lsb-release (&optional option)
  "Gets the output of `lsb-release -option`, default to -a."
  (multiple-value-bind (code stdout stderr)
      (run-program `("lsb-release" ,option))
    (declare (ignore stderr))
    (when (= 0 code)
      stdout)))

(defun lsb-name-and-version ()
  "Return a list of os-name and os-version, for Linux systems."
  (let ((lsb (parse-properties-output (lsb-release "-a"))))
    (list (cdr (assoc "Distributor ID" lsb :test #'string=))
          (cdr (assoc "Release" lsb :test #'string=)))))


;;
;; Mac OS X
;;
(defun sw-vers (&optional option)
  "Gets the output of `sw_vers -option`."
  (multiple-value-bind (code stdout stderr)
      (run-program (if option `("sw_vers" ,option) "sw_vers"))
    (declare (ignore stderr))
    (when (= 0 code)
      (if option
          ;; read a single line of output
          (with-input-from-string (s stdout)
            (read-line s))

          ;; no option, return the whole thing
          stdout))))

(defun macosx-name-and-version ()
  "Return a list of os-name and os-version."
  (let ((sw-vers  (parse-properties-output (sw-vers))))
    (list (cdr (assoc "ProductName" sw-vers :test #'string=))
          (cdr (assoc "ProductVersion" sw-vers :test #'string=)))))


;;
;; Parse key value strings where we have the following format:
;;
;;   KeyName:    value
;;   OtherKey:   Value with spaces
;;
(defun parse-properties-output (stdout &optional comment-prefixes)
  "Parse properties result from commands such as `sw_vers` or `lsb_release`."
  (with-input-from-string (s stdout)
    (loop :for line := (read-line s nil nil)
       :while line
       :when (or (null comment-prefixes)
                 (and comment-prefixes
                      (< 1 (length line))
                      (member (char line 0) comment-prefixes :test #'char=)))
       :collect (cl-ppcre:register-groups-bind (key value)
                    ("([A-Za-z]*)\\s*[:=]\\s+(.*)" line)
                  (cons key value)))))
