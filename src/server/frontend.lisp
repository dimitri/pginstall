;;;
;;; The repository server is also offering a web frontend to manage the
;;; information found in the database, read logs, etc.
;;;
;;;

(in-package #:pginstall.server)

(defvar *root*
  (asdf:system-relative-pathname :pginstall "web/")
  "Where to find our static resources")

(defvar *header* (merge-pathnames "header.html" *root*))
(defvar *footer* (merge-pathnames "footer.html" *root*))

(defvar *dist*   (merge-pathnames
                  (make-pathname :directory '(:relative "bootstrap-3.1.1-dist"))
                  *root*))
(defvar *pict*   (merge-pathnames
                  (make-pathname :directory '(:relative "images")) *root*))

(defvar *docroot* (asdf:system-relative-pathname :pginstall "doc/"))

(defun serve-bootstrap-file ()
  "Anything under URL /dist/ gets routed here."
  (let* ((url-path      (hunchentoot:script-name*))
         (relative-path
          (format nil "~{~a~^/~}"
                  ;; the path is known to begin with /dist/
                  (cddr (split-sequence:split-sequence #\/ url-path)))))
    (hunchentoot:handle-static-file (merge-pathnames relative-path *dist*))))

(defun serve-pict-file ()
  "Anything under URL /pict/ gets routed here."
  (let* ((url-path      (hunchentoot:script-name*))
         (relative-path
          (format nil "~{~a~^/~}"
                  ;; the path is known to begin with /pict/
                  (cddr (split-sequence:split-sequence #\/ url-path)))))
    (hunchentoot:handle-static-file (merge-pathnames relative-path *pict*))))

;;;
;;; Tools to render specific set of pages
;;;
(defun compute-dashboard-menu (current-url-path)
  "List all files found in the *DOCROOT* directory and turns the listing
   into a proper bootstrap menu."
  (let ((entries '(("/"          . "Dashboard")
                   ("/extension" . "Extensions")
                   ("/animal"    . "Animals")
                   ("/builds"    . "Builds")
                   ("/archives"  . "Downloads"))))
    (with-html-output-to-string (s)
      (htm
       (:div :class "col-sm-3 col-md-2 sidebar"
             (:ul :class "nav nav-sidebar"
                  (loop :for (href . title) :in entries
                     :for active := (string= href current-url-path)
                     :do (if active
                             (htm
                              (:li :class "active"
                                   (:a :href (str href) (str title))))
                             (htm
                              (:li
                               (:a :href (str href) (str title))))))))))))

(defun serve-dashboard-page (content)
  "Serve a static page: header then footer."
  (format t "PLOP: ~s~%" (hunchentoot:script-name*))
  (concatenate 'string
               (read-file-into-string *header*)
               (compute-dashboard-menu (hunchentoot:script-name*))
               content
               (read-file-into-string *footer*)))

;;;
;;; Render documentation
;;;
(defun compute-help-menu (current-url-path)
  "List all files found in the *DOCROOT* directory and turns the listing
   into a proper bootstrap menu."
  (let ((files (iolib.os:list-directory *docroot*)))
    (with-html-output-to-string (s)
      (htm
       (:div :class "col-sm-3 col-md-2 sidebar"
             (:ul :class "nav nav-sidebar"
                  (loop :for file-path :in files
                     :for title := (file-path-file-name file-path)
                     :for href := (format nil "/help/~a" title)
                     :for active := (string= href current-url-path)
                     :when (string= "md" (file-path-file-type file-path))
                     :do (if active
                             (htm
                              (:li :class "active"
                                   (:a :href (str href) (str title))))
                             (htm
                              (:li
                               (:a :href (str href) (str title))))))))))))

(defun markdown-to-html (path)
  "Produce some HTML output from the Markdown document found at PATH."
  (with-html-output-to-string (s)
    (htm
     (:div :class "col-sm-9 col-sm-offset-3 col-md-10 col-md-offset-2 main"
           (str
            (multiple-value-bind (md-object html-string)
                (cl-markdown:markdown path :stream nil)
              (declare (ignore md-object))
              html-string))))))

(defun render-doc-page ()
  "Anything under URL /help/ gets routed here."
  (let* ((url-path      (hunchentoot:script-name*))
         (relative-path
          (format nil "~{~a~^/~}.md"
                  ;; the path is known to begin with /help/
                  (cddr (split-sequence:split-sequence #\/ url-path)))))
    (concatenate 'string
                 (read-file-into-string *header*)
                 (compute-help-menu url-path)
                 (markdown-to-html (merge-pathnames relative-path *docroot*))
                 (read-file-into-string *footer*))))

;;;
;;; Main entry points for the web server.
;;;
(defun home ()
  "Serve a static page for the home."
  (serve-dashboard-page
   (read-file-into-string (merge-pathnames "home.html" *root*))))

(defun config ()
  "Serve the configuration file as a textarea..."
  (let ((ini (read-file-into-string *config-filename*)))
    (serve-dashboard-page
     (with-html-output-to-string (s)
       (htm
        (:div :class "col-sm-9 col-sm-offset-3 col-md-10 col-md-offset-2 main"
              (:h1 :class "page-header" "Repository Server Configuration")
              (:form :role "config"
                     (:div :class "pull-right"
                           :style "margin-bottom: 1em;"
                           (:button :type "submit"
                                    :disabled "disabled"
                                    :class "btn btn-danger"
                                    "Save and Reload Server Configuration"))
                     (:div :class "form-group"
                           (:label :for "config" (str (namestring *config-filename*)))
                           (:textarea :id "config"
                                      :class "form-control"
                                      :rows 15
                                      (str ini))))))))))


;;;
;;; Some listings
;;;
(defun front-list-extensions ()
  "List all our extensions."
  (serve-dashboard-page
   (with-html-output-to-string (s)
     (htm
      (:div :class "col-sm-9 col-sm-offset-3 col-md-10 col-md-offset-2 main"
            (:h1 :class "page-header" "Extensions")
            (:div :class "table-responsive"
                  (:table :class "table table-stripped"
                          (:thead
                           (:tr (:th "#")
                                (:th "Short Name")
                                (:th "Full Name")
                                (:th "Description")))
                          (:tbody
                           (loop :for extension :in (select-star 'extension)
                              :do (htm
                                   (:tr
                                    (:td (str (ext-id extension)))
                                    (:td (str (short-name extension)))
                                    (:td (:a :href (uri extension)
                                             (str (full-name extension))))
                                    (:td (str (desc extension))))))))))))))

(defun front-list-animals ()
  "List all our animals."
  (let ((animal-list
         (with-pgsql-connection (*dburi*)
           (query "select a.name, p.os_name, p.os_version, p.arch,
                          case when substring(r.pict, 1, 7) = 'http://'
                               then pict
                               else '/pict/' || r.pict
                           end as pict,
                          count(p.os_name) over() as same_os,
                          count(p.arch)    over() as same_arch
                   from animal a
                        join platform p on a.platform = p.id
                        join registry r on a.name = r.name
               order by a.name"))))
    (serve-dashboard-page
     (with-html-output-to-string (s)
       (htm
        (:div :class "col-sm-9 col-sm-offset-3 col-md-10 col-md-offset-2 main"
              (:h1 :class "page-header" "Build Farm Animals")
              (:div :class "row"
               (loop :for (name os version arch pict os-nb arch-nb) :in animal-list
                  :do (htm
                       (:div :class "col-xs-6 col-md-3"
                             (:ul :class "list-group"
                                  (:li :class "list-group-item list-group-item-info"
                                       (:h4 :class "list-group-item-heading"
                                            (str name)))
                                  (:li :class "list-group-item"
                                       (:img :src (str pict)
                                             :alt (str name)))
                                  (:li :class "list-group-item"
                                       (str arch)
                                       (:span :class "badge" (str arch-nb)))
                                  (:li :class "list-group-item"
                                       (str os)
                                       (:span :class "badge" (str os-nb)))
                                  (:li :class "list-group-item"
                                       (str version)))))))))))))


