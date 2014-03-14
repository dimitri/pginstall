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

(defun serve-web-page (content)
  "Serve a static page: header then footer."
  (setf (hunchentoot:content-type*) "text/html")
  (concatenate 'string
               (read-file-into-string *header*)
               content
               (read-file-into-string *footer*)))

;;;
;;; Main entry points for the web server.
;;;
(defun home ()
  "Server a static page for the home."
  (serve-web-page (read-file-into-string (merge-pathnames "home.html" *root*))))

(defun front-list-extensions ()
  "List all our extensions."
  (serve-web-page
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
                          '/pict/' || r.pict
                   from animal a
                        join platform p on a.platform = p.id
                        join registry r on a.name = r.name
               order by a.name"))))
    (serve-web-page
     (with-html-output-to-string (s)
       (htm
        (:div :class "col-sm-9 col-sm-offset-3 col-md-10 col-md-offset-2 main"
              (:h1 :class "page-header" "Build Farm Animals")
              (:div :class "table-responsive"
                    (:table :class "table table-stripped"
                            (:thead
                             (:tr (:th "")
                                  (:th "Name")
                                  (:th "OS Name")
                                  (:th "OS Version")
                                  (:th "Architecture")))
                            (:tbody
                             (loop
                                :for (name os version arch pict) :in animal-list
                                :do (htm
                                     (:tr
                                      (:td (:img :src (str pict)))
                                      (:th (str name))
                                      (:td (str os))
                                      (:td (str version))
                                      (:td (str arch))))))))))))))

