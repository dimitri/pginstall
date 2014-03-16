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

(defvar *dashboard-menu* '(("/"          . "Dashboard")
                           ("/extension" . "Extensions")
                           ("/animal"    . "Animals")
                           ("/build"     . "Builds")
                           ("/archive"   . "Archives"))
  "An alist of HREF and TITLE for the main dashboard menu.")

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
  (with-html-output-to-string (s)
    (htm
     (:div :class "col-sm-3 col-md-2 sidebar"
           (:ul :class "nav nav-sidebar"
                (loop :for (href . title) :in *dashboard-menu*
                   :for active := (string= href current-url-path)
                   :do (if active
                           (htm
                            (:li :class "active"
                                 (:a :href (str href) (str title))))
                           (htm
                            (:li
                             (:a :href (str href) (str title)))))))))))

(defun serve-dashboard-page (content)
  "Serve a static page: header then footer."
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
  "Display some default home page when the setup hasn't been made"
  "Hello, world?")

(defun dashboard ()
  "Serve a static page for the home."
  (let ((counts
         (with-pgsql-connection (*dburi*)
           (query "select (select count(*) from extension)::text || ' Extensions',
                          (select count(*) from animal)::text || ' Animals',
                          (select count(*) from platform)::text || ' Platforms',
                          (select count(*) from archive)::text || ' Archives'"
                  :row))))
    (serve-dashboard-page
     (with-html-output-to-string (s)
       (htm
        (:div :class "col-sm-9 col-sm-offset-3 col-md-10 col-md-offset-2 main"
              (:h1 :class "page-header" "Repository Server Queue")
              (loop :for counter :in counts
                 :for color :in '("progress-bar progress-bar-success"
                                  "progress-bar progress-bar-info"
                                  "progress-bar progress-bar-warning"
                                  "progress-bar progress-bar-danger")
                 :do (htm
                      (:div :class "progress"
                            (:div :class color
                                  :role "progressbar"
                                  :aria-valuenow "100"
                                  :aria-valuemin "0"
                                  :aria-valuemax "100"
                                  :style "width: 100%"
                                  (str counter)))))))))))

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
                       :for href := (format nil "/animal/~a" name)
                       :do (htm
                            (:div :class "col-xs-6 col-md-3"
                                  (:ul :class "list-group"
                                       (:li :class "list-group-item list-group-item-info"
                                            (:h4 :class "list-group-item-heading"
                                                 (:a :href (str href)
                                                     (str name))))
                                       (:li :class "list-group-item"
                                            (:div :style "width: 150px; height: 150px;"
                                                  (:a :href (str href)
                                                      :class "thumbnail"
                                                     (:img :src (str pict)
                                                           :alt (str name)))))
                                       (:li :class "list-group-item"
                                            (str arch)
                                            (:span :class "badge" (str arch-nb)))
                                       (:li :class "list-group-item"
                                            (str os)
                                            (:span :class "badge" (str os-nb)))
                                       (:li :class "list-group-item"
                                            (str version)))))))))))))

(defun front-display-animal (name)
  "Display a detailed view about a given animal (by NAME)."
  (destructuring-bind (animal pgconfig-list)
      (with-pgsql-connection (*dburi*)
        (let ((animal (query "select a.name, p.os_name, p.os_version, p.arch,
                                     case when substring(r.pict, 1, 7) = 'http://'
                                          then pict
                                          else '/pict/' || r.pict
                                      end as pict
                                from animal a
                                     join platform p on a.platform = p.id
                                     join registry r on a.name = r.name
                               where a.name = $1"
                             (hunchentoot:url-decode name)
                             :row))
              (pgconfig-list
               (query-dao 'pgconfig "select pgc.*
                                       from pgconfig pgc
                                            join animal a on a.id = pgc.animal
                                      where a.name = $1"
                          (hunchentoot:url-decode name))))
          (list animal pgconfig-list)))
    (serve-dashboard-page
     (with-html-output-to-string (s)
       (htm
        (:div :class "col-sm-9 col-sm-offset-3 col-md-10 col-md-offset-2 main"
              (:h1 :class "page-header" (format nil "Build Farm Animal ~a" name))
              (:div :class "row"
                    (destructuring-bind (name os version arch pict)
                        animal
                      (htm
                       (:div :class "col-xs-6 col-md-4"
                             (:ul :class "list-group"
                                  (:li :class "list-group-item list-group-item-info"
                                       (:h4 :class "list-group-item-heading"
                                            (str name)))
                                  (:li :class "list-group-item"
                                       (:div :style "width: 150px; height: 150px;"
                                             (:img :src (str pict)
                                                   :alt (str name))))
                                  (:li :class "list-group-item" (str arch))
                                  (:li :class "list-group-item" (str os))
                                  (:li :class "list-group-item" (str version))))
                       (:div :class "col-xs-6 col-md-8"
                             (loop :for pgcfg :in pgconfig-list
                                :do (htm
                                     (:h3 (str (pg-config pgcfg)))
                                     (:table :class "table table-bordered table-striped"
                                             (:colgroup (:col :class "col-xs-1")
                                                        (:col :class "col-xs-7"))
                                             (:thead
                                              (:tr
                                               (:th "Property")
                                               (:th "Value")))
                                             (htm
                                              (:tbody
                                               (:tr
                                                (:th "version")
                                                (:td (str (pg-version pgcfg))))
                                               (:tr
                                                (:th "configure")
                                                (:td (:code
                                                      (str (pg-configure pgcfg)))))
                                               (:tr
                                                (:th "cc")
                                                (:td (:code
                                                      (str (pg-cc pgcfg)))))
                                               (:tr
                                                (:th "cflags")
                                                (:td (:code
                                                      (str (pg-cflags pgcfg))))))))))))))))))))

(defun front-list-builds ()
  "List recent build logs."
  (let ((builds
         (with-pgsql-connection (*dburi*)
           (query "select bl.id, format('/build/%s', bl.id) as href,
                          to_char(bl.buildstamp, 'YYYY-MM-DD HH24:MI:SS'),
                          e.fullname, a.name, p.os_name, p.os_version, p.arch,
                          bl.log
                     from buildlog bl
                          join extension e on e.id = bl.extension
                          join animal a    on a.id = bl.animal
                          join platform p  on p.id = a.platform
                 order by bl.buildstamp desc nulls last
                    limit 15"))))
    (serve-dashboard-page
     (with-html-output-to-string (s)
       (htm
        (:div :class "col-sm-9 col-sm-offset-3 col-md-10 col-md-offset-2 main"
              (:h1 :class "page-header" "Build logs")
              (:div :class "table-responsive"
                    (:table :class "table table-stripped"
                            (:thead
                             (:tr (:th "Build Log Information")
                                  (:th "Log")))
                            (:tbody
                             (loop :for (id href stamp extension animal
                                            os version arch log) :in builds
                                :do (htm
                                     (:tr
                                      (:td
                                       (:ul :class "list-group"
                                            (:li :class "list-group-item list-group-item-info"
                                                 (:h4 :class "list-group-item-heading"
                                                      (str extension)))
                                            (:li :class "list-group-item"
                                                 (:dl :class "dl-horizontal"
                                                      :style "margin-left: -6em;"
                                                      (:dt "#")
                                                      (:dd (:a :href href
                                                               (:strong
                                                                (str id))))
                                                      (:dt "Build date")
                                                      (:dd (str stamp))
                                                      (:dt "Animal")
                                                      (:dd (:a :href
                                                               (str (format nil "/animal/~a" animal))
                                                               (str animal)))
                                                      (:dt "OS")
                                                      (:dd (str os))
                                                      (:dt "Version")
                                                      (:dd (str version))
                                                      (:dt "Arch")
                                                      (:dd (str arch))))))
                                      (:td (:pre :class "pre-scrollable"
                                                 :style "white-space: pre-wrap;"
                                                 (str log)))))))))))))))

(defun front-display-build (id)
  "Display a detailed view of the given build number."
  (let ((build
         (with-pgsql-connection (*dburi*)
           (query "select bl.id,
                          to_char(bl.buildstamp, 'YYYY-MM-DD HH24:MI:SS'),
                          e.fullname, a.name, p.os_name, p.os_version, p.arch,
                          bl.log
                     from buildlog bl
                          join extension e on e.id = bl.extension
                          join animal a    on a.id = bl.animal
                          join platform p  on p.id = a.platform
                    where bl.id = $1"
                  id
                  :row))))
    (destructuring-bind (id stamp extension animal os version arch log)
        build
      (serve-dashboard-page
       (with-html-output-to-string (s)
         (htm
          (:div :class "col-sm-9 col-sm-offset-3 col-md-10 col-md-offset-2 main"
                (:h1 :class "page-header" (format nil "Build log ~d" id))
                (:div :class "table-responsive"
                      (:table :class "table table-stripped"
                              (:thead
                               (:tr (:th "#")
                                    (:th "Build Date")
                                    (:th "Extension")
                                    (:th "Animal")
                                    (:th "OS")
                                    (:th "Version")
                                    (:th "Architecture")))
                              (:tbody
                               (:tr
                                (:td (str id))
                                (:td (str stamp))
                                (:td (str extension))
                                (:td (:a :href (str (format nil "/animal/~a" animal))
                                         (str animal)))
                                (:td (str os))
                                (:td (str version))
                                (:td (str arch))))))
                (:pre :style "overflow:auto; word-wrap: normal;"
                      (str log)))))))))

(defun front-list-archives ()
  "List recent build logs."
  (let ((archives
         (with-pgsql-connection (*dburi*)
           (query "select ar.id,
                          bl.id, format('/build/%s', bl.id) as blhref,
                          e.fullname, e.shortname, pgversion,
                          p.os_name, p.os_version, p.arch
                     from archive ar
                          join buildlog bl on bl.id = ar.log
                          join extension e on e.id = ar.extension
                          join platform p  on p.id = ar.platform
                 order by bl.buildstamp desc nulls last
                    limit 15"))))
    (serve-dashboard-page
     (with-html-output-to-string (s)
       (htm
        (:div :class "col-sm-9 col-sm-offset-3 col-md-10 col-md-offset-2 main"
              (:h1 :class "page-header" "Extension Archives")
              (:div :class "table-responsive"
                    (:table :class "table table-stripped"
                            (:thead
                             (:tr (:th "#")
                                  (:th "Build log")
                                  (:th "Extension")
                                  (:th "Archive")))
                            (:tbody
                             (loop :for (archive-id log-id log-href
                                                    extension shortname
                                                    pgversion os version arch)
                                :in archives
                                :for filename := (format nil
                                                         "~a--~a--~a--~a--~a.tar.gz"
                                                         shortname
                                                         pgversion
                                                         os
                                                         version
                                                         arch)
                                :for href := (format nil
                                                     "/api/fetch/~a/~a/~a/~a/~a"
                                                     shortname
                                                     pgversion
                                                     os
                                                     version
                                                     arch)
                                :do (htm
                                     (:tr
                                      (:td (str archive-id))
                                      (:td (:a :href log-href
                                               (:strong (str log-id))))
                                      (:td (str extension))
                                      (:td (:a :href href
                                               (str filename)))))))))))))))
