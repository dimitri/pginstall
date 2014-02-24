;;;; pginstall.asd

(asdf:defsystem #:pginstall
    :serial t
    :description "Repository of PostgreSQL Extensions"
    :author "Dimitri Fontaine <dimitri@2ndQuadrant.fr>"
    :license "The PostgreSQL Licence"
    :depends-on (#:uiop			; host system integration
		 #:postmodern		; PostgreSQL protocol implementation
		 #:esrap		; parser generator
		 #:drakma		; http client, download archives
		 #:local-time		; UDP date parsing
		 #:py-configparser	; Read old-style INI config files
                 #:iolib                ; I/O library
                 #:iolib.os             ; OS level interface, pathnames
                 #:puri                 ; URI validation and manipulation
                 #:cl-ppcre             ; Regular Expressions
                 #:alexandria           ; Some utilities
                 #:archive              ; produce an extension archive file
		 )
    :components
    ((:module "src"
	      :components
              ((:module common
			:components
			((:file "package")
                         (:file "dburi" :depends-on ("package"))
			 (:file "pgsql" :depends-on ("package" "dburi"))))
               (:module config
                        :depends-on ("common")
			:components
			((:file "package")
			 (:file "config" :depends-on ("package"))))
               (:module animal
                        :depends-on ("common")
                        :components
                        ((:file "package")
                         (:file "dao" :depends-on ("package"))
                         (:file "animal"  :depends-on ("package" "dao"))
                         (:file "archive" :depends-on ("package" "dao"))
                         (:file "build"  :depends-on ("package"
                                                      "dao"
                                                      "archive"
                                                      "animal"))))))))

