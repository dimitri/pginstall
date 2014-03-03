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
                 #:split-sequence       ; split sequences
		 )
    :components
    ((:module "src"
	      :components
              ((:module common
			:components
			((:file "package")
                         (:file "pgconfig" :depends-on ("package"))
                         (:file "platform" :depends-on ("package"))
                         (:file "dburi" :depends-on ("package"))
			 (:file "pgsql" :depends-on ("package" "dburi"))))
               (:module config
                        :depends-on ("common")
			:components
			((:file "package")
			 (:file "config" :depends-on ("package"))))
               (:module repo
                        :depends-on ("common")
			:components
			((:file "package")
                         (:file "dao" :depends-on ("package"))
			 (:file "setup" :depends-on ("package" "dao"))))
               (:module animal
                        :depends-on ("common" "repo")
                        :components
                        ((:file "package")
                         (:file "animal"  :depends-on ("package"))
                         (:file "archive" :depends-on ("package"))
                         (:file "build"  :depends-on ("package"
                                                      "archive"
                                                      "animal"))))))))

