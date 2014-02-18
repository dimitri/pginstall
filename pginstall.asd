;;;; pginstall.asd

(asdf:defsystem #:pginstall
    :serial t
    :description "Repository of PostgreSQL Extensions"
    :author "Dimitri Fontaine <dimitri@2ndQuadrant.fr>"
    :license "The PostgreSQL Licence"
    :depends-on (#:uiop			; host system integration
		 #:cl-log		; logging
		 #:postmodern		; PostgreSQL protocol implementation
		 #:split-sequence	; some parsing is made easy
		 #:cl-fad		; file and directories
		 #:esrap		; parser generator
		 #:alexandria		; utils
		 #:drakma		; http client, download archives
		 #:zip			; support for zip archive files
		 #:local-time		; UDP date parsing
                 #:cl-markdown          ; To produce the website
		 )
    :components
    ((:module "src"
	      :components
              ((:module animal
			:components
			((:file "package")
			 (:file "animal" :depends-on ("package"))))))))

