# pginstall build tool
ASDF_CONFD = ~/.config/common-lisp/source-registry.conf.d
ASDF_CONF  = $(ASDF_CONFD)/pginstall.conf

LIBS       = build/libs.stamp
BUILDAPP   = build/buildapp
MANIFEST   = build/manifest.ql
PGINSTALL  = build/pginstall.exe

all: $(PGINSTALL)

extension:
	$(MAKE) -C src/client

~/quicklisp/local-projects/iolib:
	git clone https://github.com/sionescu/iolib.git $@

iolib: ~/quicklisp/local-projects/iolib ;

~/quicklisp/setup.lisp:
	curl -o ~/quicklisp.lisp http://beta.quicklisp.org/quicklisp.lisp
	sbcl --load ~/quicklisp.lisp                  \
	     --eval '(quicklisp-quickstart:install)'  \
	     --eval '(quit)'

quicklisp: ~/quicklisp/setup.lisp ;

$(ASDF_CONF):
	mkdir -p $(ASDF_CONFD)
	echo "(:tree \"`pwd`\")" > $@

asdf-config: $(ASDF_CONF) ;

$(LIBS): quicklisp $(ASDF_CONF) iolib
	sbcl --load ~/quicklisp/setup.lisp                        \
	     --eval '(ql:quickload "pginstall")'                  \
	     --eval '(quit)'
	touch $@

libs: $(LIBS) ;

$(MANIFEST): libs
	sbcl --load ~/quicklisp/setup.lisp                                 \
	     --eval '(ql:write-asdf-manifest-file "./build/manifest.ql")'  \
	     --eval '(quit)'

manifest: $(MANIFEST) ;

$(BUILDAPP): quicklisp
	sbcl --load ~/quicklisp/setup.lisp                          \
	     --eval '(ql:quickload "buildapp")'                     \
	     --eval '(buildapp:build-buildapp "./build/buildapp")'  \
	     --eval '(quit)'

buildapp: $(BUILDAPP) ;

$(PGINSTALL): manifest buildapp
	./build/buildapp --logfile /tmp/build.log                \
	                 --require sb-posix                      \
	                 --require sb-bsd-sockets                \
	                 --require sb-rotate-byte                \
	                 --asdf-tree ~/quicklisp/local-projects  \
	                 --manifest-file ./build/manifest.ql     \
	                 --asdf-tree ~/quicklisp/dists           \
	                 --asdf-path .                           \
	                 --load-system pginstall                 \
	                 --entry pginstall:main                  \
	                 --dynamic-space-size 4096               \
	                 --compress-core                         \
	                 --output $@

pginstall: $(PGINSTALL) ;
