# pginstall build tool
APP_NAME   = pginstall

SBCL	   = sbcl
SBCL_OPTS  = --no-sysinit --no-userinit

COMPRESS_CORE ?= yes

ifeq ($(COMPRESS_CORE),yes)
COMPRESS_CORE_OPT = --compress-core
else
COMPRESS_CORE_OPT = 
endif

BUILDDIR   = build
LIBS       = $(BUILDDIR)/libs.stamp
QLDIR      = $(BUILDDIR)/quicklisp
MANIFEST   = $(BUILDDIR)/manifest.ql
BUILDAPP   = $(BUILDDIR)/bin/buildapp
PGINSTALL  = $(BUILDDIR)/bin/pginstall

all: $(PGINSTALL)

clean:
	rm -rf $(LIBS) $(QLDIR) $(MANIFEST) $(BUILDAPP) $(PGINSTALL)

extension:
	$(MAKE) -C src/client

$(QLDIR)/setup.lisp:
	mkdir -p $(BUILDDIR)
	curl -o $(BUILDDIR)/quicklisp.lisp http://beta.quicklisp.org/quicklisp.lisp
	$(SBCL) $(SBCL_OPTS) --load $(QLDIR).lisp                                  \
             --eval '(quicklisp-quickstart:install :path "$(BUILDDIR)/quicklisp")' \
	     --eval '(quit)'

quicklisp: $(QLDIR)/setup.lisp ;

$(LIBS): $(QLDIR)/setup.lisp
	$(SBCL) $(SBCL_OPTS) --load $(QLDIR)/setup.lisp           \
	     --eval '(ql:quickload "pginstall")'                  \
	     --eval '(quit)'
	touch $@

libs: $(LIBS) ;

$(MANIFEST): $(LIBS)
	$(SBCL) $(SBCL_OPTS) --load $(QLDIR)/setup.lisp            \
	     --eval '(ql:write-asdf-manifest-file "$(MANIFEST)")'  \
	     --eval '(quit)'

manifest: $(MANIFEST) ;

$(BUILDAPP): $(QLDIR)/setup.lisp
	$(SBCL) $(SBCL_OPTS) --load $(QLDIR)/setup.lisp          \
	     --eval '(ql:quickload "buildapp")'                  \
	     --eval '(buildapp:build-buildapp "$@")'             \
	     --eval '(quit)'

buildapp: $(BUILDAPP) ;

$(PGINSTALL): $(MANIFEST) $(BUILDAPP)
	mkdir -p $(BUILDDIR)/bin
	$(BUILDAPP)      --logfile /tmp/build.log                \
	                 --require sb-posix                      \
	                 --require sb-bsd-sockets                \
	                 --require sb-rotate-byte                \
                         --asdf-path .                           \
	                 --asdf-tree $(QLDIR)/local-projects     \
	                 --manifest-file $(MANIFEST)             \
	                 --asdf-tree $(QLDIR)/dists              \
	                 --asdf-path .                           \
	                 --load-system pginstall                 \
	                 --entry pginstall:main                  \
	                 --dynamic-space-size 4096               \
                         $(COMPRESS_CORE_OPT)                    \
	                 --output $@

pginstall: $(PGINSTALL) ;
