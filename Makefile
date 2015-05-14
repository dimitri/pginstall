# pginstall build tool
APP_NAME   = pginstall

# use either sbcl or ccl
CL	   = sbcl

LISP_SRC   = $(wildcard src/animal/*lisp) \
             $(wildcard src/common/*lisp) \
             $(wildcard src/config/*lisp) \
             $(wildcard src/main/*lisp) \
             $(wildcard src/repo/*lisp) \
             $(wildcard src/server/*lisp) \
             pginstall.asd

DOCS       = $(wildcard doc/*md) README.md

BUILDDIR   = build
LIBS       = $(BUILDDIR)/libs.stamp
QLDIR      = $(BUILDDIR)/quicklisp
MANIFEST   = $(BUILDDIR)/manifest.ql
BUILDAPP   = $(BUILDDIR)/bin/buildapp
PGINSTALL  = $(BUILDDIR)/bin/pginstall

BUILDAPP_CCL  = $(BUILDDIR)/bin/buildapp.ccl
BUILDAPP_SBCL = $(BUILDDIR)/bin/buildapp.sbcl

ifeq ($(CL),sbcl)
BUILDAPP   = $(BUILDAPP_SBCL)
CL_OPTS    = --no-sysinit --no-userinit
else
BUILDAPP   = $(BUILDAPP_CCL)
CL_OPTS    = --no-init
endif

COMPRESS_CORE ?= yes

ifeq ($(CL),sbcl)
ifeq ($(COMPRESS_CORE),yes)
COMPRESS_CORE_OPT = --compress-core
else
COMPRESS_CORE_OPT = 
endif
endif

ifeq ($(CL),sbcl)
BUILDAPP_OPTS =          --require sb-posix                      \
                         --require sb-bsd-sockets                \
                         --require sb-rotate-byte
endif

DEBUILD_ROOT = /tmp/pginstall/debian/pginstall

all: $(PGINSTALL)

docs:
	ronn -roff pginstall.1.md

clean:
	rm -rf $(LIBS) $(QLDIR) $(MANIFEST) $(BUILDAPP) $(PGINSTALL)

extension:
	$(MAKE) -C src/client

$(QLDIR)/setup.lisp:
	mkdir -p $(BUILDDIR)
	curl -o $(BUILDDIR)/quicklisp.lisp http://beta.quicklisp.org/quicklisp.lisp
	$(CL) $(CL_OPTS) --load $(BUILDDIR)/quicklisp.lisp            \
             --eval '(quicklisp-quickstart:install :path "$(QLDIR)")' \
	     --eval '(quit)'

quicklisp: $(QLDIR)/setup.lisp ;

$(LIBS): $(QLDIR)/setup.lisp
	$(CL) $(CL_OPTS) --load $(QLDIR)/setup.lisp               \
             --eval '(push "$(PWD)/" asdf:*central-registry*)'    \
	     --eval '(ql:quickload "pginstall")'                  \
	     --eval '(quit)'
	touch $@

libs: $(LIBS) ;

$(MANIFEST): $(LIBS)
	$(CL) $(CL_OPTS) --load $(QLDIR)/setup.lisp                \
	     --eval '(ql:write-asdf-manifest-file "$(MANIFEST)")'  \
	     --eval '(quit)'

manifest: $(MANIFEST) ;

$(BUILDAPP_CCL): $(QLDIR)/setup.lisp
	mkdir -p $(BUILDDIR)/bin
	$(CL) $(CL_OPTS) --load $(QLDIR)/setup.lisp               \
             --eval '(ql:quickload "buildapp")'                   \
             --eval '(buildapp:build-buildapp "$@")'              \
             --eval '(quit)'

$(BUILDAPP_SBCL): $(QLDIR)/setup.lisp
	mkdir -p $(BUILDDIR)/bin
	$(CL) $(CL_OPTS) --load $(QLDIR)/setup.lisp               \
             --eval '(ql:quickload "buildapp")'                   \
             --eval '(buildapp:build-buildapp "$@")'              \
             --eval '(quit)'

buildapp: $(BUILDAPP) ;

$(PGINSTALL): $(MANIFEST) $(BUILDAPP) $(LISP_SRC) $(DOCS)
	mkdir -p $(BUILDDIR)/bin
	$(BUILDAPP)      --logfile /tmp/build.log                \
                         $(BUILDAPP_OPTS)                        \
                         --sbcl $(CL)                            \
                         --asdf-path .                           \
	                 --asdf-tree $(QLDIR)/local-projects     \
	                 --manifest-file $(MANIFEST)             \
	                 --asdf-tree $(QLDIR)/dists              \
	                 --asdf-path .                           \
	                 --load-system $(APP_NAME)               \
	                 --entry pginstall:main                  \
	                 --dynamic-space-size 4096               \
                         $(COMPRESS_CORE_OPT)                    \
	                 --output $@

pginstall: $(PGINSTALL) ;

deb:
	# intended for use on a debian system
	mkdir -p $(DEBUILD_ROOT) && rm -rf $(DEBUILD_ROOT)
	rsync -Ca --exclude=build/* --exclude=.vagrant ./ $(DEBUILD_ROOT)/
	mkdir -p $(DEBUILD_ROOT)/build/bin
	cd $(DEBUILD_ROOT) && pg_buildext updatecontrol && make -f debian/rules orig
	cd $(DEBUILD_ROOT) && debuild -us -uc -sa
	cp -a /tmp/pginstall/debian/pginstall_* build/
	cp -a /tmp/pginstall/debian/postgresql-*-pginstall* build/
