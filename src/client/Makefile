MODULE_big  = pginstall
OBJS        = communicate.o \
              fun.o 	    \
              pgarchive.o   \
              pginstall.o   \
              platform.o    \
              psprintf.o    \
              utils.o

DOCS        = README.md
DATA        = pginstall--1.0.sql
EXTENSION   = pginstall

#
# In MacOSX when using the brew packaging system, libarchive and jansson are
# not installed in a system wide location, so we have to tweak the setup:
#
UNAME := $(shell uname -s)

ifeq ($(UNAME),Darwin)
PG_CPPFLAGS = -I/usr/local/opt/libarchive/include
SHLIB_LINK  = -L/usr/local/opt/libarchive/lib -larchive -ljansson -lcurl
endif

PG_CONFIG = pg_config
PGXS = $(shell $(PG_CONFIG) --pgxs)
include $(PGXS)
