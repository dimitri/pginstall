MODULE_big  = pginstall
OBJS        = archive.o communicate.o fun.o pginstall.o platform.o utils.o
DOCS        = README.md
DATA        = pginstall--1.0.sql
EXTENSION   = pginstall

PG_CPPFLAGS = -I/usr/local/opt/libarchive/include
SHLIB_LINK  = -L/usr/local/opt/libarchive/lib -larchive -ljansson -lcurl

PG_CONFIG = pg_config
PGXS = $(shell $(PG_CONFIG) --pgxs)
include $(PGXS)
