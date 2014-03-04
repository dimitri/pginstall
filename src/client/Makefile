MODULE_big  = pginstall
OBJS        = archive.o platform.o utils.o pginstall.o
DOCS        = README.md
PG_CPPFLAGS = -I/usr/local/opt/libarchive/include
SHLIB_LINK  = -L/usr/local/opt/libarchive/lib -larchive

PG_CONFIG = pg_config
PGXS = $(shell $(PG_CONFIG) --pgxs)
include $(PGXS)
