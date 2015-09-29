# The PostgreSQL Extension Installer

PostgreSQL 9.1 brings in a new concept of Extensions, an SQL object that
packs together a bunch of other SQL objects. The aim of extensions is to
allow for easy dump and restore of SQL objects managed externally, such
as... extensions.

In PostgreSQL, the `CREATE EXTENSION` command installs the SQL objects of an
extension by running a *SQL script*. This script and some more files must be
available at the right place on the PostgreSQL server's file system for the
`CREATE EXTENSION` command to be successful.

The *PostgreSQL Extension Installer* software is an *Extension Repository
Manager*.

## Overview

The pginstall software is composed of the following 3 main components:

  - PostgreSQL Extensions Repository Server

    The *Extension Repository Server* serves platform-dependent binary
    extension archives to the *Extensions Installer*.

  - PostgreSQL Extensions Build Farm
  
    The *Extension Build Farm* prepares the platform specific binary
    archives from the extension sources. Currently, it only knows how to
    build from `git` hosted sources.
  
  - PostgreSQL Extensions Installer
  
    The *Extension Installer* uses the repository server to fetch a platform
    specific binary extension archive and knows how to install that archive
    so that PostgreSQL can use it in its `CREATE EXTENSION` command.
    
The repository server offers an HTTP API which delivers JSON formated
information.

## Glossary

The following terms are often used in the documentation about the
*PostgreSQL Extension Installer*:

#### Animal

The Animal builds extensions and prepare an archive files set for it. The
archive file are platform dependent, so any given Animal is only going to
produce files for its own platform. A single Animal might build archive
files for as many PostgreSQL versions as available locally, though, for each
extension it's building.

#### Archive

When building an extension, the PostgreSQL Extension Installer prepares a
binary archive file, made to be easy to install on PostgreSQL servers at
`CREATE EXTENSION` time. The archive format currently is `.tar.gz`.
    
#### Build Farm

A set of animals all working together to build the Extensions for the
Repository Server. You will need at least one Animal in the Build Farm per
platform you want to support.

#### Extension

A kind of a PostgreSQL plugin, made of a *control file* needed to help
PostgreSQL know how to install the extension, one or more *auxiliary
control files* that allow changing meta-data from an extension release to
the next, one or more *SQL script files* that are going to be run by
PostgreSQL for creating the extension's objects, and possibly a *dynamic
library file* (often a `.so`, the file name depends on the platform).

#### Platform

The goal of the notion of a *platform* is to be able to deploy archives
prepared on a server (one of the Buildfarm Animals). `pginstall` defines a
platform as being a unique tuple of *Operating System Name*, *Operating
System Version* and *Architecture*. A couple of supported platforms might be
`Debian 7.1 x86_64` and `CentOS 6.4 x86_64`, for example.

#### Queue

The repository server manages a queue of extensions to build, so that it
doesn't have to contact the buildfarm animals in order for the work to get
done. That allows the build farm animals to be on-demand virtual
machines. When you want to build extensions, you push them on a queue.

#### Registry

The build farm animals are given names so that it's easier for us human to
reason about them. The name isn't needed for any operation other than
registering the build farm animals and saving away the build logs. In order
to be able to automate the naming, the repository server API include a *pick
a name for me please* call.

#### Repository

The repository server is driving the build of extension and making extension
archive available to the PostgreSQL installations using pginstall.

## Installing pginstall

The repository server and the buildfarm animals are managed through the same
`pginstall` binary. The setup and the commands you use determine the role of
any single instance.

It's possible for the reposiroty server to also be a Build Farm Animal,
which will then typically talk to the repository server at
`http://localhost:8042/`.

### Setting up a Repository Server

The *repository server* needs a PostgreSQL database to register a list of
extensions, platforms, animals and to manage the extension build queue.

Start the server, which will ask you for a PostgreSQL connection string
where to install itself into:

    pginstall server start

Now that the server is started, head to
[http://localhost:8042/](http://localhost:8042/), replacing the `localhost`
part with the *ip address* of the server if needed.

Then check that it's running fine:

    pginstall server status
    pginstall server config

Note that the configuration active at this point is the default in-memory
set of settings that allow you getting started. You might want to change
several of the settings in place, using the following commands:

    pginstall config set dburi postgresql://user@host:5432/dbname
    pginstall config set listen-port 8042
    pginstall config set archive-path "/var/cache/pginstall"

As soon as you use the `pginstall config set` command, a configuration file
is created to save your preferences in `~/.pginstall.ini`.

### Setting up a Buildfarm Animal

Please refer to the documentation found in the file `doc/buildfarm.md` in
this repostory, or to the *Buildfarm* menu entry of the Repository Web
Application.

### Setting up the installer in a PostgreSQL installation

The idea is that pginstall will divert the normal execution of the following
PostgreSQL commands, by installing what PostgreSQL calls a *Process Utility
Hook*:

    CREATE EXTENSION
    ALTER EXTENSION
    DROP EXTENSION

To install a PostgreSQL hook, you need to edit your `postgresql.conf` file
(the SQL command `show config_file;` will tell you where to find this file).

Here's the list of settings you need to edit, either changing them or adding
them to the file:

    local_preload_libraries = 'pginstall'
    pginstall.archive_dir   = '/var/cache/pginstall/fetch'
    pginstall.control_dir   = '/Users/dim/pgsql/ddl/share/extension'
    pginstall.extension_dir = '/var/lib/pginstall'
    pginstall.repository    = 'http://localhost:8042/'
    pginstall.custom_path   = '/etc/pginstall/custom'
    #pginstall.whitelist     = 'hstore,ltree,prefix,plproxy'
    pginstall.sudo          = true

You then need to reload PostgreSQL for this setup to take effect.

## Usage

Here's how to use pginstall components.

### The reposistory server

You need to start the repository server for the buildfarm and the clients to
be able to talk to it.

    pginstall server start
    pginstall server status
    pginstall server config

Then you can queue extension builds, that will get done as soon as some
animal on the buildfarm wakes up and ask for some work to do.

    pginstall extension queue prefix

### The buildfarm animals

The main command you need to manage your buildfarm animal, once they are
setup, is the following:

    pginstall animal build
    
If you happen to add new PostgreSQL version support in an existing animal,
it will be automatically picked up by the `build` command. You can register
the new setup to the repository server to ease the management of the build
farm animals with the following command:

    pginstall animal add pgconfig /path/to/X.Y/bin/pgconfig

### The pginstall Extension

Once the extension has been setup as described before, you should see
something like the following:

    > select name, setting from pg_settings where name ~ 'pginstall';
              name           |               setting                
    -------------------------+--------------------------------------
     pginstall.archive_dir   | /var/cache/pginstall/fetch
     pginstall.control_dir   | /Users/dim/pgsql/ddl/share/extension
     pginstall.custom_path   | /etc/pginstall/custom
     pginstall.extension_dir | /var/lib/pginstall
     pginstall.repository    | http://localhost:8042/
     pginstall.sudo          | on
     pginstall.whitelist     | hstore,ltree,prefix,plproxy
    (7 rows)

You can then install the pginstall extension itself (as a superuser in your
database):

    # CREATE EXTENSION pginstall;

The following view is made available by the extension and lists all the
extensions available for your current PostgreSQL server, based on
discovering its version and platform:

    pginstall> TABLE pginstall_available_extensions;

The `pginstall_platform` function allows to check the platform detection
that *pginstall* does for you:

    pginstall> SELECT * FROM pginstall_platform();
    select * from pginstall_platform();
     os_name  | os_version |  arch  
    ----------+------------+--------
     Mac OS X | 10.9.1     | x86_64
    (1 row)


### Full Example

First install from the local archive only, without contacting the repository
server, because we have `pginstall.serve_from_archive_dir` set to *true*. We
begin with a local build of the *prefix* extension then go on to install it.

    $ ./build/bin/pginstall build github.com/dimitri/prefix
    Building extension github.com/dimitri/prefix
              git clone "https://github.com/dimitri/prefix.git"
              building with "/Users/dim/pgsql/ddl/bin/pg_config"
    
    Built: /tmp/pginstall/prefix--9.4devel--Mac_OS_X--10.9.2--x86_64.tar.gz 
     logs: /tmp/pginstall/logs/prefix.txt
    
    $ psql pginstall
    
    ~# set client_min_messages to 'debug1';
    SET
    
    ~# select name, setting from pg_settings where name ~ 'pginstall';
                   name               |               setting                
    ----------------------------------+--------------------------------------
     pginstall.archive_dir            | /tmp/pginstall
     pginstall.control_dir            | /Users/dim/pgsql/ddl/share/extension
     pginstall.custom_path            | /tmp/pginstall/custom
     pginstall.extension_dir          | /tmp/pginstall/extension
     pginstall.repository             | http://localhost:8042/
     pginstall.serve_from_archive_dir | on
     pginstall.sudo                   | on
     pginstall.whitelist              | 
    (8 rows)
    
    ~# create extension prefix;
    DEBUG:  00000: Unpacking archive "/tmp/pginstall/prefix--9.4devel--Mac_OS_X--10.9.2--x86_64.tar.gz"
    LOCATION:  extract, pgarchive.c:165
    DEBUG:  00000: Extracting "prefix.control" to "/Users/dim/pgsql/ddl/share/extension/prefix.control"
    LOCATION:  extract, pgarchive.c:187
    DEBUG:  00000: Extracting "prefix/prefix.so" to "/tmp/pginstall/extension/prefix/prefix.so"
    LOCATION:  extract, pgarchive.c:187
    DEBUG:  00000: Extracting "prefix/prefix--unpackaged--1.2.0.sql" to "/tmp/pginstall/extension/prefix/prefix--unpackaged--1.2.0.sql"
    LOCATION:  extract, pgarchive.c:187
    DEBUG:  00000: Extracting "prefix/prefix--1.2.0.sql" to "/tmp/pginstall/extension/prefix/prefix--1.2.0.sql"
    LOCATION:  extract, pgarchive.c:187
    DEBUG:  00000: Extracting "prefix/prefix--1.1--1.2.0.sql" to "/tmp/pginstall/extension/prefix/prefix--1.1--1.2.0.sql"
    LOCATION:  extract, pgarchive.c:187
    DEBUG:  00000: Extracting "prefix/TESTS.md" to "/tmp/pginstall/extension/prefix/TESTS.md"
    LOCATION:  extract, pgarchive.c:187
    DEBUG:  00000: Extracting "prefix/README.md" to "/tmp/pginstall/extension/prefix/README.md"
    LOCATION:  extract, pgarchive.c:187
    DEBUG:  00000: Rewriting control file "/Users/dim/pgsql/ddl/share/extension/prefix.control"
    LOCATION:  rewrite_control_file, utils.c:172
    CREATE EXTENSION

Now, let's prevent the local PostgreSQL from being allowed to use the local
archive repository as its source for installing, forcing it to contact the
repository server instead.

Note that the repository server here has built the *prefix* extension
independantly.

    ~# set pginstall.serve_from_archive_dir to false;
    SET
    
    ~# drop extension prefix;
    DROP EXTENSION
    
    ~# create extension prefix;
    DEBUG:  00000: curl -O "/tmp/pginstall/prefix--9.4devel--Mac_OS_X--10.9.2--x86_64.tar.gz" http://localhost:8042/api/fetch/prefix/9.4devel/Mac%20OS%20X/10.9.2/x86_64
    LOCATION:  download, communicate.c:184
    DEBUG:  00000: Unpacking archive "/tmp/pginstall/prefix--9.4devel--Mac_OS_X--10.9.2--x86_64.tar.gz"
    LOCATION:  extract, pgarchive.c:165
    DEBUG:  00000: Extracting "prefix.control" to "/Users/dim/pgsql/ddl/share/extension/prefix.control"
    LOCATION:  extract, pgarchive.c:187
    DEBUG:  00000: Extracting "prefix/prefix.so" to "/tmp/pginstall/extension/prefix/prefix.so"
    LOCATION:  extract, pgarchive.c:187
    DEBUG:  00000: Extracting "prefix/prefix--unpackaged--1.2.0.sql" to "/tmp/pginstall/extension/prefix/prefix--unpackaged--1.2.0.sql"
    LOCATION:  extract, pgarchive.c:187
    DEBUG:  00000: Extracting "prefix/prefix--1.2.0.sql" to "/tmp/pginstall/extension/prefix/prefix--1.2.0.sql"
    LOCATION:  extract, pgarchive.c:187
    DEBUG:  00000: Extracting "prefix/prefix--1.1--1.2.0.sql" to "/tmp/pginstall/extension/prefix/prefix--1.1--1.2.0.sql"
    LOCATION:  extract, pgarchive.c:187
    DEBUG:  00000: Extracting "prefix/TESTS.md" to "/tmp/pginstall/extension/prefix/TESTS.md"
    LOCATION:  extract, pgarchive.c:187
    DEBUG:  00000: Extracting "prefix/README.md" to "/tmp/pginstall/extension/prefix/README.md"
    LOCATION:  extract, pgarchive.c:187
    DEBUG:  00000: Rewriting control file "/Users/dim/pgsql/ddl/share/extension/prefix.control"
    LOCATION:  rewrite_control_file, utils.c:172
    CREATE EXTENSION
