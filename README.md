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

Animal

  ~ The Animal builds extensions and prepare an archive files set for
    it. The archive file are platform dependent, so any given Animal is only
    going to produce files for its own platform. A single Animal might build
    archive files for as many PostgreSQL versions as available locally,
    though, for each extension it's building.

Archive

  ~ When building an extension, the PostgreSQL Extension Installer prepares
    a binary archive file, made to be easy to install on PostgreSQL servers
    at `CREATE EXTENSION` time. The archive format currently is `.tar.gz`.
    
Build Farm

  ~ A set of animals all working together to build the Extensions for the
    Repository Server. You will need at least one Animal in the Build Farm
    per platform you want to support.

Extension

  ~ A kind of a PostgreSQL plugin, made of a *control file* needed to help
    PostgreSQL know how to install the extension, one or more *auxilliary
    control files* that allow changing meta-data from an extension release
    to the next, one or more *SQL script files* that are going to be run by
    PostgreSQL for creating the extension's objects, and possibly a *dynamic
    library file* (often a `.so`, the file name depends on the platform).

Platform

  ~ The goal of the notion of a *platform* is to be able to deploy archives
    prepared on a server (one of the Buildfarm Animals). `pginstall` defines
    a platform as being a unique tuple of *Operating System Name*,
    *Operating System Version* and *Architecture*. A couple of supported
    platforms might be `Debian 7.1 x86_64` and `CentOS 6.4 x86_64`, for
    example.

Queue

  ~ The repository server manages a queue of extensions to build, so that it
    doesn't have to contact the buildfarm animals in order for the work to
    get done. That allows the build farm animals to be on-demand virtual
    machines. When you want to build extensions, you push them on a queue.

Registry

  ~ The build farm animals are given names so that it's easier for us human
    to reason about them. The name isn't needed for any operation other than
    registering the build farm animals and saving away the build logs. In
    order to be able to automate the naming, the repository server API
    include a *pick a name for me please* call.

Repository

  ~ The repository server is driving the build of extension and making
    extension archive available to the PostgreSQL installations using
    pginstall. 

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

Setup the necessary bits:

    pginstall server start

Then open your browser at `http://localhost:8042/`, maybe replacing
`localhost` with the hostname of the *repository server* you are setting
up. By default the respository server listens on port `8042`, you can edit
that setting before starting the server:

    pginstall config set listen-port 12345

Once started time, the web interface of the repository server will guide you
through some more setup that you need to consider, or you can use the
following commands:

    pginstall config set dburi postgresql://user@host:5432/dbname
    pginstall config set listen-port 8042
    pginstall config set archive-path "/var/cache/pginstall"

### Setting up a Buildfarm Animal

A Build Farm Animal needs to be able to talk to the Repository Server in
order to be fed with work to do, then to upload the archives it's been
building. Also, the animal needs a name and to register the `pgconfig` paths
available to build extensions, to make it easier for us humans to track
things:

    pginstall config set server http://pginstall.mycompany.com:8042/
    pginstall animal register

The default setting for the server uri is `http://localhost:8042/` so that
by default, your machine can both serve Extensions and Build them.

If you have installed several version of the `make` and `git` tools and want
to make sure that pginstall will pick the right one, you can use the
following settings:

    pginstall config set gmake /usr/local/bin/gmake
    pginstall config set git /usr/local/bin/git

The final step is to setup where to build the extensions, then you can tell
the animal to have at the build queue:

    pginstall config set build-root /home/pginstall/build
    pginstall animal build

The `pginstall animal build` command will have the animal take the next
extension to build on the local platform from the repository build queue,
build it, prepare an archive, and upload the archive. Then the animal will
ask for the next extension to build again, and will only stops when the
queue is empty for its platform.

#### pgconfig automatic detection

The command `pginstall animal register` will find the existing `pg_config`
programs available on the current build machine. You can check that list
with the following command:

    pginstall animal list pgconfig

To find the list of `pg_config` build environments to use, pginstall search
in the following places:

  - current `$PATH`
  - debian style `/usr/lib/postgresql/X.Y/bin`
  - centos style `/usr/pgsql-X.Y/bin`
  
Any path entry containing `X.Y` will be used as a pattern and the template
is replaced in a loop with the value `8.4`, `9.0`, `9.1`, `9.2`, `9.3` and
`9.4`.

To add your own `pg_config` entries to the list of build environments used
by pginstall, tweak the `PATH` under which you run the `pginstall animal
build` command.

You can register newly added environments with the command

    pginstall animal add pgconfig /path/to/X.Y/bin/pg_config

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
    pginstall.extension_dir = '/usr/share/pginstall'
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
     pginstall.extension_dir | /usr/share/pginstall
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

