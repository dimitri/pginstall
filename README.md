# PGInstall

`pginstall` is a tool suite which goal is to enable integrated management of
PostgreSQL extensions. The goal of the several components is to allow for
the following user story:

    prompt=> CREATE EXTENSION pginstall;
    CREATE EXTENSION
   
    prompt=> CREATE EXTENSION awesome;
    NOTICE: extension "awesome" is not locally available
    NOTICE: updating repository heroku
    NOTICE: updating repository pginstall.postgresql.org
    NOTICE: found extension awesome, version 1.2, at heroku
    NOTICE: fetching http://pginstall.heroku.com/awesome/awesome--1.2.sql
    CREATE EXTENSION

To achieve that goal, we need several software components to play together:

  - A repository server (http) with meta-data about available extensions and
    versions and archive files,
  
  - A client embedded in PostgreSQL that knows how to fetch an extension
    archive and unpack it at the right place,
  
  - A build system that uses the *PGXS* mechanism to build an extension and
    installs it into a local prefix path from where it's able to build the
    extension archive and its Manifest,

  - A repository management application that drives the *Build System* over
    a list of extensions and prepare the binary archives for those.

In the first version of the `pginstall` application, the build system and
the repository management applications are provided with a *Command Line
Interface* only. In a later version, a full control web application might be
added so as to make it easier to use in some cases.

## A single Command Line application to rule them all

The whole set of features is contained within a single application binary
named `pginstall`, except for the client-side application that needs to be a
separate PostgreSQL extension.

    pginstall config dburi postgresql://host:port/dbname    
    pginstall config port 8042
    pginstall config build-path /tmp/pginstall
    pginstall config archive-path /var/cache/pginstall
    pginstall config server http://pginstall.mydomain.tld/
    pginstall config auth
    pginstall config pg 9.3 /path/to/9.3/bin/pg_control
    pginstall config pg 9.2 /path/to/9.2/bin/pg_control
        
    pginstall server start
    pginstall server stop
    pginstall server status
    
    pginstall build /path/to/sources/of/extension
    pginstall upload extname

    pginstall repo ls
    pginstall repo add extname uri description
    pginstall repo rm extname [ ... ]
    pginstall repo update

## The Extension Archive Format

We're using [libarchive](http://www.libarchive.org/) for the archiving
support in the C coded extension in PostgreSQL, and the
[archive](https://github.com/froydnj/archive) and the
[Salza2](http://www.cliki.net/Salza2) Common Lisp librairies to produce it.

The Archive contains the files built by the *PGXS* install command `make
install`, plus a `pginstall` specific *Manifest* file, in the following
format:

    # Manifest file for pginstall
    control:  path/to/extension.control         # main control always first
              path/to/extension--1.1.control    # auxilliary control files
    module:   path/to/extension.so
              path/to/dependency.so             # needs serious checking
    scripts:  path/to/extension--1.0.sql
              path/to/extension--1.0--1.1.sql
    docs:     path/to/README.extension.txt

See the *Build System* section for details about the relative *path* to be
found in the *Manifest*.

## The `pginstall` Repository Server

The role of the *Repository Server* is to publish the list of available
extensions and to deliver the *binary archive* for any specific extension,
or extension's version.

It's possible too register the same *extension name* several time: the
unique identifier of an *extension archive* is composed of the *name* and
the *default_version* of the extension. Then the whole list of upgrade paths
are maintained in the *Repostory Server* to better serve clients.

The *Repository Server* is publishing the information in the *JSON* format
over the *HTTP* protocol. Its main API consists of:

  - The URL path,
  - The JSON format.

To register a new extension, its archive needs to be uploaded using the
*HTTP PUT* protocol with the archive contents as the binary payload. The
client side command `pginstall upload extension` does that.

The *Repository Server* needs its own *PostgreSQL* database server in order
to maintain the list of extension meta-data, in particular default and
available versions.

### Repository credentials and authentication

TODO

### Repository Server URLs

Public non authenticated API:

    http://host.domain.tld/list/extensions/
    http://host.domain.tld/list/<extname>/versions
    http://host.domain.tld/list/<extname>/updates
    http://host.domain.tld/get/<extname>/<version>

Authenticated API:

    http://host.domain.tld/upload/extname          PUT: archive bytes

    http://host.domain.tld/add/extension/extname   POST: uri, description
    http://host.domain.tld/rm/extension/extname
    http://host.domain.tld/update/extension/extname

    http://host.domain.tld/add/pgversion           POST: version, path/to/pg_config
    http://host.domain.tld/rm/pgversion

### Repository Server JSON format

TODO, per URL?

## The `pginstall` Build System

The *Build System* depends on *PGXS*, the extension build infrastructure
maintained within PostgreSQL sources. The `pginstall build` command builds
against all registered PostgreSQL versions for the current platform.

TODO: Also, the *Build System* edits the control file to add a `directory`
      parameter, in order to control where to store the extension's scripts
      and auxilliary control files and documentation, separately from the
      main control file.

#### What is a platform

TODO. OS name, OS version, Architecture, ...

#### Archive naming

TODO. The naming must take into account:

  - extension name
  - extension default_version
  - PostgreSQL target version(s)
  - Platform constraints if any (when using modules only)

## The `pginstall` Repository Manager

The *Repository Manager* is an helper application making it easier to
maintain ones own repository of extensions. It allows adding extension
*sources* to the repository. A *source* is an extension name, a location URI
and a description.

In its first version, `pginstall` only support git compatible URIs.

When adding an extension, it is being built locally then uploaded to the
locally configured *Repository Server*. To enable the same extension sources
to be available for more than one platform, it's necessary to build it as
many times as target platforms.

TODO: review the HTTP API so that it's easy to handle a build farm, or maybe
      include direct support for a build farm in the *Repository Manager*,
      in which case we might need to add a whole set or *HTTP URI* to handle
      that, and to have the *Build System* run an *HTTP* server too.

## The `pginstall` PostgreSQL Extension

This extension is a `ProcessUtility_hook` that gets involved for the
following commands:

    CREATE EXTENSION
    ALTER EXTENSION ... UPDATE ...

Its role is to check that the extension asked for is available, possibly in
the version that's being asked, and to then fetch the extension archive
(over HTTP, using [libcurl](http://curl.haxx.se/libcurl/)) and unpack its
files at the right places on the file system given the *Manifest* file in
the archive and the local client setup.

TODO: review the local setup

    pginstall.control_path = 'path/where/to/unpack/extension.control'
    pginstall.library_path = 'path/where/to/unpack/modules.so'


