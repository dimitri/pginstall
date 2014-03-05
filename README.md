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
    a list of extensions and prepare the binary archives for those,
    
  - A *Build Farm* composed of one or more *Animals* each able of building
    extensions for one or more PostgreSQL versions.

In the first version of the `pginstall` application, the build system and
the repository management applications are provided with a *Command Line
Interface* only. In a later version, a full control web application might be
added so as to make it easier to use in some cases.

## A distributed Software Architecture

A single server is provided, which is able to serve all given roles. When
building an extension locally on your development machine, a single server
is expected to allow you to:

  - build your extension,
  - publish it,
  - serving it to a testing PostgreSQL client.

The setup of the system is maintained in plain text files, so that it's
possible to setup the PostgreSQL database service location. It also makes it
possible to run a *Build Farm Animal* wihout ever needing to run a
PostgreSQL database service.

The PostgreSQL service is used to maintain the *Repository Server* data.

## A single Command Line application to rule them all

The whole set of features is contained within a single application binary
named `pginstall`, except for the client-side application that needs to be a
separate PostgreSQL extension.

    pginstall config dburi postgresql://host:port/dbname    
    pginstall config listen-port 8042
    pginstall config server-uri http://pginstall.mydomain.tld:8042/
    pginstall config archive-path /var/cache/pginstall
    
    pginstall start
    pginstall stop
    pginstall status
    pginstall reload
    pginstall restart

    pginstall config animal-name bat
    pginstall config build-root /tmp/pginstall
    pginstall config gmake /path/to/gmake

    pginstall animal name bat
    pginstall animal register server-uri
    pginstall animal pg ls
    pginstall animal pg add /path/to/pg_config
    pginstall animal pg rm /path/to/pg_config

    pginstall animal ls
    pginstall animal build extname /path/to/pg_config
        
    pginstall repo ls
    pginstall repo add extension-full-name uri description
    pginstall repo rm extname [ ... ]
    
    pginstall repo build extname
    pginstall repo build-world

    pginstall repo update extname
    pginstall repo update-world

Where `extension-full-name` is expected to be an *Extension Full Name* as
described below, and `extname` is an *Extension Name*, which is either an
*Extension Full Name* or an *Extension Short Name*.

## Extension Names and Avoiding Singleton Central Registries

An *extension name* is either a *short name* or a *full name*, where the
*full name* of the extension allows the system to identify it uniquely,
without resorting to a separate registry service.

The *full name* of an extension is a partial URI where the *scheme name*,
*query* and *fragment* have been omited. An example of such an *Extension
Full Name* is `github.com/dim/prefix`.

The *short name* of an extension is the last element of the URL *path*. For
example, the *Extension Short Name* of `github.com/dim/prefix` is `prefix`.

Everywhere an extension name is expected a *short name* or a *full name* may
be given. When a *short name* is given and more than a single *full name*
are matching it, then an error occurs and the candidates are made available.
The user is then expected to choose among the candidates and try again with
the extension *full name* of his or her choice.

## The Extension Archive Format

An Extension Archive is a very simple `.tar.gz` file containing the main
extension control at its root location and a directory named after the
extension short name. The directory containst the library, scripts and
auxilliary control files.

The directory is *relocatable* in as so much as the pginstall client will
tweak the control file parameters *directory* and *module_pathname* at
`CREATE EXTENSION` time.

## The `pginstall` Repository Server

The role of the *Repository Server* is to publish the list of available
extensions and to deliver the *binary archive* for any specific extension,
or extension's version.

<!-- TODO: check how PostGIS works against *Full Names* and URIs.

It's possible too register the same *extension name* several time: the
unique identifier of an *extension archive* is composed of the *name* and
the *default_version* of the extension. Then the whole list of upgrade paths
are maintained in the *Repostory Server* to better serve clients.

 -->

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

    http://host.domain.tld/add/extension/extname   POST: uri, description
    http://host.domain.tld/rm/extension/extname
    http://host.domain.tld/update/extension/extname

    http://host.domain.tld/animal/register         POST: json "specs"
    http://host.domain.tld/animal/list/pg
    http://host.domain.tld/animal/build/extname    application/octet-stream
    
    http://host.domain.tld/build/extname
    http://host.domain.tld/update/extname

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

## The `pginstall` Build Farm

The *Build Farm* is a service of the *Repository Server*, and the
*Repository Server* itself is a buildfarm member by default.

Each *Animal* in the *Build Farm* is running a full server, and may or may
not serve a repository. A PostgreSQL database service is only needed to run
the repository, running a *Bare Animal* does not require the database
service.

Each *Animal* provides at least one *PostgreSQL Server Development
Environment* such as provided by a `postgresql-server-dev-X.Y` debian
package.

Each *Animal* must register itself to its *Repository Server*, and the local
`server` configuration item is then published. The `server` must be an URI
that the *Repository Server* knows how to communicate with using the `HTTP`
protocol.

When registering the *Animal* published to the *Repository Server* its
specification, which are the detailed list of the PostgreSQL development
environments available, local architecture and other relevant platform
information.

The *Repository Server* can then ask an *Animal* to build an extension using
the HTTP URL `/animal/build/extname`, sending the full path of the
`pg_config` to use, and the result is the archive artefact obtained with the
specified `pg_config` build setup.

TODO: Maybe send an URI where to publish the artefact instead, and have the
      `animal/build/extname` return the build log while it happens instead.
      When it's done, the animal can then hit previously given URI and `PUT`
      there the just build artefact.

#### What is a platform

TODO. OS name, OS version, Architecture, ...

#### Archive naming

TODO. The naming must take into account:

  - extension full name
  - extension default_version
  - PostgreSQL target version(s)
  - Platform constraints if any (when using modules only)

## The `pginstall` Repository Manager

The *Repository Manager* is an helper application making it easier to
maintain ones own repository of extensions. It allows adding extension
*sources* to the repository. A *source* is an extension full name, a
location URI and a description.

Note: In its first version, `pginstall` only support github compatible URIs.

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

    CREATE EXTENSION extname;
    ALTER EXTENSION extname UPDATE ...

Its role is to check that the extension asked for is available, possibly in
the version that's being asked, and to then fetch the extension archive
(over HTTP, using [libcurl](http://curl.haxx.se/libcurl/)) and unpack its
files at the right places on the file system given the *Manifest* file in
the archive and the local client setup.

When unambiguous, it's possible to use the *Extension Short Name* in the
`CREATE EXTENSION` and `ALTER EXTENSION` commands. If there's more than one
known *Extension Full Name* matching the given *Extension Short Name*, an
ERROR is signaled with the list of alternatives to choose from.

    $ CREATE EXTENSION prefix;
    ERROR: Extension short name "prefix" matches several providers
    DETAIL: The extensions "github.com/dim/prefix" and
            "github.com/fdr/prefix" matches the short name "prefix".
    HINT: Please use the full name of the extension you want to install.
  
    $ CREATE EXTENSION "github.com/fdr/prefix";
    CREATE EXTENSION

TODO: review the local setup

    pginstall.control_path = 'path/where/to/unpack/extension.control'
    pginstall.library_path = 'path/where/to/unpack/modules.so'


# Hosting internal (private) software

In order to allow for private organisation to use the `pginstall` system
with their own private code (PL or C coded) added to a list of whitelisted
extensions, pginstall allows to mirror and merge *upstream repositories*.

## Mirroring

TODO: describe how to setup mirroring from one or several *upstream
repositories*.

## Merging

TODO: describe how to setup pginstall to host some local packages and merge
packages from an *upstream repository*.
