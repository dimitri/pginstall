# pginstall PostgreSQL Extension

The pginstall extension allows PostgreSQL to fetch extensions at `CREATE
EXTENSION` time, making them locally available.

To make extensions locally available, pginstall downloads an extension
*archive* then installs the files contained therein at a location where
PostgreSQL expects them.

The extensions archives themselves are made *relocatable* by the pginstall
build system, so that the contents of the extensions (module, scripts, etc)
can be installed wherever you want on the file system. Only the main control
file needs to be located someplace PostgreSQL expects them, which defaults
to:

    $(pg_config --sharedir)/extension

Some extensions do require *superuser* privileges to be installed, and
pginstall allows the local system administrator to enable auto-escalation to
*superuser* for running the `CREATE EXTENSION`, `ALTER EXTENSION` or `DROP
EXTENSION` commands when the parameter `pginstall.sudo` is set to true.

It's also possible for the admins to run custom pre- or post- installation
and upgrade scripts for certain extensions, see the `pginstall.custom_path`
setting and its documentation below.

## Setup

To be able to use the pginstall extension you need to edit your
`postgresql.conf` file and add the following entries:

* `local_preload_libraries`

  Add `pginstall` to the `local_preload_libraries` setting. Don't forget to
  add the module in the `$plugin` directory.

* `custom_variable_classes`

  Add `pginstall` to the `custom_variable_classes` setting if you're using
  9.1, in 9.2 and following this setting disapeared.

* `pginstall.archive_dir`

  Filesystem path where to download extensions *archives* to.
  
* `pginstall.control_dir`

  Filesystem path where to install extensions control files, defaults to
  `$(pg_config --sharedir)/extension/`.
  
* `pginstall.extension_dir`

  Filesystem path where to install extension contents: module, scripts,
  auxilliary control files and docs.
  
* `pginstall.repository`

  HTTP URL to use to fetch available extensions and archives from the
  repository server, something like `http://pginstall.postgresql.org/`.
  
* `pginstall.serve_from_archive_dir`

  Allow the `archive_dir` repository to be used as a local repository
  server. Any archive file that has been made available here will be used
  directly. Beware that this will prevent refreshing files from the
  `pginstall.repository` server, acting as the authoritative source for the
  archive files found in there. Useful when using `pginstall build
  <fullname>` in a development environment.

* `pginstall.sudo`

  Allow current user to aquire *superuser* privileges when executing the
  `CREATE EXTENSION` command.

* `pginstall.custom_path`

  Filesystem path where to look for *custom scripts*, as described below.

## Installation

To use some parts of the user interface

## Usage

The main interface is the usual `CREATE EXTENSION` command, which is
modified by the pginstall extension:

    CREATE EXTENSION foo;
    ALTER EXTENSION foo UPDATE;

The pginstall extension also provides the following system views:

* `pginstall.pg_available_extensions`

  Returns a list of extensions available on the repository server, for the
  current platform.
  
* `pginstall.pg_available_extension_versions`

  Returns a list of extensions and versions available on the repository
  server, for the current platform.


## Custom Scripts

Some extensions are installing objects that only the *superuser* can make
use of by default, it's then a good idea to tweak permissions and grant
usage to the *current_user* or even the *database owner*, depending.

The custom scripts feature allows to do that by providing scripts to be run
around the execution of the extension's script itself.

#### create extension custom scripts

For the creation of extension `extname` version `1.0` the following scripts
will be used when they do exist, as shown here:

  #. `${extwlist.custom_path}/extname/before--1.0.sql`

  #. `${extwlist.custom_path}/extname/before-create.sql`, only when the
     previous one, specific to the version being installed, does not exists.
	
  #. The `CREATE EXTENSION` command now runs normally

  #. `${extwlist.custom_path}/extname/after--1.0.sql`

  #. `${extwlist.custom_path}/extname/after-create.sql`

#### alter extension update custom scripts

For the update of extension `extname` from version `1.0` to version `1.1`
the following scripts will be used when they do exist, as shown here:

  #. `${extwlist.custom_path}/extname/before--1.0--1.1.sql`

  #. `${extwlist.custom_path}/extname/before-update.sql`, only when the
   previous one does not exists.
	
  #. The `ALTER EXTENSION UPDATE` command now runs normally

  #. `${extwlist.custom_path}/extname/after--1.0--1.1.sql`

  #. `${extwlist.custom_path}/extname/after-update.sql` only when the
     previous one, specific to the versions being considered, does not
     exists.

#### custom scripts templating

Before executing them, the *extwlist* extension applies the following
substitions to the *custom scripts*:

  - any line that begins with `\echo` is removed,

  - the literal `@extschema@` is unconditionnaly replaced by the current
    schema being used to create the extension objects,

  - the literal `@current_user@` is replaced by the name of the current
    user,
	
  - the literal `@database_owner@` is replaced by the name of the current
    database owner.

Tip: remember that you can execute `DO` blocks if you need dynamic sql.

## Internals

The whitelisting works by overloading the `ProcessUtility_hook` and gaining
control each time a utility statement is issued. When this statement is a
`CREATE EXTENSION`, the extension's name is extracted from the `parsetree`
and checked against the whitelist. *Superuser* is obtained as in the usual
`SECURITY DEFINER` case, except hard coded to target the *bootstrap user*.
