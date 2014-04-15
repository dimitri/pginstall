# The PostgreSQL Extension Installer Build Farm

The role of the PostgreSQL Extension Installer Build Farm is to produce
those *platform-specific* *Extension Archive Files* that `pginstall` clients
want to download.

## Animals

The build farm is composed of a herd of animals that are given packaging
work to do. The Animals must connect and use the API to get the next build
job from the queue, which is done with the following command:

    pginstall animal build

When the build is done, the result usually is a set of archives files, one
per PostgreSQL Major Version. Using the API, those files are then uploaded
to the Repository Manager.

## Setting up a new Build Farm Animal

A Build Farm Animal needs to be able to talk to the Repository Server in
order to be fed with work to do, then to upload the archives it's been
building. Also, the animal needs a name and to register the `pgconfig` paths
available to build extensions, to make it easier for us humans to track
things:

    pginstall config set server http://pginstall.mycompany.com:8042/
    pginstall animal register
    pginstall animal whoami

If you have installed several version of the `make` and `git` tools and want
to make sure that pginstall will pick the right one, you can use the
following settings:

    pginstall config set gmake /usr/local/bin/gmake
    pginstall config set git /usr/local/bin/git

The final step is to setup where to build the extensions, then you can tell
the animal to have at the build queue:

    pginstall config set build-root /home/pginstall/build
    pginstall animal build
    
Note that the command `pginstall animal build` is an alias to the command
`pginstall build`.

## The Animal `pgconfig` list

The command `pginstall animal register` will find the existing `pg_config`
programs available on the current build machine. You can check that list
with the following commands:

    pginstall animal find pgconfig
    pginstall animal list pgconfig

The `find` command finds again all `pg_config` binaries that you have on
your system while the `list` command lists those that you have already
registered against the Repository Server.

To find the list of `pg_config` build environments to use, pginstall search
in the following places:

  - current `$PATH`
  - debian style `/usr/lib/postgresql/X.Y/bin`
  - centos style `/usr/pgsql-X.Y/bin`
  
Any path entry containing `X.Y` will be used as a pattern and the template
is replaced in a loop with the value `9.1`, `9.2`, `9.3` and `9.4`.

To add your own `pg_config` entries to the list of build environments used
by pginstall, tweak the `PATH` under which you run the `pginstall animal
build` command.

You can register newly added environments with the command

    pginstall animal add pgconfig /path/to/X.Y/bin/pg_config

## Building an extension's archive

Each time you queue an extension for building, the animal will do the
following steps:

 1. Clean up any existing build directory for the extension.
 
 2. Fetch the extension sources with `git clone`.
 
 3. Find all `pg_config` binaries located at the following places, where
    `X.Y` gets expaneded to the PostgreSQL versions `9.1`, `9.2`, `9.3` and
    `9.4`:
 
      - `/usr/lib/postgresql/X.Y/bin`
      - `/usr/pgsql-X.Y/bin`
      - `$PATH`
      
 4. For each `pg_config` PostgreSQL development environment found, build the
    extension using the following commands:
    
      - `git clean -fdx`
      - `make PG_CONFIG=... DESTDIR=... install`

 5. Prepare the archive directory layout and copy files from the built
    result in their archive location:
    
      - the control file goes to the main directory,
      - the script, module and doc files go to the subdirectory named the
        same as the extension's short name.
        
    Files are found in locations we get from the following properties of the
    `pg_config` output for `--docdir`, `--pkglibdir` and `--sharedir`.
    
 6. Pack each archive as a `.tar.gz` file and upload them all to the
    repository server. The archive filename is made from the short name of
    the extension, the PostgreSQL version it's been built against, the
    animal's platform operating system name, the animal's platform operating
    system version and the animal's platform architecture.


