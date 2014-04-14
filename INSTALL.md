# Installing pginstall

pginstall has been written mainly in Common Lisp for the repository server
and build animal parts, and in C for the PostgreSQL embedded client parts.

## The lisp parts

The steps depend on the OS you are currently using.

### debian

If you're using debian, it's quite simple actually, see the file
`bootstrap-debian.sh` within the main pginstall distribution to get yourself
started.

You will note in particular:

    sudo apt-get install -y postgresql-9.3 postgresql-contrib-9.3 \
                            sbcl                                  \
                            git patch unzip                       \
                            devscripts pandoc                     \
                            postgresql-server-dev-all             \
                            libarchive-dev libfixposix-dev        \
                            libjansson-dev libcurl4-openssl-dev   \
                            libbsd-dev

We need a recent enough [SBCL](http://sbcl.org/) version and that means
backporting the one found in `sid` rather than using the very old one found
in current *stable* debian release.

Also, the `libfixposix` software has not been updated in debian for a long
time, so we grab it from its maintainer own repository, at
`http://download.opensuse.org/repositories/home:/sionescu/Debian_Wheezy/`.

### Mac OS X

We suppose you already have `git` and `make` available, if that's not the
case now is the time to install those tools.

You need to fetch a custom version of `libfixposix` and compile from sources:

    git clone https://github.com/sionescu/libfixposix.git
    cd libfixposix
    ./configure
    make install

Then you need to install about the same set of software dependencies that we
need, and the process has been tested only with using the `brew` packaging
tools:

    brew install libarchive
    brew install jansson

## Building the pginstall tool

Now that the dependences are installed, just type make.

    make

If using Mac OS X, and depending on how you did install `SBCL` and which
version you have (the brew default did change recently), you might need to
ask the Makefile to refrain from trying to compress your binary image:

    make COMPRESS_CORE=no

Then you will have a new tool to play with:

    ./build/bin/pginstall
    
This command should spit out the *usage* information on which parameters are
accepted in the command line actually.

## PostgreSQL

For running the repository server, a PostgreSQL database that you can
connect to is necessary, and for trying the client parts, you need to be
superuser and have access to the underlying file system... hopefully for the
last time around.

## Setup and basic testing

The basics:

    pginstall server setup pgsql:///pginstall
    pginstall server start
    
    pginstall server status
    pginstall server config

Then you can actually queue an extension and build it locally, provided that
you first register at least an animal, such as your own laptop where the
repository server is now running:

    pginstall animal register
    pginstall queue prefix
    pginstall build

Currently, an animal only builds extensions that are queued in the
system. That's going to change soon, stay tuned.

## PostgreSQL client setup and basic testing

You need to refer to the main docs about that, we're not duplicating the
contents, see the README.md file.
