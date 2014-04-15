# Building PostgreSQL Extensions Archives

Is also a PostgreSQL extension itself, and quite the extension if you ask
me: it diverts the `CREATE EXTENSION` command to download binary archives
for you, at PostgreSQL run time.

## Building Extensions

Two modes of operations are provided: you can either connect to an existing
repository server and download archive files from there, or use locally
built extensions directly.

### Repository Server

When running a repository server, you can *register* one or more buildfarm
animals to it, and each of them is supposed to be building extensions for
you. The *repository server* tells the buildfarm animals what to build, and
they pick up a job by running the following command:

    pginstall animal build

Which extension is going to be built that way is entirely up to the
repository server.

### Building on your local machine

As a developer, if you want to just build an existing extension locally,
without running the repository server and registering your own *laptop* (or
any computer really) to it as a *buildfarm* member, you can use the
following command line:

    pginstall build github.com/dimitri/prefix

Where an *extension full name* argument is expected, and at the moment only
github URLs are supported.

Here's a full screen example of such a *local build*:

    ~/dev/pginstall ./build/bin/pginstall build github.com/dimitri/prefix
    Building extension github.com/dimitri/prefix
              git clone "https://github.com/dimitri/prefix.git"
              building with "/Users/dim/pgsql/ddl/bin/pg_config"
    
    Built: /tmp/pginstall/prefix--9.4devel--Mac_OS_X--10.9.2--x86_64.tar.gz 
     logs: /tmp/pginstall/logs/prefix.txt

