# pginstall(1) -- PostgreSQL data loader

## SYNOPSIS

`pginstall` [<options>] [<command-file>]...

## DESCRIPTION

pginstall is The PostgreSQL Extension Installer server.

## OPTIONS

  * `-h`, `--help`:
    Show command usage summary and exit.

  * `-V`, `--version`:
    Show pginstall version string and exit.

  * `-v`, `--verbose`:
    Be verbose.

  * `-d`, `--debug`:
    Show debug level information messages.

  * `-c`, `--config`:
    Use the given configuration file (default to "~/.pginstall.ini").

## COMMANDS

The pginstall binary allows both running and controling the pginstall server
and managing extension builds and upload from a buildfarm animal.

### CONFIGURATION CONTROL

While it's possible to ship a configuration file or to prepare it by hand,
the following commands allow to control the setup from the command line.

  - `config [ name ] [ value ]`
     
    Without arguments, print the whole configuration file content. When
    given a variable *name*, print its current value. When given both a
    *name* and a *value*, set the configuration variable to the given value.
     
  - `config get <name>`
    
    Print the current value of the configuration variable *name*.

  - `config set <name> <value>`
    
    Set the variable *name* to the given *value*.

### SERVER CONTROL

The PostgreSQL Extension Installer comes with a PostgreSQL plugin that
downloads static files: that part doesn't need any server at all. This
server is meant to be used by maintainers of a set of extension archives,
when they want to ease the maintenance and setup of the building.

  - `server start`
     
    Start the embedded pginstall HTTP server on the port it's been setup to
    listen to, which defaults to 8042. The
     
  - `server stop`
     
    Stops the server.
     
  - `server status`
    
    Print the result of querying the HTTP status API against the (hopefully)
    running server.

  - `server status`
    
    Print the registered pid of the server process. This information might
    be stale in case of unexpected termination of the server.

  - `server reload`
    
    Forces the server to reload its configuration file.
    
  - `server setup <dburi>`
    
    Connects to the PostgreSQL database specified with the *dburi* parameter
    and install the database model there.

### BUILD FARM ANINAL CONTROL

The PostgreSQL Extension Installer allows controlling a build farm where
*animal* are building extensions binary archives to be distributed to users.

  - `animal register`
    
    Before using an animal as a build farm member, it needs to register
    against the server. That only allows the server to remember if a build
    has been started on some animal. This command assigns a name (randomly,
    from an hard-coded list) to the current animal and sets the name into
    its configuration file.
    
  - `animal find pgconfig`
    
    Search in the `$PATH` for `pg_config` entries to use when building an
    extension. That list is dynamically built each time an extension is to
    be built, so the command serves to debug this animal automated setup.
    
  - `animal list pgconfig`
    
    Print the `pg_config` entries known to the server for this animal. The
    list is sent at register time and can evolve without pginstall being
    involved.
    
  - `animal add pgconfig <path>`
     
     Send a new `pg_config` path entry to the server, for maintaining the
     information up-to-date.
     
  - `animal config`
    
    Print the current animal configuration.
    
  - `animal build`
    
    Ask the server for an extension to build for our platform, then build
    the extension archives (one archve per `pg_config` entry) and upload
    them all on the server.
    
  - `whoami`
    
    Print information about the current animal: its name and platform.
    
### DEVELOPPER USAGE (SINGLE MACHINE)

It's also possible to use pginstall as a developper, to simply build an
archive for a given extension.

  - `build <uri>`
    
    Build the extension whose sources are to be found at the given git *uri*.

### MANAGING EXTENSIONS

The pginstall server is meant to manage a list of extensions. While it's
possible to use the embedded web server to list and add extensions, it's
also possible to do it on the command line.

  - `extension list`

    List all extensions known on the server.
  
  - `extension add <uri>`
  
    Add an extension to the server, given its github *uri*.
  
  - `queue <name>`
  - `extension queue <name>`
  
    Queue an extension to be built on by the build farm animals. That way
    you can schedule in a single command a build for all the binary
    architectures you support.
  
  - `fetch <name> <pgversion> [ <dir> ]`

    Fetches the archive file for a given extension *name* and a specific
    PostgreSQL version *pgversion*, in either the current working directory
    or given *dir*, by asking the pginstall server.

## AUTHOR

Dimitri Fontaine <dimitri@2ndQuadrant.fr>

## SEE ALSO

The pginstall source code and all documentation may be downloaded from
<https://github.com/dimitri/pginstall/>.
