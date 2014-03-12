The pginstall source code is organized into several module:

  - `animal/`

    The buildfarm *animal* is responsible for building *extensions*. That
    means `git clone` and `make install`, then packaging the resulting files
    into a platform specific *archive*.

    The *animal* connects to the *repository server* in order to know what
    extension to build, then connects again to upload the generated binary
    archives.

  - `client/`

    The main *pginstall client* is a PostgreSQL Extension that implements a
    `ProcessUtility_hook` to divert the commands `CREATE|ALTER|DROP
    EXTENSION`.
    
    The *pginstall client* downloads an extension archive file from the
    repository server and unpacks its content at the configured places. It
    also knows how to `sudo` itself into being *superuser* when dealing with
    the extension, so that unprivileged users are able to benefit from
    extensions.

  - `common/`

    The *pginstall server* and *buildfarm animal* both share some common
    code infrastructure, found in that directory.

  - `config/`
  
    Most of the configuration of the *pginstall* software is to be found in
    a PostgreSQL database, but for the *URI* of that database. A
    configuration file is necessary to bootstrap the *pginstall repository
    server*.
    
    The *pginstall buildfarm* doesn't use the pginstall PostgreSQL database
    at all, so all its setup is to be found in the configuration file,
    expected to be found at `~/.pginstall.ini`.

  - `doc/`
  
    The documentation of the *pginstall* software and its different
    components, covering the API specifications and how to setup and operate
    the software.
  
  - `main/`

    The command line interface of the *pginstall* software, allowing to
    control either the *repository server* or the *buildfarm animal*.

  - `repo/`
  
    The *pginstall repository server* code manages a list of extensions and
    buildfarm animal. Each animal runs a platform and one or more PostgreSQL
    development version to build extension against.
    
    Extension builds are queued on the server, the build animal fetches from
    the queue then uploads the resulting per-plaftorm and
    per-PostgreSQL-major-version binary archive to the repository server.
  
  - `server/`
  
    The *pginstall repository server* offers an HTTP API, where the API is
    partly implemented as a set of URL and a set of JSON documents.

