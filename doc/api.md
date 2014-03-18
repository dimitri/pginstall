# PostgreSQL Extension Installer API

The PostgreSQL Extension Installer provides a *Repository Manager* that
exposes most of its features through an `HTTP` API.

In the following documentation of the API, you need to replace `:<name>`
with the name of the object you want to use. For example when you see the
docs for the `/api/build/:extension` API, if you want to queue a build for
the extension `prefix` you need to target the URL `/api/build/prefix`.

## JSON

The `HTTP API` returns JSON encoded objects that are described here.

## Extension API

The Extension API allows to control stuff for extensions.

### `GET /api/build/:extension`

  - Queue a build for the given extension's short name.
  
  - Returns a `JSON` document representing the queued extensions:

        $ curl -s http://localhost:8042/api/build/prefix | jq '.'
        {
          "DESCRIPTION": "prefix",
          "URI": "https://github.com/dimitri/prefix.git",
          "FULLNAME": "github.com/dimitri/prefix",
          "EXT-ID": 1,
          "ID": 27
        }

### `POST /api/add/extension`

  - Add an extension to the repository server.
  
  - Expected parameters are:
  
      - `fullname`, the extension's fullname;
      - `uri`, the uri of the extension's git repository, to use in a `git
        clone` command;
      - `description`, a description field.

### `GET /api/fetch/:extension/:pgversion/:os/:version/:arch`

  - Returns the extension's Archive Binary File for given PostgreSQL major
    version and platform, if such a file exists.
    
  - A `404` HTTP error is returned when the file does not exists.

### `GET /api/list/extension`

  - List all known extensions in the system, serving JSON straight from the
    database.
    
  - Here's a JSON format example for extensions:

        $ curl -s http://localhost:8042/api/list/extension | jq '.'
        [
          {
            "DESCRIPTION": "PL/Proxy",
            "URI": "https://github.com/markokr/plproxy-dev.git",
            "FULLNAME": "github.com/markokr/plproxy",
            "SHORTNAME": "plproxy",
            "ID": 2
          },
          {
            "DESCRIPTION": "prefix",
            "URI": "/Users/dim/dev/prefix",
            "FULLNAME": "github.com/dimitri/prefix",
            "SHORTNAME": "prefix",
            "ID": 1
          },
          {
            "DESCRIPTION": "software that plays chess written only on top of postgresql",
            "URI": "https://github.com/gciolli/pgchess.git",
            "FULLNAME": "github.com/gciolli/pgchess",
            "SHORTNAME": "pgchess",
            "ID": 3
          }
        ]

## Buildfarm Animal API

The Buildfarm Animal API allows animal in the buildfarm to communicate with
the main repository server, mainly fetching some work they have to do and
uploading archives.

### `GET /api/get/work/for/:animal`

  - Asks the repository server for some work to do for a known animal. The
    work is assigned when requested, which allows several animals on the
    same platform to appear whenever they want to and share the work among
    them.
    
    When a queue job is registered as *running* on an animal and the same
    animal uses the *get work* API again, the same job is immediately
    returned, with no modification made on the database. That allows a
    failing animal to come back and try again.
    
    When the animal keeps failing, the application currently has no way to
    clean up the state, the admin needs to issue
    
        DELETE FROM running
              WHERE animal = (SELECT id FROM animal WHERE name = '...')
                AND done IS NULL;

    Done is supposed to be the timestamp of when the build was registered
    terminated.
    
  - Returns the extension to build as a JSON object:
  
        $ curl -s http://localhost:8042/api/get/work/for/Sphinx | jq '.'
        {
          "DESCRIPTION": "prefix",
          "URI": "https://github.com/dimitri/prefix.git",
          "FULLNAME": "github.com/dimitri/prefix",
          "SHORTNAME": "prefix",
          "ID": 1
        }

### `POST /api/upload/archive`

  - The animal uses this API to upload the archive it builds for extensions.
  
  - Expected parameters are:
  
      - `extension`, the full name of the extension;
      - `pgversion`, the PostgreSQL Major Version used to build the archive;
      - `animal`, the animal name who just built the extension's archive;
      - `os-name`, the *Operating System Name* of the animal's current platform;
      - `os-version`, the *Operating System Version String* of the animal's
        current platform;
      - `arch`, the current animal's platform architecture;
      - `buildlog`, the full text log of the build operation;
      - `archive`, the binary archive contents.

### `GET /api/pick/my/name/:os/:version/:arch`

  - When registering an animal, the server will actually pick a free name
    from its registry and assign it to the animal, which is supposed to be
    using that API.
    
  - Returns a JSON document for the chosen animal.
  
### `GET /api/register/animal/:name/:os/:version/:arch`

  - Allows to register an animal and force its name.

### `GET /api/list/animal`

  - List all registered build farm animals on the server:
  
        $ curl -s 
        curl -s http://localhost:8042/api/list/animal | jq '.'
        [
          {
            "PLATFORM": 1,
            "NAME": "bat",
            "ID": 1
          },
          {
            "PLATFORM": 1,
            "NAME": "Griffon",
            "ID": 3
          },
          ...
        ]

### `GET /api/list/platform`

  - List all registered platform on the server, returns a JSON document:
  
        $ curl -s http://localhost:8042/api/list/platform | jq '.'
        [
          {
            "ARCH": "x86_64",
            "OS-VERSION": "10.9.1",
            "OS-NAME": "Mac OS X",
            "ID": 1
          },
          {
            "ARCH": "x86_64",
            "OS-VERSION": "7.1",
            "OS-NAME": "Debian",
            "ID": 3
          }
        ]        

### `GET /api/list/pgconfig/:animal`

  - List all registered `pg_config` entries for a given animal name.
  
        curl -s http://localhost:8042/api/list/pgconfig/Greek%20Griffin | jq '.'
        [
          {
            "CFLAGS": "-g -o2 -fstack-protector --param=ssp-buffer-size=4 -wformat -werror=format-security -fpic -pie -wall -wmissing-prototypes -wpointer-arith -wdeclaration-after-statement -wendif-labels -wformat-security -fno-strict-aliasing -fwrapv -fexcess-precision=standard -g",
            "CC": "gcc",
            "CONFIGURE": "'--with-tcl' '--with-perl' '--with-python' '--with-pam' '--with-openssl' '--with-libxml' '--with-libxslt' '--with-tclconfig=/usr/lib/tcl8.5' '--with-tkconfig=/usr/lib/tk8.5' '--with-includes=/usr/include/tcl8.5' 'python=/usr/bin/python' '--mandir=/usr/share/postgresql/9.1/man' '--docdir=/usr/share/doc/postgresql-doc-9.1' '--sysconfdir=/etc/postgresql-common' '--datarootdir=/usr/share/' '--datadir=/usr/share/postgresql/9.1' '--bindir=/usr/lib/postgresql/9.1/bin' '--libdir=/usr/lib/' '--libexecdir=/usr/lib/postgresql/' '--includedir=/usr/include/postgresql/' '--enable-nls' '--enable-integer-datetimes' '--enable-thread-safety' '--enable-debug' '--disable-rpath' '--with-ossp-uuid' '--with-gnu-ld' '--with-pgport=5432' '--with-system-tzdata=/usr/share/zoneinfo' 'cflags=-g -o2 -fstack-protector --param=ssp-buffer-size=4 -wformat -werror=format-security -fpic -pie' 'cppflags=-d_fortify_source=2 -dlinux_oom_adj=0' 'ldflags=-wl,-z,relro -wl,-z,now -wl,--as-needed' '--with-krb5' '--with-gssapi' '--with-ldap'",
            "VERSION": "postgresql 9.1.12",
            "PG-CONFIG": "/usr/lib/postgresql/9.1/bin/pg_config",
            "ANIMAL": 26,
            "ANIMAL-NAME": "Greek Griffin",
            "ID": 44
          },
          {
            "CFLAGS": "-g -o2 -fstack-protector --param=ssp-buffer-size=4 -wformat -werror=format-security -fpic -pie -i/usr/include/mit-krb5 -dlinux_oom_adj=0 -wall -wmissing-prototypes -wpointer-arith -wdeclaration-after-statement -wendif-labels -wmissing-format-attribute -wformat-security -fno-strict-aliasing -fwrapv -fexcess-precision=standard -g",
            "CC": "gcc",
            "CONFIGURE": "'--with-tcl' '--with-perl' '--with-python' '--with-pam' '--with-krb5' '--with-gssapi' '--with-openssl' '--with-libxml' '--with-libxslt' '--with-ldap' '--with-tclconfig=/usr/lib/tcl8.5' '--with-tkconfig=/usr/lib/tk8.5' '--with-includes=/usr/include/tcl8.5' 'python=/usr/bin/python' '--mandir=/usr/share/postgresql/9.2/man' '--docdir=/usr/share/doc/postgresql-doc-9.2' '--sysconfdir=/etc/postgresql-common' '--datarootdir=/usr/share/' '--datadir=/usr/share/postgresql/9.2' '--bindir=/usr/lib/postgresql/9.2/bin' '--libdir=/usr/lib/' '--libexecdir=/usr/lib/postgresql/' '--includedir=/usr/include/postgresql/' '--enable-nls' '--enable-integer-datetimes' '--enable-thread-safety' '--enable-debug' '--disable-rpath' '--with-ossp-uuid' '--with-gnu-ld' '--with-pgport=5432' '--with-system-tzdata=/usr/share/zoneinfo' 'cflags=-g -o2 -fstack-protector --param=ssp-buffer-size=4 -wformat -werror=format-security -fpic -pie -i/usr/include/mit-krb5 -dlinux_oom_adj=0' 'ldflags=-wl,-z,relro -wl,-z,now -wl,--as-needed -l/usr/lib/mit-krb5 -l/usr/lib/x86_64-linux-gnu/mit-krb5' 'cppflags=-d_fortify_source=2'",
            "VERSION": "postgresql 9.2.7",
            "PG-CONFIG": "/usr/lib/postgresql/9.2/bin/pg_config",
            "ANIMAL": 26,
            "ANIMAL-NAME": "Greek Griffin",
            "ID": 45
          },
          {
            "CFLAGS": "-g -o2 -fstack-protector --param=ssp-buffer-size=4 -wformat -werror=format-security -fpic -pie -i/usr/include/mit-krb5 -dlinux_oom_score_adj=0 -wall -wmissing-prototypes -wpointer-arith -wdeclaration-after-statement -wendif-labels -wmissing-format-attribute -wformat-security -fno-strict-aliasing -fwrapv -fexcess-precision=standard -g",
            "CC": "gcc",
            "CONFIGURE": "'--with-tcl' '--with-perl' '--with-python' '--with-pam' '--with-openssl' '--with-libxml' '--with-libxslt' '--with-tclconfig=/usr/lib/tcl8.5' '--with-tkconfig=/usr/lib/tk8.5' '--with-includes=/usr/include/tcl8.5' 'python=/usr/bin/python' '--mandir=/usr/share/postgresql/9.3/man' '--docdir=/usr/share/doc/postgresql-doc-9.3' '--sysconfdir=/etc/postgresql-common' '--datarootdir=/usr/share/' '--datadir=/usr/share/postgresql/9.3' '--bindir=/usr/lib/postgresql/9.3/bin' '--libdir=/usr/lib/' '--libexecdir=/usr/lib/postgresql/' '--includedir=/usr/include/postgresql/' '--enable-nls' '--enable-integer-datetimes' '--enable-thread-safety' '--enable-debug' '--disable-rpath' '--with-ossp-uuid' '--with-gnu-ld' '--with-pgport=5432' '--with-system-tzdata=/usr/share/zoneinfo' 'cflags=-g -o2 -fstack-protector --param=ssp-buffer-size=4 -wformat -werror=format-security -fpic -pie -i/usr/include/mit-krb5 -dlinux_oom_score_adj=0' 'ldflags=-wl,-z,relro -wl,-z,now -wl,--as-needed -l/usr/lib/mit-krb5 -l/usr/lib/x86_64-linux-gnu/mit-krb5' '--with-krb5' '--with-gssapi' '--with-ldap' 'cppflags=-d_fortify_source=2'",
            "VERSION": "postgresql 9.3.3",
            "PG-CONFIG": "/usr/lib/postgresql/9.3/bin/pg_config",
            "ANIMAL": 26,
            "ANIMAL-NAME": "Greek Griffin",
            "ID": 46
          },
          {
            "CFLAGS": "-g -o2 -fstack-protector --param=ssp-buffer-size=4 -wformat -werror=format-security -fpic -pie -i/usr/include/mit-krb5 -dlinux_oom_score_adj=0 -wall -wmissing-prototypes -wpointer-arith -wdeclaration-after-statement -wendif-labels -wmissing-format-attribute -wformat-security -fno-strict-aliasing -fwrapv -fexcess-precision=standard -g",
            "CC": "gcc",
            "CONFIGURE": "'--with-tcl' '--with-perl' '--with-python' '--with-pam' '--with-openssl' '--with-libxml' '--with-libxslt' '--with-tclconfig=/usr/lib/tcl8.5' '--with-tkconfig=/usr/lib/tk8.5' '--with-includes=/usr/include/tcl8.5' 'python=/usr/bin/python' '--mandir=/usr/share/postgresql/9.3/man' '--docdir=/usr/share/doc/postgresql-doc-9.3' '--sysconfdir=/etc/postgresql-common' '--datarootdir=/usr/share/' '--datadir=/usr/share/postgresql/9.3' '--bindir=/usr/lib/postgresql/9.3/bin' '--libdir=/usr/lib/' '--libexecdir=/usr/lib/postgresql/' '--includedir=/usr/include/postgresql/' '--enable-nls' '--enable-integer-datetimes' '--enable-thread-safety' '--enable-debug' '--disable-rpath' '--with-ossp-uuid' '--with-gnu-ld' '--with-pgport=5432' '--with-system-tzdata=/usr/share/zoneinfo' 'cflags=-g -o2 -fstack-protector --param=ssp-buffer-size=4 -wformat -werror=format-security -fpic -pie -i/usr/include/mit-krb5 -dlinux_oom_score_adj=0' 'ldflags=-wl,-z,relro -wl,-z,now -wl,--as-needed -l/usr/lib/mit-krb5 -l/usr/lib/x86_64-linux-gnu/mit-krb5' '--with-krb5' '--with-gssapi' '--with-ldap' 'cppflags=-d_fortify_source=2'",
            "VERSION": "postgresql 9.3.3",
            "PG-CONFIG": "/usr/bin/pg_config",
            "ANIMAL": 26,
            "ANIMAL-NAME": "Greek Griffin",
            "ID": 47
          }
        ]

## Repostitory Server Remote Control

The following API allows to control the Repository Server activity, which
centers around queueing extension builds and adding new extensions.

### `GET /api/config`

  - Returns the server's current configuration as an INI file:
  
        $ curl -s http://localhost:8042/api/config
        [animal]
        server = http://localhost:8042/
        git = /usr/bin/git
        gmake = /usr/bin/make
        build-root = /tmp/pginstall/
        name = Sphinx
        
        [server]
        archive-path = /tmp/var/cache/pginstall/
        http-logfile = /tmp/pginstall-http.log
        repo-logfile = /tmp/pginstall-repo.log
        listen-port = 8042
        dburi = postgresql:///pginstall

### `GET /api/status`

  - Returns `"OK"` when the server is running fine, or something else when
    it's not the case:
    
        $ curl -s http://localhost:8042/api/status
        OK
        
### `GET /api/reload`

  - Reload the server's configuration file, and returns it in the same
    format as when using the `GET /api/config` API.

### `GET /api/terminate/yourself`

  - Ask the server to quit serving, which it does after returning a message
    to inform you that it did receive the command.
