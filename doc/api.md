# PostgreSQL Extension Installer API

The PostgreSQL Extension Installer provides a *Repository Manager* that
exposes most of its features through an `HTTP` API. The best way to have a
overview of that API is with the source code implementation of it:

       ;; Extension API
       (:GET  "/api/list/extension" 'api-list-extension)
       (:GET  "/api/list/extension/:os/:version/:arch"
              'api-list-available-extensions)

       (:GET  "/api/fetch/:extension/:pgversion/:os/:version/:arch"
              'api-fetch-archive)

       (:POST "/api/add/extension" 'api-add-extension)

       ;; Buildfarm animal API
       (:GET  "/api/list/platform"         'api-list-platform)
       (:GET  "/api/list/animal"           'api-list-animal)

       (:GET  "/api/list/pgconfig/:animal" 'api-list-pgconfig)
       (:POST "/api/add/pgconfig/:animal"  'api-add-pgconfig)

       (:GET  "/api/get/work/for/:animal" 'api-get-work)
       (:POST "/api/upload/archive"       'api-upload-archive)

       ;; Repository server API
       (:GET  "/api/pick/my/name/:os/:version/:arch" 'api-pick-my-name)
       (:GET  "/api/register/animal/:name/:os/:version/:arch"
              'api-register-animal)
       (:GET  "/api/build/:extension"
              'api-queue-extension-build)

## Extension API

The Extension API allows to control stuff for extensions.

`/api/list/extension`

  ~ List all known extensions in the system, serving JSON straight from the
    database.
    
  ~ Here's a JSON format example for extensions:
  
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

`/api/list/platform`

  ~ List all registered platform on the server, as a single JSON document
  that looks like the following:
  
    [
      {
        "ARCH": "x86_64",
        "OS-VERSION": "10.9.1",
        "OS-NAME": "Mac OS X",
        "ID": 1
      }
    ]

## Repostitory Server API

The following API allows to control the Repository Server activity, which
centers around queueing extension builds and adding new extensions.
