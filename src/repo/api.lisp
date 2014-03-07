;;;
;;; The implementation of the API exposed through the server.
;;;

(in-package #:pginstall.repo)

(defun select-star (type)
  "Return a list of all extensions known to the system."
  (with-pgsql-connection (*dburi*)
    (select-dao type)))

(defun queue-extension-build (extension-name)
  "Queue a build request for EXTENSION-NAME."
  (with-pgsql-connection (*dburi*)
    (let ((exists (car (query-dao 'build-queue
                                   "select q.id, e.id as ext_id,
                                           e.fullname, e.uri, e.description
                                     from queue q
                                          join extension e on q.extension = e.id
                                    where e.shortname = $1"
                                   extension-name))))
      (or exists
          (query-dao 'build-queue
                      "with queue as (
                         insert into queue (extension)
                              select id from extension where shortname = $1
                           returning id, extension
                       )
                       select q.id, e.id as ext_id,
                              e.fullname, e.uri, e.description
                         from queue q
                              join extension e on q.extension = e.id"
                      extension-name)))))

(defun queue-get-work (animal-name)
  "Return an Extension object from the build queue"
  (with-pgsql-connection (*dburi*)
    (car (query-dao 'extension
                    "with running as (
                       insert into running(extension, animal)
                            select q.extension, a.id
                              from queue q
                                     CROSS JOIN
                                  (platform p join animal a on a.platform = p.id)
                             where a.name = $1
                            except
                            select r.extension, a.id
                              from running r
                                   join animal a on r.animal = a.id
                             where a.name = $1
                             limit 1
                         returning running.*
                     )
                     select e.*
                       from extension e
                            join running on e.id = running.extension"
                animal-name))))

(defun select-extensions-available-on-platform (os version arch)
  "Return the list of available extensions on a given platform."
  (with-pgsql-connection (*dburi*)
    (query-dao 'extension
               "select e.*
                  from extension e
                       join archive a on a.extension = e.id
                       join platform p on p.id = a.platform
                  where     p.os_name = $1
                        and p.os_version = $2
                        and p.arch = $3"
                os version arch)))
