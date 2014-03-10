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
                                          left join running r on r.queue = q.id
                                    where e.shortname = $1
                                          and r.done is null
                                 group by q.id, e.id"
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
                       insert into running(queue, animal)
                            select q.id, a.id
                              from queue q
                                     CROSS JOIN
                                  (platform p join animal a on a.platform = p.id)
                             where a.name = $1
                            except
                            select r.queue, a.id
                              from running r
                                   join animal a on r.animal = a.id
                             where a.name = $1
                             limit 1
                         returning running.*
                     )
                     select e.*
                       from extension e
                            join queue q on q.extension = e.id
                            join running r on r.queue = q.id"
                animal-name))))

(defun receive-archive (extension pgversion
                        animal-name os version arch
                        buildlog archive-filename archive)
  "Register a new extension's archive and move the file at the expected place."
  (let ((archive-full-name (merge-pathnames archive-filename *archive-path*)))
    (iolib.base:copy-file archive archive-full-name)

    (with-pgsql-connection (*dburi*)
      (car (query-dao 'archive
                  "with this_animal as (
                     select id from animal where name = $3
                   ),
                        this_platform as (
                     select id from platform
                      where os_name = $4 and os_version = $5 and arch = $6
                   ),
                        this_extension as (
                     select id from extension where fullname = $1
                   ),
                        done as (
                     update running r
                        set done = now()
                       from this_extension e, this_animal a, queue q
                      where     r.queue = q.id
                            and q.extension = e.id
                            and r.animal = a.id
                   ),
                       this_buildlog as (
                    insert into buildlog(extension, animal, log)
                         select e.id, a.id, $7
                           from this_extension e, this_animal a
                      returning *
                   ),
                         update as (
                     update archive a
                        set log = b.id, archive = $8
                       from this_buildlog b, this_extension e, this_platform p
                      where     a.extension = e.id
                            and a.platform = p.id
                            and a.pgversion = $2
                  returning a.*
                   ),
                         insert as (
              insert into archive(extension, platform, pgversion, log, archive)
                   select e.id, p.id, $2, b.id, $8
                     from this_extension e, this_platform p, this_buildlog b
                    where not exists (select 1
                                        from archive
                                       where     extension = e.id
                                             and platform = p.id
                                             and pgversion = $2)
                returning archive.*
                   )
                   select * from insert
                union all
                   select * from update
                    limit 1"
                  extension pgversion animal-name os version arch
                  buildlog
                  (namestring archive-full-name))))))

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

