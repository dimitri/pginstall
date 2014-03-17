;;;
;;; The implementation of the API exposed through the server.
;;;

(in-package #:pginstall.repo)

(defun queue-extension-build (extension-name)
  "Queue a build request for EXTENSION-NAME."
  (with-pgsql-connection (*dburi*)
    (or (query "select q.id, e.id as ext_id,
                       e.fullname, e.uri, e.description
                 from queue q
                      join extension e on q.extension = e.id
                      left join running r on r.queue = q.id
                where e.shortname = $1
                      and r.done is null
             group by q.id, e.id
                limit 1"
               extension-name (:dao build-queue :single))
        (query "with queue as (
          insert into queue (extension)
               select id from extension where shortname = $1
            returning id, extension
        )
        select q.id, e.id as ext_id,
               e.fullname, e.uri, e.description
          from queue q
               join extension e on q.extension = e.id"
                   extension-name (:dao build-queue :single)))))

(defun queue-get-work (animal-name)
  "Return an Extension object from the build queue"
  (with-pgsql-connection (*dburi*)
    (query "with pending(queue, platform) as (
                select q.id, p.id
                  from queue q
                       cross join (     platform p
                                   join animal a on a.name = $1
                                                and a.platform = p.id
                                  )
                 except

                 select r.queue, p.id
                   from running r
                        join animal a on a.id = r.animal
                        join platform p on p.id = a.platform
            ),
                 pick_one as (
               select array_agg(queue) as queue
                 from pending p
                      join queue q on q.id = p.queue
             group by extension
                limit 1
            ),
                 work as (
              insert into running(queue, animal)
                   select unnest(po.queue),
                          (select id from animal where name = $1)
                from pick_one po
           returning running.queue
            )
               select e.*
                 from extension e
                      join queue q on q.extension = e.id
                      join work w on w.queue = q.id"
               animal-name
               (:dao extension :single))))

(defun receive-archive (extension pgversion
                        animal-name os version arch
                        buildlog archive-filename archive)
  "Register a new extension's archive and move the file at the expected place."
  (let ((archive-full-name (merge-pathnames archive-filename *archive-path*)))
    (iolib.base:copy-file archive archive-full-name)

    (with-pgsql-connection (*dburi*)
      (query    "with this_animal as (
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
                (namestring archive-full-name)
                (:dao archive :single)))))

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

(defun archive-pathname (extension pgversion os version arch)
  "Return the pathname to the extension's archive file for given version of
   PostgreSQL and OS specifications."
  (with-pgsql-connection (*dburi*)
    (query "select archive
              from archive a
                   join extension e on e.id = a.extension
                   join platform p on p.id = a.platform
             where     a.pgversion = $2
                   and e.shortname = $1
                   and p.os_name = $3
                   and p.os_version = $4
                   and p.arch = $5"
           extension pgversion os version arch :single)))
