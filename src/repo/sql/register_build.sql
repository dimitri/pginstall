create or replace function pginstall.register_build
 (
   in fullname  text,
   in pgversion text,
   in animal    text,
   in os        text,
   in version   text,
   in arch      pginstall.arch,
   in buildlog  text,
   in filename  text
 )
 returns setof pginstall.archive
 language sql
as $$
     with this_animal as (
       select id
         from pginstall.animal
        where name = $3
     ),
          this_platform as (
       select id
         from pginstall.platform
        where os_name = $4
          and os_version = $5
          and arch = $6
     ),
          this_extension as (
       select id
         from pginstall.extension
        where fullname = $1
     ),
          done as (
       update pginstall.running r
          set done = now()
         from this_extension e, this_animal a, pginstall.queue q
        where     r.queue = q.id
              and q.extension = e.id
              and r.animal = a.id
     ),
         this_buildlog as (
      insert into pginstall.buildlog(extension, animal, log)
           select e.id, a.id, $7
             from this_extension e, this_animal a
        returning *
     ),
           update as (
       update pginstall.archive a
          set log = b.id, archive = $8
         from this_buildlog b, this_extension e, this_platform p
        where     a.extension = e.id
              and a.platform = p.id
              and a.pgversion = $2
    returning a.*
     ),
           insert as (
insert into pginstall.archive(extension, platform, pgversion, log, archive)
     select e.id, p.id, $2, b.id, $8
       from this_extension e, this_platform p, this_buildlog b
      where not exists (select 1
                          from pginstall.archive
                         where     extension = e.id
                               and platform = p.id
                               and pgversion = $2)
  returning archive.*
     )
     select * from insert
  union all
     select * from update
     limit 1
$$;
