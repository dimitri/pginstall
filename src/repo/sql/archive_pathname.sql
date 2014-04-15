create or replace function pginstall.archive_pathname
 (
   in fullname  text,
   in pgversion text,
   in os        text,
   in version   text,
   in arch      pginstall.arch
 )
 returns text
 language sql
as $$
  select archive

    from      pginstall.archive a
         join pginstall.extension e on e.id = a.extension
         join pginstall.platform p on p.id = a.platform

   where     a.pgversion = $2
         and e.fullname = $1
         and p.os_name = $3
         and p.os_version = $4
         and p.arch = $5
$$;
