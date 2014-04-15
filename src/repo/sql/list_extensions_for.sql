create or replace function pginstall.list_extensions_for
 (
   in os      text,
   in version text,
   in arch    pginstall.arch
 )
 returns setof pginstall.extension
 language sql
as $$
  select e.*
    from      pginstall.extension e
         join pginstall.archive a on a.extension = e.id
         join pginstall.platform p on p.id = a.platform
    where     p.os_name = $1
          and p.os_version = $2
          and p.arch = $3
$$;
