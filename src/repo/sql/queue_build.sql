drop type if exists pginstall.build_queue cascade;

create type pginstall.build_queue as
 (
   queue       integer,
   extension   integer,
   fullname    text,
   uri         text,
   description text
 );

create or replace function pginstall.queue_build
 (
   in  extname     text
 )
 returns setof pginstall.build_queue
 language plpgsql
as
$$
declare
  item pginstall.build_queue;
begin
    select into item
           q.id, e.id as ext_id,
           e.fullname, e.uri, e.description
     from pginstall.queue q
          join pginstall.extension e on q.extension = e.id
          left join pginstall.running r on r.queue = q.id
    where e.shortname = $1
          and r.done is null
 group by q.id, e.id
    limit 1;

  if found
  then
    return next item;
  else
    return query with queue as (
                   insert into pginstall.queue (extension)
                        select id from pginstall.extension where shortname = $1
                     returning id, extension
                 )
                 select q.id, e.id as ext_id,
                        e.fullname, e.uri, e.description
                   from queue q
                        join pginstall.extension e on q.extension = e.id;
  end if;
end;
$$;
