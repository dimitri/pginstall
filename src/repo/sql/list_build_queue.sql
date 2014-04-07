create or replace function pginstall.list_build_queue
 (
   out queue      integer,
   out shortname  text,
   out state      text,
   out done       timestamptz,
   out started    timestamptz,
   out animal     text,
   out os         text,
   out version    text,
   out arch       arch
 )
  returns setof record
  language sql
as $$
with queue_max as (
     select q.id, q.extension,
            max(q.id) over(partition by q.extension)
      from queue q
),
      recentq as (
    select *
      from queue_max
     where id = max
),
      rstate as (
   select distinct on(queue, platform, state)
          r.queue,
          case when r.done is not null
               then 'done'
               else 'running'
           end as state,
          p.id as platform,
          r.done,
          r.started,
          a.name as animal
     from running r
          join queue q on q.id = r.queue
          join animal a on r.animal = a.id
          join platform p on a.platform = p.id
 ),
       runningq as (
    select distinct on(q.extension, p.id)
           q.id as queue,
           q.extension,
           rs.state,
           rs.done,
           rs.started,
           rs.animal,
           p.os_name as os, p.os_version as version, p.arch
      from recentq q
           cross join platform p
           left join rstate rs on rs.queue = q.id
                              and rs.platform = p.id
  order by q.extension, p.id, q.id, p.id
 )
   select q.id as queue, e.shortname,
          coalesce(rq.state, 'not started') as state,
          rq.done, rq.started, rq.animal,
          coalesce(rq.os, '') as os,
          coalesce(rq.version, '') as version,
          rq.arch
     from recentq q
          join extension e on e.id = q.extension
          left join runningq rq on q.id = rq.queue
                               and q.extension = rq.extension;
$$;
