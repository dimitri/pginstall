create or replace function pginstall.pick_from_queue
 (
  in animal text
 )
 returns setof extension
 language plpgsql
as $$
declare
  job extension;
begin
  select into job
         e.*
    from running r
         join animal a on a.id = r.animal
         join queue q on q.id = r.queue
         join extension e on q.extension = e.id
   where     a.name = $1
         and done is null;

  if found
  then
    return next job;
  else
    return query with pending(queue, platform) as (
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
                           join work w on w.queue = q.id;
  end if;
end;
$$;
