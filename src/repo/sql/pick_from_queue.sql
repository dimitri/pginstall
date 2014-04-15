create or replace function pginstall.pick_from_queue
 (
  in animal text
 )
 returns setof pginstall.extension
 language plpgsql
as $$
declare
  job pginstall.extension;
begin
  select into job
         e.*
    from pginstall.running r
         join pginstall.animal a on a.id = r.animal
         join pginstall.queue q on q.id = r.queue
         join pginstall.extension e on q.extension = e.id
   where     a.name = $1
         and done is null;

  if found
  then
    return next job;
  else
    return query with pending(queue, platform) as (
                     select q.id, p.id
                       from pginstall.queue q
                            cross join (     pginstall.platform p
                                        join pginstall.animal a
                                             on a.name = $1
                                            and a.platform = p.id
                                       )
                      except

                      select r.queue, p.id
                        from      pginstall.running r
                             join pginstall.animal a on a.id = r.animal
                             join pginstall.platform p on p.id = a.platform
                 ),
                      pick_one as (
                    select array_agg(queue) as queue
                      from pending p
                           join pginstall.queue q on q.id = p.queue
                  group by extension
                     limit 1
                 ),
                      work as (
                   insert into pginstall.running(queue, animal)
                        select unnest(po.queue),
                               (select id
                                  from pginstall.animal
                                 where name = $1)
                     from pick_one po
                returning running.queue
                 )
                    select e.*
                      from      pginstall.extension e
                           join pginstall.queue q on q.extension = e.id
                           join work w on w.queue = q.id;
  end if;
end;
$$;
