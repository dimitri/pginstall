create or replace function pginstall.cancel_running_build
 (
   in animal text
 )
 returns setof pginstall.queue
 language sql
as $$
with entry(queue) as
(
    delete from pginstall.running r
          using pginstall.animal a
          where r.animal = a.id
            and a.name = $1
 returning r.queue
)
select q.*
  from      pginstall.queue q
       join entry e on q.id = e.queue;
$$;
