--
-- Implement the data access as a set of Stored Procedure
-- See each function in the relevant file.
--
create schema if not exists pginstall;

\i sql/cancel_build.sql
\i sql/list_build_queue.sql
\i sql/pick_from_queue.sql
