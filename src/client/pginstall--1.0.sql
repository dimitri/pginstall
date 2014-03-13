-- complain if script is sourced in psql, rather than via CREATE EXTENSION
\echo Use "CREATE EXTENSION pginstall" to load this file. \quit

CREATE OR REPLACE FUNCTION pginstall_platform(
  OUT os_name     text,
  OUT os_version  text,
  OUT arch        text
)
RETURNS SETOF record
AS 'pginstall'
LANGUAGE C IMMUTABLE STRICT;

CREATE OR REPLACE FUNCTION pginstall_available_extensions(
  OUT id          bigint,
  OUT shortname   text,
  OUT fullname    text,
  OUT uri         text,
  OUT description text
)
RETURNS SETOF record
AS 'pginstall'
LANGUAGE C IMMUTABLE STRICT;

CREATE VIEW pginstall_available_extensions AS
  SELECT * FROM pginstall_available_extensions();
