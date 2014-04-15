---
--- pginstall: The PostgreSQL Extension Repository
---
create schema if not exists pginstall;

create table pginstall.extension (
  id          serial primary key,
  fullname    text not null unique,
  shortname   text,
  uri         text,
  description text
);

--
-- buildfarm animals and their PostgreSQL server dev environments
--
create type pginstall.arch as enum ('x86', 'x86_64', 'arm');

create table pginstall.platform (
  id         serial primary key,
  os_name    text,
  os_version text,
  arch       pginstall.arch,
  unique(os_name, os_version, arch)
);

create table pginstall.registry (
    name     text primary key,
    pict     text
);

create table pginstall.animal (
  id       serial primary key,
  name     text unique,
  platform integer references pginstall.platform(id)
);

create table pginstall.pgconfig (
  id  serial primary key,
  animal     integer references pginstall.animal(id),
  pg_config  text, -- /path/to/9.2/pg_config  
  version    text, -- output of pg_config --version
  configure  text, -- output of pg_config --configure
  cc         text, -- output of pg_config --cc
  cflags     text, -- output of pg_config --cflags
  unique(animal, pg_config)
);

--
-- build jobs, a kind of queue
--
create table pginstall.queue (
  id        serial primary key,
  extension integer references pginstall.extension(id),
  queued    timestamptz default now(),
  unique(extension, queued)
);

create table pginstall.running (
  id        serial primary key,
  queue     integer references pginstall.queue(id) on delete cascade,
  animal    integer references pginstall.animal(id),
  started   timestamptz default now(),
  done      timestamptz,
  unique(queue, animal)
);
    
create table pginstall.buildlog (
  id         serial primary key,
  extension  integer references pginstall.extension(id),
  animal     integer references pginstall.animal(id),
  buildstamp timestamptz default now(),
  log        text
);

--
-- Once an extension has been built, register its availability
--
create table pginstall.archive (
  id        serial primary key,
  extension integer references pginstall.extension(id),
  platform  integer references pginstall.platform(id),
  pgversion text,
  log       integer references pginstall.buildlog(id),
  archive   text,
  unique(extension, platform, pgversion)
);
