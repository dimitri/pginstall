---
--- pginstall: The PostgreSQL Extension Repository
---

create table extension (
  id          serial primary key,
  fullname    text not null unique,
  shortname   text,
  uri         text,
  description text
);

--
-- buildfarm animals and their PostgreSQL server dev environments
--
create type arch as enum ('x86', 'x86_64', 'arm');

create table platform (
  id         serial primary key,
  os_name    text,
  os_version text,
  arch       arch,
  unique(os_name, os_version, arch)
);

create table animal (
  id       serial primary key,
  name     text unique,
  platform integer references platform(id)  
);

create table pgconfig (
  id  serial primary key,
  animal     integer references animal(id),
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
create table queue (
  id        serial primary key,
  extension integer references extension(id) unique
);

create table running (
  id        serial primary key,
  extension integer references extension(id),
  animal    integer references animal(id),
  started   timestamptz default now(),
  unique(extension, animal)
);
    
create table buildlog (
  id         serial primary key,
  extension  integer references extension(id),
  animal     integer references animal(id),
  buildstamp timestamptz,
  log        text
);

--
-- Once an extension has been built, register its availability
--
create table archive (
  id        serial primary key,
  extension integer references extension(id),
  platform  integer references platform(id),
  pgversion text,
  archive   text,
  unique(extension, platform, pgversion)
);
