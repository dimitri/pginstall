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
