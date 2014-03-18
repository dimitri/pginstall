# The PostgreSQL Repository Server

The role of the PostgreSQL Repository Server is to allow you managing an
online archive of PostgreSQL Extensions, readily installable on any
PostgreSQL server where the `pginstall` extension has been properly setup.

The main actions you can do from the repository server are:

  - Queue the build of an extension,
  - Fetch an extension's archive binary file.
  
The Repository Server includes an embedded web server that provides an
[HTTP API](/help/api) returning JSON documents, so that you can easily use
your own client to manage the extension building.

The web server also includes a *dashboard* facility including online
documentation.
