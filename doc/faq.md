# Hypothetically Frequently Asked Questions

### How do I install an extension?

Use `CREATE EXTENSION` on the PostgreSQL Server where you want this to
happen, as usual.

### So, why do I need the PostgreSQL Installer?

At `CREATE EXTENSION` time, PostgreSQL checks that you have already
installed the *Operating System* parts that it needs to be able to install
the extension in your database. The PostgreSQL Extension Installer takes
care of that extra step for you, so that you don't have to worry about it.

### Wait a minute, you're installing files in the OS for me?

Exactly. At `CREATE EXTENSION extname;` time, when you have the `pginstall`
extension already setup, the following will now happen:

 1. retrieve your current platform,

 2. ask the repository server if there's an available archive file for the
    extension `extname` on your current platform, and download that *binary
    archive* file if it exists,
    
 3. unpack the archive file, installing its components at the expected place,
 
 4. allow the PostgreSQL implementation of the `CREATE EXTENSION` command to
    now actually take place as usual.
    
### Fine, can I check the platform discovery is about half sane?

With the `pginstall` extension properly installed, you can:

    select * from pginstall_platform();
     os_name  | os_version |  arch  
    ----------+------------+--------
     Mac OS X | 10.9.1     | x86_64
    (1 row)

### Can I host buildfarm animals on transient virtual machines?

Yes, the API has been made so that it's easy and welcome for the buildfarm
animals to just disappear whenever they want to: the *Repository Server*
never connects to the Animals, it's always the other way round:

  - the animal connects to the repository server to register itself;
  - then it connects to the repository server to ask for an extension to build;
  - then it connects to the repository server to upload the extension's
    archive files.

### How do I update an extension?

You schedule another build.

### Can I add my own extension only locally?

This mode of operation is part of the design, but not yet implemented. You
will soon be able to *mirror* an existing *upstream* repository server, and
at the same time to manage a local set of extensions and build queue, with
your own build farm animals.

### What about security?

The current design about answering your well founded security concerns is to
run everything behing http proxy with full SSL certificates validation
enabled (both client and server), so that you implement a trusted network.

Once you've setup a trusted network, do you still have security issues to
talk about?

### What about running some tests before uploading archives?

That's not implemented yet, and it's in the plan of course. The idea is that
maybe the buildfarm animal shouldn't be the one who tests the extension it
just compiled, because it has the whole development environment, which is
different from the typical production environment.

So the current design is to allow for a test farm to be setup, where animals
this time would pick up just build archives and somehow *promote* them from
the *untable* to the *stable* repository, or something like that.

### I have way moar question that I don't see answer for...

Consider sending me an email or opening an issue on the github page for this
project, at [https://github.com/dimitri/pginstall](). Well, soon enough to
be there.
