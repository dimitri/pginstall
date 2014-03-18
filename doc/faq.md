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

### I have way moar question that I don't see answer for...

Consider sending me an email or opening an issue on the github page for this
project, at [https://github.com/dimitri/pginstall](). Well, soon enough to
be there.
