/* PostgreSQL Extension Installer -- Dimitri Fontaine
 *
 * Author: Dimitri Fontaine <dimitri@2ndQuadrant.fr>
 * Licence: PostgreSQL
 * Copyright Dimitri Fontaine, 2011-2014
 *
 * For a description of the features see the README.md file from the same
 * distribution.
 */

#include "pginstall.h"
#include "platform.h"

#include <stdio.h>
#include <unistd.h>
#include <sys/utsname.h>

#include "storage/fd.h"

char *read_debian_version(char *filename);
char *read_centos_version(char *filename);
void parse_osx_name_and_version(Platform platform);

/*
 * The main entry point, determines the current OS and fetches the values
 * defining the current platform.
 */
void
current_platform(Platform platform)
{
    struct utsname utsname;

    if (uname(&utsname) == -1)
        ereport(ERROR,
                (errcode(ERRCODE_IO_ERROR),
                 errmsg("Failed to retrieve uname information"),
                 errdetail("%s", strerror(errno))));

    /* We have the architecture */
    platform->arch = pstrdup(utsname.machine);

    /*
     * Now retrieve the real OS name and version depending on the kernel name,
     * uname.sysname being something like "Linux" or "Darwin".
     */
    if (strncmp("Darwin", utsname.sysname, strlen("Darwin")) == 0)
    {
        parse_osx_name_and_version(platform);
    }
    else if (strncmp("Linux", utsname.sysname, strlen("Linux")) == 0)
    {
        if (access(DEBIAN_VERSION, R_OK) == 0)
        {
            platform->os_name = pstrdup("Debian");
            platform->os_version = read_debian_version(DEBIAN_VERSION);
        }
        else if (access(CENTOS_RELEASE, R_OK) == 0)
        {
            platform->os_name = pstrdup("CentOS");
            platform->os_version = read_centos_version(CENTOS_RELEASE);
        }
    }
    else
    {
        ereport(ERROR,
                (errcode(ERRCODE_IO_ERROR),
                 errmsg("System is not supported by pginstall: \"%s\"",
                        utsname.sysname)));
    }
    return;
}

/*
 * This one is easy, just read the whole file in.
 */
char *
read_debian_version(char *filename)
{
    char *p, *res;
    char *version = (char *)palloc0(MAXVERSION);
    FILE *file;

    if ((file = AllocateFile(filename, "r")) == NULL)
    {
        /* we still need to handle the following error here */
        ereport(ERROR,
                (errcode_for_file_access(),
                 errmsg("could not open  \"%s\": %m", filename)));
    }
    res = fgets(version, MAXVERSION, file);
    FreeFile(file);

    if (res == NULL)
        ereport(ERROR,
                (errcode(ERRCODE_NO_DATA),
                 errmsg("could not read a line in \"%s\"", filename)));

    for(p=version; *p != '\0'; p++)
        if (*p == '\n')
            *p = '\0';

    return version;
}

/*
 * In CentOS, we have to parse "CentOS release 6.4 (Final)" and retain only th
 * 6.4 number.
 */
char *
read_centos_version(char *filename)
{
    char *version, *next_space, *res;
    char *osversion = (char *)palloc0(MAXVERSION);
    FILE *file;

    if ((file = AllocateFile(filename, "r")) == NULL)
    {
        /* we still need to handle the following error here */
        ereport(ERROR,
                (errcode_for_file_access(),
                 errmsg("could not open  \"%s\": %m", filename)));
    }
    res = fgets(osversion, MAXVERSION, file);
    FreeFile(file);

    if (res == NULL)
        ereport(ERROR,
                (errcode(ERRCODE_NO_DATA),
                 errmsg("could not read a line in \"%s\"", filename)));

    /* let's breathe and be optimistic and confident */
    version = strchr(strchr(osversion, ' ') + 1, ' ') + 1;
    next_space = strchr(version, ' ');
    *next_space = '\0';

    return pstrdup(version);
}

#ifdef __APPLE__
/*
 * To discover OSX name and version, the best way seems to be parsing the
 * /System/Library/CoreServices/SystemVersion.plist property file, which is an
 * XML based file.
 *
 * We won't actually use an XML parser here, the file is pretty simple.
 */
void
parse_osx_name_and_version(Platform platform)
{
    FILE  *file;
    size_t len;
    char  *line;
    char **next = NULL;

    if ((file = AllocateFile(SYSTEM_VERSION_PLIST, "r")) == NULL)
    {
        /* we still need to handle the following error here */
        ereport(ERROR,
                (errcode_for_file_access(),
                 errmsg("could not open  \"%s\": %m", SYSTEM_VERSION_PLIST)));
    }

    while ((line = fgetln(file, &len)) != NULL)
    {
        if (next != NULL)
        {
            /* Parse a <string> value in the XML file. */
            char *first = strchr(line, '>') + 1;
            char *last  = strchr(first, '<') - 1;
            char *value = (char *)palloc0(last-first+2);

            memcpy(value, first, last-first+1);

            *next = value;
            next = NULL;

            continue;
        }

        if (line[len - 1] == '\n')
            line[len - 1] = '\0';

        if (strstr(line, "ProductName") != NULL)
        {
            next = &(platform->os_name);
        }
        else if (strstr(line, "ProductVersion") != NULL)
        {
            next = &(platform->os_version);
        }
    }
    FreeFile(file);
}
#endif
