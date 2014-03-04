/* PostgreSQL Extension Installer -- Dimitri Fontaine
 *
 * Author: Dimitri Fontaine <dimitri@2ndQuadrant.fr>
 * Licence: PostgreSQL
 * Copyright Dimitri Fontaine, 2011-2014
 *
 * For a description of the features see the README.md file from the same
 * distribution.
 */

#ifndef __PGINSTALL_PLATFORM_H__
#define __PGINSTALL_PLATFORM_H__

#define MAXVERSION 64			/* CentOS release 6.4 (Final) */
#define SYSTEM_VERSION_PLIST "/System/Library/CoreServices/SystemVersion.plist"
#define CENTOS_RELEASE "/etc/centos-release"
#define DEBIAN_VERSION "/etc/debian_version"

typedef struct platform
{
	char *os_name;			/* Mac_OS_X or Debian or CentOS */
	char *os_version;		/* 10.9.1 or 7.1 or 6.4 */
	char *arch;				/* x86_64 or x86 etc */
} PlatformData;

typedef PlatformData * Platform;

void current_platform(Platform platform);

#endif
