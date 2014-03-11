/* PostgreSQL Extension Installer -- Dimitri Fontaine
 *
 * Author: Dimitri Fontaine <dimitri@2ndQuadrant.fr>
 * Licence: PostgreSQL
 * Copyright Dimitri Fontaine, 2011-2014
 *
 * For a description of the features see the README.md file from the same
 * distribution.
 */

#ifndef __PGINSTALL_ARCHIVE_H__
#define __PGINSTALL_ARCHIVE_H__

#include "platform.h"

#include <archive.h>
#include <archive_entry.h>

void download_and_unpack_archive(const char *extname);
void extract(const char *extname, const char *filename);

#endif
