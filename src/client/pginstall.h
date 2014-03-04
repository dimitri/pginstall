/* PostgreSQL Extension WhiteList -- Dimitri Fontaine
 *
 * Author: Dimitri Fontaine <dimitri@2ndQuadrant.fr>
 * Licence: PostgreSQL
 * Copyright Dimitri Fontaine, 2011-2013
 *
 * For a description of the features see the README.md file from the same
 * distribution.
 */

#ifndef __PGINSTALL_H__
#define __PGINSTALL_H__

#include "postgres.h"

#ifdef PG_VERSION_NUM
#define PG_MAJOR_VERSION (PG_VERSION_NUM / 100)
#else
#error "Unknown PostgreSQL version"
#endif

#if PG_MAJOR_VERSION != 901    \
	&& PG_MAJOR_VERSION != 902 \
	&& PG_MAJOR_VERSION != 903 \
	&& PG_MAJOR_VERSION != 904
#error "Unsupported postgresql version"
#endif

extern char *pginstall_archive_dir;
extern char *pginstall_control_dir;
extern char *pginstall_extension_dir;
extern char *pginstall_repository;
extern char *pginstall_custom_path;
extern char *pginstall_whitelist;
extern bool pginstall_sudo;

#endif
