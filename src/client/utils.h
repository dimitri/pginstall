/* PostgreSQL Extension Installer -- Dimitri Fontaine
 *
 * Author: Dimitri Fontaine <dimitri@2ndQuadrant.fr>
 * Licence: PostgreSQL
 * Copyright Dimitri Fontaine, 2011-2014
 *
 * For a description of the features see the README.md file from the same
 * distribution.
 */

#ifndef __PGINSTALL_UTILS_H__
#define __PGINSTALL_UTILS_H__

#include "psprintf.h"

#include "utils/builtins.h"

#define MAXPGPATH 1024

char *get_extension_control_filename(const char *extname);

void rewrite_control_file(const char *extname, const char *control_filename);

char *get_specific_custom_script_filename(const char *name,
										  const char *when,
										  const char *from_version,
										  const char *version);

char *get_generic_custom_script_filename(const char *name,
										 const char *action,
										 const char *when);

char *get_extension_current_version(const char *extname);

void fill_in_extension_properties(const char *extname,
								  List *options,
								  char **schema,
								  char **old_version,
								  char **new_version);

void execute_custom_script(const char *schemaName, const char *filename);

#endif
