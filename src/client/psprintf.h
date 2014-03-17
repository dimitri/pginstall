/* PostgreSQL Extension Installer -- Dimitri Fontaine
 *
 * Author: Dimitri Fontaine <dimitri@2ndQuadrant.fr>
 * Licence: PostgreSQL
 * Copyright Dimitri Fontaine, 2011-2014
 *
 * For a description of the features see the README.md file from the same
 * distribution.
 */

#ifndef __PGINSTALL_PSPRINTF_H__
#define __PGINSTALL_PSPRINTF_H__

#include "pginstall.h"

#if PG_MAJOR_VERSION < 904

/* sprintf into a palloc'd buffer --- these are in psprintf.c */
char *psprintf(const char *fmt,...)
__attribute__((format(PG_PRINTF_ATTRIBUTE, 1, 2)));

size_t pvsnprintf(char *buf, size_t len, const char *fmt, va_list args)
__attribute__((format(PG_PRINTF_ATTRIBUTE, 3, 0)));
#endif

#endif
