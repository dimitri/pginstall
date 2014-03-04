/* PostgreSQL Extension Installer -- Dimitri Fontaine
 *
 * Author: Dimitri Fontaine <dimitri@2ndQuadrant.fr>
 * Licence: PostgreSQL
 * Copyright Dimitri Fontaine, 2011-2014
 *
 * For a description of the features see the README.md file from the same
 * distribution.
 */

#include "archive.h"
#include "pginstall.h"

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "postgres.h"

static int copy_data(struct archive *ar, struct archive *aw);

/*
 * Given a filename read within an extension archive, compute where to extract
 * the associated data.
 *
 * The main control file is named "extname.control", it is to be extracted in
 * pginstall_control_dir.
 *
 * Other files are named "extname/<path>" and are to be extracted in
 * pginstall_extension_dir/extname/<path>.
 */
static char *
compute_target_path(const char *extname, const char *filename)
{
	char control_filename[MAXPGPATH];
	int len =
		snprintf(control_filename, MAXPGPATH, "%s/%s.control", extname, extname);

	if (strncmp(filename, control_filename, len) == 0)
		return psprintf("%s/%s", pginstall_control_dir, filename);

	else
		return psprintf("%s/%s", pginstall_extension_dir, filename);
}

/*
 * The main archive extract function, loops over the archive entries and unpack
 * them at the right place.
 */
void
extract(const char *extname, const char *filename)
{
	struct archive *a;
	struct archive *ext;
	struct archive_entry *entry;
	int flags = ARCHIVE_EXTRACT_TIME;
	int r;

	a = archive_read_new();
	ext = archive_write_disk_new();
	archive_write_disk_set_options(ext, flags);

	archive_read_support_format_tar(a);

	if ((archive_read_open_filename(a, filename, 10240)))
		ereport(ERROR,
				(errcode(ERRCODE_INVALID_PARAMETER_VALUE),
				 errmsg("Failed to open archive \"%s\"", filename),
				 errdetail("%s", archive_error_string(a))));

	for (;;)
	{
		struct archive_entry *target;

		r = archive_read_next_header(a, &entry);
		if (r == ARCHIVE_EOF)
			break;

		if (r != ARCHIVE_OK)
			ereport(ERROR,
					(errcode(ERRCODE_UNDEFINED_FILE),
					 errmsg("%s", archive_error_string(a))));

		target = archive_entry_clone(entry);
		archive_entry_set_pathname(target,
								   compute_target_path(extname, filename));

		r = archive_write_header(ext, entry);

		if (r != ARCHIVE_OK)
			ereport(WARNING,
					(errcode(ERRCODE_IO_ERROR),
					 errmsg("%s", archive_error_string(ext))));
		else
		{
			copy_data(a, ext);
			r = archive_write_finish_entry(ext);
			if (r != ARCHIVE_OK)
				ereport(ERROR,
						(errcode(ERRCODE_IO_ERROR),
						 errmsg("%s", archive_error_string(ext))));
		}
		archive_entry_free(target);
	}

	archive_read_close(a);
	archive_read_free(a);
}

static int
copy_data(struct archive *ar, struct archive *aw)
{
	int r;
	const void *buff;
	size_t size;
#if ARCHIVE_VERSION >= 3000000
	int64_t offset;
#else
	off_t offset;
#endif

	for (;;)
	{
		r = archive_read_data_block(ar, &buff, &size, &offset);

		if (r == ARCHIVE_EOF)
			return ARCHIVE_OK;

		if (r != ARCHIVE_OK)
			return r;

		r = archive_write_data_block(aw, buff, size, offset);

		if (r != ARCHIVE_OK)
		{
			ereport(WARNING,
					(errcode(ERRCODE_IO_ERROR),
					 errmsg("%s", archive_error_string(aw))));
			return r;
		}
	}
}
