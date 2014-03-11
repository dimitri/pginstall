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
#include "platform.h"
#include "pginstall.h"
#include "utils.h"

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "postgres.h"

static int copy_data(struct archive *ar, struct archive *aw);

/*
 * At CREATE EXTENSION time we check if the extension is already available,
 * which is driven by the presence of its control file on disk.
 *
 * If the extension is not already available, we ask the repository server for
 * it, and unpack received binary archive to the right place.
 *
 * TODO: actually talk to the repository server. Current prototype version
 * directly uses the local archive cache.
 */
void
maybe_unpack_archive(const char *extname, Platform platform)
{
    char *control_filename = get_extension_control_filename(extname);
    char *archive_filename;

    if (access(control_filename, F_OK) == 0)
        return;

    archive_filename = psprintf("%s/%s--%s--%s--%s--%s.tar.gz",
                                pginstall_archive_dir,
                                extname,
                                PG_VERSION,
                                platform->os_name,
                                platform->os_version,
                                platform->arch);

    if (access(archive_filename, R_OK) == 0)
    {
        extract(extname, archive_filename);

        /* now rewrite the control file to "relocate" the extension */
        rewrite_control_file(extname, control_filename);
    }
    return;
}

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
compute_target_path(const char *filename,
                    const char *control_filename,
                    int control_filename_len)
{
    if (strncmp(filename, control_filename, control_filename_len) == 0)
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

    char *control_filename = psprintf("%s.control", extname);
    int cflen = strlen(control_filename);

    a = archive_read_new();
    ext = archive_write_disk_new();
    archive_write_disk_set_options(ext, flags);

    /*
     * Do we care enough about the .so size to limit ourselves here? We might
     * want to reconsider and use archive_read_support_format_all() and
     * archive_read_support_filter_all() rather than just tar.gz.
     */
    archive_read_support_format_tar(a);
    archive_read_support_filter_gzip(a);

    if ((archive_read_open_filename(a, filename, 10240)))
        ereport(ERROR,
                (errcode(ERRCODE_INVALID_PARAMETER_VALUE),
                 errmsg("Failed to open archive \"%s\"", filename),
                 errdetail("%s", archive_error_string(a))));

    elog(DEBUG1, "Unpacking archive \"%s\"", filename);

    for (;;)
    {
        char *path;
        struct archive_entry *target;

        r = archive_read_next_header(a, &entry);
        if (r == ARCHIVE_EOF)
            break;

        if (r != ARCHIVE_OK)
            ereport(ERROR,
                    (errcode(ERRCODE_UNDEFINED_FILE),
                     errmsg("%s", archive_error_string(a))));

        target = archive_entry_clone(entry);
        path   = (char *)archive_entry_pathname(target);
        path   = (char *)compute_target_path(path, control_filename, cflen);
        archive_entry_set_pathname(target, path);

        elog(DEBUG1, "Extracting \"%s\" to \"%s\"",
             archive_entry_pathname(entry), path);

        r = archive_write_header(ext, target);

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
