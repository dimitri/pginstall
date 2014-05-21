/* PostgreSQL Extension Installer -- Dimitri Fontaine
 *
 * Author: Dimitri Fontaine <dimitri@2ndQuadrant.fr>
 * Licence: PostgreSQL
 * Copyright Dimitri Fontaine, 2011-2014
 *
 * For a description of the features see the README.md file from the same
 * distribution.
 */

#include "communicate.h"
#include "pginstall.h"
#include "platform.h"

#include "postgres.h"

#include "storage/fd.h"

#include <curl/curl.h>
#include <jansson.h>

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

/*
 * https://github.com/akheron/jansson/blob/master/doc/github_datas.c
 */
struct write_result
{
    char *data;
    int pos;
};

static size_t
write_response(void *ptr, size_t size, size_t nmemb, void *stream)
{
    struct write_result *result = (struct write_result *)stream;

    if(result->pos + size * nmemb >= BUFFER_SIZE - 1)
    {
        ereport(ERROR,
                (errcode(ERRCODE_PROGRAM_LIMIT_EXCEEDED),
                 errmsg("out of memory")));
    }

    memcpy(result->data + result->pos, ptr, size * nmemb);
    result->pos += size * nmemb;

    return size * nmemb;
}

static char *
request(const char *baseurl, int nbargs, ...)
{
    CURL *curl = NULL;
    CURLcode status;
    struct curl_slist *headers = NULL;
    struct write_result write_result;
    char *data = NULL;
    long code;

    char *str;
    char *escaped_url = psprintf("%sapi", baseurl);
    int i;

    /* Process the URL components */
    va_list arg_list;

    va_start (arg_list, nbargs);

    for(i=0; i<nbargs; i++)
    {
        str = va_arg (arg_list, char *);

        escaped_url = psprintf("%s/%s", escaped_url, curl_easy_escape(curl, str, 0));
    }
    va_end(arg_list);

    curl_global_init(CURL_GLOBAL_ALL);
    curl = curl_easy_init();
    if(!curl)
        goto error;

    data = malloc(BUFFER_SIZE);
    if(!data)
        goto error;

    write_result.data = data;
    write_result.pos  = 0;

    curl_easy_setopt(curl, CURLOPT_URL, escaped_url);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_response);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &write_result);

    status = curl_easy_perform(curl);
    if(status != 0)
    {
        if(data)
            free(data);
        if(curl)
            curl_easy_cleanup(curl);
        if(headers)
            curl_slist_free_all(headers);
        curl_global_cleanup();

        ereport(ERROR,
                (errcode(ERRCODE_IO_ERROR),
                 errmsg("unable to request data from \"%s\"", escaped_url),
                 errdetail("%s", curl_easy_strerror(status))));
    }

    curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &code);
    if(code != 200)
    {
        if(data)
            free(data);
        if(curl)
            curl_easy_cleanup(curl);
        if(headers)
            curl_slist_free_all(headers);
        curl_global_cleanup();

        ereport(ERROR,
                (errcode(ERRCODE_IO_ERROR),
                 errmsg("server responded with code %ld", code)));
    }

    curl_easy_cleanup(curl);
    curl_global_cleanup();

    /* zero-terminate the result */
    data[write_result.pos] = '\0';

    return data;

error:
    if(data)
        free(data);
    if(curl)
        curl_easy_cleanup(curl);
    if(headers)
        curl_slist_free_all(headers);
    curl_global_cleanup();

    return NULL;
}

/*
 * Now almost the same CURL code but this time to download a binary file.
 */
static size_t
write_data(void *ptr, size_t size, size_t nmemb, void *stream)
{
    size_t written = fwrite(ptr, size, nmemb, (FILE *)stream);
    return written;
}

static bool
download(const char *filename, const char *baseurl, int nbargs, ...)
{
    CURL    *curl = NULL;
    CURLcode status;
    FILE    *file;
    long     code;

    char *str;
    char *escaped_url = psprintf("%sapi", baseurl);
    int i;

    /* Process the URL components */
    va_list arg_list;

    va_start (arg_list, nbargs);

    for (i=0; i<nbargs; i++)
    {
        str = va_arg (arg_list, char *);

        escaped_url = psprintf("%s/%s", escaped_url, curl_easy_escape(curl, str, 0));
    }
    va_end(arg_list);

    elog(DEBUG1, "curl -O \"%s\" %s", filename, escaped_url);

    /* Init the CURL processing. */
    curl_global_init(CURL_GLOBAL_ALL);
    curl = curl_easy_init();
    curl_easy_setopt(curl, CURLOPT_URL, escaped_url);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_data);

    /* Open the file and download its content from the the URL. */
    if ((file = AllocateFile(filename, "w")) == NULL)
    {
        ereport(ERROR,
                (errcode_for_file_access(),
                 errmsg("could not open extension archive file \"%s\": %m",
                        filename)));
    }
    curl_easy_setopt(curl, CURLOPT_FILE, file);
    status = curl_easy_perform(curl);

    if (status != 0)
    {
        curl_easy_cleanup(curl);
        curl_global_cleanup();

        /* we have create an empty archive file that we now need to remove */
        unlink(filename);

        ereport(ERROR,
                (errcode(ERRCODE_IO_ERROR),
                 errmsg("unable to request data from \"%s\"", escaped_url),
                 errdetail("%s", curl_easy_strerror(status))));
    }

    curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &code);
    if (code != 200 && code != 404)
    {
        curl_easy_cleanup(curl);
        curl_global_cleanup();

        /* we have create an empty archive file that we now need to remove */
        unlink(filename);

        ereport(ERROR,
                (errcode(ERRCODE_IO_ERROR),
                 errmsg("server responded with code %ld", code)));
    }

    /* Close the file and cleanup CURL context. */
    fclose(file);
    curl_easy_cleanup(curl);
    curl_global_cleanup();

    if (code != 200)
    {
        /* The server's error message is to be found in the file... */
        unlink(filename);
    }

    /* We just return "false" when we get a 404. */
    return code == 200;
}

/*
 * Fetch available extensions for this platform
 */
List *
list_available_extensions_on_repository(Platform platform)
{
    List *result = NIL;

    char *text = request(pginstall_repository, 5,
                         "list", "extension",
                         platform->os_name,
                         platform->os_version,
                         platform->arch);

    size_t i;
    json_t *root;
    json_error_t error;

    if (!text)
    {
        ereport(ERROR,
                (errcode(ERRCODE_NO_DATA_FOUND),
                 errmsg("no data found")));
    }

    root = json_loads(text, 0, &error);
    elog(DEBUG1, "Received from server: %s", text);
    free(text);


    if (!root)
    {
        ereport(ERROR,
                (errcode(ERRCODE_DATA_EXCEPTION),
                 errmsg("invalid JSON string on line %d", error.line),
                 errdetail("%s", error.text)));
    }

    if (!json_is_array(root))
    {
        ereport(ERROR,
                (errcode(ERRCODE_DATA_EXCEPTION),
                 errmsg("expected a JSON array.")));
    }

    for(i = 0; i < json_array_size(root); i++)
    {
        pginstall_extension *ext =
            (pginstall_extension *)palloc(sizeof(pginstall_extension));
        json_t *data, *id, *shortname, *fullname, *uri, *description;

        data = json_array_get(root, i);

        if(!json_is_object(data))
        {
            ereport(ERROR,
                    (errcode(ERRCODE_DATA_EXCEPTION),
                     errmsg("couldn't parse an extension JSON object.")));
        }

        id          = json_object_get(data, "ID");
        shortname   = json_object_get(data, "SHORTNAME");
        fullname    = json_object_get(data, "FULLNAME");
        uri         = json_object_get(data, "URI");
        description = json_object_get(data, "DESCRIPTION");

        Assert(json_is_number(id));
        Assert(json_is_string(shortname));
        Assert(json_is_string(fullname));
        Assert(json_is_string(uri));
        Assert(json_is_string(description));

        ext->id          = json_integer_value(id);
        ext->shortname   = pstrdup(json_string_value(shortname));
        ext->fullname    = pstrdup(json_string_value(fullname));
        ext->uri         = pstrdup(json_string_value(uri));
        ext->description = pstrdup(json_string_value(description));

        result = lappend(result, ext);
    }
    json_decref(root);

    return result;
}

/*
 * Download an archive
 */
void
download_archive(const char *filename, const char *extname, Platform platform)
{
    char *archive_filename = psprintf("%s--%s--%s--%s--%s.tar.gz",
                                      extname,
                                      PG_VERSION,
                                      platform->os_name,
                                      platform->os_version,
                                      platform->arch);

    bool ok = download(filename,
                       pginstall_repository,
                       2,
                       "archive", archive_filename);

    if (!ok)
    {
        /*
         * We couln't find the extension on the Repository Server, it might
         * still be available locally.
         */
        ereport(WARNING,
                (errcode(ERRCODE_NO_DATA_FOUND),
                 errmsg("extension \"%s\" is not available on pginstall repository server",
                        extname),
                 errdetail("HTTP 404 on %sapi/archive/%s",
                           pginstall_repository, archive_filename)));
    }
    return;
}
