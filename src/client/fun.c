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
#include "fun.h"

#include "postgres.h"

#include "funcapi.h"
#include "miscadmin.h"
#include "utils/builtins.h"
#include "utils/memutils.h"

Datum		pginstall_available_extensions(PG_FUNCTION_ARGS);

PG_FUNCTION_INFO_V1(pginstall_available_extensions);

Datum
pginstall_available_extensions(PG_FUNCTION_ARGS)
{
	ReturnSetInfo *rsinfo = (ReturnSetInfo *) fcinfo->resultinfo;
	TupleDesc	tupdesc;
	Tuplestorestate *tupstore;
	MemoryContext per_query_ctx;
	MemoryContext oldcontext;
	List *extensions;
	ListCell *lc;
	PlatformData platform;

	/* Fetch the list of available extensions */
	current_platform(&platform);
	extensions = list_available_extensions(&platform);

	/* check to see if caller supports us returning a tuplestore */
	if (rsinfo == NULL || !IsA(rsinfo, ReturnSetInfo))
		ereport(ERROR,
				(errcode(ERRCODE_FEATURE_NOT_SUPPORTED),
				 errmsg("set-valued function called in context that cannot accept a set")));
	if (!(rsinfo->allowedModes & SFRM_Materialize))
		ereport(ERROR,
				(errcode(ERRCODE_FEATURE_NOT_SUPPORTED),
				 errmsg("materialize mode required, but it is not " \
						"allowed in this context")));

	/* Build a tuple descriptor for our result type */
	if (get_call_result_type(fcinfo, NULL, &tupdesc) != TYPEFUNC_COMPOSITE)
		elog(ERROR, "return type must be a row type");

	/* Build tuplestore to hold the result rows */
	per_query_ctx = rsinfo->econtext->ecxt_per_query_memory;
	oldcontext = MemoryContextSwitchTo(per_query_ctx);

	tupstore = tuplestore_begin_heap(true, false, work_mem);
	rsinfo->returnMode = SFRM_Materialize;
	rsinfo->setResult = tupstore;
	rsinfo->setDesc = tupdesc;

	MemoryContextSwitchTo(oldcontext);

	foreach(lc, extensions)
	{
		pginstall_extension *ext = (pginstall_extension *) lfirst(lc);

		Datum		values[5];
		bool		nulls[5];

		memset(values, 0, sizeof(values));
		memset(nulls, 0, sizeof(nulls));

		/* id */
		values[0] = Int64GetDatumFast(ext->id);

		/* shortname */
		values[1] = CStringGetTextDatum(ext->shortname);

		/* fullname */
		values[2] = CStringGetTextDatum(ext->fullname);

		/* uri */
		values[3] = CStringGetTextDatum(ext->uri);

		/* description */
		if (ext->description == NULL)
			nulls[4] = true;
		else
			values[4] = CStringGetTextDatum(ext->description);

		tuplestore_putvalues(tupstore, tupdesc, values, nulls);
	}

	/* clean up and return the tuplestore */
	tuplestore_donestoring(tupstore);

	return (Datum) 0;
}
