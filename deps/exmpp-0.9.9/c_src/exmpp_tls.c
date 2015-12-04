/*
 * Copyright ProcessOne 2006-2010. All Rights Reserved.
 *
 * The contents of this file are subject to the Erlang Public License,
 * Version 1.1, (the "License"); you may not use this file except in
 * compliance with the License. You should have received a copy of the
 * Erlang Public License along with this software. If not, it can be
 * retrieved online at http://www.erlang.org/.
 *
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
 * the License for the specific language governing rights and limitations
 * under the License.
 *
 */

#include <stdlib.h>
#include <string.h>

#include "exmpp_tls.h"

#if defined(_WIN32)
#define	strcasecmp(s1, s2) _stricmp(s1, s2)
#endif

int
match_hostname(const char *cert_id, const char *expected_id)
{
	size_t cert_id_len;
	char *id;

	cert_id_len = strlen(cert_id);

	if (cert_id_len > 2 && cert_id[0] == '*' && cert_id[1] == '.') {
		/* The certificate contains a pattern like:
		 *     *.example.org
		 * Therefore, we look for the first dot in the expected_id.
		 */
		id = strchr(expected_id, '.');
		if (id == NULL)
			return (0);

		if (strcasecmp(&cert_id[1], id) == 0)
			return (1);
	} else {
		/* The certificate requires an exact match. */
		if (strcasecmp(cert_id, expected_id) == 0)
			return (1);
	}

	return (0);
}
