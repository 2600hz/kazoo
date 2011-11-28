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

#ifndef EXMPP_COMPRESS_H
#define	EXMPP_COMPRESS_H 1

#include "exmpp_driver.h"

/* Return codes. */
enum {
	RET_OK = 0,
	RET_ERROR
};

/* Control operations. */
enum {
	COMMAND_SET_COMPRESS_METHOD = 1,
	COMMAND_SET_COMPRESS_LEVEL,
	COMMAND_PREPARE_COMPRESS,
	COMMAND_PREPARE_UNCOMPRESS,
	COMMAND_COMPRESS,
	COMMAND_UNCOMPRESS,
	COMMAND_SVN_REVISION
};

#endif /* !defined(EXMPP_COMPRESS_H) */
