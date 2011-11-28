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

#ifndef EXMPP_DRIVER_H
#define	EXMPP_DRIVER_H 1

#if defined(HAVE_CONFIG_H)
#include "config.h"
#endif

#include <ei.h>
#include <erl_driver.h>

/*
 * This macro is used to stringify the driver name in the driver's
 * ErlDrvEntry. One use it like this:
 *     static ErlDrvEntry driver_entry = {
 *             ...
 *             S(DRIVER_NAME),
 *             ...
 *     };
 */

#define	_S(s)		#s
#define	S(s)		_S(s)

/*
 * On Win32, strdup(3) exists under the name _strdup.
 */

#if defined(_WIN32)
#define	strdup(s) _strdup(s)
#endif

/* Simple hashtable. */
struct exmpp_hashtable;

typedef void exmpp_ht_free_value_cb(void *value);

struct exmpp_hashtable *	exmpp_ht_create(unsigned int min_length,
				    exmpp_ht_free_value_cb free_value);
int				exmpp_ht_store(struct exmpp_hashtable *ht,
				    const char *key, int key_len,
				    void *value);
void *				exmpp_ht_fetch(struct exmpp_hashtable *ht,
				    const char *key, int key_len);
int				exmpp_ht_exists(struct exmpp_hashtable *ht,
				    const char *key, int key_len);
void				exmpp_ht_destroy(struct exmpp_hashtable *ht);
void				exmpp_ht_dump_keys(struct exmpp_hashtable *ht);

/*
 * The follwing function help in reading and preparing the buffers which are
 * exchanged with Erlang.
 */

int		exmpp_skip_version(const char *buf);
ei_x_buff *	exmpp_new_xbuf(void);
void		exmpp_free_xbuf(ei_x_buff *xbuf);

/* This function uses driver_alloc() but behaves as strdup(3) otherwise. */
char *		exmpp_strdup(const char *str);

#endif /* !defined(EXMPP_DRIVER_H) */
