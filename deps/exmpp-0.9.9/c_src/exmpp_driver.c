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

#include <math.h>
#include <stdio.h>
#include <string.h>

#include "exmpp_driver.h"

struct exmpp_ht_entry;

struct exmpp_ht_entry {
	char			 *key;
	int			  key_len;
	void			 *value;
	unsigned int		  hash;
	struct exmpp_ht_entry	 *next;
};

struct exmpp_hashtable {
	struct exmpp_ht_entry	**entries;
	unsigned int		  length;
	unsigned int		  entries_count;
	unsigned int		  load_limit;
	unsigned int		  prime_index;
	exmpp_ht_free_value_cb	 *free_value;
#if defined(USE_RWLOCK)
	ErlDrvRWLock		 *lock;
#endif
};

static unsigned int	ht_hash(const char *key);
static unsigned int	ht_hash_len(const char *key, int key_len);
static int		ht_expand(struct exmpp_hashtable *ht);

/*
 * Credit for primes table: Aaron Krowne
 *   http://br.endernet.org/~akrowne/
 *   http://planetmath.org/encyclopedia/GoodHashTablePrimes.html
 */

static const unsigned int primes[] = {
	53, 97, 193, 389,
	769, 1543, 3079, 6151,
	12289, 24593, 49157, 98317,
	196613, 393241, 786433, 1572869,
	3145739, 6291469, 12582917, 25165843,
	50331653, 100663319, 201326611, 402653189,
	805306457, 1610612741
};
static const unsigned int prime_table_length =
  sizeof(primes) / sizeof(primes[0]);

#define	MAX_LOAD_FACTOR 0.65f

/* -------------------------------------------------------------------
 * Simple hashtable.
 * ------------------------------------------------------------------- */

/*
 * Copyright (c) 2002, 2004, Christopher Clark
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 *   * Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *
 *   * Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *
 *   * Neither the name of the original author; nor the names of any
 *     contributors may be used to endorse or promote products derived
 *     from this software without specific prior written permission.
 *
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * http://www.cl.cam.ac.uk/~cwc22/hashtable/
 *
 * It was modified to use Erlang memory allocators and to add locking.
 */

struct exmpp_hashtable *
exmpp_ht_create(unsigned int min_length, exmpp_ht_free_value_cb free_value)
{
	struct exmpp_hashtable *ht;
	unsigned int pindex, length;

	/* Check requested length isn't too large. */
	if (min_length > (1u << 30))
		return (NULL);

	/* Enforce length as prime. */
	length = primes[0];
	for (pindex = 0; pindex < prime_table_length; ++pindex) {
		if (primes[pindex] > min_length) {
			length = primes[pindex];
			break;
		}
	}

	ht = driver_alloc(sizeof(*ht));
	if (ht == NULL)
		return (NULL);
	ht->entries = driver_alloc(length * sizeof(*(ht->entries)));
	if (ht->entries == NULL) {
		driver_free(ht);
		return (NULL);
	}
	memset(ht->entries, 0, length * sizeof(*(ht->entries)));

#if defined(USE_RWLOCK)
	ht->lock = erl_drv_rwlock_create("exmpp_hashtable");
	if (ht->lock == NULL) {
		driver_free(ht->entries);
		driver_free(ht);
		return (NULL);
	}
#endif

	ht->length = length;
	ht->entries_count = 0;
	ht->load_limit = (unsigned int)ceil(length * MAX_LOAD_FACTOR);
	ht->prime_index = pindex;
	ht->free_value = free_value;

	return (ht);
}

int
exmpp_ht_store(struct exmpp_hashtable *ht, const char *key, int key_len,
    void *value)
{
	unsigned int index;
	struct exmpp_ht_entry *entry;

	if (ht == NULL || ht->entries == NULL)
		return (-1);

	/* Allocate the new entry. */
	entry = driver_alloc(sizeof(*entry));
	if (entry == NULL)
		return (-1);

	if (key_len == -1) {
		entry->hash = ht_hash(key);
		entry->key = exmpp_strdup(key);
		if (entry->key == NULL)
			return (-1);
	} else {
		entry->hash = ht_hash_len(key, key_len);
		entry->key = driver_alloc(key_len + 1);
		if (entry->key == NULL)
			return (-1);
		memcpy(entry->key, key, key_len);
		entry->key[key_len] = '\0';
	}
	entry->key_len = key_len;
	entry->value = value;

#if defined(USE_RWLOCK)
	erl_drv_rwlock_rwlock(ht->lock);
#endif

	/* Expand the table is necessary. */
	if (++(ht->entries_count) > ht->load_limit) {
		/*
		 * Ignore the return value. If expand fails, we should
		 * still try cramming just this value into the existing
		 * table -- we may not have memory for a larger table,
		 * but one more element may be ok. Next time we insert,
		 * we'll try expanding again.
		 */
		ht_expand(ht);
	}

	/* Wire the new entry. */
	index = entry->hash % ht->length;
	entry->next = ht->entries[index];
	ht->entries[index] = entry;

#if defined(USE_RWLOCK)
	erl_drv_rwlock_rwunlock(ht->lock);
#endif

	return (0);
}

void *
exmpp_ht_fetch(struct exmpp_hashtable *ht, const char *key, int key_len)
{
	struct exmpp_ht_entry *entry;
	unsigned int hash, index;

	if (ht == NULL || ht->entries == NULL)
		return (NULL);

	hash = key_len == -1 ? ht_hash(key) : ht_hash_len(key, key_len);

#if defined(USE_RWLOCK)
	erl_drv_rwlock_rlock(ht->lock);
#endif

	index = hash % ht->length;
	entry = ht->entries[index];

	while (entry != NULL) {
		/* Check hash value to short circuit heavier comparison. */
		if (entry->hash == hash &&
		    ((key_len == -1 &&
		      strcmp(entry->key, key) == 0) ||
		     (entry->key_len == key_len &&
		      strncmp(entry->key, key, key_len) == 0))) {
#if defined(USE_RWLOCK)
			erl_drv_rwlock_runlock(ht->lock);
#endif
			return (entry->value);
		}

		entry = entry->next;
	}

#if defined(USE_RWLOCK)
	erl_drv_rwlock_runlock(ht->lock);
#endif

	return (NULL);
}

int
exmpp_ht_exists(struct exmpp_hashtable *ht, const char *key, int key_len)
{

	if (exmpp_ht_fetch(ht, key, key_len) != NULL)
		return (1);

	return (0);
}

void
exmpp_ht_destroy(struct exmpp_hashtable *ht)
{
	unsigned int i;
	struct exmpp_ht_entry *entry, *next;

	if (ht == NULL || ht->entries == NULL)
		return;

#if defined(USE_RWLOCK)
	erl_drv_rwlock_rwlock(ht->lock);
#endif

	for (i = 0; i < ht->length; ++i) {
		next = ht->entries[i];
		while (next != NULL) {
			entry = next;
			next = next->next;
			driver_free(entry->key);
			if (ht->free_value != NULL)
				ht->free_value(entry->value);
			driver_free(entry);
		}
	}

	driver_free(ht->entries);
	ht->entries = NULL;
	ht->entries_count = 0;
	ht->length = 0;

#if defined(USE_RWLOCK)
	erl_drv_rwlock_rwunlock(ht->lock);
	erl_drv_rwlock_destroy(ht->lock);
#endif

	driver_free(ht);
}

void
exmpp_ht_dump_keys(struct exmpp_hashtable *ht)
{
	unsigned int i, j;
	struct exmpp_ht_entry *entry;

	if (ht == NULL || ht->entries == NULL)
		return;

#if defined(USE_RWLOCK)
	erl_drv_rwlock_rlock(ht->lock);
#endif

	for (i = 0; i < ht->length; ++i) {
		entry = ht->entries[i];
		j = 0;
		while (entry != NULL) {
			if (j == 0)
				printf("  %3u: '%s'\r\n", i, entry->key);
			else
				printf("       '%s'\r\n", entry->key);
			entry = entry->next;
			++j;
		}
	}

#if defined(USE_RWLOCK)
	erl_drv_rwlock_runlock(ht->lock);
#endif
}

static unsigned int
ht_hash(const char *key)
{
	int c;
	const char *str;
	unsigned int hash;

	str = key;
	hash = 5381;
	while ((c = *str++))
		hash = ((hash << 5) + hash) ^ c; /* hash * 33 ^ c */

	return (hash);
}

static unsigned int
ht_hash_len(const char *key, int key_len)
{
	int i;
	unsigned int hash;

	hash = 5381;
	for (i = 0; i < key_len; ++i)
		hash = ((hash << 5) + hash) ^ key[i]; /* hash * 33 ^ c */

	return (hash);
}

static int
ht_expand(struct exmpp_hashtable *ht)
{
	unsigned int new_length, i, index;
	struct exmpp_ht_entry *entry, **new_entries;

	/* This function must be called with 'ht' locked! */

	/* Check we're not hitting max capacity. */
	if (ht->prime_index == (prime_table_length - 1))
		return (0);
	new_length = primes[++(ht->prime_index)];

	new_entries = driver_alloc(new_length * sizeof(*new_entries));
	if (new_entries == NULL)
		return (-1);
	memset(ht->entries, 0, new_length * sizeof(*(new_entries)));

	/*
	 * This algorithm is not "stable"; ie. it reverses the list
	 * when it transfers entries between the tables.
	 */
	for (i = 0; i < ht->length; ++i) {
		while ((entry = ht->entries[i]) != NULL) {
			ht->entries[i] = entry->next;
			index = entry->hash % new_length;
			entry->next = new_entries[index];
			new_entries[index] = entry;
		}
	}

	driver_free(ht->entries);
	ht->entries = new_entries;
	ht->length = new_length;
	ht->load_limit = (unsigned int)ceil(new_length * MAX_LOAD_FACTOR);

	return (0);
}

/* -------------------------------------------------------------------
 * Misc.
 * ------------------------------------------------------------------- */

int
exmpp_skip_version(const char *buf)
{
	int index, version;

	index = 0;
	if (ei_decode_version(buf, &index, &version) != 0)
		return (-1);

	return (index);
}

ei_x_buff *
exmpp_new_xbuf()
{
	ei_x_buff *xbuf;

	xbuf = driver_alloc(sizeof(*xbuf));
	if (xbuf == NULL)
		return (NULL);

	if (ei_x_new_with_version(xbuf) != 0) {
		driver_free(xbuf);
		return (NULL);
	}

	return (xbuf);
}

void
exmpp_free_xbuf(ei_x_buff *xbuf)
{

	ei_x_free(xbuf);
	driver_free(xbuf);
}

char *
exmpp_strdup(const char *str)
{
	size_t len;
	char *dup;

	len = strlen(str);
	dup = driver_alloc(len + 1);
	if (dup == NULL)
		return (NULL);

	memcpy(dup, str, len + 1);

	return (dup);
}
