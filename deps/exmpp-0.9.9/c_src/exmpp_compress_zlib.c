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

#include <string.h>
#include <zlib.h>

#include "exmpp_compress.h"

#define	DRIVER_NAME	exmpp_compress_zlib

#define	BUF_SIZE	1024

/* Driver data. */
struct exmpp_compress_zlib_data {
	/* Options. */
	int		compress_level;
	int		use_gzip;

	z_stream	inf_z;
	z_stream	def_z;
};

#define	COPY_AND_FREE_BUF(to_send, size, b, ret)			\
	(size) = (to_send)->index + 1;					\
	(b) = driver_alloc_binary((size));				\
	(b)->orig_bytes[0] = (ret);					\
	memcpy((b)->orig_bytes + 1, (to_send)->buff,			\
	    (to_send)->index);						\
	exmpp_free_xbuf((to_send));

static void*
exmpp_compress_zlib_alloc(void *opaque, unsigned int items, unsigned int size)
{
	return driver_alloc(items*size);
}

static void
exmpp_compress_zlib_free(void *opaque, void *addr)
{
	driver_free(addr);
}

/* -------------------------------------------------------------------
 * Erlang port driver callbacks.
 * ------------------------------------------------------------------- */

static ErlDrvData
exmpp_compress_zlib_start(ErlDrvPort port, char *command)
{
	struct exmpp_compress_zlib_data *edd;

	/* Set binary mode. */
	set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);

	/* Allocate driver data structure. */
	edd = driver_alloc(sizeof(*edd));
	if (edd == NULL)
		return (NULL);

	edd->use_gzip = 0;
	edd->compress_level = Z_DEFAULT_COMPRESSION;

	edd->inf_z.next_in  = edd->def_z.next_in  = 0;
	edd->inf_z.avail_in = edd->def_z.avail_in = 0;
	edd->inf_z.zalloc   = edd->def_z.zalloc   = exmpp_compress_zlib_alloc;
	edd->inf_z.zfree    = edd->def_z.zfree    = exmpp_compress_zlib_free;
	edd->inf_z.opaque   = edd->def_z.opaque   = NULL;

	return (ErlDrvData)edd;
}

static void
exmpp_compress_zlib_stop(ErlDrvData drv_data)
{
	struct exmpp_compress_zlib_data *edd;

	edd = (struct exmpp_compress_zlib_data *)drv_data;

	inflateEnd(&edd->inf_z);
	deflateEnd(&edd->def_z);

	driver_free(edd);
}

static ErlDrvSSizeT
exmpp_compress_zlib_control(ErlDrvData drv_data, unsigned int command,
    char *buf, ErlDrvSizeT len, char **rbuf, ErlDrvSizeT rlen)
{
	struct exmpp_compress_zlib_data *edd;
	int ret, index, type, type_size;
	char atom[MAXATOMLEN];
	size_t size;
	long compress_level;
	ErlDrvBinary *b;
	ei_x_buff *to_send;
	z_stream *z;

	edd = (struct exmpp_compress_zlib_data *)drv_data;

	size = 0;
	b = NULL;

	switch (command) {
	case COMMAND_SET_COMPRESS_METHOD:
		index = exmpp_skip_version(buf);

		/* Get compression method. */
		ei_decode_atom(buf, &index, atom);

		if (strcmp(atom, "zlib") == 0) {
			edd->use_gzip = 0;
		} else if (strcmp(atom, "gzip") == 0) {
			edd->use_gzip = 1;
		} else {
			/* Only zlib is supported by this port driver. */
			to_send = exmpp_new_xbuf();
			if (to_send == NULL)
				return (-1);
			ei_x_encode_tuple_header(to_send, 2);
			ei_x_encode_atom(to_send,
			    "unsupported_compress_method");
			ei_x_encode_string(to_send, atom);

			COPY_AND_FREE_BUF(to_send, size, b, RET_ERROR);

			break;
		}

		break;
	case COMMAND_SET_COMPRESS_LEVEL:
		index = exmpp_skip_version(buf);

		/* Get the compression level. */
		ei_get_type(buf, &index, &type, &type_size);
		switch (type) {
		case ERL_INTEGER_EXT:
			ei_decode_long(buf, &index, &compress_level);

			if (compress_level < Z_NO_COMPRESSION ||
			    compress_level > Z_BEST_COMPRESSION) {
				/* Valid levels are 0..9. */
				to_send = exmpp_new_xbuf();
				if (to_send == NULL)
					return (-1);
				ei_x_encode_tuple_header(to_send, 2);
				ei_x_encode_atom(to_send,
				    "invalid_compress_level");
				ei_x_encode_long(to_send, compress_level);

				COPY_AND_FREE_BUF(to_send, size, b, RET_ERROR);

				goto out;
			}

			edd->compress_level = compress_level;

			break;
		case ERL_ATOM_EXT:
			ei_decode_atom(buf, &index, atom);

			if (strcmp(atom, "default") != 0) {
				/* Valid levels are 0..9. */
				to_send = exmpp_new_xbuf();
				if (to_send == NULL)
					return (-1);
				ei_x_encode_tuple_header(to_send, 2);
				ei_x_encode_atom(to_send,
				    "invalid_compress_level");
				ei_x_encode_atom(to_send, atom);

				COPY_AND_FREE_BUF(to_send, size, b, RET_ERROR);

				goto out;
			}

			break;
		}

		break;
	case COMMAND_PREPARE_COMPRESS:
	case COMMAND_PREPARE_UNCOMPRESS:
		if (command == COMMAND_PREPARE_COMPRESS) {
			if (edd->use_gzip)
				ret = deflateInit2(&edd->def_z,
				    edd->compress_level,
				    Z_DEFLATED, 15 + 16, 8,
				    Z_DEFAULT_STRATEGY);
			else
				ret = deflateInit(&edd->def_z,
				    edd->compress_level);
		} else {
			if (edd->use_gzip)
				ret = inflateInit2(&edd->inf_z,
				    15 + 16);
			else
				ret = inflateInit(&edd->inf_z);
		}

		if (ret != Z_OK) {
			to_send = exmpp_new_xbuf();
			if (to_send == NULL)
				return (-1);
			if (ret == Z_MEM_ERROR)
				ei_x_encode_atom(to_send, "no_memory");
			else if (ret == Z_VERSION_ERROR)
				ei_x_encode_atom(to_send,
				    "incompatible_zlib_version");
			else
				ei_x_encode_atom(to_send,
				    "zlib_internal_error");

			COPY_AND_FREE_BUF(to_send, size, b, RET_ERROR);

			break;
		}

		break;
	case COMMAND_COMPRESS:
	case COMMAND_UNCOMPRESS:
		size = BUF_SIZE + 1;
		rlen = 1;
		b = driver_alloc_binary(size);
		b->orig_bytes[0] = RET_OK;

		z = NULL;
		if (command == COMMAND_COMPRESS)
			z = &edd->def_z;
		else
			z = &edd->inf_z;

		z->next_in = (unsigned char *)buf;
		z->avail_in = len;
		z->avail_out = 0;
		ret = Z_OK;

		while (ret == Z_OK && z->avail_out == 0) {
			z->next_out = (unsigned char *)b->orig_bytes + rlen;
			z->avail_out = BUF_SIZE;

			if (command == COMMAND_COMPRESS) {
				ret = deflate(z, Z_SYNC_FLUSH);
				if (ret != Z_OK && ret != Z_STREAM_END) {
					driver_free_binary(b);

					to_send = exmpp_new_xbuf();
					if (to_send == NULL)
						return (-1);
					ei_x_encode_atom(to_send,
					    "deflate_error");

					COPY_AND_FREE_BUF(to_send, size, b,
					    RET_ERROR);

					goto out;
				}
			} else {
				ret = inflate(z, Z_SYNC_FLUSH);
				if (ret != Z_OK && ret != Z_STREAM_END) {
					driver_free_binary(b);

					to_send = exmpp_new_xbuf();
					if (to_send == NULL)
						return (-1);
					ei_x_encode_atom(to_send,
					    "inflate_error");

					COPY_AND_FREE_BUF(to_send, size, b,
					    RET_ERROR);

					goto out;
				}
			}

			rlen += (BUF_SIZE - z->avail_out);
			size += (BUF_SIZE - z->avail_out);
			b = driver_realloc_binary(b, size);
		}

		size = rlen;
		b = driver_realloc_binary(b, size);

		break;
	case COMMAND_SVN_REVISION:
		/* Store the revision in the buffer. */
		to_send = exmpp_new_xbuf();
		if (to_send == NULL)
			return (-1);
		ei_x_encode_string(to_send, "$Revision$");

		COPY_AND_FREE_BUF(to_send, size, b, RET_ERROR);

		break;
	default:
		/* Commad not recognized. */
		to_send = exmpp_new_xbuf();
		if (to_send == NULL)
			return (-1);
		ei_x_encode_tuple_header(to_send, 2);
		ei_x_encode_atom(to_send, "unknown_command");
		ei_x_encode_ulong(to_send, command);

		COPY_AND_FREE_BUF(to_send, size, b, RET_ERROR);
	}

out:
	if (b == NULL) {
		size = 1;
		b = driver_alloc_binary(size);
		b->orig_bytes[0] = RET_OK;
	}

	*rbuf = (char *)b;

	return (size);
}

/* -------------------------------------------------------------------
 * Driver declaration.
 * ------------------------------------------------------------------- */

static ErlDrvEntry compress_zlib_driver_entry = {
	NULL,				/* init */
	exmpp_compress_zlib_start,	/* start */
	exmpp_compress_zlib_stop,	/* stop */
	NULL,				/* output */
	NULL,				/* ready_input */
	NULL,				/* ready_output */
	S(DRIVER_NAME),			/* driver name */
	NULL,				/* finish */
	NULL,				/* handle */
	exmpp_compress_zlib_control,	/* control */
	NULL,				/* timeout */
	NULL				/* outputv */
};

DRIVER_INIT(DRIVER_NAME)
{
	compress_zlib_driver_entry.extended_marker = ERL_DRV_EXTENDED_MARKER;
	compress_zlib_driver_entry.major_version = ERL_DRV_EXTENDED_MAJOR_VERSION;
	compress_zlib_driver_entry.minor_version = ERL_DRV_EXTENDED_MINOR_VERSION;
#if defined(SMP_SUPPORT)
	compress_zlib_driver_entry.driver_flags = ERL_DRV_FLAG_USE_PORT_LOCKING;
#endif
	return &compress_zlib_driver_entry;
}
