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
#include <openssl/ssl.h>
#include <openssl/err.h>
#include <openssl/x509v3.h>

#include "exmpp_tls.h"

#if (defined(__MACH__) && defined(__APPLE__))
#  pragma GCC diagnostic ignored "-Wdeprecated-declarations"
#endif

#define	DRIVER_NAME	exmpp_tls_openssl
#define CIPHERS         "DEFAULT:!EXPORT:!LOW:!SSLv2"

#define	BUF_SIZE	1024

/* Driver data. */
struct exmpp_tls_openssl_data {
	int		 mode;

	/* Identity. */
	char		*certificate;
	char		*private_key;

	/* Peer verification. */
	int		 verify_peer;
	char		*expected_id;

	/* Options. */
	char		*trusted_certs;
	int		 peer_cert_required;
	int		 accept_expired_cert;
	int		 accept_revoked_cert;
	int		 accept_non_trusted_cert;
	int		 accept_corrupted_cert;

	SSL_CTX		*ctx;
	SSL		*ssl;
	BIO		*bio_read;
	BIO		*bio_write;
};

static int	init_library(struct exmpp_tls_openssl_data *edd,
		    ei_x_buff **to_send, size_t *size, ErlDrvBinary **b);
static int	verify_callback(int preverify_ok, X509_STORE_CTX *x509_ctx);

static int	ssl_ex_index;

#ifndef OPENSSL_NO_DH
/*
1024-bit MODP Group with 160-bit prime order subgroup (RFC5114)
-----BEGIN DH PARAMETERS-----
MIIBDAKBgQCxC4+WoIDgHd6S3l6uXVTsUsmfvPsGo8aaap3KUtI7YWBz4oZ1oj0Y
mDjvHi7mUsAT7LSuqQYRIySXXDzUm4O/rMvdfZDEvXCYSI6cIZpzck7/1vrlZEc4
+qMaT/VbzMChUa9fDci0vUW/N982XBpl5oz9p21NpwjfH7K8LkpDcQKBgQCk0cvV
w/00EmdlpELvuZkF+BBN0lisUH/WQGz/FCZtMSZv6h5cQVZLd35pD1UE8hMWAhe0
sBuIal6RVH+eJ0n01/vX07mpLuGQnQ0iY/gKdqaiTAh6CR9THb8KAWm2oorWYqTR
jnOvoy13nVkY0IvIhY9Nzvl8KiSFXm7rIrOy5QICAKA=
-----END DH PARAMETERS-----
 */
static unsigned char dh1024_p[] = {
        0xB1,0x0B,0x8F,0x96,0xA0,0x80,0xE0,0x1D,0xDE,0x92,0xDE,0x5E,
        0xAE,0x5D,0x54,0xEC,0x52,0xC9,0x9F,0xBC,0xFB,0x06,0xA3,0xC6,
        0x9A,0x6A,0x9D,0xCA,0x52,0xD2,0x3B,0x61,0x60,0x73,0xE2,0x86,
        0x75,0xA2,0x3D,0x18,0x98,0x38,0xEF,0x1E,0x2E,0xE6,0x52,0xC0,
        0x13,0xEC,0xB4,0xAE,0xA9,0x06,0x11,0x23,0x24,0x97,0x5C,0x3C,
        0xD4,0x9B,0x83,0xBF,0xAC,0xCB,0xDD,0x7D,0x90,0xC4,0xBD,0x70,
        0x98,0x48,0x8E,0x9C,0x21,0x9A,0x73,0x72,0x4E,0xFF,0xD6,0xFA,
        0xE5,0x64,0x47,0x38,0xFA,0xA3,0x1A,0x4F,0xF5,0x5B,0xCC,0xC0,
        0xA1,0x51,0xAF,0x5F,0x0D,0xC8,0xB4,0xBD,0x45,0xBF,0x37,0xDF,
        0x36,0x5C,0x1A,0x65,0xE6,0x8C,0xFD,0xA7,0x6D,0x4D,0xA7,0x08,
        0xDF,0x1F,0xB2,0xBC,0x2E,0x4A,0x43,0x71,
};
static unsigned char dh1024_g[] = {
        0xA4,0xD1,0xCB,0xD5,0xC3,0xFD,0x34,0x12,0x67,0x65,0xA4,0x42,
        0xEF,0xB9,0x99,0x05,0xF8,0x10,0x4D,0xD2,0x58,0xAC,0x50,0x7F,
        0xD6,0x40,0x6C,0xFF,0x14,0x26,0x6D,0x31,0x26,0x6F,0xEA,0x1E,
        0x5C,0x41,0x56,0x4B,0x77,0x7E,0x69,0x0F,0x55,0x04,0xF2,0x13,
        0x16,0x02,0x17,0xB4,0xB0,0x1B,0x88,0x6A,0x5E,0x91,0x54,0x7F,
        0x9E,0x27,0x49,0xF4,0xD7,0xFB,0xD7,0xD3,0xB9,0xA9,0x2E,0xE1,
        0x90,0x9D,0x0D,0x22,0x63,0xF8,0x0A,0x76,0xA6,0xA2,0x4C,0x08,
        0x7A,0x09,0x1F,0x53,0x1D,0xBF,0x0A,0x01,0x69,0xB6,0xA2,0x8A,
        0xD6,0x62,0xA4,0xD1,0x8E,0x73,0xAF,0xA3,0x2D,0x77,0x9D,0x59,
        0x18,0xD0,0x8B,0xC8,0x85,0x8F,0x4D,0xCE,0xF9,0x7C,0x2A,0x24,
        0x85,0x5E,0x6E,0xEB,0x22,0xB3,0xB2,0xE5,
};

static DH *dh1024;
#endif

/*
 * ECDHE is enabled only on OpenSSL 1.0.0e and later.
 * See http://www.openssl.org/news/secadv_20110906.txt
 * for details.
 */
#if OPENSSL_VERSION_NUMBER >= 0x1000005fL && !defined(OPENSSL_NO_ECDH)
static EC_KEY *ecdh;
#endif

#define	COPY_AND_FREE_BUF(to_send, size, b, ret)			\
	(size) = (to_send)->index + 1;					\
	(b) = driver_alloc_binary((size));				\
	(b)->orig_bytes[0] = (ret);					\
	memcpy((b)->orig_bytes + 1, (to_send)->buff,			\
	    (to_send)->index);						\
	exmpp_free_xbuf((to_send));

/* -------------------------------------------------------------------
 * Erlang port driver callbacks.
 * ------------------------------------------------------------------- */

static ErlDrvData
exmpp_tls_openssl_start(ErlDrvPort port, char *command)
{
	struct exmpp_tls_openssl_data *edd;

	/* Set binary mode. */
	set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);

	/* Allocate driver data structure. */
	edd = driver_alloc(sizeof(*edd));
	if (edd == NULL)
		return (NULL);

	memset(edd, 0, sizeof(*edd));

	return (ErlDrvData)edd;
}

static void
exmpp_tls_openssl_stop(ErlDrvData drv_data)
{
	struct exmpp_tls_openssl_data *edd;

	edd = (struct exmpp_tls_openssl_data *)drv_data;

	if (edd->certificate != NULL)
		driver_free(edd->certificate);
	if (edd->private_key != NULL)
		driver_free(edd->private_key);
	if (edd->expected_id != NULL)
		driver_free(edd->expected_id);
	if (edd->trusted_certs != NULL)
		driver_free(edd->trusted_certs);
	if (edd->ssl != NULL)
		SSL_free(edd->ssl);
	if (edd->ctx != NULL)
		SSL_CTX_free(edd->ctx);

	driver_free(edd);
}

static ErlDrvSSizeT
exmpp_tls_openssl_control(ErlDrvData drv_data, unsigned int command,
    char *buf, ErlDrvSizeT len, char **rbuf, ErlDrvSizeT rlen)
{
	struct exmpp_tls_openssl_data *edd;
	int ret, index, arity, type, type_size, flag;
	char atom[MAXATOMLEN];
	size_t size;
	long mode, verify_result;
	unsigned long data_len;
	unsigned char *out;
	ErlDrvBinary *b;
	ei_x_buff *to_send;
	X509 *cert;

	edd = (struct exmpp_tls_openssl_data *)drv_data;

	size = 0;
	b = NULL;

	/*
	 * We never check return codes against SSL_ERROR_WANT_WRITE because
	 * writes to BIO_mem() always succeed and OpenSSL can't tell if
	 * the data was effectively written to the socket.
	 */

	switch (command) {
	case COMMAND_SET_MODE:
		index = exmpp_skip_version(buf);

		/* Get the mode (client vs. server). */
		ei_decode_long(buf, &index, &mode);
		edd->mode = mode;

		break;
	case COMMAND_SET_IDENTITY:
		index = exmpp_skip_version(buf);

		/* Get auth method. */
		ei_decode_tuple_header(buf, &index, &arity);
		ei_decode_atom(buf, &index, atom);
		if (strcmp(atom, "x509") != 0) {
			/* Only X.509 is supported by this port driver. */
			to_send = exmpp_new_xbuf();
			if (to_send == NULL)
				return (-1);
			ei_x_encode_tuple_header(to_send, 2);
			ei_x_encode_atom(to_send, "unsupported_auth_method");
			ei_x_encode_string(to_send, atom);

			COPY_AND_FREE_BUF(to_send, size, b, RET_ERROR);

			break;
		}

		/* Get certificate filename. */
		ei_get_type(buf, &index, &type, &type_size);
		edd->certificate = driver_alloc(type_size + 1);
		if (edd->certificate == NULL)
			return (-1);
		ei_decode_string(buf, &index, edd->certificate);

		/* Get private key filename. */
		ei_get_type(buf, &index, &type, &type_size);
		edd->private_key = driver_alloc(type_size + 1);
		if (edd->private_key == NULL)
			return (-1);
		ei_decode_string(buf, &index, edd->private_key);

		break;
	case COMMAND_SET_PEER_VERIF:
		index = exmpp_skip_version(buf);

		/* Check if the identity of the remote peer must be
		 * verified. */
		ei_get_type(buf, &index, &type, &type_size);
		switch (type) {
		case ERL_ATOM_EXT:
			/* The peer will be checked by OpenSSL. */
			ei_decode_boolean(buf, &index, &(edd->verify_peer));
			break;
		case ERL_STRING_EXT:
			/* The peer will be checked by OpenSSL, then
			 * the certificate will be compared to the
			 * given expected identity. */
			edd->expected_id = driver_alloc(type_size + 1);
			if (edd->expected_id == NULL)
				return (-1);
			ei_decode_string(buf, &index, edd->expected_id);
			edd->verify_peer = 1;
			break;
		}

		break;
	case COMMAND_SET_TRUSTED_CERTS:
		index = exmpp_skip_version(buf);

		/* Get auth method. */
		ei_decode_tuple_header(buf, &index, &arity);
		ei_decode_atom(buf, &index, atom);
		if (strcmp(atom, "x509") != 0) {
			/* Only X.509 is supported by this port driver. */
			to_send = exmpp_new_xbuf();
			if (to_send == NULL)
				return (-1);
			ei_x_encode_tuple_header(to_send, 2);
			ei_x_encode_atom(to_send, "unsupported_auth_method");
			ei_x_encode_string(to_send, atom);

			COPY_AND_FREE_BUF(to_send, size, b, RET_ERROR);

			break;
		}

		/* Get the filename for the trusted certificates. */
		ei_get_type(buf, &index, &type, &type_size);
		edd->trusted_certs = driver_alloc(type_size + 1);
		if (edd->trusted_certs == NULL)
			return (-1);
		ei_decode_string(buf, &index, edd->trusted_certs);

		break;
	case COMMAND_SET_OPTIONS:
		index = exmpp_skip_version(buf);

		/* Get auth method. */
		ei_decode_tuple_header(buf, &index, &arity);
		ei_decode_atom(buf, &index, atom);
		ei_decode_boolean(buf, &index, &flag);

		if (strcmp(atom, "peer_cert_required") == 0)
			edd->peer_cert_required = flag;
		else if (strcmp(atom, "accept_expired_cert") == 0)
			edd->accept_expired_cert = flag;
		else if (strcmp(atom, "accept_non_trusted_cert") == 0)
			edd->accept_non_trusted_cert = flag;
		else if (strcmp(atom, "accept_revoked_cert") == 0)
			edd->accept_revoked_cert = flag;
		else if (strcmp(atom, "accept_corrupted_cert") == 0)
			edd->accept_corrupted_cert = flag;
		else {
			to_send = exmpp_new_xbuf();
			if (to_send == NULL)
				return (-1);
			ei_x_encode_tuple_header(to_send, 2);
			ei_x_encode_atom(to_send, "unsupported_option");
			ei_x_encode_atom(to_send, atom);

			COPY_AND_FREE_BUF(to_send, size, b, RET_ERROR);

			break;
		}

		break;
	case COMMAND_PREPARE_HANDSHAKE:
		ret = init_library(edd, &to_send, &size, &b);
		if (ret != 0) {
			/* Initialization failed. */
			break;
		}

		break;
	case COMMAND_HANDSHAKE:
		/* Try handshake. */
		ret = SSL_do_handshake(edd->ssl);
		if (ret <= 0) {
			switch (SSL_get_error(edd->ssl, ret)) {
			case SSL_ERROR_WANT_READ:
				/* OpenSSL is waiting for more data. */
				size = 1;
				b = driver_alloc_binary(size);
				b->orig_bytes[0] = RET_WANT_READ;

				break;
			default:
				/* An error occured. */
				ret = ERR_get_error();

				to_send = exmpp_new_xbuf();
				if (to_send == NULL)
					return (-1);
				ei_x_encode_tuple_header(to_send, 2);
				ei_x_encode_long(to_send, ret);
				ei_x_encode_string(to_send,
				    ERR_error_string(ret, NULL));

				COPY_AND_FREE_BUF(to_send, size, b, RET_ERROR);
			}
		}

		break;
	case COMMAND_SET_ENCRYPTED_INPUT:
		BIO_write(edd->bio_read, buf, len);

		break;
	case COMMAND_GET_DECRYPTED_INPUT:
		index = exmpp_skip_version(buf);

		/* Get data length the caller is waiting for. */
		ei_decode_ulong(buf, &index, &data_len);
		if (data_len == 0)
			data_len = BUF_SIZE;

		/* Allocate binary to copy decrypted data. */
		rlen = data_len + 1;
		size = 1;
		b = driver_alloc_binary(rlen);
		b->orig_bytes[0] = RET_OK;

		/* Copy data. */
		ret = SSL_read(edd->ssl, b->orig_bytes + size, data_len);

		/* Check for errors. */
		if (ret > 0) {
			size += ret;
			b = driver_realloc_binary(b, size);
		} else {
			driver_free_binary(b);
			b = NULL;

			switch (SSL_get_error(edd->ssl, ret)) {
			case SSL_ERROR_WANT_READ:
				/* OpenSSL is waiting for more data. */
				size = 1;
				b = driver_alloc_binary(size);
				b->orig_bytes[0] = RET_WANT_READ;

				break;
			default:
				/* An error occured. */
				to_send = exmpp_new_xbuf();
				if (to_send == NULL)
					return (-1);
				ei_x_encode_atom(to_send, "decrypt_failed");

				COPY_AND_FREE_BUF(to_send, size, b, RET_ERROR);
			}
		}

		break;
	case COMMAND_SET_DECRYPTED_OUTPUT:
		ret = SSL_write(edd->ssl, buf, len);
		if (ret <= 0) {
			switch (SSL_get_error(edd->ssl, ret)) {
			case SSL_ERROR_WANT_READ:
				/* OpenSSL is waiting for more data. */
				size = 1;
				b = driver_alloc_binary(size);
				b->orig_bytes[0] = RET_WANT_READ;

				break;
			default:
				/* An error occured. */
				to_send = exmpp_new_xbuf();
				if (to_send == NULL)
					return (-1);
				ei_x_encode_atom(to_send, "encrypt_failed");

				COPY_AND_FREE_BUF(to_send, size, b, RET_ERROR);
			}
		}

		break;
	case COMMAND_GET_ENCRYPTED_OUTPUT:
		/* Allocate binary to copy encrypted data. */
		size = BIO_ctrl_pending(edd->bio_write) + 1;
		b = driver_alloc_binary(size);
		b->orig_bytes[0] = RET_OK;

		BIO_read(edd->bio_write, b->orig_bytes + 1, size - 1);

		break;
	case COMMAND_GET_PEER_CERTIFICATE:
		/* Get the peer certificate. */
		cert = SSL_get_peer_certificate(edd->ssl);
		if (cert == NULL || (rlen = i2d_X509(cert, NULL)) < 0) {
			to_send = exmpp_new_xbuf();
			if (to_send == NULL)
				return (-1);
			ei_x_encode_atom(to_send, "no_certificate");

			COPY_AND_FREE_BUF(to_send, size, b, RET_ERROR);
			if (cert != NULL) {
				X509_free(cert);
			}
			break;
		}

		/* Copy it to a binary. */
		size = rlen + 1;
		b = driver_alloc_binary(size);
		b->orig_bytes[0] = RET_OK;
		out = (unsigned char *)&(b->orig_bytes[1]);
		i2d_X509(cert, &out);
		X509_free(cert);

		break;
	case COMMAND_GET_VERIFY_RESULT:
		verify_result = SSL_get_verify_result(edd->ssl);

		to_send = exmpp_new_xbuf();
		if (to_send == NULL)
			return (-1);
		ei_x_encode_long(to_send, verify_result);

		COPY_AND_FREE_BUF(to_send, size, b, RET_OK);

		break;
	case COMMAND_SHUTDOWN:
		type = SSL_get_shutdown(edd->ssl);
		ret = SSL_shutdown(edd->ssl);
		if (ret == 1) {
			/* The shutdown is complete but if the peer
			 * initiated it, the output buffer contains
			 * our "close notify". */
			if (!(type & SSL_SENT_SHUTDOWN)) {
				/* Our "close notify" must be sent now. */
				size = 1;
				b = driver_alloc_binary(size);
				b->orig_bytes[0] = RET_WANT_WRITE;
			}

			break;
		} else if (ret == 0) {
			/* We are waiting for the peer "close notify" */
			if (!(type & SSL_SENT_SHUTDOWN)) {
				/* Our "close notify" must be sent now. */
				size = 1;
				b = driver_alloc_binary(size);
				b->orig_bytes[0] = RET_WANT_WRITE;
			} else {
				/* Ouf "close notify" was already sent. */
				size = 1;
				b = driver_alloc_binary(size);
				b->orig_bytes[0] = RET_WANT_READ;
			}

			break;
		} else if (ret < 0) {
			switch (SSL_get_error(edd->ssl, ret)) {
			case SSL_ERROR_WANT_READ:
				/* OpenSSL is waiting for more data. */
				size = 1;
				b = driver_alloc_binary(size);
				b->orig_bytes[0] = RET_WANT_READ;

				break;
			default:
				/* An error occured. */
				ret = ERR_get_error();

				to_send = exmpp_new_xbuf();
				if (to_send == NULL)
					return (-1);
				ei_x_encode_tuple_header(to_send, 2);
				ei_x_encode_long(to_send, ret);
				ei_x_encode_string(to_send,
				    ERR_error_string(ret, NULL));

				COPY_AND_FREE_BUF(to_send, size, b, RET_ERROR);
			}
		}

		break;
	case COMMAND_QUIET_SHUTDOWN:
		SSL_set_shutdown(edd->ssl,
		    SSL_SENT_SHUTDOWN | SSL_RECEIVED_SHUTDOWN);

		break;
	case COMMAND_PORT_REVISION:
		/* Store the revision in the buffer. */
		to_send = exmpp_new_xbuf();
		if (to_send == NULL)
			return (-1);
		ei_x_encode_string(to_send, "$Revision$");

		COPY_AND_FREE_BUF(to_send, size, b, RET_ERROR);

		break;
	case COMMAND_GET_PEER_FINISHED:
		size = BUF_SIZE + 1;
		b = driver_alloc_binary(size);
		b->orig_bytes[0] = RET_OK;
		ret = SSL_get_peer_finished(edd->ssl, &(b->orig_bytes[1]), BUF_SIZE);
		size = ret + 1;
		b = driver_realloc_binary(b, size);
		break;
	case COMMAND_GET_FINISHED:
		size = BUF_SIZE + 1;
		b = driver_alloc_binary(size);
		b->orig_bytes[0] = RET_OK;
		ret = SSL_get_finished(edd->ssl, &(b->orig_bytes[1]), BUF_SIZE);
		size = ret + 1;
		b = driver_realloc_binary(b, size);
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

	if (b == NULL) {
		size = 1;
		b = driver_alloc_binary(size);
		b->orig_bytes[0] = RET_OK;
	}

	*rbuf = (char *)b;

	return (size);
}

/* -------------------------------------------------------------------
 * Internal functions.
 * ------------------------------------------------------------------- */

static int
init_library(struct exmpp_tls_openssl_data *edd,
    ei_x_buff **to_send, size_t *size, ErlDrvBinary **b)
{
	int ret, verify;

	/* Create an SSL context. */
	edd->ctx = SSL_CTX_new(SSLv23_method());
	if (edd->ctx == NULL) {
		*to_send = exmpp_new_xbuf();
		if (*to_send == NULL)
			return (-1);
		ei_x_encode_atom(*to_send,
		    "ssl_context_init_failed");

		COPY_AND_FREE_BUF(*to_send, *size, *b, RET_ERROR);

		goto err;
	}

	// SSL 2.0 is deprecated for many years
	SSL_CTX_set_options(edd->ctx, SSL_OP_NO_SSLv2);

	SSL_CTX_set_cipher_list(edd->ctx, CIPHERS);

	/*
	 * Since sessions are cached in SSL_CTX and currently new context
	 * is used for every connection, then session caching makes little
	 * sense, turn it off.
	 */
	SSL_CTX_set_session_cache_mode(edd->ctx, SSL_SESS_CACHE_OFF);
	SSL_CTX_set_options(edd->ctx, SSL_OP_NO_TICKET);
#ifdef SSL_MODE_RELEASE_BUFFERS
	/*
	 * This appeared in OpenSSL 1.0.0,
	 * reduces memory usage on idle connections.
	 */
	SSL_CTX_set_mode(edd->ctx, SSL_MODE_RELEASE_BUFFERS);
#endif

#ifndef OPENSSL_NO_DH
	if (dh1024 != NULL && edd->mode == TLS_MODE_SERVER) {
		SSL_CTX_set_options(edd->ctx, SSL_OP_SINGLE_DH_USE);
		SSL_CTX_set_tmp_dh(edd->ctx, dh1024);
	}
#endif
#if OPENSSL_VERSION_NUMBER >= 0x1000005fL && !defined(OPENSSL_NO_ECDH)
	if (ecdh != NULL && edd->mode == TLS_MODE_SERVER) {
		SSL_CTX_set_options(edd->ctx, SSL_OP_SINGLE_ECDH_USE);
		SSL_CTX_set_tmp_ecdh(edd->ctx, ecdh);
	}
#endif

	/* Set our certificate. */
	if (edd->certificate != NULL) {
		ret = SSL_CTX_use_certificate_chain_file(edd->ctx,
		    edd->certificate);
		if (ret != 1) {
			*to_send = exmpp_new_xbuf();
			if (*to_send == NULL)
				goto err;
			ei_x_encode_atom(*to_send,
			    "load_cert_failed");

			COPY_AND_FREE_BUF(*to_send, *size, *b, RET_ERROR);

			goto err;
		}
	}

	/* Set the private key. */
	if (edd->private_key != NULL) {
		ret = SSL_CTX_use_PrivateKey_file(edd->ctx,
		    edd->private_key, SSL_FILETYPE_PEM);
		if (ret != 1) {
			*to_send = exmpp_new_xbuf();
			if (*to_send == NULL)
				goto err;
			ei_x_encode_atom(*to_send,
			    "load_pk_failed");

			COPY_AND_FREE_BUF(*to_send, *size, *b, RET_ERROR);

			goto err;
		}
	}

	/* Prepare OpenSSL for verification. */
	verify = edd->verify_peer ?
	    SSL_VERIFY_PEER | SSL_VERIFY_CLIENT_ONCE : SSL_VERIFY_NONE;
	verify |= edd->peer_cert_required ?
	    SSL_VERIFY_FAIL_IF_NO_PEER_CERT : 0;
	SSL_CTX_set_verify(edd->ctx, verify, verify_callback);

	/* Set trusted certificates. */
	if (edd->trusted_certs != NULL) {
		ret = SSL_CTX_load_verify_locations(edd->ctx,
		    edd->trusted_certs, NULL);
		if (ret != 1) {
			*to_send = exmpp_new_xbuf();
			if (*to_send == NULL)
				goto err;
			ei_x_encode_atom(*to_send,
			    "load_trusted_certs_failed");

			COPY_AND_FREE_BUF(*to_send, *size, *b, RET_ERROR);

			goto err;
		}
	}

	/* Create an SSL connection handle. */
	edd->ssl = SSL_new(edd->ctx);
	if (edd->ssl == NULL) {
		*to_send = exmpp_new_xbuf();
		if (*to_send == NULL)
			goto err;
		ei_x_encode_atom(*to_send,
		    "ssl_init_failed");

		COPY_AND_FREE_BUF(*to_send, *size, *b, RET_ERROR);

		goto err;
	}

	/* Associate buffers. */
	edd->bio_read = BIO_new(BIO_s_mem());
	edd->bio_write = BIO_new(BIO_s_mem());
	SSL_set_bio(edd->ssl, edd->bio_read, edd->bio_write);

	/* Set SSL state. */
	switch (edd->mode) {
	case TLS_MODE_SERVER:
		SSL_set_accept_state(edd->ssl);
		break;
	case TLS_MODE_CLIENT:
		SSL_set_connect_state(edd->ssl);
		break;
	}

	/* Add our own data to SSL. This will be used by verify_callback. */
	SSL_set_ex_data(edd->ssl, ssl_ex_index, edd);

	return (0);

err:
	if (edd->ssl != NULL) {
		SSL_free(edd->ssl);
		edd->ssl = NULL;
	}
	if (edd->ctx != NULL) {
		SSL_CTX_free(edd->ctx);
		edd->ctx = NULL;
	}

	return (-1);
}

static int
verify_callback(int preverify_ok, X509_STORE_CTX *x509_ctx)
{
	SSL *ssl;
	struct exmpp_tls_openssl_data *edd;
	int err, depth, gens_count, i;
	X509 *cert;
	STACK_OF(GENERAL_NAME) *gens;
	const GENERAL_NAME *gen;
	char *dnsname;

	/* Get the port driver's private data. We need it to access
	 * verification options. */
	ssl = X509_STORE_CTX_get_ex_data(x509_ctx,
	    SSL_get_ex_data_X509_STORE_CTX_idx());
	edd = SSL_get_ex_data(ssl, ssl_ex_index);

	if (!preverify_ok) {
		/* The verification done by OpenSSL failed. We check if the
		 * error is acceptable for the user. */

		/* Get the verification error code. */
		err = X509_STORE_CTX_get_error(x509_ctx);

		/* Is this kind of error is accepted? */
		switch (err) {
		case X509_V_ERR_UNABLE_TO_GET_ISSUER_CERT:
		case X509_V_ERR_UNABLE_TO_GET_CRL:
		case X509_V_ERR_CERT_SIGNATURE_FAILURE:
		case X509_V_ERR_CRL_SIGNATURE_FAILURE:
		case X509_V_ERR_DEPTH_ZERO_SELF_SIGNED_CERT:
		case X509_V_ERR_SELF_SIGNED_CERT_IN_CHAIN:
		case X509_V_ERR_UNABLE_TO_GET_ISSUER_CERT_LOCALLY:
		case X509_V_ERR_UNABLE_TO_VERIFY_LEAF_SIGNATURE:
		case X509_V_ERR_CERT_CHAIN_TOO_LONG:
		case X509_V_ERR_INVALID_CA:
		case X509_V_ERR_PATH_LENGTH_EXCEEDED:
		case X509_V_ERR_INVALID_PURPOSE:
		case X509_V_ERR_CERT_UNTRUSTED:
		case X509_V_ERR_CERT_REJECTED:
		case X509_V_ERR_SUBJECT_ISSUER_MISMATCH:
		case X509_V_ERR_AKID_SKID_MISMATCH:
		case X509_V_ERR_AKID_ISSUER_SERIAL_MISMATCH:
		case X509_V_ERR_KEYUSAGE_NO_CERTSIGN:
			/* The peer certificate isn't trusted. */
			if (!edd->accept_non_trusted_cert)
				return (0);
			break;
		case X509_V_ERR_CERT_NOT_YET_VALID:
		case X509_V_ERR_CERT_HAS_EXPIRED:
		case X509_V_ERR_CRL_NOT_YET_VALID:
		case X509_V_ERR_CRL_HAS_EXPIRED:
			/* The peer certificate or a CA certificate has
			 * expired. */
			if (!edd->accept_expired_cert)
				return (0);
			break;
		case X509_V_ERR_CERT_REVOKED:
			/* The peer certificate or a CA certificate was
			 * revoked. */
			if (!edd->accept_revoked_cert)
				return (0);
			break;
		case X509_V_ERR_UNABLE_TO_DECRYPT_CERT_SIGNATURE:
		case X509_V_ERR_UNABLE_TO_DECRYPT_CRL_SIGNATURE:
		case X509_V_ERR_UNABLE_TO_DECODE_ISSUER_PUBLIC_KEY:
		case X509_V_ERR_ERROR_IN_CERT_NOT_BEFORE_FIELD:
		case X509_V_ERR_ERROR_IN_CERT_NOT_AFTER_FIELD:
		case X509_V_ERR_ERROR_IN_CRL_LAST_UPDATE_FIELD:
		case X509_V_ERR_ERROR_IN_CRL_NEXT_UPDATE_FIELD:
			/* The verification couldn't be made because of
			 * bad data. */
			if (!edd->accept_corrupted_cert)
				return (0);
			break;
		default:
			/* Other types of error are not accepted. */
			return (0);
		}
	}

	/* Basic verification was successful. We now proceed with a more
	 * XMPP-oriented verification, only on the peer certificate and
	 * only if the caller specified an expected ID. */

	/* XXX We need an option to accept non-matching certificates. */

	/* Peer certificate is at depth 0. */
	depth = X509_STORE_CTX_get_error_depth(x509_ctx);
	if (depth > 0 || edd->expected_id == NULL)
		return (1);

	/* Get the peer certificate. */
	cert = X509_STORE_CTX_get_current_cert(x509_ctx);

	/* Get 'subjectAltName' extensions. */
	gens = X509_get_ext_d2i(cert, NID_subject_alt_name, 0, 0);
	if (gens) {
		gens_count = sk_GENERAL_NAME_num(gens);
		dnsname = NULL;

		/* XXX We must first check for the 'xmpp' extension. */

		/* We walk the extensions list to find the 'dNSName'
		 * extension. */
		for (i = 0; i < gens_count; ++i) {
			gen = sk_GENERAL_NAME_value(gens, i);
			if (gen->type != GEN_DNS)
				/* This one is not a 'dNSName' extension. */
				continue;

			/* We expect the OpenSSL library to construct
			 * GEN_DNS extesion objects as ASN1_IA5STRING
			 * values. Check we got the right union member. */
			if (ASN1_STRING_type(gen->d.ia5) != V_ASN1_IA5STRING)
				continue;

			dnsname = (char *)ASN1_STRING_data(gen->d.ia5);

			/* ASN1_IA5STRING may contain NUL character; check
			 * it. */
			if (ASN1_STRING_length(gen->d.ia5) != strlen(dnsname))
				continue;

			/* Safe to treat as an ASCII string possibly
			 * holding a DNS name */
			if (match_hostname(dnsname, edd->expected_id)) {
				sk_GENERAL_NAME_pop_free(gens,
				    GENERAL_NAME_free);
				return (1);
			}
		}

		if (dnsname != NULL) {
			/* A 'dNSName' extension was found but didn't
			 * match. The certificate is not acceptable. */
			/* XXX We must set the relevant X.509 error. */
			sk_GENERAL_NAME_pop_free(gens, GENERAL_NAME_free);
			return (0);
		}

		sk_GENERAL_NAME_pop_free(gens, GENERAL_NAME_free);
	}

	/* XXX We now fallback on the commonName extension. */

	return (0);
}

/* -------------------------------------------------------------------
 * Driver declaration.
 * ------------------------------------------------------------------- */

static ErlDrvEntry tls_openssl_driver_entry = {
	NULL,				/* init */
	exmpp_tls_openssl_start,	/* start */
	exmpp_tls_openssl_stop,		/* stop */
	NULL,				/* output */
	NULL,				/* ready_input */
	NULL,				/* ready_output */
	S(DRIVER_NAME),			/* driver name */
	NULL,				/* finish */
	NULL,				/* handle */
	exmpp_tls_openssl_control,	/* control */
	NULL,				/* timeout */
	NULL				/* outputv */
};

DRIVER_INIT(DRIVER_NAME)
{
	/* Initialize OpenSSL. */
	SSL_library_init();
	SSL_load_error_strings();

	/* This index is used to store application-specific data inside
	 * SSL structs. This index is the same for each SSL instance,
	 * that's why we keep it global.
	 *
	 * See:
	 * http://www.mail-archive.com/openssl-users@openssl.org/msg52326.html
	 */
	ssl_ex_index = SSL_get_ex_new_index(0, "exmpp_tls_openssl_data",
	    NULL, NULL, NULL);

#ifndef OPENSSL_NO_DH
	// Initialize ephemeral Diffie-Hellman parameters.
	dh1024 = DH_new();
	if (dh1024 != NULL) {
		dh1024->p = BN_bin2bn(dh1024_p, sizeof(dh1024_p), NULL);
		dh1024->g = BN_bin2bn(dh1024_g, sizeof(dh1024_g), NULL);
		if (dh1024->p == NULL || dh1024->g == NULL) {
			DH_free(dh1024);
			dh1024 = NULL;
		}
	}
#endif

#if OPENSSL_VERSION_NUMBER >= 0x1000005fL && !defined(OPENSSL_NO_ECDH)
	ecdh = EC_KEY_new_by_curve_name(NID_X9_62_prime256v1);
#endif

	tls_openssl_driver_entry.extended_marker = ERL_DRV_EXTENDED_MARKER;
	tls_openssl_driver_entry.major_version = ERL_DRV_EXTENDED_MAJOR_VERSION;
	tls_openssl_driver_entry.minor_version = ERL_DRV_EXTENDED_MINOR_VERSION;
#if defined(SMP_SUPPORT)
	/**
	 * To make OpenSSL thread-safe, two callbacks must be set
	 * as described in http://www.openssl.org/docs/crypto/threads.html
	 *
	 * However, OTP comes with crypto module, that links with OpenSSL
	 * and sets the needed callbacks itself. If another set of
	 * callbacks had been provided here, it would overwrite
	 * or be overwritten by those from crypto module.
	 *
	 * So instead of providing callbacks, start crypto module
	 * from Erlang code before loading this driver. As a result
	 * crypto module will install the needed callbacks and
	 * this driver also can be made thread safe.
	 */
	if (CRYPTO_get_locking_callback() != NULL &&
	    CRYPTO_get_id_callback() != NULL) {
		tls_openssl_driver_entry.driver_flags = ERL_DRV_FLAG_USE_PORT_LOCKING;
	}
#endif

	return &tls_openssl_driver_entry;
}
