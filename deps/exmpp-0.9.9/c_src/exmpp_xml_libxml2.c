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

#include <libxml/parser.h>
#include <string.h>

#include "exmpp_xml.h"

#define	DRIVER_NAME	exmpp_xml_libxml2

/* Driver data (also, user data for LibXML2). */
struct exmpp_xml_data {
	/* Generic context. */
	struct exmpp_xml_ctx	  ctx;

	/* Driver instance's context. */
	xmlParserCtxtPtr	  parser;
	int			  declared_nss_count;
	int			  attrs_count;
	const xmlChar		**declared_nss;
	const xmlChar		**attrs;
};

/* LibXML2 handler prototypes */
static void		libxml2_cb_start_element(void *user_data,
			    const xmlChar *elem,
			    const xmlChar *prefix, const xmlChar *ns,
			    int declared_nss_count,
			    const xmlChar **declared_nss,
			    int attrs_count, int attrs_defaulted,
			    const xmlChar **attrs);
static void		libxml2_cb_end_element(void *user_data,
			    const xmlChar *elem,
			    const xmlChar *prefix, const xmlChar *ns);
static xmlEntityPtr	libxml2_cb_get_entity(void *user_data,
			    const xmlChar *name);
static void		libxml2_cb_character_data(void *user_data,
			    const xmlChar *data, int len);

static make_declared_nss_cb	exmpp_xml_cb_make_declared_nss;
static make_attributes_cb	exmpp_xml_cb_make_attributes;

static int		create_parser(struct exmpp_xml_data *edd);
static void		destroy_parser(struct exmpp_xml_data *edd);

/* LibXML2 SAX handler. */
static xmlSAXHandler sax_handler;

/* -------------------------------------------------------------------
 * Erlang port driver callbacks.
 * ------------------------------------------------------------------- */

static int
exmpp_xml_init()
{

	/* Initialize the global known lists. */
	if (init_known_lists() != 0)
		return (-1);

	if (xmlMemSetup(driver_free, driver_alloc,
			driver_realloc, exmpp_strdup) != 0) {
		return (-1);
	}

	/* Initialize and check LibXML2. */
	LIBXML_TEST_VERSION; /* be safe, plus calls xmlInitParser */

	/* Initialize SAX callbacks. */
	sax_handler.initialized = XML_SAX2_MAGIC;
	sax_handler.startElementNs = libxml2_cb_start_element;
	sax_handler.endElementNs = libxml2_cb_end_element;
	sax_handler.getEntity = libxml2_cb_get_entity;
	sax_handler.characters = libxml2_cb_character_data;

	return (0);
}

static void
exmpp_xml_finish()
{

	/* Terminate LibXML2. */
	xmlCleanupParser();

	/* Free the global known lists. */
	free_known_lists();
}

static ErlDrvData
exmpp_xml_start(ErlDrvPort port, char *command)
{
	struct exmpp_xml_data *edd;

	/* Set binary mode. */
	set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);

	/* Allocate driver data structure. */
	edd = driver_alloc(sizeof(*edd));
	if (edd == NULL)
		return (ERL_DRV_ERROR_GENERAL);

	/* Initialize generic context. */
	if (init_context(&edd->ctx) != 0) {
		driver_free(edd);
		return (ERL_DRV_ERROR_GENERAL);
	}
	edd->ctx.make_declared_nss = exmpp_xml_cb_make_declared_nss;
	edd->ctx.make_attributes = exmpp_xml_cb_make_attributes;

	/* Initialize driver instance's context. */
	edd->parser = NULL;

	return ((ErlDrvData)edd);
}

static void
exmpp_xml_stop(ErlDrvData drv_data)
{
	struct exmpp_xml_data *edd;

	edd = (struct exmpp_xml_data *)drv_data;

	/* Destroy the parser. */
	destroy_parser(edd);

	/* Free generic context. */
	free_context(&edd->ctx);

	/* Free driver data structure. */
	driver_free(edd);
}

static ErlDrvSSizeT
exmpp_xml_control(ErlDrvData drv_data, unsigned int command,
    char *buf, ErlDrvSizeT len, char **rbuf, ErlDrvSizeT rlen)
{
	struct exmpp_xml_data *edd;
	ei_x_buff *to_return;
	ErlDrvBinary *bin;
	int size, ret;

	edd = (struct exmpp_xml_data *)drv_data;
	size = 0;
	bin = NULL;
	to_return = NULL;

	switch (command) {
	/*
	 * Parsing.
	 */

	case COMMAND_PARSE:
	case COMMAND_PARSE_FINAL:
		if (edd->parser == NULL) {
			/* Start a parser. */
			if (create_parser(edd) != 0) {
				to_return = exmpp_new_xbuf();
				if (to_return == NULL)
					return (-1);

				ret = RET_ERROR;
				ei_x_encode_atom(to_return,
				    "parser_setup_failed");

				break;
			}
		}

		/* Control the total size of data to parse. */
		if (!is_data_size_under_limit(&edd->ctx, len)) {
			to_return = exmpp_new_xbuf();
			if (to_return == NULL)
				return (-1);

			ret = RET_ERROR;
			ei_x_encode_atom(to_return, "stanza_too_big");

			break;
		}

		/* Run XML document parsing. */
		ret = xmlParseChunk(edd->parser, buf, len,
		    command == COMMAND_PARSE_FINAL);

		if (ret > 0) {
			xmlError *error;

			/* An error occured during parsing; most probably,
			 * XML wasn't well-formed. */
			error = xmlCtxtGetLastError(edd->parser);

			to_return = exmpp_new_xbuf();
			if (to_return == NULL)
				return (-1);

			ret = RET_ERROR;
			ei_x_encode_tuple_header(to_return, 2);
			ei_x_encode_atom(to_return, "parsing_failed");
			ei_x_encode_tuple_header(to_return, 2);
			ei_x_encode_long(to_return, error->code);
			ei_x_encode_string(to_return, error->message);

			break;
		}

		/* Return the complete tree(s). */
		ret = RET_OK;
		if (edd->ctx.complete_trees_ready) {
			/* Terminate the complete trees list. */
			ei_x_encode_empty_list(edd->ctx.complete_trees);

			to_return = edd->ctx.complete_trees;
			size = 1 + to_return->index;
			bin = driver_alloc_binary(size);
			if (bin == NULL)
				return (-1);
			bin->orig_bytes[0] = (char)ret;
			memcpy(bin->orig_bytes + 1,
			    to_return->buff, to_return->index);
		} else {
			/* We need more data to produce a tree. */
			to_return = exmpp_new_xbuf();
			if (to_return == NULL)
				return (-1);

			ei_x_encode_atom(to_return,
			    command == COMMAND_PARSE ? "continue" : "done");
		}

		if (command == COMMAND_PARSE) {
			/* Update the size of processed data. */
			add_data_size(&edd->ctx, len);

			/* Reset the complete trees list. */
			reset_complete_trees(&edd->ctx);
		} else {
			/* We're done with the parser. */
			destroy_parser(edd);
		}

		break;

	case COMMAND_RESET_PARSER:
		if (edd->parser != NULL) {
			reset_context(&edd->ctx);
			xmlCtxtResetPush(edd->parser, NULL, 0, NULL, NULL);
		}
		ret = RET_OK;
		break;

	/*
	 * Misc.
	 */

	case COMMAND_PORT_REVISION:
		/* Store the revision in the buffer. */
		to_return = exmpp_new_xbuf();
		if (to_return == NULL)
			return (-1);

		ret = RET_OK;
		ei_x_encode_string(to_return, "$Revision$");

		break;

	default:
		/* Other commands are handled in 'exmpp_xml.c' */
		to_return = exmpp_new_xbuf();
		if (to_return == NULL)
			return (-1);

		ret = control(&edd->ctx, command, buf, to_return);
		if (ret < 0)
			return (-1);
	}

	if (bin == NULL) {
		if (to_return != NULL) {
			size = 1 + to_return->index;
			bin = driver_alloc_binary(size);
			if (bin == NULL)
				return (-1);
			bin->orig_bytes[0] = (char)ret;
			if (to_return->index > 0)
				memcpy(bin->orig_bytes + 1,
				    to_return->buff, to_return->index);
			exmpp_free_xbuf(to_return);
		} else {
			/* The command called doesn't return anything. */
			size = 1;
			bin = driver_alloc_binary(size);
			bin->orig_bytes[0] = RET_OK;
		}
	}

	/* Set the returned buffer. */
	*rbuf = (char *)bin;

	/* Return the size of this buffer. */
	return (size);
}

/* -------------------------------------------------------------------
 * LibXML2 handlers.
 * ------------------------------------------------------------------- */

static void
libxml2_cb_start_element(void *user_data,
    const xmlChar *elem, const xmlChar *prefix, const xmlChar *ns,
    int declared_nss_count, const xmlChar **declared_nss,
    int attrs_count, int attrs_defaulted, const xmlChar **attrs)
{
	struct exmpp_xml_data *edd;

	edd = (struct exmpp_xml_data *)user_data;

	edd->declared_nss_count = declared_nss_count;
	edd->declared_nss = declared_nss;
	edd->attrs_count = attrs_count;
	edd->attrs = attrs;

	enter_element(&edd->ctx,
	    (char *)ns, -1,
	    (char *)elem, -1,
	    edd, edd);

	/* We can now reset the declared_nss and attrs lists. */
	edd->declared_nss_count = 0;
	edd->declared_nss = NULL;
	edd->attrs_count = 0;
	edd->attrs = NULL;
}

static void
libxml2_cb_end_element(void *user_data,
    const xmlChar *elem, const xmlChar *prefix, const xmlChar *ns)
{
	struct exmpp_xml_data *edd;

	edd = (struct exmpp_xml_data *)user_data;

	exit_element(&edd->ctx,
	    (char *)ns, -1,
	    (char *)elem, -1);
}

static xmlEntityPtr
libxml2_cb_get_entity(void *user_data, const xmlChar *name)
{

	return (xmlGetPredefinedEntity(name));
}

static void
libxml2_cb_character_data(void *user_data,
    const xmlChar *data, int len)
{
	struct exmpp_xml_data *edd;

	edd = (struct exmpp_xml_data *)user_data;

	add_character_data(&edd->ctx, (char *)data, len);
}

static int
exmpp_xml_cb_make_declared_nss(struct exmpp_xml_ctx *ctx, void *user_data)
{
	int n, i;
	const char *ns, *prefix;
	struct exmpp_xml_data *edd;

	if (user_data == NULL)
		return (0);

	edd = (struct exmpp_xml_data *)user_data;

	for (n = 0, i = 0; n < edd->declared_nss_count; ++n, i += 2) {
		ns = (char *)edd->declared_nss[i + 1];
		prefix = (char *)edd->declared_nss[i];

		make_declared_ns(ctx,
		    ns, -1,
		    prefix, -1);
	}

	return (0);
}

static int
exmpp_xml_cb_make_attributes(struct exmpp_xml_ctx *ctx, void *user_data)
{
	int n, i, value_len;
	const char *ns, *attr, *value;
	struct exmpp_xml_data *edd;

	if (user_data == NULL)
		return (0);

	edd = (struct exmpp_xml_data *)user_data;

	for (n = 0, i = 0; n < edd->attrs_count; ++n, i += 5) {
		ns = (char *)edd->attrs[i + 2];
		attr = (char *)edd->attrs[i];
		value = (char *)edd->attrs[i + 3];
		value_len = edd->attrs[i + 4] - edd->attrs[i + 3];

		make_attribute(ctx,
		    ns, -1,
		    attr, -1,
		    value, value_len);
	}

	return (0);
}

/* -------------------------------------------------------------------
 * Internal functions.
 * ------------------------------------------------------------------- */

static int
create_parser(struct exmpp_xml_data *edd)
{
	/* Create a parser. */
	edd->parser = xmlCreatePushParserCtxt(&sax_handler, edd,
	    NULL, 0, NULL);
	if (edd->parser == NULL)
		return (-1);

	return (0);
}

static void
destroy_parser(struct exmpp_xml_data *edd)
{

	if (edd->parser != NULL) {
		/* Destroy the parser. */
		xmlFreeParserCtxt(edd->parser);
		edd->parser = NULL;

		/* Reset generic context. */
		reset_context(&edd->ctx);
	}
}

/* -------------------------------------------------------------------
 * Driver declaration.
 * ------------------------------------------------------------------- */

static ErlDrvEntry driver_entry;

DRIVER_INIT(DRIVER_NAME)
{
	driver_entry.driver_name = S(DRIVER_NAME);
	driver_entry.init = exmpp_xml_init;
	driver_entry.finish = exmpp_xml_finish;
	driver_entry.start = exmpp_xml_start;
	driver_entry.stop = exmpp_xml_stop;
	driver_entry.control = exmpp_xml_control;

	driver_entry.extended_marker = ERL_DRV_EXTENDED_MARKER;
	driver_entry.major_version = ERL_DRV_EXTENDED_MAJOR_VERSION;
	driver_entry.minor_version = ERL_DRV_EXTENDED_MINOR_VERSION;
#if defined(SMP_SUPPORT)
	driver_entry.driver_flags = ERL_DRV_FLAG_USE_PORT_LOCKING;
#endif

	return (&driver_entry);
}
