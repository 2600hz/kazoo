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

#ifndef EXMPP_XML_H
#define	EXMPP_XML_H 1

#include "exmpp_driver.h"

/* XML namespace is implicitly declared. This constant is used in known_ns
 * and prefixes hashtables. */
#define	XML_NS	"http://www.w3.org/XML/1998/namespace"

/* Return codes. */
enum {
	RET_OK = 0,
	RET_ERROR
};

/* Control operations. */
enum {
	COMMAND_ADD_KNOWN_NSS = 1,
	COMMAND_ADD_KNOWN_ELEMS,
	COMMAND_SET_MAX_SIZE,
	COMMAND_SET_ROOT_DEPTH,
	COMMAND_SET_NAMES_AS_ATOM,
	COMMAND_SET_CHECK_NSS,
	COMMAND_SET_CHECK_ELEMS,
	COMMAND_SET_EMIT_ENDTAG,
	COMMAND_PARSE,
	COMMAND_PARSE_FINAL,
	COMMAND_RESET_PARSER,
	COMMAND_PORT_REVISION
};

struct exmpp_xml_ctx;

/* Callback used during Erlang terms construction. */
typedef	int make_declared_nss_cb(struct exmpp_xml_ctx *ctx,
	    void *declared_nss);
typedef	int make_attributes_cb(struct exmpp_xml_ctx *ctx,
	    void *attributes);
typedef	int make_attributes_legacy_cb(struct exmpp_xml_ctx *ctx,
	    void *attributes);

/* Context given to all functions. */
struct exmpp_xml_ctx {
	/* Parsing options. */
	int			 names_as_atom;
	int			 check_nss;
	int			 check_elems;
	int			 emit_endtag;
	long			 max_size;
	long			 root_depth;

	/* Callback used during Erlang terms construction. */
	make_declared_nss_cb	*make_declared_nss;
	make_attributes_cb	*make_attributes;
	make_attributes_legacy_cb *make_attributes_legacy;

	/* Parsing state. */
	struct exmpp_hashtable	*known_nss;
	struct exmpp_hashtable	*known_elems;
	long			 cur_size;
	long			 depth;
	ei_x_buff		*current_tree;
	ei_x_buff		*complete_trees;
	int			 complete_trees_ready;
};

int		init_known_lists(void);
void		free_known_lists(void);

int		control(struct exmpp_xml_ctx *ctx, unsigned int command,
		    const char *buf, ei_x_buff *to_return);

int		init_context(struct exmpp_xml_ctx *ctx);
void		reset_context(struct exmpp_xml_ctx *ctx);
void		free_context(struct exmpp_xml_ctx *ctx);

void		add_data_size(struct exmpp_xml_ctx *ctx, long size);
int		is_data_size_under_limit(struct exmpp_xml_ctx *ctx, long size);

#if defined(_REENTRANT) && !defined(FINE_GRAIN_LOCKING)
void		lock_known_lists(struct exmpp_xml_ctx *ctx);
void		unlock_known_lists(struct exmpp_xml_ctx *ctx);
#endif

int		enter_element(struct exmpp_xml_ctx *ctx,
		    const char *ns, int ns_len,
		    const char *elem, int elem_len,
		    void *declared_nss, void *attributes);
int		make_declared_ns_in_buf(struct exmpp_xml_ctx *ctx,
		    ei_x_buff *buf,
		    const char *ns, int ns_len,
		    const char *prefix, int prefix_len);
int		make_declared_ns(struct exmpp_xml_ctx *ctx,
		    const char *ns, int ns_len,
		    const char *prefix, int prefix_len);
int		make_attribute(struct exmpp_xml_ctx *ctx,
		    const char *ns, int ns_len,
		    const char *attr, int attr_len,
		    const char *value, int value_len);
int		exit_element(struct exmpp_xml_ctx *ctx,
		    const char *ns, int ns_len,
		    const char *elem, int elem_len);
int		add_character_data(struct exmpp_xml_ctx *ctx,
		    const char *text, int text_len);
void		reset_complete_trees(struct exmpp_xml_ctx *ctx);

int		enter_element_legacy(struct exmpp_xml_ctx *ctx,
		    const char *elem, int elem_len,
		    void *attributes);
int		make_attribute_legacy(struct exmpp_xml_ctx *ctx,
		    const char *attr, int attr_len,
		    const char *value, int value_len);
int		exit_element_legacy(struct exmpp_xml_ctx *ctx);

#endif /* !defined(EXMPP_XML_H) */
