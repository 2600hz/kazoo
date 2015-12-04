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

#include "exmpp_xml.h"

/* Global known lists. */
static struct exmpp_hashtable	*known_nss_index = NULL;
static struct exmpp_hashtable	*known_elems_index = NULL;

#define	KNOWN_NSS_AVG_MIN_LENGTH	 200
#define	KNOWN_ELEMS_AVG_MIN_LENGTH	1100
#define	KNOWN_ATTRS_AVG_MIN_LENGTH	 500

#define	XML_NAMESPACE			"http://www.w3.org/XML/1998/namespace"
#define	XML_NAMESPACE_LEN		36

static int		add_known_nss(const char *buf, int index);
static int		add_known_elems(const char *buf, int index);
static int		select_known_nss(struct exmpp_xml_ctx *ctx,
			    const char *buf, int index);
static int		select_known_elems(struct exmpp_xml_ctx *ctx,
			    const char *buf, int index);
static int		is_known_ns(struct exmpp_xml_ctx *ctx,
			    const char *ns, int ns_len);
static int		is_known_elem(struct exmpp_xml_ctx *ctx,
			    const char *elem, int elem_len);

static int		encode_ns(struct exmpp_xml_ctx *ctx,
			    ei_x_buff *tree, const char *ns, int ns_len);
static int		encode_elem(struct exmpp_xml_ctx *ctx,
			    ei_x_buff *tree, const char *elem, int elem_len);
static int		encode_attr(struct exmpp_xml_ctx *ctx,
			    ei_x_buff *tree, const char *attr, int attr_len);

static int		current_tree_finished(struct exmpp_xml_ctx *ctx);
static void		reset_current_tree(struct exmpp_xml_ctx *ctx);

static int DUMMY = 1;

/* -------------------------------------------------------------------
 * Known lists.
 * ------------------------------------------------------------------- */

int
init_known_lists()
{

	/* Create the 3 global known lists index and the
	 * associated locks. */
	known_nss_index = exmpp_ht_create(4,
	    (void (*)(void *))exmpp_ht_destroy);
	if (known_nss_index == NULL)
		goto err;

	known_elems_index = exmpp_ht_create(4,
	    (void (*)(void *))exmpp_ht_destroy);
	if (known_elems_index == NULL)
		goto err;


	return (0);

err:
	if (known_nss_index != NULL) {
		exmpp_ht_destroy(known_nss_index);
		known_nss_index = NULL;
	}

	if (known_elems_index != NULL) {
		exmpp_ht_destroy(known_elems_index);
		known_elems_index = NULL;
	}

	return (-1);
}

void
free_known_lists()
{

	if (known_nss_index != NULL) {
		exmpp_ht_destroy(known_nss_index);
		known_nss_index = NULL;
	}

	if (known_elems_index != NULL) {
		exmpp_ht_destroy(known_elems_index);
		known_elems_index = NULL;
	}
}

/* -------------------------------------------------------------------
 * Common commands.
 * ------------------------------------------------------------------- */

int
control(struct exmpp_xml_ctx *ctx, unsigned int command,
    const char *buf, ei_x_buff *to_return)
{
	int index;

	if (ctx == NULL || buf == NULL || to_return == NULL)
		return (-1);

	switch (command) {
	/*
	 * Global known lists update.
	 */
	case COMMAND_ADD_KNOWN_NSS:
		index = exmpp_skip_version(buf);

		if (add_known_nss(buf, index) != 0) {
			ei_x_encode_atom(to_return, "add_known_nss_failed");

			return (RET_ERROR);
		}

		break;
	case COMMAND_ADD_KNOWN_ELEMS:
		index = exmpp_skip_version(buf);

		if (add_known_elems(buf, index) != 0) {
			ei_x_encode_atom(to_return, "add_known_elems_failed");

			return (RET_ERROR);
		}

		break;

	/*
	 * Options handling.
	 */

	case COMMAND_SET_NAMES_AS_ATOM:
		index = exmpp_skip_version(buf);

		ei_decode_boolean(buf, &index, &(ctx->names_as_atom));

		break;
	case COMMAND_SET_CHECK_NSS:
		index = exmpp_skip_version(buf);

		if (select_known_nss(ctx, buf, index) != 0) {
			ei_x_encode_atom(to_return, "check_nss_failed");

			return (RET_ERROR);
		}

		break;
	case COMMAND_SET_CHECK_ELEMS:
		index = exmpp_skip_version(buf);

		if (select_known_elems(ctx, buf, index) != 0) {
			ei_x_encode_atom(to_return, "check_elems_failed");

			return (RET_ERROR);
		}

		break;
	case COMMAND_SET_ROOT_DEPTH:
		index = exmpp_skip_version(buf);

		ei_decode_long(buf, &index, &(ctx->root_depth));

		break;
	case COMMAND_SET_EMIT_ENDTAG:
		index = exmpp_skip_version(buf);

		ei_decode_boolean(buf, &index, &(ctx->emit_endtag));

		break;
	case COMMAND_SET_MAX_SIZE:
		index = exmpp_skip_version(buf);

		ei_decode_long(buf, &index, &(ctx->max_size));

		break;

	default:
		/* Command not recognized. */
		ei_x_encode_tuple_header(to_return, 2);
                ei_x_encode_atom(to_return, "unknown_command");
                ei_x_encode_ulong(to_return, command);

		return (RET_ERROR);
	}

	return (RET_OK);
}

/* -------------------------------------------------------------------
 * Context initialization.
 * ------------------------------------------------------------------- */

int
init_context(struct exmpp_xml_ctx *ctx)
{

	/* Set default options. */
	ctx->max_size = -1;
	ctx->root_depth = 0;
	ctx->emit_endtag = 0;
	ctx->names_as_atom = 1;
	ctx->check_nss = ctx->check_elems = 0;
	ctx->known_nss = ctx->known_elems = NULL;

	/* Clear callbacks. */
	ctx->make_declared_nss = NULL;
	ctx->make_attributes = NULL;
	ctx->make_attributes_legacy = NULL;

	/* Reset parsing state. */
	ctx->cur_size = 0;
	ctx->depth = 0;

	/* Allocate the current tree */
	ctx->current_tree = driver_alloc(sizeof(*(ctx->current_tree)));
	if (ctx->current_tree == NULL)
		return (-1);
	ei_x_new(ctx->current_tree);

	/* Allocate the complete trees list. */
	ctx->complete_trees = driver_alloc(sizeof(*(ctx->complete_trees)));
	if (ctx->complete_trees == NULL) {
		ei_x_free(ctx->current_tree);
		driver_free(ctx->current_tree);
		return (-1);
	}
	ei_x_new_with_version(ctx->complete_trees);
	ctx->complete_trees_ready = 0;

	return (0);
}

void
reset_context(struct exmpp_xml_ctx *ctx)
{

	ctx->cur_size = 0;
	ctx->depth = 0;

	reset_current_tree(ctx);
	reset_complete_trees(ctx);
}

void
free_context(struct exmpp_xml_ctx *ctx)
{

	if (ctx->complete_trees != NULL) {
		ei_x_free(ctx->complete_trees);
		driver_free(ctx->complete_trees);
	}

	if (ctx->current_tree != NULL) {
		ei_x_free(ctx->current_tree);
		driver_free(ctx->current_tree);
	}
}

/* -------------------------------------------------------------------
 * Data size protection.
 * ------------------------------------------------------------------- */

void
add_data_size(struct exmpp_xml_ctx *ctx, long size)
{

	ctx->cur_size += size;
}

int
is_data_size_under_limit(struct exmpp_xml_ctx *ctx, long size)
{

	if (ctx->max_size > -1 &&
	    ctx->cur_size + size > ctx->max_size)
		return (0);

	return (1);
}

/* -------------------------------------------------------------------
 * Erlang terms construction.
 * ------------------------------------------------------------------- */

int
enter_element(struct exmpp_xml_ctx *ctx,
    const char *ns, int ns_len,
    const char *elem, int elem_len,
    void *declared_nss, void *attributes)
{
	int ret;
	ei_x_buff *tree;

	/*
	 * This function build the #xmlel record:
	 *   {xmlel, NS, Declared_NS, Name, Attributes, Children}
	 *
	 * Construction of "Delcared_NS" and "Attributes" are made by
	 * helper functions but they're walked through by callbacks inside
	 * driver's code.
	 *
	 * Note that "Children" may not be built here but by following
	 * calls to this same function.
	 */

	tree = ctx->current_tree;

	/*
	 * If we're not at the required depth, we treat each node
	 * independently.
	 *
	 * For instance in XMPP, if ctx->root_depth is 1, a stand-alone
	 * term will be made right away with a <stream> opening tag. But
	 * for a <message> element, we will build a complete tree including
	 * children.
	 */
	if (ctx->root_depth == -1 ||
	    ctx->depth <= ctx->root_depth) {
		/* We're above root depth (or the feature is disabled),
		 * so we start a new Erlang term each time. */
		
		/* The current_tree buffer was reset by
		 * current_tree_finished(). */
	} else {
		/* We're under root depth, thus we continue with the
		 * current tree. */
		ei_x_encode_list_header(tree, 1);
	}

	/* Start an #xmlel record. */
	ei_x_encode_tuple_header(tree, 6);
	ei_x_encode_atom(tree, "xmlel");

	/* Check if the namespace is known and encode it. */
	encode_ns(ctx, tree, ns, ns_len);

	if (ctx->make_declared_nss != NULL) {
		/* Call a caller-provided function to walk the declared
		 * namespaces list. */
		ret = ctx->make_declared_nss(ctx, declared_nss);
		if (ret != 0)
			return (ret);
	} else if (declared_nss != NULL) {
		/* The caller doesn't provided a callback but an already
		 * formatted list. We just have to append it to the tree. */
		ei_x_append(tree, (ei_x_buff *)declared_nss);
	}
	/* Terminate the declared namespaces Erlang list. */
	ei_x_encode_empty_list(tree);

	/* Check if the element is known and encode it. */
	encode_elem(ctx, tree, elem, elem_len);

	/* Call a caller-provided function to walk the attributes list. */
	if (ctx->make_attributes != NULL) {
		ret = ctx->make_attributes(ctx, attributes);
		if (ret != 0)
			return (ret);
	}
	/* Terminate the attributes Erlang list. */
	ei_x_encode_empty_list(tree);

	/* Handle the tree if it's ready to ship. */
	if (ctx->root_depth == -1 ||
	    ctx->depth < ctx->root_depth) {
		/* Stand-alone elements are moved to the final list. */
		ei_x_encode_atom(tree, "undefined");
		current_tree_finished(ctx);
	}

	/* Update depth. */
	ctx->depth++;

	return (0);
}

int
make_declared_ns_in_buf(struct exmpp_xml_ctx *ctx, ei_x_buff *buf,
    const char *ns, int ns_len,
    const char *prefix, int prefix_len)
{

	/* Start declared namespace entry. */
	ei_x_encode_list_header(buf, 1);
	ei_x_encode_tuple_header(buf, 2);

	/* Check if the namespace is known and encode it. */
	encode_ns(ctx, buf, ns, ns_len);

	/* Encode prefix. */
	if (prefix != NULL)
		if (prefix_len == -1)
			ei_x_encode_string(buf, prefix);
		else
			ei_x_encode_string_len(buf, prefix, prefix_len);
	else
		ei_x_encode_atom(buf, "none");

	return (0);
}

int
make_declared_ns(struct exmpp_xml_ctx *ctx,
    const char *ns, int ns_len,
    const char *prefix, int prefix_len)
{
	int ret;

	ret = make_declared_ns_in_buf(ctx, ctx->current_tree,
	    ns, ns_len, prefix, prefix_len);

	return (ret);
}

int
make_attribute(struct exmpp_xml_ctx *ctx,
    const char *ns, int ns_len,
    const char *attr, int attr_len,
    const char *value, int value_len)
{
	ei_x_buff *tree;

	tree = ctx->current_tree;

	/* Start #xmlattr record. */
	ei_x_encode_list_header(tree, 1);
	ei_x_encode_tuple_header(tree, 4);
	ei_x_encode_atom(tree, "xmlattr");

	/* Check if the namespace is known and encode it. */
	encode_ns(ctx, tree, ns, ns_len);

	/* Check if the attribute is known and encode it. */
	encode_attr(ctx, tree, attr, attr_len);

	/* Encode the value. */
	if (value_len == -1)
		ei_x_encode_binary(tree, value, strlen(value));
	else
		ei_x_encode_binary(tree, value, value_len);

	return (0);
}

int
add_character_data(struct exmpp_xml_ctx *ctx,
    const char *text, int text_len)
{
	ei_x_buff *tree;

	tree = ctx->current_tree;
	if (tree == NULL)
		return (-1);

	if (ctx->root_depth == -1 || ctx->depth <= ctx->root_depth) {
		/* We're above root depth (or the feature is disabled),
		 * so we start a new Erlang term each time. */
		
		/* The current_tree buffer was reset by
		 * current_tree_finished(). */
	} else {
		/* We continue the children list. */
		ei_x_encode_list_header(tree, 1);
	}

	/* Start an #xmlcdata record. */
	ei_x_encode_tuple_header(tree, 2);
	ei_x_encode_atom(tree, "xmlcdata");

	/* Encode the data. */
	ei_x_encode_binary(tree, text, text_len);

	if (ctx->root_depth == -1 || ctx->depth <= ctx->root_depth)
		current_tree_finished(ctx);

	return (0);
}

int
exit_element(struct exmpp_xml_ctx *ctx,
    const char *ns, int ns_len,
    const char *elem, int elem_len)
{
	ei_x_buff *tree;

	/* Update depth. */
	if (ctx->depth > 0)
		ctx->depth--;

	tree = ctx->current_tree;

	if (ctx->emit_endtag && (ctx->root_depth == -1 ||
	    ctx->depth < ctx->root_depth)) {
		/* We're above root depth (or the feature is disabled),
		 * so we start a new Erlang term each time. */

		/* The current_tree buffer was reset by
		 * current_tree_finished(). */

		/* Start an #xmlendtag record. */
		ei_x_encode_tuple_header(tree, 3);
		ei_x_encode_atom(tree, "xmlendtag");

		/* Check if the namespace is known and encode it. */
		encode_ns(ctx, tree, ns, ns_len);

		/* Check if the element is known and encode it. */
		encode_elem(ctx, tree, elem, elem_len);

		current_tree_finished(ctx);
	} else if (ctx->root_depth != -1 && ctx->depth >= ctx->root_depth) {
		/* Terminate the children list. */
		ei_x_encode_empty_list(tree);

		if (ctx->depth == ctx->root_depth)
			current_tree_finished(ctx);
	}

	return (0);
}

void
reset_complete_trees(struct exmpp_xml_ctx *ctx)
{

	ctx->complete_trees_ready = 0;
	ctx->complete_trees->index = 0;
	ei_x_encode_version(ctx->complete_trees);
}

/* -------------------------------------------------------------------
 * Legacy Erlang terms construction.
 * ------------------------------------------------------------------- */

int
enter_element_legacy(struct exmpp_xml_ctx *ctx,
    const char *elem, int elem_len,
    void *attributes)
{
	int ret;
	ei_x_buff *tree;

	/*
	 * This function build the legacy #xmlelement record:
	 *   {xmlelement, Name, Attributes, Children}
	 *
	 * Construction of "Attributes" is made by a helper function
	 * but it's walked through by a callback inside driver's code.
	 *
	 * Note that "Children" may not be built here but by following
	 * calls to this same function.
	 */

	tree = ctx->current_tree;

	/*
	 * If we're not at the required depth, we treat each node
	 * independently.
	 *
	 * For instance in XMPP, if ctx->root_depth is 1, a stand-alone
	 * term will be made right away with a <stream> opening tag. But
	 * for a <message> element, we will build a complete tree including
	 * children.
	 */
	if (ctx->root_depth == -1 ||
	    ctx->depth <= ctx->root_depth) {
		/* We're above root depth (or the feature is disabled),
		 * so we start a new Erlang term each time. */
		
		/* The current_tree buffer was reset by
		 * current_tree_finished(). */
	} else {
		/* We're under root depth, thus we continue with the
		 * current tree. */
		ei_x_encode_list_header(tree, 1);
	}

	/* Start an #xmlel record. */
	ei_x_encode_tuple_header(tree, 4);
	ei_x_encode_atom(tree, "xmlelement");

	/* Check if the element is known and encode it. */
	encode_elem(ctx, tree, elem, elem_len);

	/* Call a caller-provided function to walk the attributes list. */
	if (ctx->make_attributes_legacy != NULL) {
		ret = ctx->make_attributes_legacy(ctx, attributes);
		if (ret != 0)
			return (ret);
	}
	/* Terminate the attributes Erlang list. */
	ei_x_encode_empty_list(tree);

	/* Handle the tree if it's ready to ship. */
	if (ctx->root_depth == -1 ||
	    ctx->depth < ctx->root_depth) {
		/* Stand-alone elements are moved to the final list. */
		ei_x_encode_atom(tree, "undefined");
		current_tree_finished(ctx);
	}

	/* Update depth. */
	ctx->depth++;

	return (0);
}

int
make_attribute_legacy(struct exmpp_xml_ctx *ctx,
    const char *attr, int attr_len,
    const char *value, int value_len)
{
	ei_x_buff *tree;

	tree = ctx->current_tree;

	/* Start a simple tuple. */
	ei_x_encode_list_header(tree, 1);
	ei_x_encode_tuple_header(tree, 2);

	/* Check if the attribute is known and encode it. */
	encode_attr(ctx, tree, attr, attr_len);

	/* Encode the value. */
	if (value_len == -1)
		ei_x_encode_string(tree, value);
	else
		ei_x_encode_string_len(tree, value, value_len);

	return (0);
}

int
exit_element_legacy(struct exmpp_xml_ctx *ctx)
{
	ei_x_buff *tree;

	/* Update depth. */
	if (ctx->depth > 0)
		ctx->depth--;

	tree = ctx->current_tree;

	if (ctx->root_depth != -1 && ctx->depth >= ctx->root_depth) {
		/* Terminate the children list. */
		ei_x_encode_empty_list(tree);

		if (ctx->depth == ctx->root_depth)
			current_tree_finished(ctx);
	}

	return (0);
}

/* -------------------------------------------------------------------
 * Internal functions.
 * ------------------------------------------------------------------- */

static int
get_known_list_name(const char *buf, int *index,
    char *list_name, int *list_name_len)
{
	int arity, type;

	/* The term has the form {List_Name, List}. */
	if (ei_decode_tuple_header(buf, index, &arity) != 0)
		return (-1);
	if (arity != 2)
		return (-1);

	/* Decode the list name. */
	if (ei_get_type(buf, index, &type, list_name_len) != 0)
		return (-1);
	if (ei_decode_atom(buf, index, list_name) != 0)
		return (-1);

	return (0);
}

static struct exmpp_hashtable *
lookup_known_nss(const char *list_name, int list_name_len)
{
	struct exmpp_hashtable *kl;

	if (known_nss_index == NULL)
		return (NULL);

	/* Lookup the known list. */
	kl = exmpp_ht_fetch(known_nss_index, list_name, list_name_len);
	if (kl == NULL) {
		/* This list doesn't exist yet, create it. */
		kl = exmpp_ht_create(KNOWN_NSS_AVG_MIN_LENGTH, NULL);
		if (kl == NULL)
			return (NULL);

		/* Always add the XML namespace to the known ones. */
		if (exmpp_ht_store(kl,
		    XML_NAMESPACE, XML_NAMESPACE_LEN, &DUMMY) != 0) {
			exmpp_ht_destroy(kl);
			return (NULL);
		}

		/* Add it to the lists index. */
		exmpp_ht_store(known_nss_index,
		    list_name, list_name_len, kl);
	}

	return (kl);
}

static struct exmpp_hashtable *
lookup_known_elems(const char *list_name, int list_name_len)
{
	struct exmpp_hashtable *kl;

	if (known_elems_index == NULL)
		return (NULL);

	/* Lookup the known list. */
	kl = exmpp_ht_fetch(known_elems_index, list_name, list_name_len);
	if (kl == NULL) {
		/* This list doesn't exist yet, create it. */
		kl = exmpp_ht_create(KNOWN_ELEMS_AVG_MIN_LENGTH, NULL);
		if (kl == NULL)
			return (NULL);

		/* Add it to the lists index. */
		exmpp_ht_store(known_elems_index,
		    list_name, list_name_len, kl);
	}

	return (kl);
}


static int
update_list(struct exmpp_hashtable *kl, const char *buf, int *index)
{
	int nb_items, i, type, item_len;
	char item[MAXATOMLEN];

	/* We check that we have a real list. */
	if (ei_decode_list_header(buf, index, &nb_items) != 0)
		return (-1);

	if (nb_items < 0)
		return (0);

	for (i = 0; i < nb_items; ++i) {
		/* Decode the item. */
		if (ei_get_type(buf, index, &type, &item_len) != 0)
			return (-1);
		if (ei_decode_atom(buf, index, item) != 0)
			return (-1);

		/* Add it to the list. */
		if (!exmpp_ht_exists(kl, item, item_len))
			exmpp_ht_store(kl, item, item_len, &DUMMY);
	}

	return (0);
}

static int
add_known_nss(const char *buf, int index)
{
	int list_name_len;
	char list_name[MAXATOMLEN];
	struct exmpp_hashtable *kl;

	/* Get the list name from the given Erlang term. */
	if (get_known_list_name(buf, &index, list_name, &list_name_len) != 0)
		return (RET_ERROR);

	/* We can lookup the list (it will be created if it doesn't
	 * already exist. */
	kl = lookup_known_nss(list_name, list_name_len);
	if (kl == NULL)
		return (RET_ERROR);

	/* Update the list. */
	if (update_list(kl, buf, &index) != 0)
		return (RET_ERROR);

	return (RET_OK);
}

static int
add_known_elems(const char *buf, int index)
{
	int list_name_len;
	char list_name[MAXATOMLEN];
	struct exmpp_hashtable *kl;

	/* Get the list name from the given Erlang term. */
	if (get_known_list_name(buf, &index, list_name, &list_name_len) != 0)
		return (RET_ERROR);

	/* We can lookup the list (it will be created if it doesn't
	 * already exist. */
	kl = lookup_known_elems(list_name, list_name_len);
	if (kl == NULL)
		return (RET_ERROR);

	/* Update the list. */
	if (update_list(kl, buf, &index) != 0)
		return (RET_ERROR);

	return (RET_OK);
}


static int
select_known_nss(struct exmpp_xml_ctx *ctx, const char *buf, int index)
{
	int type, list_name_len;
	char list_name[MAXATOMLEN];
	struct exmpp_hashtable *kl;

	if (ei_get_type(buf, &index, &type, &list_name_len) != 0)
		return (-1);
	if (ei_decode_atom(buf, &index, list_name) != 0)
		return (-1);

	if (strcmp(list_name, "false") == 0) {
		ctx->check_nss = 0;
		return (0);
	}
	
	if (strcmp(list_name, "true") == 0) {
		if (ctx->known_nss == NULL)
			return (-1);

		ctx->check_nss = 1;
		return (0);
	}

	kl = exmpp_ht_fetch(known_nss_index, list_name, list_name_len);
	if (kl == NULL)
		return (-1);

	ctx->known_nss = kl;
	ctx->check_nss = 1;

	return (0);
}

static int
select_known_elems(struct exmpp_xml_ctx *ctx, const char *buf, int index)
{
	int type, list_name_len;
	char list_name[MAXATOMLEN];
	struct exmpp_hashtable *kl;

	if (ei_get_type(buf, &index, &type, &list_name_len) != 0)
		return (-1);
	if (ei_decode_atom(buf, &index, list_name) != 0)
		return (-1);

	if (strcmp(list_name, "false") == 0) {
		ctx->check_elems = 0;
		return (0);
	}
	
	if (strcmp(list_name, "true") == 0) {
		if (ctx->known_elems == NULL)
			return (-1);

		ctx->check_elems = 1;
		return (0);
	}

	kl = exmpp_ht_fetch(known_elems_index, list_name, list_name_len);
	if (kl == NULL)
		return (-1);

	ctx->known_elems = kl;
	ctx->check_elems = 1;

	return (0);
}


static int
is_known_ns(struct exmpp_xml_ctx *ctx, const char *ns, int ns_len)
{
	int is_known;

	if (ctx == NULL)
		return (0);
	if (!ctx->check_nss)
		return (1);
	if (ctx->known_nss == NULL)
		return (0);

	is_known = exmpp_ht_exists(ctx->known_nss, ns, ns_len);

	return (is_known);
}

static int
is_known_elem(struct exmpp_xml_ctx *ctx, const char *elem, int elem_len)
{
	int is_known;

	if (ctx == NULL)
		return (0);
	if (!ctx->check_elems)
		return (1);
	if (ctx->known_elems == NULL)
		return (0);

	is_known = exmpp_ht_exists(ctx->known_elems, elem, elem_len);

	return (is_known);
}


static int
encode_ns(struct exmpp_xml_ctx *ctx, ei_x_buff *tree,
    const char *ns, int ns_len)
{
	int ret;

	if (ns != NULL) {
		/* Check if the namespace is known and encode it. */
		if (ns_len <= MAXATOMLEN && ctx->names_as_atom) {
			if (is_known_ns(ctx, ns, ns_len))
				if (ns_len == -1)
					ret = ei_x_encode_atom(tree, ns);
				else
					ret = ei_x_encode_atom_len(tree,
					    ns, ns_len);
			else
				if (ns_len == -1)
					ret = ei_x_encode_string(tree, ns);
				else
					ret = ei_x_encode_string_len(tree,
					    ns, ns_len);
		} else {
			if (ns_len == -1)
				ret = ei_x_encode_string(tree, ns);
			else
				ret = ei_x_encode_string_len(tree,
				    ns, ns_len);
		}
	} else {
		/* The namespace is "empty". The element may have none. */
		ret = ei_x_encode_atom(tree, "undefined");
	}

	return (ret);
}

static int
encode_elem(struct exmpp_xml_ctx *ctx, ei_x_buff *tree,
    const char *elem, int elem_len)
{
	int ret;

	/* Check if the element is known and encode it. */
	if (elem_len <= MAXATOMLEN && ctx->names_as_atom) {
		if (is_known_elem(ctx, elem, elem_len))
			if (elem_len == -1)
				ret = ei_x_encode_atom(tree, elem);
			else
				ret = ei_x_encode_atom_len(tree,
				    elem, elem_len);
		else
			if (elem_len == -1)
				ret = ei_x_encode_string(tree, elem);
			else
				ret = ei_x_encode_string_len(tree,
				    elem, elem_len);
	} else {
		if (elem_len == -1)
			ret = ei_x_encode_string(tree, elem);
		else
			ret = ei_x_encode_string_len(tree, elem, elem_len);
	}

	return (ret);
}

static int
encode_attr(struct exmpp_xml_ctx *ctx, ei_x_buff *tree,
    const char *attr, int attr_len)
{
	int ret;

	if (attr_len == -1)
		ret = ei_x_encode_binary(tree, attr, strlen(attr));
	else
		ret = ei_x_encode_binary(tree, attr, attr_len);
	
	return (ret);
}

static int
current_tree_finished(struct exmpp_xml_ctx *ctx)
{
	ei_x_buff *trees;

	trees = ctx->complete_trees;
	if (trees == NULL) {
		/* Allocate the complete trees list. */
		trees = driver_alloc(sizeof(*trees));
		if (trees == NULL)
			return (-1);
		ei_x_new_with_version(trees);
		ctx->complete_trees = trees;
	}

	/* Append the current tree to the complete trees list. */
	ei_x_encode_list_header(trees, 1);
	ei_x_append(trees, ctx->current_tree);
	ctx->complete_trees_ready = 1;

	/* Reset the current tree. */
	reset_current_tree(ctx);

	return (0);
}

static void
reset_current_tree(struct exmpp_xml_ctx *ctx)
{

	ctx->current_tree->index = 0;
}
