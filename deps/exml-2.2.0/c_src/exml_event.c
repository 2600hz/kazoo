#include "exml_event.h"

static ErlNifResourceType *PARSER_POINTER = NULL;

static ERL_NIF_TERM encode_name(expat_parser *parser_data, const XML_Char *name)
{
    //ErlNifBinary encoded;
    ERL_NIF_TERM encoded;
    int name_len, prefix_len;
    char *name_start;
    char *prefix_start;

    if((name_start = strchr(name, '\n')))
        {
            if((prefix_start = strchr(name_start+1, '\n')))
                {
                    name_len = prefix_start - name_start;
                    prefix_len = strlen(prefix_start+1);
                    unsigned char *data =  enif_make_new_binary(parser_data->env, name_len+prefix_len, &encoded);
                    strncpy((char*)data, prefix_start+1, prefix_len);
                    strncpy((char*)data+prefix_len, name_start, name_len);
                    data[prefix_len] = ':';
                } else
                {
                    name_len = strlen(name_start+1);
                    unsigned char *data = enif_make_new_binary(parser_data->env, name_len, &encoded);
                    strncpy((char*)data, name_start+1, name_len);
                }
        } else
        {
            size_t name_len = strlen(name);
            unsigned char* data = enif_make_new_binary(parser_data->env, name_len, &encoded);
            strncpy((char*)data, name, name_len);
        };

    return encoded;
};

static void *start_element_handler(expat_parser *parser_data, const XML_Char *name, const XML_Char **atts)
{
    ERL_NIF_TERM element_name;
    ERL_NIF_TERM attrs_list = enif_make_list(parser_data->env, 0);
    int i;

    element_name = encode_name(parser_data, name);
    for(i = 0; atts[i]; i += 2);
    while(i)
        {
            ERL_NIF_TERM attr_name, attr_value;

            size_t atts_len = strlen(atts[i-1]);
            unsigned char *attr_data = enif_make_new_binary(parser_data->env, atts_len, &attr_value);
            strncpy((char*)attr_data, (const char *)atts[i-1], atts_len);
            attr_name = encode_name(parser_data, atts[i-2]);

            ERL_NIF_TERM attr = enif_make_tuple(parser_data->env, 2, attr_name, attr_value);
            attrs_list = enif_make_list_cell(parser_data->env, attr, attrs_list);

            i -= 2;
        };


    ERL_NIF_TERM event = enif_make_tuple(parser_data->env, 4, XML_ELEMENT_START,
                                         element_name,
                                         parser_data->xmlns,
                                         attrs_list);
    parser_data->result = enif_make_list_cell(parser_data->env, event, parser_data->result);
    parser_data->xmlns = enif_make_list(parser_data->env, 0);

    return NULL;
};

static void *end_element_handler(expat_parser *parser_data, const XML_Char *name)
{
    ERL_NIF_TERM element_name = encode_name(parser_data, name);

    ERL_NIF_TERM event = enif_make_tuple(parser_data->env, 2, XML_ELEMENT_END, element_name);
    parser_data->result = enif_make_list_cell(parser_data->env, event, parser_data->result);

    return NULL;
};

static void *character_data_handler(expat_parser *parser_data, const XML_Char *s, int len)
{
    ERL_NIF_TERM cdata;

    unsigned char *cdata_data = enif_make_new_binary(parser_data->env, len, &cdata);
    strncpy((char*)cdata_data, (const char *)s, len);

    ERL_NIF_TERM event = enif_make_tuple(parser_data->env, 2, XML_CDATA, cdata);
    parser_data->result = enif_make_list_cell(parser_data->env, event, parser_data->result);

    return NULL;
};

static void *namespace_decl_handler(expat_parser *parser_data, const XML_Char *prefix, const XML_Char *uri)
{
    ERL_NIF_TERM ns_prefix_bin, ns_uri_bin, ns_pair;

    if(uri == NULL)
        {
            /* parser_data->xmlns = (ERL_NIF_TERM)NULL; */
            fprintf(stderr, "URI IS NULL?\n");
            return NULL;
        }

    if(prefix)
        {
            size_t prefix_len = strlen(prefix);
            unsigned char *ns_prefix_data = enif_make_new_binary(parser_data->env, prefix_len, &ns_prefix_bin);
            strncpy((char*)ns_prefix_data, (const char *)prefix, prefix_len);
        } else
        {
            ns_prefix_bin = NONE;
        }

    size_t uri_len = strlen(uri);
    unsigned char *ns_uri_data = enif_make_new_binary(parser_data->env, uri_len, &ns_uri_bin);
    strncpy((char*)ns_uri_data, uri, uri_len);

    ns_pair = enif_make_tuple(parser_data->env, 2, ns_uri_bin, ns_prefix_bin);
    parser_data->xmlns = enif_make_list_cell(parser_data->env, ns_pair, parser_data->xmlns);

    return NULL;
};

static void init_parser(XML_Parser parser, expat_parser *parser_data)
{
    XML_SetUserData(parser, parser_data);

    XML_SetStartElementHandler(parser, (XML_StartElementHandler)start_element_handler);
    XML_SetEndElementHandler(parser, (XML_EndElementHandler)end_element_handler);
    XML_SetCharacterDataHandler(parser, (XML_CharacterDataHandler)character_data_handler);
    XML_SetStartNamespaceDeclHandler(parser, (XML_StartNamespaceDeclHandler)namespace_decl_handler);

    XML_SetReturnNSTriplet(parser, 1);
    XML_SetDefaultHandler(parser, NULL);
};

static ERL_NIF_TERM new_parser(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    XML_Parser parser;
    expat_parser *parser_data = (expat_parser *)enif_alloc(sizeof(expat_parser));
    ERL_NIF_TERM parser_resource;

    parser = XML_ParserCreate_MM("UTF-8", &ms, "\n");
    parser_data->env = env;
    parser_data->xmlns = enif_make_list(env, 0);

    init_parser(parser, parser_data);

    XML_Parser *xml_parser = (XML_Parser *)enif_alloc_resource(PARSER_POINTER, sizeof(XML_Parser));
    *xml_parser = parser;
    parser_resource = enif_make_resource(env, (void *)xml_parser);
    enif_release_resource(xml_parser);
    return enif_make_tuple(env, 2, OK, parser_resource);
};

static ERL_NIF_TERM reset_parser(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    XML_Parser **parser;

    assert(argc == 1);

    if (!enif_get_resource(env, argv[0], PARSER_POINTER, (void **)&parser))
        return enif_make_badarg(env);

    expat_parser *parser_data = XML_GetUserData((XML_Parser)(*parser));
    parser_data->result = enif_make_list(env, 0);
    parser_data->xmlns = enif_make_list(env, 0);
    parser_data->env = env;

    assert(XML_TRUE == XML_ParserReset((XML_Parser)(*parser), "UTF-8"));
    init_parser((XML_Parser)(*parser), parser_data);

    return OK;
};

static void parser_dtor( ErlNifEnv *env, void* obj )
{
    XML_Parser* pParser = (XML_Parser*) obj;
    if (!pParser)
        return;

    expat_parser* parser_data;
    parser_data = XML_GetUserData( *pParser );

    enif_free( parser_data );
    parser_data = NULL;

    XML_ParserFree( *pParser );
    pParser = NULL;
}

static ERL_NIF_TERM free_parser(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    return reset_parser( env, argc, argv );
};

static ERL_NIF_TERM parse(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    XML_Parser **parser;
    int is_final, res, errcode;
    ErlNifBinary stream;
    char *errstring;

    assert(argc == 3);

    if (!enif_get_resource(env, argv[0], PARSER_POINTER, (void **)&parser))
        return enif_make_badarg(env);

    if (!enif_is_binary(env, argv[1]))
        return enif_make_badarg(env);

    enif_get_int(env, argv[2], &is_final);
    enif_inspect_binary(env, argv[1], &stream);

    expat_parser *parser_data = XML_GetUserData((XML_Parser)(*parser));
    parser_data->result = enif_make_list(env, 0);
    parser_data->env = env;
    XML_SetUserData((XML_Parser)(*parser), parser_data);

    res = XML_Parse((XML_Parser)(*parser), (const char *)stream.data, stream.size, is_final);
    if(!res)
        {
            errcode = XML_GetErrorCode((XML_Parser)(*parser));
            errstring = (char *)XML_ErrorString(errcode);

            return enif_make_tuple(env, 2, ERROR,
                                   enif_make_string(env, errstring, ERL_NIF_LATIN1));
        }

    return enif_make_tuple(env, 2, OK, parser_data->result);
};

static int load(ErlNifEnv* env, void **priv, ERL_NIF_TERM info)
{
    PARSER_POINTER = enif_open_resource_type(env, NULL, "parser_pointer", &parser_dtor,
                                             ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER,
                                             NULL);
    XML_ELEMENT_START = enif_make_atom(env, "xml_element_start");
    XML_ELEMENT_END = enif_make_atom(env, "xml_element_end");
    XML_CDATA = enif_make_atom(env, "xml_cdata");
    OK = enif_make_atom(env, "ok");
    NONE = enif_make_atom(env, "none");
    ERROR = enif_make_atom(env, "error");

    return 0;
};

static int reload(ErlNifEnv* env, void** priv, ERL_NIF_TERM info)
{
    return 0;
}

static int upgrade(ErlNifEnv* env, void** priv, void** old_priv, ERL_NIF_TERM info)
{
    *priv = *old_priv;
    return 0;
}

static void unload(ErlNifEnv* env, void* priv)
{
}

static ErlNifFunc funcs[] =
    {
        {"new_parser", 0, new_parser},
        {"reset_parser", 1, reset_parser},
        {"free_parser", 1, free_parser},
        {"parse_nif", 3, parse}
    };

ERL_NIF_INIT(exml_event, funcs, &load, &reload, &upgrade, &unload);
