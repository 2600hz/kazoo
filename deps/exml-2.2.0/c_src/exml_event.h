#ifndef EXML_EVENT_H
#define EXML_EVENT_H

#include <erl_nif.h>
#include <expat.h>
#include <assert.h>
#include <stdio.h>
#include <string.h>

static XML_Memory_Handling_Suite ms =
    {enif_alloc, enif_realloc, enif_free};

// structure used as a private data by expat parser
typedef struct
{
    ErlNifEnv *env;
    ERL_NIF_TERM result;
    ERL_NIF_TERM xmlns;
} expat_parser;

// functions 'exported' by exml_event.c module
static ERL_NIF_TERM new_parser(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM reset_parser(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM free_parser(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM parse(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

// pre-allocated Erlang atoms used commonly within the driver
static ERL_NIF_TERM XML_ELEMENT_START;
static ERL_NIF_TERM XML_ELEMENT_END;
static ERL_NIF_TERM XML_CDATA;
static ERL_NIF_TERM XML_ELEMENT_START;
static ERL_NIF_TERM OK;
static ERL_NIF_TERM NONE;
static ERL_NIF_TERM ERROR;

#endif
