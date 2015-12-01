#ifndef EXML_ESCAPE_H
#define EXML_ESCAPE_H

#include <erl_nif.h>
#include <string.h>
#include <stdio.h>
#include <assert.h>

#define EXML_CDATA_BUF_SIZE 1024
#define EXML_ATTR_BUF_SIZE 64

static ERL_NIF_TERM escape_attr(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM unescape_attr(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM escape_cdata(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM unescape_cdata(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

#endif
