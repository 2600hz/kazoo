#include "exml_escape.h"

struct buf {
  int limit;
  int len;
  unsigned char *b;
};

static struct buf *init_buf(ErlNifEnv* env, int buf_len)
{
    struct buf *rbuf = enif_alloc(sizeof(struct buf));
    rbuf->limit = buf_len;
    rbuf->len = 0;
    rbuf->b = enif_alloc(rbuf->limit);
    return rbuf;
}

static void destroy_buf(ErlNifEnv* env, struct buf *rbuf)
{
  if (rbuf) {
    if (rbuf->b) {
      enif_free(rbuf->b);
    };
    enif_free(rbuf);
  };
}

static inline void resize_buf(ErlNifEnv* env, int buf_len, struct buf *rbuf, int len_to_add)
{
    int new_len = rbuf->len + len_to_add;
    if (new_len >= rbuf->limit) {
        rbuf->limit = ((new_len / buf_len) + 1) * buf_len;
        rbuf->b = enif_realloc(rbuf->b, rbuf->limit);
    };
}

static void buf_add_char(ErlNifEnv* env, int buf_len, struct buf *rbuf, unsigned char c)
{
    resize_buf(env, buf_len, rbuf, 1);
    (rbuf->b)[rbuf->len] = c;
    rbuf->len += 1;
}

static void buf_add_str(ErlNifEnv* env, int buf_len, struct buf *rbuf, char *data, int len)
{
    resize_buf(env, buf_len, rbuf, len);
    memcpy(rbuf->b + rbuf->len, data, len);
    rbuf->len += len;
}

static int match_tag(ErlNifBinary str, int index, char* tag, int len)
{
    if ((str.size - index) < len) return(0);
    do {
        if (str.data[index++] != *tag) {
            return(0);
        }
    } while (*++tag);
    return(1);
}

static ERL_NIF_TERM escape_cdata(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary input;
    ERL_NIF_TERM output;
    struct buf *rbuf;
    int i;

    if (argc != 1) {
        return enif_make_badarg(env);
    }

    // CData should be iolist() or binary()
    if (enif_is_binary(env, argv[0])) {
        if (!enif_inspect_binary(env, argv[0], &input)) {
            return enif_make_badarg(env);
        }
    } else {
        if (!enif_inspect_iolist_as_binary(env, argv[0], &input)) {
            return enif_make_badarg(env);
        }
    }

    rbuf = init_buf(env, EXML_CDATA_BUF_SIZE);

    for (i = 0; i < input.size; i++) {
        switch (input.data[i]) {
            case '&':
                buf_add_str(env, EXML_CDATA_BUF_SIZE, rbuf, "&amp;", 5);
                break;
            case '<':
                buf_add_str(env, EXML_CDATA_BUF_SIZE, rbuf, "&lt;", 4);
                break;
            case '>':
                buf_add_str(env, EXML_CDATA_BUF_SIZE, rbuf, "&gt;", 4);
                break;
            default:
                buf_add_char(env, EXML_CDATA_BUF_SIZE, rbuf, input.data[i]);
                break;
        };
    };

    unsigned char* data = enif_make_new_binary(env, rbuf->len, &output);
    memcpy(data, rbuf->b, rbuf->len);
    destroy_buf(env, rbuf);
    return output;
}

static ERL_NIF_TERM unescape_cdata(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary input;
    ERL_NIF_TERM output;
    struct buf *rbuf;
    int i;

    if (argc != 1) {
        return enif_make_badarg(env);
    }

    // CData should be iolist() or binary()
    if (enif_is_binary(env, argv[0])) {
        if (!enif_inspect_binary(env, argv[0], &input)) {
            return enif_make_badarg(env);
        }
    } else {
        if (!enif_inspect_iolist_as_binary(env, argv[0], &input)) {
            return enif_make_badarg(env);
        }
    }

    rbuf = init_buf(env, EXML_CDATA_BUF_SIZE);

    for (i = 0; i < input.size; i++) {
        if (input.data[i] == '&') {
            if (match_tag(input, i+1, "amp;", 4)) {
                buf_add_char(env, EXML_CDATA_BUF_SIZE, rbuf, '&');
                i += 4;
            } else if (match_tag(input, i+1, "lt;", 3)) {
                buf_add_char(env, EXML_CDATA_BUF_SIZE, rbuf, '<');
                i += 3;
            } else if (match_tag(input, i+1, "gt;", 3)) {
                buf_add_char(env, EXML_CDATA_BUF_SIZE, rbuf, '>');
                i += 3;
            } else {
                buf_add_char(env, EXML_CDATA_BUF_SIZE, rbuf, input.data[i]);
            }
        } else {
            buf_add_char(env, EXML_CDATA_BUF_SIZE, rbuf, input.data[i]);
        }
    }

    unsigned char *data = enif_make_new_binary(env, rbuf->len, &output);
    memcpy(data, rbuf->b, rbuf->len);
    destroy_buf(env, rbuf);
    return output;
}

static ERL_NIF_TERM escape_attr(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary input;
    ERL_NIF_TERM output;
    struct buf *rbuf;
    int i;

    if (argc != 1) {
        return enif_make_badarg(env);
    }

    if (!enif_inspect_binary(env, argv[0], &input)) {
        return enif_make_badarg(env);
    }

    rbuf = init_buf(env, EXML_ATTR_BUF_SIZE);

    for (i = 0; i < input.size; i++) {
        switch (input.data[i]) {
            case '&':
                buf_add_str(env, EXML_ATTR_BUF_SIZE, rbuf, "&amp;", 5);
                break;
            case '<':
                buf_add_str(env, EXML_ATTR_BUF_SIZE, rbuf, "&lt;", 4);
                break;
            case '>':
                buf_add_str(env, EXML_ATTR_BUF_SIZE, rbuf, "&gt;", 4);
                break;
            case '"':
                buf_add_str(env, EXML_ATTR_BUF_SIZE, rbuf, "&quot;", 6);
                break;
            case '\'':
                buf_add_str(env, EXML_ATTR_BUF_SIZE, rbuf, "&apos;", 6);
                break;
            case '\t':
                buf_add_str(env, EXML_ATTR_BUF_SIZE, rbuf, "&#x9;", 5);
                break;
            case '\n':
                buf_add_str(env, EXML_ATTR_BUF_SIZE, rbuf, "&#xA;", 5);
                break;
            case '\r':
                buf_add_str(env, EXML_ATTR_BUF_SIZE, rbuf, "&#xD;", 5);
                break;
            default:
                buf_add_char(env, EXML_ATTR_BUF_SIZE, rbuf, input.data[i]);
                break;
        };
    };

    unsigned char *data = enif_make_new_binary(env, rbuf->len, &output);
    memcpy(data, rbuf->b, rbuf->len);
    destroy_buf(env, rbuf);
    return output;
}

static ERL_NIF_TERM unescape_attr(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary input;
    ERL_NIF_TERM  output;
    struct buf *rbuf;
    int i;

    if (argc != 1) {
        return enif_make_badarg(env);
    }

    if (!enif_inspect_binary(env, argv[0], &input)) {
        return enif_make_badarg(env);
    }

    rbuf = init_buf(env, EXML_ATTR_BUF_SIZE);

    for (i = 0; i < input.size; i++) {
        if (input.data[i] == '&') {
            if (match_tag(input, i+1, "amp;", 4)) {
                buf_add_char(env, EXML_ATTR_BUF_SIZE, rbuf, '&');
                i += 4;
            } else if (match_tag(input, i+1, "apos;", 5)) {
                buf_add_char(env, EXML_ATTR_BUF_SIZE, rbuf, '\'');
                i += 5;
            } else if (match_tag(input, i+1, "lt;", 3)) {
                buf_add_char(env, EXML_ATTR_BUF_SIZE, rbuf, '<');
                i += 3;
            } else if (match_tag(input, i+1, "gt;", 3)) {
                buf_add_char(env, EXML_ATTR_BUF_SIZE, rbuf, '>');
                i += 3;
            } else if (match_tag(input, i+1, "quot;", 5)) {
                buf_add_char(env, EXML_ATTR_BUF_SIZE, rbuf, '"');
                i += 5;
            } else if (match_tag(input, i+1, "#x9;", 4)) {
                buf_add_char(env, EXML_ATTR_BUF_SIZE, rbuf, '\t');
                i += 4;
            } else if (match_tag(input, i+1, "#xA;", 4)) {
                buf_add_char(env, EXML_ATTR_BUF_SIZE, rbuf, '\n');
                i += 4;
            } else if (match_tag(input, i+1, "#xD;", 4)) {
                buf_add_char(env, EXML_ATTR_BUF_SIZE, rbuf, '\r');
                i += 4;
            } else {
                buf_add_char(env, EXML_ATTR_BUF_SIZE, rbuf, input.data[i]);
            }
        } else {
            buf_add_char(env, EXML_ATTR_BUF_SIZE, rbuf, input.data[i]);
        }
    }

    unsigned char* data = enif_make_new_binary(env, rbuf->len, &output);
    memcpy((char*)data, rbuf->b, rbuf->len);
    destroy_buf(env, rbuf);
    return output;
}

static int load(ErlNifEnv* env, void **priv, ERL_NIF_TERM info)
{
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
    return;
}

static ErlNifFunc nif_funcs[] =
{
    {"escape_attr_nif", 1, escape_attr},
    {"unescape_attr_nif", 1, unescape_attr},
    {"escape_cdata_nif", 1, escape_cdata},
    {"unescape_cdata_nif", 1, unescape_cdata}
};

ERL_NIF_INIT(exml, nif_funcs, &load, &reload, &upgrade, &unload);

