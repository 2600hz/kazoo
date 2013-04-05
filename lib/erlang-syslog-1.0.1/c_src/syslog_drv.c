/*
Copyright 2009 Andrew Thompson <andrew@hijacked.us>. All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

  1. Redistributions of source code must retain the above copyright notice,
     this list of conditions and the following disclaimer.
  2. Redistributions in binary form must reproduce the above copyright
     notice, this list of conditions and the following disclaimer in the
     documentation and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED ``AS IS'' AND ANY EXPRESS OR
IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO
EVENT SHALL THE CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/
#include <stdint.h>
#include <syslog.h>
#include <stdarg.h>
#include <string.h>

#include <ei.h>
#include <erl_driver.h>

/* for pre-R15 compatibility */
#if ERL_DRV_EXTENDED_MAJOR_VERSION < 2
typedef int ErlDrvSizeT;
typedef int ErlDrvSSizeT;
#endif

#define DRV_NAME "syslog_drv"

/* the following constants have to match those in syslog.erl */
#define SYSLOGDRV_OPEN  1
#define SYSLOGDRV_CLOSE 2

struct syslogdrv {
    ErlDrvPort port;
    char *ident;
    int logopt;
    int facility;
    unsigned char open;
};

typedef struct syslogdrv syslogdrv_t;

static ErlDrvSSizeT encode_error(char* buf, char* error) {
    int index = 0;
    if (ei_encode_version(buf, &index) ||
        ei_encode_tuple_header(buf, &index, 2) ||
        ei_encode_atom(buf, &index, "error") ||
        ei_encode_atom(buf, &index, error)) {
        return (ErlDrvSSizeT)ERL_DRV_ERROR_GENERAL;
    }
    return index+1;
}

static ErlDrvData syslogdrv_start(ErlDrvPort port, char *buf)
{
    syslogdrv_t* d = (syslogdrv_t*)driver_alloc(sizeof(syslogdrv_t));
    d->port = port;
    d->open = 0;
    d->ident = NULL;
    set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);
    return (ErlDrvData)d;
}

static void syslogdrv_stop(ErlDrvData handle)
{
    syslogdrv_t* d = (syslogdrv_t*)handle;
    closelog();
    if (d->ident) {
        driver_free(d->ident);
    }
    driver_free((char*)handle);
}

static void syslogdrv_output(ErlDrvData handle, char *buf, ErlDrvSizeT len)
{
    syslogdrv_t* d = (syslogdrv_t*)handle;
    /* Incoming data is expected to start with an integer priority encoded
       as a 4-byte integer in network order, therefore make sure there's at
       least 5 bytes in the message. */
    if (d->open && len > 4) {
        int priority = ntohl(*(uint32_t*)buf);
        buf += 4;
        /* re-call openlog in case another instance of the port driver
         * was called in the mean time */
        openlog(d->ident, d->logopt, d->facility);
        syslog(priority, "%s", buf);
    }
}

static ErlDrvSSizeT syslogdrv_control(ErlDrvData handle, unsigned int command,
                                      char *buf, ErlDrvSizeT len,
                                      char **rbuf, ErlDrvSizeT rlen)
{
    syslogdrv_t* d = (syslogdrv_t*)handle;
    int index = 0, version, arity, type, size;

    if (command != SYSLOGDRV_OPEN) {
        return (ErlDrvSSizeT)ERL_DRV_ERROR_BADARG;
    }

    if (ei_decode_version(buf, &index, &version)) {
        return encode_error(*rbuf, "badver");
    }
    if (ei_decode_tuple_header(buf, &index, &arity) || arity != 4) {
        return (ErlDrvSSizeT)ERL_DRV_ERROR_BADARG;
    }
    if (ei_get_type(buf, &index, &type, &size)) {
        return (ErlDrvSSizeT)ERL_DRV_ERROR_BADARG;
    }
    if (type == ERL_STRING_EXT) {
        long logopt, facility, len;
        ErlDrvBinary* ref = 0;

        syslogdrv_t* nd = (syslogdrv_t*)driver_alloc(sizeof(syslogdrv_t));
        if (nd == NULL) {
            return encode_error(*rbuf, "enomem");
        }
        nd->ident = driver_alloc(size+1);
        if (nd->ident == NULL) {
            return encode_error(*rbuf, "enomem");
        }
        if (ei_decode_string(buf, &index, nd->ident)) {
            driver_free(nd->ident);
            driver_free(nd);
            return (ErlDrvSSizeT)ERL_DRV_ERROR_BADARG;
        }
        if (ei_decode_long(buf, &index, &logopt) ||
            ei_decode_long(buf, &index, &facility)) {
            driver_free(nd->ident);
            driver_free(nd);
            return (ErlDrvSSizeT)ERL_DRV_ERROR_BADARG;
        }
        if (ei_get_type(buf, &index, &type, &size)) {
            driver_free(nd->ident);
            driver_free(nd);
            return (ErlDrvSSizeT)ERL_DRV_ERROR_BADARG;
        }
        if (type != ERL_BINARY_EXT) {
            driver_free(nd->ident);
            driver_free(nd);
            return (ErlDrvSSizeT)ERL_DRV_ERROR_BADARG;
        }
        ref = driver_alloc_binary(size);
        if (ref == NULL) {
            return encode_error(*rbuf, "enomem");
        }
        if (ei_decode_binary(buf, &index, ref->orig_bytes, &len)) {
            driver_free_binary(ref);
            driver_free(nd->ident);
            driver_free(nd);
            return (ErlDrvSSizeT)ERL_DRV_ERROR_BADARG;
        }
        nd->logopt = (int)logopt;
        nd->facility = (int)facility;
        nd->open = 1;
        {
            ErlDrvTermData refdata = TERM_DATA(ref->orig_bytes);
            ErlDrvPort port = d->port;
            ErlDrvTermData pid = driver_caller(port);
            ErlDrvData data = (ErlDrvData)nd;
            nd->port = driver_create_port(port, pid, DRV_NAME, data);
            if (nd->port == (ErlDrvPort)-1) {
                driver_free_binary(ref);
                driver_free(nd->ident);
                driver_free(nd);
                return (ErlDrvSSizeT)ERL_DRV_ERROR_GENERAL;
            }
            set_port_control_flags(nd->port, PORT_CONTROL_FLAG_BINARY);
            ErlDrvTermData term[] = {
                ERL_DRV_EXT2TERM, refdata, ref->orig_size,
                ERL_DRV_ATOM, driver_mk_atom("ok"),
                ERL_DRV_PORT, driver_mk_port(nd->port),
                ERL_DRV_TUPLE, 2,
                ERL_DRV_TUPLE, 2,
            };
            driver_output_term(port, term, sizeof term/sizeof *term);
        }
        driver_free_binary(ref);
        return 0;
    } else {
        return (ErlDrvSSizeT)ERL_DRV_ERROR_BADARG;
    }
}

static ErlDrvSSizeT syslogdrv_call(ErlDrvData handle, unsigned int command,
                                   char *buf, ErlDrvSizeT len,
                                   char **rbuf, ErlDrvSizeT rlen, unsigned int* flags)
{
    syslogdrv_t* d = (syslogdrv_t*)handle;
    int index = 0;

    if (command != SYSLOGDRV_CLOSE) {
        return (ErlDrvSSizeT)ERL_DRV_ERROR_BADARG;
    }

    if (d->ident) {
        driver_free(d->ident);
    }
    d->ident = NULL;
    d->open = 0;
    if (ei_encode_version(*rbuf, &index) ||
        ei_encode_atom(*rbuf, &index, "ok")) {
        return (ErlDrvSSizeT)ERL_DRV_ERROR_GENERAL;
    }
    return index+1;
}

/*
 * Initialize and return a driver entry struct
 */
static ErlDrvEntry syslogdrv_driver_entry = {
    NULL,
    syslogdrv_start,
    syslogdrv_stop,
    syslogdrv_output,
    NULL,
    NULL,
    DRV_NAME,
    NULL,
    NULL,
    syslogdrv_control,
    NULL,
    NULL,
    NULL,
    NULL,
    syslogdrv_call,
    NULL,
    ERL_DRV_EXTENDED_MARKER,
    ERL_DRV_EXTENDED_MAJOR_VERSION,
    ERL_DRV_EXTENDED_MINOR_VERSION,
    0,
    NULL,
    NULL,
    NULL,
};

DRIVER_INIT(syslogdrv)
{
    return &syslogdrv_driver_entry;
}
