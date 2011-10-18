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

#include <syslog.h>
#include <stdarg.h>
#include <string.h>

#include <ei.h>
#include <erl_driver.h>

/* atoms which are sent to erlang */
static ErlDrvTermData am_ok;
static ErlDrvTermData am_value;
static ErlDrvTermData am_error;
static ErlDrvTermData am_badver;
static ErlDrvTermData am_badarg;
static ErlDrvTermData am_notopen;

struct syslogdrv {
	ErlDrvPort port;
	unsigned char open;
	char *ident;
	int logopt;
	int facility;
};

typedef struct syslogdrv syslogdrv_t;

static ErlDrvEntry syslogdrv_driver_entry;

static void encode_error(ei_x_buff *buff, char *error) {
	ei_x_encode_tuple_header(buff, 2);
	ei_x_encode_atom(buff, "error");
	ei_x_encode_atom(buff, error);
}

static ErlDrvData syslogdrv_start(ErlDrvPort port, char *buf)
{
	syslogdrv_t* d = (syslogdrv_t*)driver_alloc(sizeof(syslogdrv_t));
	d->port = port;
	d->open = 0;
	d->ident = NULL;
	return (ErlDrvData)d;
}

static void syslogdrv_stop(ErlDrvData handle)
{
	driver_free((char*)handle);
}

/* messages from erlang */
static void syslogdrv_output(ErlDrvData handle, char *buff, int bufflen)
{
	syslogdrv_t* d = (syslogdrv_t*)handle;
	int index = 0, version, arity;
	ei_x_buff result;
	char operation[MAXATOMLEN];

	ei_x_new_with_version(&result);
	
	if (ei_decode_version(buff, &index, &version)) {
		encode_error(&result, "badver");
		goto done;
	}

	if (ei_decode_tuple_header(buff, &index, &arity) || ei_decode_atom(buff, &index, operation)) {
		encode_error(&result, "badarg");
		goto done;
	}

	if (!strcmp("open", operation)) {
		int size, type;

		if (d->ident)
			free(d->ident); /* free old ident string */

		ei_get_type(buff, &index, &type, &size);
		if (type == ERL_STRING_EXT) {
			long logopt, facility;

			d->ident = malloc(size+1);
			ei_decode_string(buff, &index, d->ident);
			if (ei_decode_long(buff, &index, &logopt) || ei_decode_long(buff, &index, &facility)) {
				encode_error(&result, "badarg");
				goto done;
			}
			openlog(d->ident, logopt, facility);
			d->logopt = (int)logopt;
			d->facility = (int)facility;
			d->open = 1;
			ei_x_encode_atom(&result, "ok");
		} else {
			encode_error(&result, "badarg");
		}
	} else if (!strcmp("log", operation) && d->open) {
		int size, type;
		long priority;
		if (ei_decode_long(buff, &index, &priority)) {
			encode_error(&result, "badarg");
			goto done;
		}
		ei_get_type(buff, &index, &type, &size);
		if (type == ERL_STRING_EXT) {
			char *message;

			message = malloc(size+1);
			ei_decode_string(buff, &index, message);
			/* re-call openlog in-case another instance of the port driver
			 * was called in the mean time */
			openlog(d->ident, d->logopt, d->facility);
			syslog(priority, "%s", message);
			free(message);
			ei_x_encode_atom(&result, "ok");
		} else {
			encode_error(&result, "badarg");
		}
	} else if (!strcmp("log", operation)) { /* NOT open */
		encode_error(&result, "notopen");
	} else {
		encode_error(&result, "badarg");
	}

done:
	driver_output(d->port, result.buff, result.index);
	ei_x_free(&result);
}

/*
 * Initialize and return a driver entry struct
 */

DRIVER_INIT(syslogdrv)
{
	am_ok           = driver_mk_atom("ok");
	am_value        = driver_mk_atom("value");
	am_error        = driver_mk_atom("error");
	am_badver       = driver_mk_atom("badver");
	am_badarg        = driver_mk_atom("badarg");
	am_notopen        = driver_mk_atom("notopen");

	syslogdrv_driver_entry.init         = NULL;   /* Not used */
	syslogdrv_driver_entry.start        = syslogdrv_start;
	syslogdrv_driver_entry.stop         = syslogdrv_stop;
	syslogdrv_driver_entry.output       = syslogdrv_output;
	syslogdrv_driver_entry.ready_input  = NULL;
	syslogdrv_driver_entry.ready_output = NULL;
	syslogdrv_driver_entry.driver_name  = "syslog_drv";
	syslogdrv_driver_entry.finish       = NULL;
	syslogdrv_driver_entry.outputv      = NULL;
	return &syslogdrv_driver_entry;
}
