/* ``The contents of this file are subject to the Erlang Public License,
 * Version 1.1, (the "License"); you may not use this file except in
 * compliance with the License. You should have received a copy of the
 * Erlang Public License along with this software. If not, it can be
 * retrieved via the world wide web at http://www.erlang.org/.
 * 
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
 * the License for the specific language governing rights and limitations
 * under the License.
 * 
 * The Initial Developer of the Original Code is Ericsson Utvecklings AB.
 * Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
 * AB. All Rights Reserved.''
 * 
 *     $Id$
 */

/*
 * Include file for erlang driver writers.
 */

#ifndef __ERL_DRIVER_H__
#define __ERL_DRIVER_H__

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include <stdlib.h>

#if defined(VXWORKS)
#  include <ioLib.h>
typedef struct iovec SysIOVec;
#elif defined(__WIN32__) || defined(_WIN32) || defined(_WIN32_)
#ifndef STATIC_ERLANG_DRIVER
   /* Windows dynamic drivers, everything is different... */
#define ERL_DRIVER_TYPES_ONLY
#define WIN32_DYNAMIC_ERL_DRIVER
#endif
/*
 * This structure can be cast to a WSABUF structure.
 */
typedef struct _SysIOVec {
    unsigned long iov_len;
    char* iov_base;
} SysIOVec;
#else  /* Unix */
#  ifdef HAVE_SYS_UIO_H
#    include <sys/types.h>
#    include <sys/uio.h>
typedef struct iovec SysIOVec;
#  else
typedef struct {
    char* iov_base;
    size_t iov_len;
} SysIOVec;
#  endif
#endif

#ifndef EXTERN
#  ifdef __cplusplus
#    define EXTERN extern "C"
#  else
#    define EXTERN extern
#  endif
#endif

/* Values for mode arg to driver_select() */

#define DO_READ	 (1 << 0)
#define DO_WRITE (1 << 1)
#ifdef _OSE_
#define DO_START (1 << 2)
#define DO_STOP  (1 << 3)
#endif /* _OSE_ */

/* Values for set_port_control_flags() */

#define PORT_CONTROL_FLAG_BINARY	(1 << 0)
#define PORT_CONTROL_FLAG_HEAVY		(1 << 1)

/* Values for get_port_flags() */

#define PORT_FLAG_BINARY                (1 << 0)
#define PORT_FLAG_LINE                  (1 << 1)


/*
 * A binary as seen in a driver. Note that a binary should never be
 * altered by the driver when it has been sent to Erlang.
 */

typedef struct erl_drv_binary {
    long orig_size;        /* total length of binary */
    char orig_bytes[1];   /* the data (char instead of byte!) */
} ErlDrvBinary;


/*
 * Note: These types are incomplete to catch type errors easier.
 */

typedef struct _erl_drv_data* ErlDrvData; /* Data to be used by the driver itself. */
#ifndef ERL_SYS_DRV
typedef struct _erl_drv_event* ErlDrvEvent; /* An event to be selected on. */
typedef struct _erl_drv_port* ErlDrvPort; /* A port descriptor. */
#endif
typedef struct _erl_drv_port* ErlDrvThreadData; /* Thread data. */

#if !defined(__WIN32__) && !defined(_WIN32) && !defined(_WIN32_) && !defined(USE_SELECT)
struct erl_drv_event_data {
    short events;
    short revents;
};
#endif
typedef struct erl_drv_event_data *ErlDrvEventData; /* Event data */

/*
 * Error codes that can be return from driver.
 */

/*
 * Exception code from open_port/2 will be {'EXIT',{einval,Where}}.
 */
#define ERL_DRV_ERROR_GENERAL ((ErlDrvData) -1)

/*
 * Exception code from open_port/2 will be {'EXIT',{Errno,Where}},
 * where Errno is a textual representation of the errno variable
 * (e.g. eacces if errno is EACCES).
 */
#define ERL_DRV_ERROR_ERRNO ((ErlDrvData) -2)

/*
 * Exception code from open_port/2 will be {'EXIT',{badarg,Where}}.
 */
#define ERL_DRV_ERROR_BADARG ((ErlDrvData) -3)

typedef struct erl_io_vec {
    int vsize;			/* length of vectors */
    int size;			/* total size in bytes */
    SysIOVec* iov;
    ErlDrvBinary** binv;
} ErlIOVec;

/*
 * This structure defines a driver.
 */

typedef struct erl_drv_entry {
    int (*init)(void);		/* called at system start up for statically
				   linked drivers, and after loading for
				   dynamically loaded drivers */ 

#ifndef ERL_SYS_DRV
    ErlDrvData (*start)(ErlDrvPort port, char *command);
				/* called when open_port/2 is invoked.
				   return value -1 means failure. */
#else
    ErlDrvData (*start)(ErlDrvPort port, char *command, SysDriverOpts* opts);
				/* special options, only for system driver */
#endif
    void (*stop)(ErlDrvData drv_data);
                                /* called when port is closed, and when the
				   emulator is halted. */
    void (*output)(ErlDrvData drv_data, char *buf, int len);
				/* called when we have output from erlang to 
				   the port */
    void (*ready_input)(ErlDrvData drv_data, ErlDrvEvent event); 
				/* called when we have input from one of 
				   the driver's handles) */
    void (*ready_output)(ErlDrvData drv_data, ErlDrvEvent event);  
				/* called when output is possible to one of 
				   the driver's handles */
    char *driver_name;		/* name supplied as command 
				   in open_port XXX ? */
    void (*finish)(void);        /* called before unloading the driver -
				   DYNAMIC DRIVERS ONLY */
    void *handle;		/* not used -- here for backwards compatibility */
    int (*control)(ErlDrvData drv_data, unsigned int command, char *buf, 
		   int len, char **rbuf, int rlen); 
				/* "ioctl" for drivers - invoked by 
				   port_command/3) */
    void (*timeout)(ErlDrvData drv_data);	/* Handling of timeout in driver */
    void (*outputv)(ErlDrvData drv_data, ErlIOVec *ev);
				/* called when we have output from erlang
				   to the port */
    void (*ready_async)(ErlDrvData drv_data, ErlDrvThreadData thread_data);
    void (*flush)(ErlDrvData drv_data);
                                /* called when the port is about to be 
				   closed, and there is data in the 
				   driver queue that needs to be flushed
				   before 'stop' can be called */
    int (*call)(ErlDrvData drv_data, unsigned int command, char *buf, 
		   int len, char **rbuf, int rlen, unsigned int *flags); 
                                /* Works mostly like 'control', a syncronous
				   call into the driver. */
    void (*event)(ErlDrvData drv_data, ErlDrvEvent event,
		  ErlDrvEventData event_data);
                                /* Called when an event selected by 
				   driver_event() has occurred */
} ErlDrvEntry;

/*
 * This macro is used to name a dynamic driver's init function in
 * a way that doesn't lead to conflicts. This is crucial when using
 * operating systems that has one namespace for all symbols
 * (e.g. VxWorks). Example: if you have an dynamic driver C source
 * file named echo_drv.c, you use the macro like this:
 * 
 *    DRIVER_INIT(echo_drv)
 *    {
 *	 ....
 *    }
 *
 * This function well be called by the Erlang I/O system when the driver is loaded.
 * It must initialize a ErlDrvEntry structure and return a pointer to it.
 */

/* For windows dynamic drivers */
#ifndef ERL_DRIVER_TYPES_ONLY

#if defined(VXWORKS)
#  define DRIVER_INIT(DRIVER_NAME) ErlDrvEntry* DRIVER_NAME  ## _init(void)
#elif defined(__WIN32__)
#  define DRIVER_INIT(DRIVER_NAME) __declspec(dllexport) ErlDrvEntry* driver_init(void)
#else 
#  define DRIVER_INIT(DRIVER_NAME) ErlDrvEntry* driver_init(void)
#endif

/*
 * These are the functions available for driver writers.
 */
EXTERN int driver_select(ErlDrvPort port, ErlDrvEvent event, int mode, int on);
EXTERN int driver_event(ErlDrvPort port, ErlDrvEvent event, 
			ErlDrvEventData event_data);
EXTERN int driver_output(ErlDrvPort port, char *buf, int len);
EXTERN int driver_output2(ErlDrvPort port, char *hbuf, int hlen, 
			  char *buf, int len);
EXTERN int driver_output_binary(ErlDrvPort port, char *hbuf, int hlen,
				ErlDrvBinary* bin, int offset, int len);
EXTERN int driver_outputv(ErlDrvPort port, char* hbuf, int hlen, ErlIOVec *ev,
			  int skip);
EXTERN int driver_vec_to_buf(ErlIOVec *ev, char *buf, int len);
EXTERN int driver_set_timer(ErlDrvPort port, unsigned long time);
EXTERN int driver_cancel_timer(ErlDrvPort port);
EXTERN int driver_read_timer(ErlDrvPort port, unsigned long *time_left);

/*
 * Get plain-text error message from within a driver
 */
EXTERN char* erl_errno_id(int error);

/*
 * The following functions are used to initiate a close of a port
 * from a driver.
 */
EXTERN int driver_failure_eof(ErlDrvPort port);
EXTERN int driver_failure_atom(ErlDrvPort port, char *string);
EXTERN int driver_failure_posix(ErlDrvPort port, int error);
EXTERN int driver_failure(ErlDrvPort port, int error);
EXTERN int driver_exit (ErlDrvPort port, int err);

/*
 * Port attributes
 */
EXTERN void set_busy_port(ErlDrvPort port, int on);
EXTERN void set_port_control_flags(ErlDrvPort port, int flags);

EXTERN int  get_port_flags(ErlDrvPort port);


/* Binary interface */

/*
 * NOTE: DO NOT overwrite a binary with new data (if the data is delivered);
 * since the binary is a shared object it MUST be written once.
 */

EXTERN ErlDrvBinary* driver_alloc_binary(int size);
EXTERN ErlDrvBinary* driver_realloc_binary(ErlDrvBinary *bin, int size);
EXTERN void driver_free_binary(ErlDrvBinary *bin);

/* Referenc count on driver binaries */
EXTERN long driver_binary_get_refc(ErlDrvBinary *dbp);
EXTERN long driver_binary_inc_refc(ErlDrvBinary *dbp);
EXTERN long driver_binary_dec_refc(ErlDrvBinary *dbp);

/* Allocation interface */
EXTERN void *driver_alloc(size_t size);
EXTERN void *driver_realloc(void *ptr, size_t size);
EXTERN void driver_free(void *ptr);

/* Queue interface */
EXTERN int driver_enq(ErlDrvPort port, char* buf, int len);
EXTERN int driver_pushq(ErlDrvPort port, char* buf, int len);
EXTERN int driver_deq(ErlDrvPort port, int size);
EXTERN int driver_sizeq(ErlDrvPort port);
EXTERN int driver_enq_bin(ErlDrvPort port, ErlDrvBinary *bin, int offset, 
			  int len);
EXTERN int driver_pushq_bin(ErlDrvPort port, ErlDrvBinary *bin, int offset,
			    int len);

EXTERN int driver_peekqv(ErlDrvPort port, ErlIOVec *ev);
EXTERN SysIOVec* driver_peekq(ErlDrvPort port, int *vlen);
EXTERN int driver_enqv(ErlDrvPort port, ErlIOVec *ev, int skip);
EXTERN int driver_pushqv(ErlDrvPort port, ErlIOVec *ev, int skip);

/*
 * Add and remove driver entries.
 */
EXTERN void add_driver_entry(ErlDrvEntry *de);
EXTERN int remove_driver_entry(ErlDrvEntry *de);

/*
 * Misc.
 */
EXTERN int null_func(void);

#endif /* !ERL_DRIVER_TYPES_ONLY */

/* Constants for return flags from the 'port_call' callback */
#define DRIVER_CALL_KEEP_BUFFER 0x1

/* ErlDrvTerm is the type to use for casts when building 
 * terms that should be sent to connected process,
 * for instance a tuple on the form {tcp, Port, [Tag|Binary]}
 *
 * ErlDrvTerm spec[] = {
 *    ERL_DRV_ATOM, driver_mk_atom("tcp"),
 *    ERL_DRV_PORT, driver_mk_port(drv->ix),
 *             ERL_DRV_INT, REPLY_TAG,
 *             ERL_DRV_BINARY, (ErlDrvTerm)bin, 50, 0,
 *             ERL_DRV_LIST, 2,
 *    ERL_DRV_TUPLE, 3,
 *  }       
 *             
 */

typedef unsigned long ErlDrvTermData;

#define TERM_DATA(x) ((ErlDrvTermData) (x))

/* Possible types to send from driver          Argument type */
#define ERL_DRV_NIL         ((ErlDrvTermData) 1)  /* None */
#define ERL_DRV_ATOM        ((ErlDrvTermData) 2)  /* driver_mk_atom(string) */
#define ERL_DRV_INT         ((ErlDrvTermData) 3)  /* int */
#define ERL_DRV_PORT        ((ErlDrvTermData) 4)  /* driver_mk_port(ix) */
#define ERL_DRV_BINARY      ((ErlDrvTermData) 5)  /* ErlDriverBinary*, 
						   * int size, int offs */
#define ERL_DRV_STRING      ((ErlDrvTermData) 6)  /* char*, int */
#define ERL_DRV_TUPLE       ((ErlDrvTermData) 7)  /* int */
#define ERL_DRV_LIST        ((ErlDrvTermData) 8)  /* int */
#define ERL_DRV_STRING_CONS ((ErlDrvTermData) 9)  /* char*, int */
#define ERL_DRV_PID         ((ErlDrvTermData) 10) /* driver_connected,... */

#define ERL_DRV_FLOAT         ((ErlDrvTermData) 11) /* double * */

#ifndef ERL_DRIVER_TYPES_ONLY

/* make terms for driver_output_term and driver_send_term */
EXTERN ErlDrvTermData driver_mk_atom(char*);
EXTERN ErlDrvTermData driver_mk_port(ErlDrvPort);
EXTERN ErlDrvTermData driver_connected(ErlDrvPort);
EXTERN ErlDrvTermData driver_caller(ErlDrvPort);
extern const ErlDrvTermData driver_term_nil;
EXTERN ErlDrvTermData driver_mk_term_nil(void);
EXTERN ErlDrvPort driver_create_port(ErlDrvEntry* driver, 
				     ErlDrvTermData connected, /* pid */
				     char* name, /* driver name */
				     ErlDrvData drv_data);
					 

/* output term data to the port owner */
EXTERN int driver_output_term(ErlDrvPort ix, ErlDrvTermData* data, int len);
/* output term data to a specific process */
EXTERN int driver_send_term(ErlDrvPort ix, ErlDrvTermData to,
			    ErlDrvTermData* data, int len);

/* Async IO functions */
EXTERN long driver_async(ErlDrvPort ix,
			 unsigned int* key,
			 void (*async_invoke)(void*), 
			 void* async_data,
			 void (*async_free)(void*));


EXTERN int driver_async_cancel(unsigned int key);

EXTERN int driver_attach(ErlDrvPort ix);
EXTERN int driver_detach(ErlDrvPort ix);

/* These were removed from the ANSI version, now they're back. */

EXTERN void *driver_dl_open(char *);
EXTERN void *driver_dl_sym(void *, char *);
EXTERN int driver_dl_close(void *);
EXTERN char *driver_dl_error(void);

#endif /* !ERL_DRIVER_TYPES_ONLY */

#ifdef WIN32_DYNAMIC_ERL_DRIVER
#  include "erl_win_dyn_driver.h"
#endif

#endif




