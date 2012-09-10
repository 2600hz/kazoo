riak_err: Limiting the maximum size of Erlang/OTP error_logger messages
=======================================================================

[![Build Status](https://secure.travis-ci.org/basho/riak_err.png?branch=master)](http://travis-ci.org/basho/riak_err)

The `riak_err` OTP application replaces about 90% of the default
Erlang/OTP info/error/warning event handling mechanism.
The replacement places strict limits on the maximum size of a
formatted message.

Why replace the default error handlers?
---------------------------------------

The Erlang/OTP default event handler will format a message using a
user-supplied formatting string or a default `"~p"` formatting string
(for report-style messages).  Using `~p` and/or `~w` formatting can
consume enormous amounts of RAM, due to how Erlang strings are
handled.  It isn't uncommon to submit an error message that is only
5MB, but formatting that message can use anywhere from 80MB to 320MB
on CPUs with a 64-bit word size.

A very brief tour of the error_logger
-------------------------------------

The OTP `kernel` application starts a system-wide event handler named
`error_logger` to receive events, format them, and write them to the
tty/console/CLI.

    $ erl -sname asdf -pz ./b/src/riak_err/ebin
    Erlang R13B04 (erts-5.7.5) [source] [64-bit] [smp:4:4] [rq:4] [async-threads:0] [hipe] [kernel-poll:false]
    
    Eshell V5.7.5  (abort with ^G)
    (asdf@sbb)1> gen_event:which_handlers(error_logger).
    [error_logger,error_logger_tty_h]

The OTP `sasl` application adds more handlers, specifically for
formatting events from processes that use `supervisor`, `gen_server`,
`gen_event`, and `gen_fsm` behavior (as well as any process that uses
`proc_lib` helper functions).

    (asdf@sbb)2> application:start(sasl).
    ok
    (asdf@sbb)3> gen_event:which_handlers(error_logger).
    [sasl_report_tty_h,error_logger,error_logger_tty_h]

The `sasl` application can be configured to add additional event
handlers that will write events to disk, either as human-friendly
ASCII/Latin-1 formatting or machine-friendly binary format.  For
example, this is the list of `error_logger`'s event handlers when the
Basho's Riak application is started.

    (riaksearch@127.0.0.1)1> gen_event:which_handlers(error_logger).
    [log_mf_h,sasl_report_file_h,error_logger,error_logger_tty_h]

When the `riak_err` application is started, the following event
handlers are removed from the `error_logger`:

* `error_logger`
* `error_logger_tty_h`
* `sasl_report_tty_h`
* `sasl_report_file_h`

... and are replaced by the `riak_err_handler` handler.

Configuration
-------------

There are two config knobs may be specified on the command line
via `-riak_err KnobName Integer` on the command line or (in a
Basho application like Riak, via the same `-riak_err KnobName Integer`
line in the `etc/vm.args` file).  Alternatively, these properties
may be set using application environment variables by the same name.

* `term_max_size` For arguments formatted in FormatString and
ArgList style, if the total size of ArgList is more than `term_max_size`,
then we'll ignore FormatString and log the message with a well-known
(and therefore safe) formatting string.  The default is 10KBytes.
* `fmt_max_bytes` When formatting a log-related term that might
be "big", limit the term's formatted output to a maximum of
`fmt_max_bytes` bytes.  The default is 12KBytes.

For example, `erl -riak_err term_max_size 8192 fmt_max_bytes 9000`

If the SASL error logger's `sasl_error_logger` configuration
parameter is set to the `{file, FileName}` form, then this
module will attempt to emulate the SASL error logger's
logging-to-file behavior.  However, the interpretation of the
`errlog_type` configuration parameter is limited: if its
value is `error`, then only error and warning messages will
be written to the file.  In all other cases (namely both
`progress` and `all`), all events will be formatted
and written to the file.

A short note about log file rotation
------------------------------------

* *NOTE:* The log file's filehandle will be re-opened once
per second, which will allow log file rotation schemes
to rotate the log file safely without undue worry about
losing log file entries or worrying about sending a
SIGHUP signal to the owner process before rotation.

Building via rebar
------------------

To build using [`rebar`](http://github.com/basho/rebar), add the
following line to your project's `rebar.config` file, in the `deps`
section:

    {riak_err, ".*", {git, "git@github.com:/basho/riak_err", "HEAD"}}

As an example, this is a mostly-complete `rebar.config` file from
Basho's Riak package:

    {sub_dirs, ["rel"]}.
    {require_otp_vsn, "R13B04|R14"}.
    {cover_enabled, true}.
    {erl_opts, [debug_info, fail_on_warning]}.
    
    %% Technically speaking, this dependency list is incomplete, but
    %% I wanted to show a complete rebar.config file here....
    {deps, [
            {riak_err, ".*", {git, "git@github.com:/basho/riak_err", "HEAD"}},
            {riak_kv, "0.13.0", {git, "git://github.com/basho/riak_kv", "HEAD"}},
            {luwak, "1.*", {git, "git://github.com/basho/luwak", "HEAD"}}
           ]}.

Licensing
---------

* Code written by Basho Technologies, Inc. is licensed under an Apache
Public License version 2.0, see the "LICENSE" file in this source code
distribution.
* Code written by Ericsson AB and contributors is licensed under the
Erlang Public License version 1.1, see http://www.erlang.org/.
* Code written by Corelatus AB and contributors is licensed under the
Erlang Public License version 1.1, see http://www.erlang.org/.
