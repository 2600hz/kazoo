Erlang driver for syslog
------------------------

This is an erlang port driver for interacting with syslog.

Installing it
-------------

    ./rebar compile
    sudo ./rebar install

Trying it
---------

You should have a look at syslog.h.

In another shell :

    $ tail -f /var/log/syslog

Or, for mac users :

    $ tail -f /var/log/system.log

In erlang shell :

    $ erl
    > syslog:start().
    > {ok,Log} = syslog:open("Beuha", [cons, perror, pid], local0).
    > syslog:log(Log, err, "Damned").
    > syslog:log(Log, info, "process count: ~w", [length(processes())]).

API
---

### syslog:open(Ident, Logopt, Facility) -> {ok, port()} ###

_Ident_ is an arbitrary string  
_Logopt_ is an atom or array of atom, you can use a number if you're brave enough :

 * pid
 * cons
 * odelay
 * ndelay
 * perror

_Facility_ is an atom :

 * kern
 * user
 * mail
 * daemon
 * auth
 * syslog
 * lpr
 * news
 * uucp
 * cron
 * authpriv
 * ftp
 * netinfo
 * remoteauth
 * install
 * ras
 * local0
 * local1
 * local2
 * local3
 * local4
 * local5
 * local6
 * local7

The `open` call returns either `{ok, Log}` where _Log_ is a syslog handle
that can be passed to subsequent `log` and `close` calls, or it will throw
{error, badarg}.

### syslog:log(Log, Priority, Message) -> ok ###

_Log_ is a syslog handle returned from `open`  
_Priority_ can be a number or better, an atom :

 * emerg
 * alert
 * crit
 * err
 * warning
 * notice
 * info
 * debug

_Message_ is a string.

### syslog:log(Log, Priority, FormatMsg, FormatArgs) -> ok ###

Same as above, but allows for the construction of log messages similar to
formatting strings via `io_lib:format/2`, where _FormatMsg_ indicates the
formatting instructions and _FormatArgs_ is a list of arguments to be
formatted.

### syslog:close(Log) -> ok ###

_Log_ is a syslog handle returned from `open`

BUGS
----

 * None known
