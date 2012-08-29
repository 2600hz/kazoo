Erlang driver for syslog
------------------------

This is an erlang port driver for interacting with syslog.

Installing it
-------------

    ./rebar compile
    sudo ./rebar install

Trying it
---------

You should have a look at syslog.h

In an other shell :

    $ tail -f /var/log/sylsog

Or, for mac users :

    $ tail -f /var/log/system.log

In erlang shell :

    $ erl
    > syslog:start().
    > syslog:open("Beuha", [cons, perror, pid], local0).
    > syslog:log(err, "Damned").

API
---

### syslog:open(Ident, Logopt, Facility) ###

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

### syslog:log(Priority, Message) ###

_Priority_ can be a number or better, an atom :

 * emerg
 * alert
 * crit
 * err
 * warning
 * notice
 * info
 * debug

_Message_ is a String

BUGS
----

 * Not a full OTP application
