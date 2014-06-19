Internals
=========

This chapter may not apply to embedded Ranch as embedding allows you
to use an architecture specific to your application, which may or may
not be compatible with the description of the Ranch application.

Note that for everything related to efficiency and performance,
you should perform the benchmarks yourself to get the numbers that
matter to you. Generic benchmarks found on the web may or may not
be of use to you, you can never know until you benchmark your own
system.

Architecture
------------

Ranch is an OTP application.

Like all OTP applications, Ranch has a top supervisor. It is responsible
for supervising the `ranch_server` process and all the listeners that
will be started.

The `ranch_server` gen_server is a central process keeping track of the
listeners and their acceptors. It does so through the use of a public ets
table called `ranch_server`. The table is owned by the top supervisor
to improve fault tolerance. This way if the `ranch_server` gen_server
fails, it doesn't lose any information and the restarted process can
continue as if nothing happened.

Ranch uses a custom supervisor for managing connections. This supervisor
keeps track of the number of connections and handles connection limits
directly. While it is heavily optimized to perform the task of creating
connection processes for accepted connections, it is still following the
OTP principles and the usual `sys` and `supervisor` calls will work on
it as expected.

Listeners are grouped into the `ranch_listener_sup` supervisor and
consist of three kinds of processes: the listener gen_server, the
acceptor processes and the connection processes, both grouped under
their own supervisor. All of these processes are registered to the
`ranch_server` gen_server with varying amount of information.

All socket operations, including listening for connections, go through
transport handlers. Accepted connections are given to the protocol handler.
Transport handlers are simple callback modules for performing operations on
sockets. Protocol handlers start a new process, which receives socket
ownership, with no requirements on how the code should be written inside
that new process.

Number of acceptors
-------------------

The second argument to `ranch:start_listener/6` is the number of
processes that will be accepting connections. Care should be taken
when choosing this number.

First of all, it should not be confused with the maximum number
of connections. Acceptor processes are only used for accepting and
have nothing else in common with connection processes. Therefore
there is nothing to be gained from setting this number too high,
in fact it can slow everything else down.

Second, this number should be high enough to allow Ranch to accept
connections concurrently. But the number of cores available doesn't
seem to be the only factor for choosing this number, as we can
observe faster accepts if we have more acceptors than cores. It
might be entirely dependent on the protocol, however.

Our observations suggest that using 100 acceptors on modern hardware
is a good solution, as it's big enough to always have acceptors ready
and it's low enough that it doesn't have a negative impact on the
system's performances.

Platform-specific TCP features
------------------------------

Some socket options are platform-specific and not supported by `inet`.
They can be of interest because they generally are related to
optimizations provided by the underlying OS. They can still be enabled
thanks to the `raw` option, for which we will see an example.

One of these features is `TCP_DEFER_ACCEPT` on Linux. It is a simplified
accept mechanism which will wait for application data to come in before
handing out the connection to the Erlang process.

This is especially useful if you expect many connections to be mostly
idle, perhaps part of a connection pool. They can be handled by the
kernel directly until they send any real data, instead of allocating
resources to idle connections.

To enable this mechanism, the following option can be used.

``` erlang
    {raw, 6, 9, << 30:32/native >>}
```

It means go on layer 6, turn on option 9 with the given integer parameter.
