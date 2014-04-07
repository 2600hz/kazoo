Transports
==========

Purpose
-------

A transport defines the interface to interact with a socket.

Transports can be used for connecting, listening and accepting
connections, but also for receiving and sending data. Both
passive and active mode are supported, although all sockets
are initialized as passive.

TCP transport
-------------

The TCP transport is a thin wrapper around `gen_tcp`.

SSL transport
-------------

The SSL transport is a thin wrapper around `ssl`. It requires
the `crypto`, `asn1`, `public_key` and `ssl` applications
to be started. When starting an SSL listener, Ranch will attempt
to automatically start them. It will not try to stop them when
the listener is removed, however.

``` erlang
ssl:start().
```

In a proper OTP setting, you will need to make your application
depend on the `crypto`, `public_key` and `ssl` applications.
They will be started automatically when starting your release.

The SSL transport `accept/2` function performs both transport
and SSL accepts. Errors occurring during the SSL accept phase
are returned as `{error, {ssl_accept, atom()}}` to differentiate
on which socket the problem occurred.

Sending and receiving data
--------------------------

This section assumes that `Transport` is a valid transport handler
(like `ranch_tcp` or `ranch_ssl`) and `Socket` is a connected
socket obtained through the listener.

You can send data to a socket by calling the `Transport:send/2`
function. The data can be given as `iodata()`, which is defined as
`binary() | iolist()`. All the following calls will work:

``` erlang
Transport:send(Socket, <<"Ranch is cool!">>).
Transport:send(Socket, "Ranch is cool!").
Transport:send(Socket, ["Ranch", ["is", "cool!"]]).
Transport:send(Socket, ["Ranch", [<<"is">>, "cool!"]]).
```

You can receive data either in passive or in active mode. Passive mode
means that you will perform a blocking `Transport:recv/2` call, while
active mode means that you will receive the data as a message.

By default, all data will be received as binary. It is possible to
receive data as strings, although this is not recommended as binaries
are a more efficient construct, especially for binary protocols.

Receiving data using passive mode requires a single function call. The
first argument is the socket, and the third argument is a timeout duration
before the call returns with `{error, timeout}`.

The second argument is the amount of data in bytes that we want to receive.
The function will wait for data until it has received exactly this amount.
If you are not expecting a precise size, you can specify 0 which will make
this call return as soon as data was read, regardless of its size.

``` erlang
{ok, Data} = Transport:recv(Socket, 0, 5000).
```

Active mode requires you to inform the socket that you want to receive
data as a message and to write the code to actually receive it.

There are two kinds of active modes: `{active, once}` and
`{active, true}`. The first will send a single message before going
back to passive mode; the second will send messages indefinitely.
We recommend not using the `{active, true}` mode as it could quickly
flood your process mailbox. It's better to keep the data in the socket
and read it only when required.

Three different messages can be received:
 *  `{OK, Socket, Data}`
 *  `{Closed, Socket}`
 *  `{Error, Socket, Reason}`

The value of `OK`, `Closed` and `Error` can be different
depending on the transport being used. To be able to properly match
on them you must first call the `Transport:messages/0` function.

``` erlang
{OK, Closed, Error} = Transport:messages().
```

To start receiving messages you will need to call the `Transport:setopts/2`
function, and do so every time you want to receive data.

``` erlang
{OK, Closed, Error} = Transport:messages(),
Transport:setopts(Socket, [{active, once}]),
receive
    {OK, Socket, Data} ->
        io:format("data received: ~p~n", [Data]);
    {Closed, Socket} ->
        io:format("socket got closed!~n");
    {Error, Socket, Reason} ->
        io:format("error happened: ~p~n", [Reason])
end.
```

You can easily integrate active sockets with existing Erlang code as all
you really need is just a few more clauses when receiving messages.

Sending files
-------------

As in the previous section it is assumed `Transport` is a valid transport
handler and `Socket` is a connected socket obtained through the listener.

To send a whole file, with name `Filename`, over a socket:

```erlang
{ok, SentBytes} = Transport:sendfile(Socket, Filename).
```

Or part of a file, with `Offset` greater than or equal to 0, `Bytes` number of
bytes and chunks of size `ChunkSize`:

```erlang
Opts = [{chunk_size, ChunkSize}],
{ok, SentBytes} = Transport:sendfile(Socket, Filename, Offset, Bytes, Opts).
```

To improve efficiency when sending multiple parts of the same file it is also
possible to use a file descriptor opened in raw mode:

```erlang
{ok, RawFile} = file:open(Filename, [raw, read, binary]),
{ok, SentBytes} = Transport:sendfile(Socket, RawFile, Offset, Bytes, Opts).
```

Writing a transport handler
---------------------------

A transport handler is a module implementing the `ranch_transport` behavior.
It defines a certain number of callbacks that must be written in order to
allow transparent usage of the transport handler.

The behavior doesn't define the socket options available when opening a
socket. These do not need to be common to all transports as it's easy enough
to write different initialization functions for the different transports that
will be used. With one exception though. The `setopts/2` function *must*
implement the `{active, once}` and the `{active, true}` options.

If the transport handler doesn't have a native implementation of `sendfile/5` a
fallback is available, `ranch_transport:sendfile/6`. The extra first argument
is the transport's module. See `ranch_ssl` for an example.
