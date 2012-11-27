Cowboy
======

Cowboy is a small, fast and modular HTTP server written in Erlang.

Cowboy is also a socket acceptor pool, able to accept connections
for any kind of TCP protocol.

Goals
-----

Cowboy aims to provide the following advantages:

* **Small** code base.
* Damn **fast**.
* **Modular**: transport and protocol handlers are replaceable.
* **Binary HTTP** for greater speed and lower memory usage.
* Easy to **embed** inside another application.
* Selectively **dispatch** requests to handlers, allowing you to send some
  requests to your embedded code and others to a FastCGI application in
  PHP or Ruby.
* No parameterized module. No process dictionary. **Clean** Erlang code.

The server is currently in early development. Comments and suggestions are
more than welcome. To contribute, either open bug reports, or fork the project
and send us pull requests with new or improved functionality. You should
discuss your plans with us before doing any serious work, though, to avoid
duplicating efforts.

Quick start
-----------

* Add Cowboy as a rebar or agner dependency to your application.
* Start Cowboy and add one or more listeners.
* Write handlers for your application.
* Check out [examples](https://github.com/extend/cowboy_examples)!

Getting Started
---------------

At heart, Cowboy is nothing more than an TCP acceptor pool. All it does is
accept connections received on a given port and using a given transport,
like TCP or SSL, and forward them to a request handler for the given
protocol. Acceptors and request handlers are of course supervised
automatically.

It just so happens that Cowboy also includes an HTTP protocol handler.
But Cowboy does nothing by default. You need to explicitly ask Cowboy
to listen on a port with your chosen transport and protocol handlers.
To do so, you must start a listener.

A listener is a special kind of supervisor that manages both the
acceptor pool and the request processes. It is named and can thus be
started and stopped at will.

An acceptor pool is a pool of processes whose only role is to accept
new connections. It's good practice to have many of these processes
as they are very cheap and allow much quicker response when you get
many connections. Of course, as with everything else, you should
**benchmark** before you decide what's best for you.

Cowboy includes a TCP transport handler for HTTP and an SSL transport
handler for HTTPS. The transport handlers can of course be reused for
other protocols like FTP or IRC.

The HTTP protocol requires one last thing to continue: dispatching rules.
Don't worry about it right now though and continue reading, it'll all
be explained.

You can start and stop listeners by calling `cowboy:start_listener/6` and
`cowboy:stop_listener/1` respectively.

The following example demonstrates the startup of a very simple listener.

``` erlang
application:start(cowboy),
Dispatch = [
    %% {Host, list({Path, Handler, Opts})}
    {'_', [{'_', my_handler, []}]}
],
%% Name, NbAcceptors, Transport, TransOpts, Protocol, ProtoOpts
cowboy:start_listener(my_http_listener, 100,
    cowboy_tcp_transport, [{port, 8080}],
    cowboy_http_protocol, [{dispatch, Dispatch}]
).
```

This is not enough though, you must also write the my_handler module
to process the incoming HTTP requests. Of course Cowboy comes with
predefined handlers for specific tasks but most of the time you'll
want to write your own handlers for your application.

Following is an example of a "Hello World!" HTTP handler.

``` erlang
-module(my_handler).
-export([init/3, handle/2, terminate/2]).

init({tcp, http}, Req, Opts) ->
    {ok, Req, undefined_state}.

handle(Req, State) ->
    {ok, Req2} = cowboy_http_req:reply(200, [], <<"Hello World!">>, Req),
    {ok, Req2, State}.

terminate(Req, State) ->
    ok.
```

You can also write handlers that do not reply directly. Instead, such handlers
will wait for an Erlang message from another process and only reply when
receiving such message, or timeout if it didn't arrive in time.

This is especially useful for long-polling functionality, as Cowboy will handle
process hibernation and timeouts properly, preventing mistakes if you were to
write the code yourself. An handler of that kind can be defined like this:

``` erlang
-module(my_loop_handler).
-export([init/3, info/3, terminate/2]).

-define(TIMEOUT, 60000).

init({tcp, http}, Req, Opts) ->
	{loop, Req, undefined_state, ?TIMEOUT, hibernate}.

info({reply, Body}, Req, State) ->
	{ok, Req2} = cowboy_http_req:reply(200, [], Body, Req),
	{ok, Req2, State};
info(Message, Req, State) ->
	{loop, Req, State, hibernate}.

terminate(Req, State) ->
	ok.
```

It is of course possible to combine both type of handlers together as long as
you return the proper tuple from init/3.

**Note**: versions prior to `0.4.0` used the
[quoted](https://github.com/klaar/quoted.erl) library instead of the built in
`cowboy_http:urldecode/2` function. If you want to retain this you must add it
as a dependency to your application and add the following cowboy_http_protocol
option:

``` erlang
    {urldecode, {fun quoted:from_url/2, quoted:make([])}}
```

Continue reading to learn how to dispatch rules and handle requests.

Dispatch rules
--------------

Cowboy allows you to dispatch HTTP requests directly to a specific handler
based on the hostname and path information from the request. It also lets
you define static options for the handler directly in the rules.

To match the hostname and path, Cowboy requires a list of tokens. For
example, to match the "dev-extend.eu" domain name, you must specify
`[<<"dev-extend">>, <<"eu">>]`. Or, to match the "/path/to/my/resource"
you must use `[<<"path">>, <<"to">>, <<"my">>, <<"resource">>]`. All the
tokens must be given as binary.

You can use the special token `'_'` (the atom underscore) to indicate that
you accept anything in that position. For example if you have both
"dev-extend.eu" and "dev-extend.fr" domains, you can use the match spec
`[<<"dev-extend">>, '_']` to match any top level extension.

Finally, you can also match multiple leading segments of the domain name and
multiple trailing segments of the request path using the atom `'...'` (the atom
ellipsis) respectively as the first host token or the last path token. For
example, host rule `['...', <<"dev-extend">>, <<"eu">>]` can match both
"cowboy.bugs.dev-extend.eu" and "dev-extend.eu" and path rule
`[<<"projects">>, '...']` can match both "/projects" and
"/projects/cowboy/issues/42". The host leading segments and the path trailing
segments can later be retrieved through `cowboy_http_req:host_info/1` and
`cowboy_http_req:path_info/1`.

Any other atom used as a token will bind the value to this atom when
matching. To follow on our hostnames example, `[<<"dev-extend">>, ext]`
would bind the values `<<"eu">>` and `<<"fr">>` to the ext atom, that you
can later retrieve in your handler by calling `cowboy_http_req:binding/{2,3}`.

You can also accept any match spec by using the atom `'_'` directly instead of
a list of tokens. Our hello world example above uses this to forward all
requests to a single handler.

There is currently no way to match multiple tokens at once.

Requests handling
-----------------

Requests are passed around in the Request variable. Although they are
defined as a record, it is recommended to access them only through the
cowboy_http_req module API.

You can retrieve the HTTP method, HTTP version, peer address and port,
host tokens, raw host, used port, path tokens, raw path, query string
values, bound values from the dispatch step, header values from the
request. You can also read the request body, if any, optionally parsing
it as a query string. Finally, the request allows you to send a response
to the client.

See the cowboy_http_req module for more information.

Websockets
----------

The Websocket protocol is built upon the HTTP protocol. It first sends
an HTTP request for an handshake, performs it and then switches
to Websocket. Therefore you need to write a standard HTTP handler to
confirm the handshake should be completed and then the Websocket-specific
callbacks.

A simple handler doing nothing but sending a repetitive message using
Websocket would look like this:

``` erlang
-module(my_ws_handler).
-export([init/3]).
-export([websocket_init/3, websocket_handle/3,
    websocket_info/3, websocket_terminate/3]).

init({tcp, http}, Req, Opts) ->
    {upgrade, protocol, cowboy_http_websocket}.

websocket_init(TransportName, Req, _Opts) ->
    erlang:start_timer(1000, self(), <<"Hello!">>),
    {ok, Req, undefined_state}.

websocket_handle({text, Msg}, Req, State) ->
    {reply, {text, << "That's what she said! ", Msg/binary >>}, Req, State};
websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

websocket_info({timeout, _Ref, Msg}, Req, State) ->
    erlang:start_timer(1000, self(), <<"How' you doin'?">>),
    {reply, {text, Msg}, Req, State};
websocket_info(_Info, Req, State) ->
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
    ok.
```

Of course you can have an HTTP handler doing both HTTP and Websocket
handling, but for the sake of this example we're ignoring the HTTP
part entirely.

As the Websocket protocol is still a draft the API is subject to change
regularly when support to the most recent drafts gets added. Features may
be added, changed or removed before the protocol gets finalized. Cowboy
tries to implement all drafts transparently and give a single interface to
handle them all, however.

Using Cowboy with other protocols
---------------------------------

One of the strengths of Cowboy is of course that you can use it with any
protocol you want. The only downside is that if it's not HTTP, you'll
probably have to write the protocol handler yourself.

The only exported function a protocol handler needs is the start_link/4
function, with arguments ListenerPid, Socket, Transport and Opts. ListenerPid
is the pid to the listener's gen_server, managing the connections. Socket is of
course the client socket; Transport is the module name of the chosen transport
handler and Opts is protocol options defined when starting the listener.

After initializing your protocol, it is recommended to call the
function cowboy:accept_ack/1 with the ListenerPid as argument,
as it will ensure Cowboy has been able to fully initialize the socket.
Anything you do past this point is up to you!

If you need to change some socket options, like enabling raw mode for example,
you can call the <em>Transport:setopts/2</em> function. It is the protocol's
responsability to manage the socket usage, there should be no need for an user
to specify that kind of options while starting a listener.

You should definitely look at the cowboy_http_protocol module for a great
example of fast request handling if you need to. Otherwise it's probably
safe to use `{active, once}` mode and handle everything as it comes.

Note that while you technically can run a protocol handler directly as a
gen_server or a gen_fsm, it's probably not a good idea, as the only call
you'll ever receive from Cowboy is the start_link/4 call. On the other
hand, feel free to write a very basic protocol handler which then forwards
requests to a gen_server or gen_fsm. By doing so however you must take
care to supervise their processes as Cowboy only knows about the protocol
handler itself.
