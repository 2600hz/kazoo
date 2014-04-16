CHANGELOG
=========

0.9.0
-----

 *  Update Ranch to 0.9.0

 *  SPDY is no longer experimental and is documented

    The SPDY development has been sponsored by the LeoFS project.

 *  Review, improve and document cowboy_static
   *  Much simplified configuration
   *  Etag generation is now enabled by default
   *  Web mimetypes are now detected by default
   *  Optionally a huge list of mimetypes can also be used
   *  It not try to magically find the priv directory anymore, use ERL_LIBS

 *  Remove the pretty printing of errors

    Cowboy will no longer print errors, it will instead let the process
    crash properly, so that links can work with Cowboy. Ranch will catch
    errors and print a one-liner containing all the error information
    instead.

 *  Trigger a human readable error when routes lack the starting slash

 *  Add websocket_compress metadata

 *  Fix parsing of hosts given as IPv6 addresses

 *  Fix the decoding of chunked bodies

 *  Fix handling of close, ping and pong Websocket replies

 *  Fix the x-webkit-deflate-frame Websocket extension

 *  Fix PUT behavior in cowboy_rest when creating a resource at the request URL

 *  Fix warnings with the reltool GUI

 *  Start moving code in a new dependency, cowlib

    The code moved there is mostly parsing code and utility functions.
    Most of it was in the undocumented cowboy_http module before.

 *  Optimize cookie date building and query string building/parsing

 *  Great number of improvements and additions to the user guide

 *  Convert all examples to releases using the erlang.mk+relx combo

    Some examples have also been fixed or slightly improved.
    The elixir example is now on a separate repository maintained
    independently. More examples in this language exist in that
    other repository.

0.8.6
-----

 *  Make sure Cowboy compiles on R16B01

 *  Update Ranch to 0.8.4

 *  Add experimental support for the x-webkit-deflate-frame Websocket extension

    This allows Cowboy to handle compressed Websocket frames,
    lowering the amount of data that needs to be sent over the
    socket.

    The extension will only be used if compression was enabled
    using the `compress` protocol option.

 *  Add experimental SPDY support

    SPDY is a new protocol implemented by most browsers. It is
    the basis for what will become HTTP/2.0.

    To use SPDY, you need to call `start_spdy` where you would
    have used `start_https` before.

    This protocol is still incomplete. It cannot accept request
    bodies yet, making most methods other than GET and HEAD
    not too useful at this point.

 *  Allow an empty method list in allowed_methods

 *  The charset parameter of content-types is now always lowercase

 *  Don't overwrite the stacktrace when a REST handler crashes

 *  Don't crash when the Cookie header is empty

 *  Don't crash on invalid Accept-Encoding header when replying

0.8.5
-----

 *  Add the Cowboy Function Reference

    Everything documented in the function reference is the API
    that will make it to Cowboy 1.0.

 *  Use erlang.mk

    The project is of course still compatible with rebar
    and can be used as a dependency just fine.

 *  Update Ranch to 0.8.3

 *  Remove cowboy_req:fragment/1

    No well-written client is sending the fragment with the URL.

 *  Add cowboy_req:set_resp_body_fun(chunked, Fun, Req)

 *  Improve various typespecs

 *  Change the return value of cowboy_req:version/1

    We now have 'HTTP/1.1' instead of {1, 1} and 'HTTP/1.0'
    instead of {1, 0}.

 *  Change the return value of REST accept callbacks

    The Path return value becomes {true, Path}.

 *  Change the return value of REST charsets_provided/2

    It was incorrectly expecting a list of tuples instead of
    a list of charsets.

 *  Move various types to the cowboy module
   *  cowboy_http:version() to cowboy:http_version()
   *  cowboy_http:headers() to cowboy:http_headers()
   *  cowboy_http:status() to cowboy:http_status()
   *  cowboy_protocol:onrequest_fun() to cowboy:onrequest_fun()
   *  cowboy_protocol:onresponse_fun() to cowboy:onresponse_fun()

 *  Add type cowboy_protocol:opts()

 *  Fix a REST bug with the OPTIONS method

 *  Fix a REST bug where iso-8859-1 would be incoditionally selected

0.8.4
-----

 *  Cookie names are now back to being case sensitive

    This should be more in line with what browsers do and what
    users would expect.

 *  REST is no longer experimental and is documented

 *  REST behavior fixed when used with the POST method

    Removes process_post, post_is_create, create_path, created_path
    callbacks. It is up to the resource accept callback to decide
    what to do when the POST method is used. Depending on the return
    value Cowboy will determine if the resource was created or not.

 *  Removes the put_path meta value in REST

 *  Fix an issue in REST with the PATCH method

    Content-types were not normalized as expected, preventing the
    use of the binary form for content-types.

 *  Add default operations for the OPTIONS method in REST

    The default will be to set the Allow header in the response
    based on the return value from allowed_methods.

 *  Add default content_types_provided "text/html" maps to to_html

    This allows non-HEAD/GET methods to work without defining
    the callback explicitly.

 *  Reject invalid content-types explicitly in REST

 *  Don't accept TRACE or CONNECT methods by default in REST

 *  Remove cowboy_req:peer_addr/1

    Because each server's proxy situation differs, it is better
    that this function is implemented by the application directly.

    The X-Forwarded-For header can now be parsed using
    cowboy_req:parse_header/2.

 *  Switch the arguments to cowboy_req:stream_body/2

    They were in the wrong order compared to the rest of the module.

 *  Add parser for the Range header

 *  Do not crash if connection times out while sending a file using sendfile

 *  Ensure we can fetch the body in the info/3 function of loop handlers

 *  Update Ranch to 0.8.1

 *  Reorganize and optimize the test suites

0.8.3
-----

 *  Remove init_stream/5, add stream_body/2

    It's better to allow configuring the streamed chunk size on
    a per chunk basis. Also easier to use.

 *  Update Ranch to 0.8.0

    Much faster. Also improved stability.

0.8.2
-----

 *  Add error_hook and ssl_hello_world example

 *  Greatly improve the performance of body reading operations

    The streamed chunk size is now configurable through the new
    function cowboy_req:init_stream/5.

 *  Add cowboy_req:body/2 and cowboy_req:body_qs/2

    These functions take an additional argument indicating the
    maximum size of the body. They will return {error, badlength}
    if the size is too large, or {error, chunked} if the body
    was sent using the chunked Transfer-Encoding and its size
    cannot be determined.

    The function body/1 is now an alias to body/2 with a maximum
    body size of 8MB. Likewise, the function body_qs/1 is an alias
    of body_qs/2 with a maximum body size of 16KB.

 *  Properly handle explicit identity Transfer-Encoding in body_length/1

 *  Small but noticeable performance improvement in the critical path

    We stopped using binary:match/2 in favor of custom functions.
    This makes Cowboy 0.5ms faster per request.

 *  Prevent loop handlers from awakening after sending a response

 *  Optimize cowboy_static initialization code

 *  Make path checks in cowboy_static cross-platform

 *  Allow '*' for REST content types parameters in content_types_provided

 *  Fix cowboy_router types

 *  Update Ranch to 0.6.2; adds support for two new SSL options

 *  Improve documentation

0.8.1
-----

 *  Add eventsource, web_server examples; improve rest_pastebin example

 *  Add cowboy:set_env/3 to more conveniently update the dispatch list

 *  Add cowboy_sub_protocol behaviour

 *  Fix cowboy_req:has_body/1 when Content-Length == 0

 *  Fix passing of state to websocket_terminate/3 on server close

 *  Fix compilation with +native

 *  Compile with more warnings enabled by default; fix warnings

 *  Set the socket in passive mode after the loop handler terminates

 *  Improve typespecs

0.8.0
-----

 *  This release drops R14 compatibility

    Behaviours now use the -callback attribute which is supported only
    since R15B.

 *  Add a user guide

 *  Add or update many examples

    Add basic_auth, compress_response, cookie, elixir_hello_world,
    markdown_middleware, rest_pastebin, rest_stream_response
    and websocket examples.

    Rename the static example to static_world for clarity.

 *  Add CONTRIBUTING.md file

 *  Use Ranch 0.6.1 for connection handling

    To start listeners you can now use cowboy:start_http/4 for HTTP,
    and cowboy:start_https/4 for HTTPS. The proper transport and
    protocol modules will be used.

 *  Add protection against slowloris vulnerability

    This protection is always enabled and has no impact on the performance
    of the system.

 *  Add a better routing syntax

 *  If a binding is used twice in routing, values must now be identical

 *  Add support for a configurable chain of middlewares

    Routing and handling are now two separate middlewares that can be
    replaced as needed.

 *  Fix application dependencies

    The crypto application must be started before Cowboy.

    The inets application is no longer needed. A few functions from
    that application were used by mistake in the REST code.

 *  Shorten the name of many modules
   *  cowboy_http_protocol becomes cowboy_protocol
   *  cowboy_http_req becomes cowboy_req
   *  cowboy_http_rest becomes cowboy_rest
   *  cowboy_http_static becomes cowboy_static
   *  cowboy_http_websocket becomes cowboy_websocket

 *  Introduce the cowboy_req:req() opaque type

    The include/http.hrl file was removed. Users are expected to use
    the cowboy_req API to access or modify the Req object.

    This required a lot of changes so cleanup and optimizations were
    performed where possible.

 *  Add many cowboy_req functions
   *  cowboy_req:delete_resp_header/2 deletes a previously set resp header
   *  cowboy_req:set_meta/3 sets metadata in the Req object
   *  cowboy_req:to_list/1 converts the Req object to a list of key/values
   *  cowboy_req:fragment/1 returns the request URL fragment
   *  cowboy_req:host_url/1 returns the request URL without the path or qs
   *  cowboy_req:url/1 returns the full request URL
   *  cowboy_req:set_resp_body_fun/2 for body streaming with no known length

 *  Improve the body streaming interface in cowboy_req

    The function now receives the Transport and Socket directly as arguments.

 *  Rename or drop many cowboy_req functions
   *  cowboy_req:raw_host/1 becomes cowboy_req:host/1, old function dropped
   *  cowboy_req:raw_path/1 becomes cowboy_req:path/1, old function dropped
   *  cowboy_req:raw_qs/1 becomes cowboy_req:qs/1
   *  Remove cowboy_req:body/2
   *  Remove cowboy_req:transport/1

 *  Change the signature of many cowboy_req functions
   *  parse_header now returns {ok, any(), Req} instead of {any(), Req}
   *  body_qs now returns {ok, QsVals, Req} instead of {QsVals, Req}
   *  multipart_data now returns {headers, Headers, Req} instead of
      {{headers, Headers}, Req} and {body, Body, Req} instead of
      {{body, Body}, Req}
   *  set_resp_* functions now return Req instead of {ok, Req}
   *  has_body now returns boolean()

 *  Rewrote cookie code

    In short we now do the same thing as PHP when setting cookies. This
    allows us to be fairly confident that our code will work on the vast
    majority of browsers.

 *  Fix consistency issues caused by erlang:decode_packet/3
   *  The method is now always a case sensitive binary string
   *  Note that standard method names are uppercase (e.g. <<"GET">>)
   *  Header names are now always lowercase binary string

 *  The max_line_length cowboy_protocol option was replaced by 3 new options:
   *  max_request_line_length, defaults to 4096 bytes
   *  max_header_name_length, defaults to 64 bytes
   *  max_header_value_length, defaults to 4096 bytes

 *  Add max_headers option, limiting the number of headers; defaults to 100

 *  The max_keepalive option now defaults to 100 instead of infinity

 *  Change terminate/2 to terminate/3 in the HTTP handler interface

 *  Enhance the loop handler API
   *  Connection close is now better detected
   *  Fix an internal message leak

 *  Enhance the Websocket API
   *  Change a websocket error from {error, protocol} to {error, badframe}
   *  Allow websocket handlers to reply more than one frame
   *  Check for errors when calling Transport:send/2 to avoid crashes
   *  Add close, {close, Payload}, {close, StatusCode, Payload},
      ping, pong frame types for replies
   *  Ensure websocket_terminate is always called
   *  Improve timeout handling
   *  Remove support for the old hixie76 protocol
   *  Add parsing support for Sec-Websocket-Protocol
   *  Check for UTF-8 correctness of text frames
   *  Perform unmasking and UTF-8 validation on the fly
   *  Reject clients that send unmasked frames
   *  Add cowboy_websocket:close_code/0 type

 *  Enhance the REST API
   *  Fix charset handling
   *  Add PATCH support
   *  Add created_path callback, used if create_path was not defined
   *  Make sure rest_terminate is always called

 *  Improved HTTP standard compatibility
   *  Revised status code used in responses
   *  Implement authorization header parsing
   *  Add opt-in automatic response body compression

 *  Improve lager compatibility

    We format errors in a special way so that lager can recognize Cowboy
    errors and put them on a single line.

 *  Remove the urldecode cowboy_protocol option

 *  Add cowboy_protocol:onrequest_fun/0 and :onresponse_fun/0 types

 *  Add the body data to onresponse_fun/0 callback

 *  Avoid a duplicate HTTP reply in cowboy_websocket:upgrade_error/1

 *  Fix use of the Vary header, was named Variances in the previous code

 *  Improve returned status code for HTTP and REST

 *  Fix charsets_provided return value

 *  Allow passing {M, F} for the mimetype function to cowboy_static

 *  Can now upgrade protocols with {upgrade, protocol, P, Req, Opts}

 *  Cowboy now only expects universal time, never local time

 *  Do not try skipping the body if the connection is to be closed

 *  Add cowboy_bstr:to_upper/1, cowboy_bstr:capitalize_token/1

 *  Many, many optimizations for the most critical code path

0.6.1
-----

 *  Add hello_world, rest_hello_world, chunked_hello_world,
    echo_get, echo_post and static examples.

 *  Add support for the "Expect: 100-continue" header.

 *  Keep the original 'Host' header value instead of modifying it.

 *  Fix use of parsed headers cache.

 *  REST: fix the matching of charsets.

 *  REST: allow <<"type/subtype">> format for content_types_accepted.

 *  Improve typespecs.

0.6.0
-----

 *  Add multipart support

 *  Add chunked transfer decoding support

    Done by reworking the body reading API. Now all the body
    reading goes through the cowboy_http_req:stream_body/1
    function. This function takes care of handling both the
    Transfer-Encoding and the Content-Encoding, returning
    properly decoded data ready for consumption.

 *  Add fragmented websocket messages support

    Properly tested by the addition of the Autobahn websocket
    test suite to our toolbox. All tests pass except a few
    related to UTF-8 handling, as Cowboy does no checks on that
    end at this point.

 *  Add 'onrequest' and 'onresponse' hooks

    The first can be used for all the special cases you may have
    that can't be dealt with otherwise. It's also pretty good for
    writing access logs or rewriting URLs.

    The second can be used for logging errors or replacing error
    pages, amongst others.

 *  Add cowboy:get_protocol_options/1 and cowboy:set_protocol_options/2

    These functions allow for retrieving a listener's protocol options,
    and for modifying them while the listener is running. This is
    most useful to upgrade the dispatch list. The upgrade applies
    to all the future connections.

 *  Add the sockname/1 function to TCP and SSL transports

 *  Improve SSL transport support

    Add support for specifying the ciphers. Add CA support. Make
    specifying the password optional.

 *  Add new HTTP status codes from RFC 6585

 *  Add a 'file' option to cowboy_http_static

    This allows for mapping /folder/ paths to a /folder/index.html file.

 *  Add the '*' catch all Content-Type for REST

 *  Add {halt, Req, State} as a possible return value for REST

 *  Add absolute URI support for requests

 *  Add cowboy_http:x_www_form_urlencoded/2

 *  Various REST bug fixes

 *  Do not send chunked replies for HTTP/1.0 connections

 *  Fix a DST bug in the cookies code

 *  Fix a bug with setting cookie values containing slashes

 *  Fix a small timer leak when using loop/websocket timeouts

 *  Make charset and media type parsing more relaxed

    This is to accomodate some widely used broken clients.

 *  Make error messages more readable

 *  Fix and improve type specifications

 *  Fix a bug preventing documentation from being generated

 *  Small improvements to the documentation

 *  Rework the HTTP test suite

    The suite now uses an integrated Cowboy HTTP client. The client
    is currently experimental and shouldn't be used.

 *  Add many many tests.

0.4.0
-----

 *  Set the cowboy_listener process priority to high

    As it is the central process used by all incoming requests
    we need to set its priority to high to avoid timeouts that
    would happen otherwise when reaching a huge number of
    concurrent requests.

 *  Add cowboy:child_spec/6 for embedding in other applications

 *  Add cowboy_http_rest, an experimental REST protocol support

    Based on the Webmachine diagram and documentation. It is a
    new implementation, not a port, therefore a few changes have
    been made. However all the callback names are the same and
    should behave similarly to Webmachine.

    There is currently no documentation other than the Webmachine
    resource documentation and the comments found in cowboy_http_rest,
    which itself should be fairly easy to read and understand.

 *  Add cowboy_http_static, an experimental static file handler

    Makes use of the aforementioned REST protocol support to
    deliver files with proper content type and cache headers.

    Note that this uses the new file:sendfile support when
    appropriate, which currently requires the VM to be started
    with the +A option defined, else errors may randomly appear.

 *  Add cowboy_bstr module for binary strings related functions

 *  Add cowboy_http module for HTTP parsing functions

    This module so far contains various functions for HTTP header
    parsing along with URL encoding and decoding.

 *  Remove quoted from the default dependencies

    This should make Cowboy much easier to compile and use by default.
    It is of course still possible to use quoted as your URL decoding
    library in Cowboy thanks to the newly added urldecode option.

 *  Fix supervisor spec for non dynamic modules to allow upgrades to complete

 *  Add cowboy:accept_ack/1 for a cleaner handling of the shoot message

    Before, when the listener accepted a connection, the newly created
    process was waiting for a message containing the atom 'shoot' before
    proceeding. This has been replaced by the cowboy:accept_ack/1 function.

    This function should be used where 'shoot' was received because the
    contents of the message have changed (and could change again in the
    distant future).

 *  Update binary parsing expressions to avoid hype crashes

    More specifically, /bits was replaced by /binary.

 *  Rename the type cowboy_dispatcher:path_tokens/0 to tokens/0

 *  Remove the cowboy_clock:date/0, time/0 and datetime/0 types

    The calendar module exports those same types properly since R14B04.

 *  Add cacertfile configuration option to cowboy_ssl_transport

 *  Add cowboy_protocol behaviour

 *  Remove -Wbehaviours dialyzer option unavailable in R15B

 *  Many tests and specs improvements

### cowboy_http_req

 *  Fix a crash when reading the request body

 *  Add parse_header/2 and parse_header/3

    The following headers can now be semantically parsed: Connection, Accept,
    Accept-Charset, Accept-Encoding, Accept-Language, Content-Length,
    Content-Type, If-Match, If-None-Match, If-Modified-Since,
    If-Unmodified-Since, Upgrade

 *  Add set_resp_header/3, set_resp_cookie/4 and set_resp_body/2

    These functions allow handlers to set response headers and body
    without having to reply directly.

 *  Add set_resp_body_fun/3

    This function allows handlers to stream the body of the response
    using the given fun. The size of the response must be known beforehand.

 *  Add transport/1 to obtain the transport and socket for the request

    This allows handlers to have low-level socket access in those cases
    where they do need it, like when streaming a response body with
    set_resp_body_fun/3.

 *  Add peer_addr/1

    This function tries to guess the real peer IP based on the HTTP
    headers received.

 *  Add meta/2 and meta/3 to save useful protocol information

    Currently used to save the Websocket protocol version currently used,
    and to save request information in the REST protocol handler.

 *  Add reply/2 and reply/3 aliases to reply/4

 *  Add upgrade_reply/3 for protocol upgrades

### cowboy_http_protocol

 *  Add the {urldecode, fun urldecode/2} option

    Added when quoted was removed from the default build. Can be used to
    tell Cowboy to use quoted or any other URL decoding routine.

 *  Add the max_keepalive option

 *  Add the max_line_length option

 *  Allow HTTP handlers to stop during init/3

    To do so they can return {shutdown, Req, State}.

 *  Add loops support in HTTP handlers for proper long-polling support

    A loop can be entered by returning either of {loop, Req, State},
    {loop, Req, State, hibernate}, {loop, Req, State, Timeout} or
    {loop, Req, State, Timeout, hibernate} from init/3.

    Loops are useful when we cannot reply immediately and instead
    are waiting for an Erlang message to be able to complete the request,
    as would typically be done for long-polling.

    Loop support in the protocol means that timeouts and hibernating
    are well tested and handled so you can use those options without
    worrying. It is recommended to set the timeout option.

    When a loop is started, handle/2 will never be called so it does
    not need to be defined. When the request process receives an Erlang
    message, it will call the info/3 function with the message as the
    first argument.

    Like in OTP, you do need to set timeout and hibernate again when
    returning from info/3 to enable them until the next call.

 *  Fix the sending of 500 errors when handlers crash

    Now we send an error response when no response has been sent,
    and do nothing more than close the connection if anything
    did get sent.

 *  Fix a crash when the server is sent HTTP responses

 *  Fix HTTP timeouts handling when the Request-Line wasn't received

 *  Fix the handling of the max number of empty lines between requests

 *  Fix the handling of HEAD requests

 *  Fix HTTP/1.0 Host header handling

 *  Reply status 400 if we receive an unexpected value or error for headers

 *  Properly close when the application sends "Connection: close" header

 *  Close HTTP connections on all errors

 *  Improve the error message for HTTP handlers

### cowboy_http_websocket

 *  Add websocket support for all versions up to RFC 6455

    Support isn't perfect yet according to the specifications, but
    is working against all currently known client implementations.

 *  Allow websocket_init/3 to return with the hibernate option set

 *  Add {shutdown, Req} return value to websocket_init/3 to fail an upgrade

 *  Fix websocket timeout handling

 *  Fix error messages: wrong callback name was reported on error

 *  Fix byte-by-byte websocket handling

 *  Fix an issue when using hixie-76 with certain proxies

 *  Fix a crash in the hixie-76 handshake

 *  Fix the handshake when SSL is used on port 443

 *  Fix a crash in the handshake when cowboy_http_req:compact/1 is used

 *  Fix handshake when a query string is present

 *  Fix a crash when the Upgrade header contains more than one token

0.2.0
-----

 *  Initial release.
