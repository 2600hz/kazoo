Reading the request body
========================

The Req object also allows you to read the request body.

Because the request body can be of any size, all body
reading operations will only work once, as Cowboy will
not cache the result of these operations.

Cowboy will not attempt to read the body until you do.
If handler execution ends without reading it, Cowboy
will simply skip it.

Cowboy provides different ways to read the request body.
You can read it directly, stream it, but also read and
parse in a single call for form urlencoded formats or
multipart. All of these except multipart are covered in
this chapter. Multipart is covered later on in the guide.

Check for request body
----------------------

You can check whether a body was sent with the request.

``` erlang
cowboy_req:has_body(Req).
```

It will return `true` if there is a request body, and
`false` otherwise.

Note that it is generally safe to assume that a body is
sent for `POST`, `PUT` and `PATCH` requests, without
having to explicitly check for it.

Request body length
-------------------

You can obtain the body length if it was sent with the
request.

``` erlang
{Length, Req2} = cowboy_req:body_length(Req).
```

The value returned will be `undefined` if the length
couldn't be figured out from the request headers. If
there's a body but no length is given, this means that
the chunked transfer-encoding was used. You can read
chunked bodies by using the stream functions.

Reading the body
----------------

You can read the whole body directly in one call.

``` erlang
{ok, Body, Req2} = cowboy_req:body(Req).
```

By default, Cowboy will attempt to read up to a
size of 8MB. You can override this limit as needed.

``` erlang
{ok, Body, Req2} = cowboy_req:body(Req, [{length, 100000000}]).
```

You can also disable it.

``` erlang
{ok, Body, Req2} = cowboy_req:body(Req, [{length, infinity}]).
```

It is recommended that you do not disable it for public
facing websites.

If the body is larger than the limit, then Cowboy will return
a `more` tuple instead, allowing you to stream it if you
would like to.

Streaming the body
------------------

You can stream the request body by chunks.

Cowboy returns a `more` tuple when there is more body to
be read, and an `ok` tuple for the last chunk. This allows
you to loop over all chunks.

``` erlang
body_to_console(Req) ->
    case cowboy_req:body(Req) of
        {ok, Data, Req2} ->
            io:format("~s", [Data]),
            Req2;
        {more, Data, Req2} ->
            io:format("~s", [Data]),
            body_to_console(Req2)
    end.
```

You can of course set the `length` option to configure the
size of chunks.

Rate of data transmission
-------------------------

You can control the rate of data transmission by setting
options when calling body functions. This applies not only
to the functions described in this chapter, but also to
the multipart functions.

The `read_length` option defines the maximum amount of data
to be received from the socket at once, in bytes.

The `read_timeout` option defines the time Cowboy waits
before that amount is received, in milliseconds.

Transfer and content decoding
-----------------------------

Cowboy will by default decode the chunked transfer-encoding
if any. It will not decode any content-encoding by default.

The first time you call a body function you can set the
`transfer_decode` and `content_decode` options. If the body
was already started being read these options are simply
ignored.

The following example shows how to set both options.

``` erlang
{ok, Req2} = cowboy_req:body(Req, [
    {transfer_decode, fun transfer_decode/2, TransferState},
    {content_decode, fun content_decode/1}
]).
```

Reading a form urlencoded body
------------------------------

You can directly obtain a list of key/value pairs if the
body was sent using the application/x-www-form-urlencoded
content-type.

``` erlang
{ok, KeyValues, Req2} = cowboy_req:body_qs(Req).
```

You can then retrieve an individual value from that list.

``` erlang
{_, Lang} = lists:keyfind(lang, 1, KeyValues).
```

You should not attempt to match on the list as the order
of the values is undefined.

By default Cowboy will reject bodies with a size above
64KB when using this function. You can override this limit
by setting the `length` option.

``` erlang
{ok, KeyValues, Req2} = cowboy_req:body_qs(Req,
    [{length, 2000000}]).
```
