%% Copyright (c) 2011-2013, Loïc Hoguin <essen@ninenines.eu>
%% Copyright (c) 2011, Anthony Ramine <nox@dev-extend.eu>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

%% @doc HTTP request manipulation API.
%%
%% The functions in this module try to follow this pattern for their
%% return types:
%% <dl>
%% <dt>access:</dt>
%%   <dd><em>{Value, Req}</em></dd>
%% <dt>action:</dt>
%%   <dd><em>{Result, Req} | {Result, Value, Req} | {error, atom()}</em></dd>
%% <dt>modification:</dt>
%%   <dd><em>Req</em></dd>
%% <dt>question (<em>has_*</em> or <em>is_*</em>):</dt>
%%   <dd><em>boolean()</em></dd>
%% </dl>
%%
%% Exceptions include <em>chunk/2</em> which always returns <em>'ok'</em>,
%% and <em>to_list/1</em> which returns a list of key/values.
%%
%% Also note that all body reading functions perform actions, as Cowboy
%% doesn't read the request body until they are called.
%%
%% Whenever <em>Req</em> is returned, it should always be kept in place of
%% the one given as argument in your function call, because it keeps
%% track of the request and response state. Doing so allows Cowboy to do
%% some lazy evaluation and cache results when possible.
-module(cowboy_req).

%% Request API.
-export([new/14]).
-export([method/1]).
-export([version/1]).
-export([peer/1]).
-export([host/1]).
-export([host_info/1]).
-export([port/1]).
-export([path/1]).
-export([path_info/1]).
-export([qs/1]).
-export([qs_val/2]).
-export([qs_val/3]).
-export([qs_vals/1]).
-export([host_url/1]).
-export([url/1]).
-export([binding/2]).
-export([binding/3]).
-export([bindings/1]).
-export([header/2]).
-export([header/3]).
-export([headers/1]).
-export([parse_header/2]).
-export([parse_header/3]).
-export([cookie/2]).
-export([cookie/3]).
-export([cookies/1]).
-export([meta/2]).
-export([meta/3]).
-export([set_meta/3]).

%% Request body API.
-export([has_body/1]).
-export([body_length/1]).
-export([init_stream/4]).
-export([stream_body/1]).
-export([stream_body/2]).
-export([skip_body/1]).
-export([body/1]).
-export([body/2]).
-export([body_qs/1]).
-export([body_qs/2]).
-export([multipart_data/1]).
-export([multipart_skip/1]).

%% Response API.
-export([set_resp_cookie/4]).
-export([set_resp_header/3]).
-export([set_resp_body/2]).
-export([set_resp_body_fun/2]).
-export([set_resp_body_fun/3]).
-export([has_resp_header/2]).
-export([has_resp_body/1]).
-export([delete_resp_header/2]).
-export([reply/2]).
-export([reply/3]).
-export([reply/4]).
-export([chunked_reply/2]).
-export([chunked_reply/3]).
-export([chunk/2]).
-export([upgrade_reply/3]).
-export([maybe_reply/2]).
-export([ensure_response/2]).

%% Private setter/getter API.
-export([append_buffer/2]).
-export([get/2]).
-export([set/2]).
-export([set_bindings/4]).

%% Misc API.
-export([compact/1]).
-export([lock/1]).
-export([to_list/1]).

-type cookie_opts() :: cow_cookie:cookie_opts().
-export_type([cookie_opts/0]).

-type content_decode_fun() :: fun((binary())
	-> {ok, binary()}
	| {error, atom()}).
-type transfer_decode_fun() :: fun((binary(), any())
	-> {ok, binary(), binary(), any()}
	| more | {more, non_neg_integer(), binary(), any()}
	| {done, non_neg_integer(), binary()}
	| {done, binary(), non_neg_integer(), binary()}
	| {error, atom()}).

-type resp_body_fun() :: fun((any(), module()) -> ok).
-type send_chunk_fun() :: fun((iodata()) -> ok | {error, atom()}).
-type resp_chunked_fun() :: fun((send_chunk_fun()) -> ok).

-record(http_req, {
	%% Transport.
	socket = undefined :: any(),
	transport = undefined :: undefined | module(),
	connection = keepalive :: keepalive | close,

	%% Request.
	pid = undefined :: pid(),
	method = <<"GET">> :: binary(),
	version = 'HTTP/1.1' :: cowboy:http_version(),
	peer = undefined :: undefined | {inet:ip_address(), inet:port_number()},
	host = undefined :: undefined | binary(),
	host_info = undefined :: undefined | cowboy_router:tokens(),
	port = undefined :: undefined | inet:port_number(),
	path = undefined :: binary(),
	path_info = undefined :: undefined | cowboy_router:tokens(),
	qs = undefined :: binary(),
	qs_vals = undefined :: undefined | list({binary(), binary() | true}),
	bindings = undefined :: undefined | cowboy_router:bindings(),
	headers = [] :: cowboy:http_headers(),
	p_headers = [] :: [any()], %% @todo Improve those specs.
	cookies = undefined :: undefined | [{binary(), binary()}],
	meta = [] :: [{atom(), any()}],

	%% Request body.
	body_state = waiting :: waiting | done | {stream, non_neg_integer(),
		transfer_decode_fun(), any(), content_decode_fun()},
	multipart = undefined :: undefined | {non_neg_integer(), fun()},
	buffer = <<>> :: binary(),

	%% Response.
	resp_compress = false :: boolean(),
	resp_state = waiting :: locked | waiting | waiting_stream
		| chunks | stream | done,
	resp_headers = [] :: cowboy:http_headers(),
	resp_body = <<>> :: iodata() | resp_body_fun()
		| {non_neg_integer(), resp_body_fun()}
		| {chunked, resp_chunked_fun()},

	%% Functions.
	onresponse = undefined :: undefined | already_called
		| cowboy:onresponse_fun()
}).

-opaque req() :: #http_req{}.
-export_type([req/0]).

%% Request API.

%% @doc Create a new HTTP Req object.
%%
%% This function takes care of setting the owner's pid to self().
%% @private
%%
%% Since we always need to parse the Connection header, we do it
%% in an optimized way and add the parsed value to p_headers' cache.
-spec new(any(), module(),
	undefined | {inet:ip_address(), inet:port_number()},
	binary(), binary(), binary(),
	cowboy:http_version(), cowboy:http_headers(), binary(),
	inet:port_number() | undefined, binary(), boolean(), boolean(),
	undefined | cowboy:onresponse_fun())
	-> req().
new(Socket, Transport, Peer, Method, Path, Query,
		Version, Headers, Host, Port, Buffer, CanKeepalive,
		Compress, OnResponse) ->
	Req = #http_req{socket=Socket, transport=Transport, pid=self(), peer=Peer,
		method=Method, path=Path, qs=Query, version=Version,
		headers=Headers, host=Host, port=Port, buffer=Buffer,
		resp_compress=Compress, onresponse=OnResponse},
	case CanKeepalive and (Version =:= 'HTTP/1.1') of
		false ->
			Req#http_req{connection=close};
		true ->
			case lists:keyfind(<<"connection">>, 1, Headers) of
				false ->
					Req; %% keepalive
				{_, ConnectionHeader} ->
					Tokens = parse_connection_before(ConnectionHeader, []),
					Connection = connection_to_atom(Tokens),
					Req#http_req{connection=Connection,
						p_headers=[{<<"connection">>, Tokens}]}
			end
	end.

%% @doc Return the HTTP method of the request.
-spec method(Req) -> {binary(), Req} when Req::req().
method(Req) ->
	{Req#http_req.method, Req}.

%% @doc Return the HTTP version used for the request.
-spec version(Req) -> {cowboy:http_version(), Req} when Req::req().
version(Req) ->
	{Req#http_req.version, Req}.

%% @doc Return the peer address and port number of the remote host.
-spec peer(Req)
	-> {{inet:ip_address(), inet:port_number()}, Req}
	when Req::req().
peer(Req) ->
	{Req#http_req.peer, Req}.

%% @doc Return the host binary string.
-spec host(Req) -> {binary(), Req} when Req::req().
host(Req) ->
	{Req#http_req.host, Req}.

%% @doc Return the extra host information obtained from partially matching
%% the hostname using <em>'...'</em>.
-spec host_info(Req)
	-> {cowboy_router:tokens() | undefined, Req} when Req::req().
host_info(Req) ->
	{Req#http_req.host_info, Req}.

%% @doc Return the port used for this request.
-spec port(Req) -> {inet:port_number(), Req} when Req::req().
port(Req) ->
	{Req#http_req.port, Req}.

%% @doc Return the path binary string.
-spec path(Req) -> {binary(), Req} when Req::req().
path(Req) ->
	{Req#http_req.path, Req}.

%% @doc Return the extra path information obtained from partially matching
%% the patch using <em>'...'</em>.
-spec path_info(Req)
	-> {cowboy_router:tokens() | undefined, Req} when Req::req().
path_info(Req) ->
	{Req#http_req.path_info, Req}.

%% @doc Return the raw query string directly taken from the request.
-spec qs(Req) -> {binary(), Req} when Req::req().
qs(Req) ->
	{Req#http_req.qs, Req}.

%% @equiv qs_val(Name, Req, undefined)
-spec qs_val(binary(), Req)
	-> {binary() | true | undefined, Req} when Req::req().
qs_val(Name, Req) when is_binary(Name) ->
	qs_val(Name, Req, undefined).

%% @doc Return the query string value for the given key, or a default if
%% missing.
-spec qs_val(binary(), Req, Default)
	-> {binary() | true | Default, Req} when Req::req(), Default::any().
qs_val(Name, Req=#http_req{qs=RawQs, qs_vals=undefined}, Default)
		when is_binary(Name) ->
	QsVals = cow_qs:parse_qs(RawQs),
	qs_val(Name, Req#http_req{qs_vals=QsVals}, Default);
qs_val(Name, Req, Default) ->
	case lists:keyfind(Name, 1, Req#http_req.qs_vals) of
		{Name, Value} -> {Value, Req};
		false -> {Default, Req}
	end.

%% @doc Return the full list of query string values.
-spec qs_vals(Req) -> {list({binary(), binary() | true}), Req} when Req::req().
qs_vals(Req=#http_req{qs=RawQs, qs_vals=undefined}) ->
	QsVals = cow_qs:parse_qs(RawQs),
	qs_vals(Req#http_req{qs_vals=QsVals});
qs_vals(Req=#http_req{qs_vals=QsVals}) ->
	{QsVals, Req}.

%% @doc Return the request URL as a binary without the path and query string.
%%
%% The URL includes the scheme, host and port only.
%% @see cowboy_req:url/1
-spec host_url(Req) -> {undefined | binary(), Req} when Req::req().
host_url(Req=#http_req{port=undefined}) ->
	{undefined, Req};
host_url(Req=#http_req{transport=Transport, host=Host, port=Port}) ->
	TransportName = Transport:name(),
	Secure = case TransportName of
		ssl -> <<"s">>;
		_ -> <<>>
	end,
	PortBin = case {TransportName, Port} of
		{ssl, 443} -> <<>>;
		{tcp, 80} -> <<>>;
		_ -> << ":", (list_to_binary(integer_to_list(Port)))/binary >>
	end,
	{<< "http", Secure/binary, "://", Host/binary, PortBin/binary >>, Req}.

%% @doc Return the full request URL as a binary.
%%
%% The URL includes the scheme, host, port, path and query string.
-spec url(Req) -> {undefined | binary(), Req} when Req::req().
url(Req=#http_req{}) ->
	{HostURL, Req2} = host_url(Req),
	url(HostURL, Req2).

url(undefined, Req=#http_req{}) ->
	{undefined, Req};
url(HostURL, Req=#http_req{path=Path, qs=QS}) ->
	QS2 = case QS of
		<<>> -> <<>>;
		_ -> << "?", QS/binary >>
	end,
	{<< HostURL/binary, Path/binary, QS2/binary >>, Req}.

%% @equiv binding(Name, Req, undefined)
-spec binding(atom(), Req) -> {binary() | undefined, Req} when Req::req().
binding(Name, Req) when is_atom(Name) ->
	binding(Name, Req, undefined).

%% @doc Return the binding value for the given key obtained when matching
%% the host and path against the dispatch list, or a default if missing.
-spec binding(atom(), Req, Default)
	-> {binary() | Default, Req} when Req::req(), Default::any().
binding(Name, Req, Default) when is_atom(Name) ->
	case lists:keyfind(Name, 1, Req#http_req.bindings) of
		{Name, Value} -> {Value, Req};
		false -> {Default, Req}
	end.

%% @doc Return the full list of binding values.
-spec bindings(Req) -> {list({atom(), binary()}), Req} when Req::req().
bindings(Req) ->
	{Req#http_req.bindings, Req}.

%% @equiv header(Name, Req, undefined)
-spec header(binary(), Req)
	-> {binary() | undefined, Req} when Req::req().
header(Name, Req) ->
	header(Name, Req, undefined).

%% @doc Return the header value for the given key, or a default if missing.
-spec header(binary(), Req, Default)
	-> {binary() | Default, Req} when Req::req(), Default::any().
header(Name, Req, Default) ->
	case lists:keyfind(Name, 1, Req#http_req.headers) of
		{Name, Value} -> {Value, Req};
		false -> {Default, Req}
	end.

%% @doc Return the full list of headers.
-spec headers(Req) -> {cowboy:http_headers(), Req} when Req::req().
headers(Req) ->
	{Req#http_req.headers, Req}.

%% @doc Semantically parse headers.
%%
%% When the value isn't found, a proper default value for the type
%% returned is used as a return value.
%% @see parse_header/3
-spec parse_header(binary(), Req)
	-> {ok, any(), Req} | {undefined, binary(), Req}
	| {error, badarg} when Req::req().
parse_header(Name, Req=#http_req{p_headers=PHeaders}) ->
	case lists:keyfind(Name, 1, PHeaders) of
		false -> parse_header(Name, Req, parse_header_default(Name));
		{Name, Value} -> {ok, Value, Req}
	end.

%% @doc Default values for semantic header parsing.
-spec parse_header_default(binary()) -> any().
parse_header_default(<<"transfer-encoding">>) -> [<<"identity">>];
parse_header_default(_Name) -> undefined.

%% @doc Semantically parse headers.
%%
%% When the header is unknown, the value is returned directly without parsing.
-spec parse_header(binary(), Req, any())
	-> {ok, any(), Req} | {undefined, binary(), Req}
	| {error, badarg} when Req::req().
parse_header(Name = <<"accept">>, Req, Default) ->
	parse_header(Name, Req, Default,
		fun (Value) ->
			cowboy_http:list(Value, fun cowboy_http:media_range/2)
		end);
parse_header(Name = <<"accept-charset">>, Req, Default) ->
	parse_header(Name, Req, Default,
		fun (Value) ->
			cowboy_http:nonempty_list(Value, fun cowboy_http:conneg/2)
		end);
parse_header(Name = <<"accept-encoding">>, Req, Default) ->
	parse_header(Name, Req, Default,
		fun (Value) ->
			cowboy_http:list(Value, fun cowboy_http:conneg/2)
		end);
parse_header(Name = <<"accept-language">>, Req, Default) ->
	parse_header(Name, Req, Default,
		fun (Value) ->
			cowboy_http:nonempty_list(Value, fun cowboy_http:language_range/2)
		end);
parse_header(Name = <<"authorization">>, Req, Default) ->
	parse_header(Name, Req, Default,
		fun (Value) ->
			cowboy_http:token_ci(Value, fun cowboy_http:authorization/2)
		end);
parse_header(Name = <<"content-length">>, Req, Default) ->
	parse_header(Name, Req, Default, fun cowboy_http:digits/1);
parse_header(Name = <<"content-type">>, Req, Default) ->
	parse_header(Name, Req, Default, fun cowboy_http:content_type/1);
parse_header(Name = <<"cookie">>, Req, Default) ->
	parse_header(Name, Req, Default, fun cow_cookie:parse_cookie/1);
parse_header(Name = <<"expect">>, Req, Default) ->
	parse_header(Name, Req, Default,
		fun (Value) ->
			cowboy_http:nonempty_list(Value, fun cowboy_http:expectation/2)
		end);
parse_header(Name, Req, Default)
		when Name =:= <<"if-match">>;
			Name =:= <<"if-none-match">> ->
	parse_header(Name, Req, Default, fun cowboy_http:entity_tag_match/1);
parse_header(Name, Req, Default)
		when Name =:= <<"if-modified-since">>;
			Name =:= <<"if-unmodified-since">> ->
	parse_header(Name, Req, Default, fun cowboy_http:http_date/1);
parse_header(Name = <<"range">>, Req, Default) ->
	parse_header(Name, Req, Default, fun cowboy_http:range/1);
parse_header(Name, Req, Default)
		when Name =:= <<"sec-websocket-protocol">>;
			Name =:= <<"x-forwarded-for">> ->
	parse_header(Name, Req, Default,
		fun (Value) ->
			cowboy_http:nonempty_list(Value, fun cowboy_http:token/2)
		end);
%% @todo Extension parameters.
parse_header(Name, Req, Default)
		when Name =:= <<"transfer-encoding">>;
			Name =:= <<"upgrade">> ->
	parse_header(Name, Req, Default,
		fun (Value) ->
			cowboy_http:nonempty_list(Value, fun cowboy_http:token_ci/2)
		end);
parse_header(Name = <<"sec-websocket-extensions">>, Req, Default) ->
	parse_header(Name, Req, Default, fun cowboy_http:parameterized_tokens/1);
parse_header(Name, Req, Default) ->
	{Value, Req2} = header(Name, Req, Default),
	{undefined, Value, Req2}.

parse_header(Name, Req=#http_req{p_headers=PHeaders}, Default, Fun) ->
	case header(Name, Req) of
		{undefined, Req2} ->
			{ok, Default, Req2#http_req{p_headers=[{Name, Default}|PHeaders]}};
		{Value, Req2} ->
			case Fun(Value) of
				{error, badarg} ->
					{error, badarg};
				P ->
					{ok, P, Req2#http_req{p_headers=[{Name, P}|PHeaders]}}
			end
	end.

%% @equiv cookie(Name, Req, undefined)
-spec cookie(binary(), Req)
	-> {binary() | undefined, Req} when Req::req().
cookie(Name, Req) when is_binary(Name) ->
	cookie(Name, Req, undefined).

%% @doc Return the cookie value for the given key, or a default if
%% missing.
-spec cookie(binary(), Req, Default)
	-> {binary() | Default, Req} when Req::req(), Default::any().
cookie(Name, Req=#http_req{cookies=undefined}, Default) when is_binary(Name) ->
	case parse_header(<<"cookie">>, Req) of
		{ok, undefined, Req2} ->
			{Default, Req2#http_req{cookies=[]}};
		{ok, Cookies, Req2} ->
			cookie(Name, Req2#http_req{cookies=Cookies}, Default)
	end;
cookie(Name, Req, Default) ->
	case lists:keyfind(Name, 1, Req#http_req.cookies) of
		{Name, Value} -> {Value, Req};
		false -> {Default, Req}
	end.

%% @doc Return the full list of cookie values.
-spec cookies(Req) -> {list({binary(), binary()}), Req} when Req::req().
cookies(Req=#http_req{cookies=undefined}) ->
	case parse_header(<<"cookie">>, Req) of
		{ok, undefined, Req2} ->
			{[], Req2#http_req{cookies=[]}};
		{ok, Cookies, Req2} ->
			cookies(Req2#http_req{cookies=Cookies});
		%% Flash player incorrectly sends an empty Cookie header.
		{error, badarg} ->
			{[], Req#http_req{cookies=[]}}
	end;
cookies(Req=#http_req{cookies=Cookies}) ->
	{Cookies, Req}.

%% @equiv meta(Name, Req, undefined)
-spec meta(atom(), Req) -> {any() | undefined, Req} when Req::req().
meta(Name, Req) ->
	meta(Name, Req, undefined).

%% @doc Return metadata information about the request.
%%
%% Metadata information varies from one protocol to another. Websockets
%% would define the protocol version here, while REST would use it to
%% indicate which media type, language and charset were retained.
-spec meta(atom(), Req, any()) -> {any(), Req} when Req::req().
meta(Name, Req, Default) ->
	case lists:keyfind(Name, 1, Req#http_req.meta) of
		{Name, Value} -> {Value, Req};
		false -> {Default, Req}
	end.

%% @doc Set metadata information.
%%
%% You can use this function to attach information about the request.
%%
%% If the value already exists it will be overwritten.
-spec set_meta(atom(), any(), Req) -> Req when Req::req().
set_meta(Name, Value, Req=#http_req{meta=Meta}) ->
	Req#http_req{meta=[{Name, Value}|lists:keydelete(Name, 1, Meta)]}.

%% Request Body API.

%% @doc Return whether the request message has a body.
-spec has_body(req()) -> boolean().
has_body(Req) ->
	case lists:keyfind(<<"content-length">>, 1, Req#http_req.headers) of
		{_, <<"0">>} ->
			false;
		{_, _} ->
			true;
		_ ->
			lists:keymember(<<"transfer-encoding">>, 1, Req#http_req.headers)
	end.

%% @doc Return the request message body length, if known.
%%
%% The length may not be known if Transfer-Encoding is not identity,
%% and the body hasn't been read at the time of the call.
-spec body_length(Req) -> {undefined | non_neg_integer(), Req} when Req::req().
body_length(Req) ->
	case parse_header(<<"transfer-encoding">>, Req) of
		{ok, [<<"identity">>], Req2} ->
			{ok, Length, Req3} = parse_header(<<"content-length">>, Req2, 0),
			{Length, Req3};
		{ok, _, Req2} ->
			{undefined, Req2}
	end.

%% @doc Initialize body streaming and set custom decoding functions.
%%
%% Calling this function is optional. It should only be used if you
%% need to override the default behavior of Cowboy. Otherwise you
%% should call stream_body/{1,2} directly.
%%
%% Two decodings happen. First a decoding function is applied to the
%% transferred data, and then another is applied to the actual content.
%%
%% Transfer encoding is generally used for chunked bodies. The decoding
%% function uses a state to keep track of how much it has read, which is
%% also initialized through this function.
%%
%% Content encoding is generally used for compression.
%%
%% Standard encodings can be found in cowboy_http.
-spec init_stream(transfer_decode_fun(), any(), content_decode_fun(), Req)
	-> {ok, Req} when Req::req().
init_stream(TransferDecode, TransferState, ContentDecode, Req) ->
	{ok, Req#http_req{body_state=
		{stream, 0, TransferDecode, TransferState, ContentDecode}}}.

%% @equiv stream_body(1000000, Req)
-spec stream_body(Req) -> {ok, binary(), Req}
	| {done, Req} | {error, atom()} when Req::req().
stream_body(Req) ->
	stream_body(1000000, Req).

%% @doc Stream the request's body.
%%
%% This is the most low level function to read the request body.
%%
%% In most cases, if they weren't defined before using init_stream/4,
%% this function will guess which transfer and content encodings were
%% used for building the request body, and configure the decoding
%% functions that will be used when streaming.
%%
%% It then starts streaming the body, returning {ok, Data, Req}
%% for each streamed part, and {done, Req} when it's finished streaming.
%%
%% You can limit the size of the chunks being returned by using the
%% first argument which is the size in bytes. It defaults to 1000000 bytes.
-spec stream_body(non_neg_integer(), Req) -> {ok, binary(), Req}
	| {done, Req} | {error, atom()} when Req::req().
stream_body(MaxLength, Req=#http_req{body_state=waiting, version=Version,
		transport=Transport, socket=Socket}) ->
	{ok, ExpectHeader, Req1} = parse_header(<<"expect">>, Req),
	case ExpectHeader of
		[<<"100-continue">>] ->
			HTTPVer = atom_to_binary(Version, latin1),
			Transport:send(Socket,
				<< HTTPVer/binary, " ", (status(100))/binary, "\r\n\r\n" >>);
		undefined ->
			ok
	end,
	case parse_header(<<"transfer-encoding">>, Req1) of
		{ok, [<<"chunked">>], Req2} ->
			stream_body(MaxLength, Req2#http_req{body_state=
				{stream, 0,
					fun cowboy_http:te_chunked/2, {0, 0},
					fun cowboy_http:ce_identity/1}});
		{ok, [<<"identity">>], Req2} ->
			{Length, Req3} = body_length(Req2),
			case Length of
				0 ->
					{done, Req3#http_req{body_state=done}};
				Length ->
					stream_body(MaxLength, Req3#http_req{body_state=
						{stream, Length,
							fun cowboy_http:te_identity/2, {0, Length},
							fun cowboy_http:ce_identity/1}})
			end
	end;
stream_body(_, Req=#http_req{body_state=done}) ->
	{done, Req};
stream_body(_, Req=#http_req{buffer=Buffer})
		when Buffer =/= <<>> ->
	transfer_decode(Buffer, Req#http_req{buffer= <<>>});
stream_body(MaxLength, Req) ->
	stream_body_recv(MaxLength, Req).

-spec stream_body_recv(non_neg_integer(), Req)
	-> {ok, binary(), Req} | {error, atom()} when Req::req().
stream_body_recv(MaxLength, Req=#http_req{
		transport=Transport, socket=Socket, buffer=Buffer,
		body_state={stream, Length, _, _, _}}) ->
	%% @todo Allow configuring the timeout.
	case Transport:recv(Socket, min(Length, MaxLength), 5000) of
		{ok, Data} -> transfer_decode(<< Buffer/binary, Data/binary >>,
			Req#http_req{buffer= <<>>});
		{error, Reason} -> {error, Reason}
	end.

-spec transfer_decode(binary(), Req)
	-> {ok, binary(), Req} | {error, atom()} when Req::req().
transfer_decode(Data, Req=#http_req{body_state={stream, _,
		TransferDecode, TransferState, ContentDecode}}) ->
	case TransferDecode(Data, TransferState) of
		{ok, Data2, Rest, TransferState2} ->
			content_decode(ContentDecode, Data2,
				Req#http_req{buffer=Rest, body_state={stream, 0,
				TransferDecode, TransferState2, ContentDecode}});
		%% @todo {header(s) for chunked
		more ->
			stream_body_recv(0, Req#http_req{buffer=Data, body_state={stream,
				0, TransferDecode, TransferState, ContentDecode}});
		{more, Length, Data2, TransferState2} ->
			content_decode(ContentDecode, Data2,
				Req#http_req{body_state={stream, Length,
				TransferDecode, TransferState2, ContentDecode}});
		{done, Length, Rest} ->
			Req2 = transfer_decode_done(Length, Rest, Req),
			{done, Req2};
		{done, Data2, Length, Rest} ->
			Req2 = transfer_decode_done(Length, Rest, Req),
			content_decode(ContentDecode, Data2, Req2);
		{error, Reason} ->
			{error, Reason}
	end.

-spec transfer_decode_done(non_neg_integer(), binary(), Req)
	-> Req when Req::req().
transfer_decode_done(Length, Rest, Req=#http_req{
		headers=Headers, p_headers=PHeaders}) ->
	Headers2 = lists:keystore(<<"content-length">>, 1, Headers,
		{<<"content-length">>, list_to_binary(integer_to_list(Length))}),
	%% At this point we just assume TEs were all decoded.
	Headers3 = lists:keydelete(<<"transfer-encoding">>, 1, Headers2),
	PHeaders2 = lists:keystore(<<"content-length">>, 1, PHeaders,
		{<<"content-length">>, Length}),
	PHeaders3 = lists:keydelete(<<"transfer-encoding">>, 1, PHeaders2),
	Req#http_req{buffer=Rest, body_state=done,
		headers=Headers3, p_headers=PHeaders3}.

%% @todo Probably needs a Rest.
-spec content_decode(content_decode_fun(), binary(), Req)
	-> {ok, binary(), Req} | {error, atom()} when Req::req().
content_decode(ContentDecode, Data, Req) ->
	case ContentDecode(Data) of
		{ok, Data2} -> {ok, Data2, Req};
		{error, Reason} -> {error, Reason}
	end.

%% @equiv body(8000000, Req)
-spec body(Req) -> {ok, binary(), Req} | {error, atom()} when Req::req().
body(Req) ->
	body(8000000, Req).

%% @doc Return the body sent with the request.
-spec body(non_neg_integer() | infinity, Req)
	-> {ok, binary(), Req} | {error, atom()} when Req::req().
body(MaxBodyLength, Req) ->
	case parse_header(<<"transfer-encoding">>, Req) of
		{ok, [<<"identity">>], Req2} ->
			{ok, Length, Req3} = parse_header(<<"content-length">>, Req2, 0),
			if 	Length > MaxBodyLength ->
					{error, badlength};
				true ->
					read_body(Req3, <<>>)
			end;
		{ok, _, _} ->
			{error, chunked}
	end.

-spec read_body(Req, binary())
	-> {ok, binary(), Req} | {error, atom()} when Req::req().
read_body(Req, Acc) ->
	case stream_body(Req) of
		{ok, Data, Req2} ->
			read_body(Req2, << Acc/binary, Data/binary >>);
		{done, Req2} ->
			{ok, Acc, Req2};
		{error, Reason} ->
			{error, Reason}
	end.

-spec skip_body(Req) -> {ok, Req} | {error, atom()} when Req::req().
skip_body(Req) ->
	case stream_body(Req) of
		{ok, _, Req2} -> skip_body(Req2);
		{done, Req2} -> {ok, Req2};
		{error, Reason} -> {error, Reason}
	end.

%% @equiv body_qs(16000, Req)
-spec body_qs(Req)
	-> {ok, [{binary(), binary() | true}], Req} | {error, atom()}
	when Req::req().
body_qs(Req) ->
	body_qs(16000, Req).

%% @doc Return the body sent with the request, parsed as an
%% application/x-www-form-urlencoded string.
%% Essentially a POST query string.
-spec body_qs(non_neg_integer() | infinity, Req)
	-> {ok, [{binary(), binary() | true}], Req} | {error, atom()}
	when Req::req().
body_qs(MaxBodyLength, Req) ->
	case body(MaxBodyLength, Req) of
		{ok, Body, Req2} ->
			{ok, cow_qs:parse_qs(Body), Req2};
		{error, Reason} ->
			{error, Reason}
	end.

%% Multipart Request API.

%% @doc Return data from the multipart parser.
%%
%% Use this function for multipart streaming. For each part in the request,
%% this function returns <em>{headers, Headers, Req}</em> followed by a sequence of
%% <em>{body, Data, Req}</em> tuples and finally <em>{end_of_part, Req}</em>. When there
%% is no part to parse anymore, <em>{eof, Req}</em> is returned.
-spec multipart_data(Req)
	-> {headers, cowboy:http_headers(), Req} | {body, binary(), Req}
		| {end_of_part | eof, Req} when Req::req().
multipart_data(Req=#http_req{body_state=waiting}) ->
	{ok, {<<"multipart">>, _SubType, Params}, Req2} =
		parse_header(<<"content-type">>, Req),
	{_, Boundary} = lists:keyfind(<<"boundary">>, 1, Params),
	{ok, Length, Req3} = parse_header(<<"content-length">>, Req2),
	multipart_data(Req3, Length, {more, cowboy_multipart:parser(Boundary)});
multipart_data(Req=#http_req{multipart={Length, Cont}}) ->
	multipart_data(Req, Length, Cont());
multipart_data(Req=#http_req{body_state=done}) ->
	{eof, Req}.

multipart_data(Req, Length, {headers, Headers, Cont}) ->
	{headers, Headers, Req#http_req{multipart={Length, Cont}}};
multipart_data(Req, Length, {body, Data, Cont}) ->
	{body, Data, Req#http_req{multipart={Length, Cont}}};
multipart_data(Req, Length, {end_of_part, Cont}) ->
	{end_of_part, Req#http_req{multipart={Length, Cont}}};
multipart_data(Req, 0, eof) ->
	{eof, Req#http_req{body_state=done, multipart=undefined}};
multipart_data(Req=#http_req{socket=Socket, transport=Transport},
		Length, eof) ->
	%% We just want to skip so no need to stream data here.
	{ok, _Data} = Transport:recv(Socket, Length, 5000),
	{eof, Req#http_req{body_state=done, multipart=undefined}};
multipart_data(Req, Length, {more, Parser}) when Length > 0 ->
	case stream_body(Req) of
		{ok, << Data:Length/binary, Buffer/binary >>, Req2} ->
			multipart_data(Req2#http_req{buffer=Buffer}, 0, Parser(Data));
		{ok, Data, Req2} ->
			multipart_data(Req2, Length - byte_size(Data), Parser(Data))
	end.

%% @doc Skip a part returned by the multipart parser.
%%
%% This function repeatedly calls <em>multipart_data/1</em> until
%% <em>{end_of_part, Req}</em> or <em>{eof, Req}</em> is parsed.
-spec multipart_skip(Req) -> {ok, Req} when Req::req().
multipart_skip(Req) ->
	case multipart_data(Req) of
		{end_of_part, Req2} -> {ok, Req2};
		{eof, Req2} -> {ok, Req2};
		{_, _, Req2} -> multipart_skip(Req2)
	end.

%% Response API.

%% @doc Add a cookie header to the response.
%%
%% The cookie name cannot contain any of the following characters:
%%   =,;\s\t\r\n\013\014
%%
%% The cookie value cannot contain any of the following characters:
%%   ,; \t\r\n\013\014
-spec set_resp_cookie(iodata(), iodata(), cookie_opts(), Req)
	-> Req when Req::req().
set_resp_cookie(Name, Value, Opts, Req) ->
	Cookie = cow_cookie:setcookie(Name, Value, Opts),
	set_resp_header(<<"set-cookie">>, Cookie, Req).

%% @doc Add a header to the response.
-spec set_resp_header(binary(), iodata(), Req)
	-> Req when Req::req().
set_resp_header(Name, Value, Req=#http_req{resp_headers=RespHeaders}) ->
	Req#http_req{resp_headers=[{Name, Value}|RespHeaders]}.

%% @doc Add a body to the response.
%%
%% The body set here is ignored if the response is later sent using
%% anything other than reply/2 or reply/3. The response body is expected
%% to be a binary or an iolist.
-spec set_resp_body(iodata(), Req) -> Req when Req::req().
set_resp_body(Body, Req) ->
	Req#http_req{resp_body=Body}.

%% @doc Add a body stream function to the response.
%%
%% The body set here is ignored if the response is later sent using
%% anything other than reply/2 or reply/3.
%%
%% Setting a response stream function without a length means that the
%% body will be sent until the connection is closed. Cowboy will make
%% sure that the connection is closed with no extra step required.
%%
%% To inform the client that a body has been sent with this request,
%% Cowboy will add a "Transfer-Encoding: identity" header to the
%% response.
-spec set_resp_body_fun(resp_body_fun(), Req) -> Req when Req::req().
set_resp_body_fun(StreamFun, Req) when is_function(StreamFun) ->
	Req#http_req{resp_body=StreamFun}.

%% @doc Add a body function to the response.
%%
%% The body set here is ignored if the response is later sent using
%% anything other than reply/2 or reply/3.
%%
%% Cowboy will call the given response stream function after sending the
%% headers. This function must send the specified number of bytes to the
%% socket it will receive as argument.
%%
%% If the body function crashes while writing the response body or writes
%% fewer bytes than declared the behaviour is undefined.
-spec set_resp_body_fun(non_neg_integer(), resp_body_fun(), Req)
	-> Req when Req::req();
	(chunked, resp_chunked_fun(), Req)
	-> Req when Req::req().
set_resp_body_fun(StreamLen, StreamFun, Req)
		when is_integer(StreamLen), is_function(StreamFun) ->
	Req#http_req{resp_body={StreamLen, StreamFun}};
set_resp_body_fun(chunked, StreamFun, Req)
		when is_function(StreamFun) ->
	Req#http_req{resp_body={chunked, StreamFun}}.

%% @doc Return whether the given header has been set for the response.
-spec has_resp_header(binary(), req()) -> boolean().
has_resp_header(Name, #http_req{resp_headers=RespHeaders}) ->
	lists:keymember(Name, 1, RespHeaders).

%% @doc Return whether a body has been set for the response.
-spec has_resp_body(req()) -> boolean().
has_resp_body(#http_req{resp_body=RespBody}) when is_function(RespBody) ->
	true;
has_resp_body(#http_req{resp_body={chunked, _}}) ->
	true;
has_resp_body(#http_req{resp_body={Length, _}}) ->
	Length > 0;
has_resp_body(#http_req{resp_body=RespBody}) ->
	iolist_size(RespBody) > 0.

%% @doc Remove a header previously set for the response.
-spec delete_resp_header(binary(), Req)
	-> Req when Req::req().
delete_resp_header(Name, Req=#http_req{resp_headers=RespHeaders}) ->
	RespHeaders2 = lists:keydelete(Name, 1, RespHeaders),
	Req#http_req{resp_headers=RespHeaders2}.

%% @equiv reply(Status, [], [], Req)
-spec reply(cowboy:http_status(), Req) -> {ok, Req} when Req::req().
reply(Status, Req=#http_req{resp_body=Body}) ->
	reply(Status, [], Body, Req).

%% @equiv reply(Status, Headers, [], Req)
-spec reply(cowboy:http_status(), cowboy:http_headers(), Req)
	-> {ok, Req} when Req::req().
reply(Status, Headers, Req=#http_req{resp_body=Body}) ->
	reply(Status, Headers, Body, Req).

%% @doc Send a reply to the client.
-spec reply(cowboy:http_status(), cowboy:http_headers(),
	iodata() | {non_neg_integer() | resp_body_fun()}, Req)
	-> {ok, Req} when Req::req().
reply(Status, Headers, Body, Req=#http_req{
		socket=Socket, transport=Transport,
		version=Version, connection=Connection,
		method=Method, resp_compress=Compress,
		resp_state=RespState, resp_headers=RespHeaders})
		when RespState =:= waiting; RespState =:= waiting_stream ->
	HTTP11Headers = if
		Transport =/= cowboy_spdy, Version =:= 'HTTP/1.1' ->
			[{<<"connection">>, atom_to_connection(Connection)}];
		true ->
			[]
	end,
	Req3 = case Body of
		BodyFun when is_function(BodyFun) ->
			%% We stream the response body until we close the connection.
			RespConn = close,
			{RespType, Req2} = if
				Transport =:= cowboy_spdy ->
					response(Status, Headers, RespHeaders, [
						{<<"date">>, cowboy_clock:rfc1123()},
						{<<"server">>, <<"Cowboy">>}
					], stream, Req);
				true ->
					response(Status, Headers, RespHeaders, [
						{<<"connection">>, <<"close">>},
						{<<"date">>, cowboy_clock:rfc1123()},
						{<<"server">>, <<"Cowboy">>},
						{<<"transfer-encoding">>, <<"identity">>}
					], <<>>, Req)
			end,
			if	RespType =/= hook, Method =/= <<"HEAD">> ->
					BodyFun(Socket, Transport);
				true -> ok
			end,
			Req2#http_req{connection=RespConn};
		{chunked, BodyFun} ->
			%% We stream the response body in chunks.
			{RespType, Req2} = chunked_response(Status, Headers, Req),
			if	RespType =/= hook, Method =/= <<"HEAD">> ->
					ChunkFun = fun(IoData) -> chunk(IoData, Req2) end,
					BodyFun(ChunkFun),
					%% Send the last chunk if chunked encoding was used.
					if
						Version =:= 'HTTP/1.0'; RespState =:= waiting_stream ->
							Req2;
						true ->
							last_chunk(Req2)
					end;
				true -> Req2
			end;
		{ContentLength, BodyFun} ->
			%% We stream the response body for ContentLength bytes.
			RespConn = response_connection(Headers, Connection),
			{RespType, Req2} = response(Status, Headers, RespHeaders, [
					{<<"content-length">>, integer_to_list(ContentLength)},
					{<<"date">>, cowboy_clock:rfc1123()},
					{<<"server">>, <<"Cowboy">>}
				|HTTP11Headers], stream, Req),
			if	RespType =/= hook, Method =/= <<"HEAD">> ->
					BodyFun(Socket, Transport);
				true -> ok
			end,
			Req2#http_req{connection=RespConn};
		_ when Compress ->
			RespConn = response_connection(Headers, Connection),
			Req2 = reply_may_compress(Status, Headers, Body, Req,
				RespHeaders, HTTP11Headers, Method),
			Req2#http_req{connection=RespConn};
		_ ->
			RespConn = response_connection(Headers, Connection),
			Req2 = reply_no_compress(Status, Headers, Body, Req,
				RespHeaders, HTTP11Headers, Method, iolist_size(Body)),
			Req2#http_req{connection=RespConn}
	end,
	{ok, Req3#http_req{resp_state=done, resp_headers=[], resp_body= <<>>}}.

reply_may_compress(Status, Headers, Body, Req,
		RespHeaders, HTTP11Headers, Method) ->
	BodySize = iolist_size(Body),
	case parse_header(<<"accept-encoding">>, Req) of
		{ok, Encodings, Req2} ->
			CanGzip = (BodySize > 300)
				andalso (false =:= lists:keyfind(<<"content-encoding">>,
					1, Headers))
				andalso (false =:= lists:keyfind(<<"content-encoding">>,
					1, RespHeaders))
				andalso (false =:= lists:keyfind(<<"transfer-encoding">>,
					1, Headers))
				andalso (false =:= lists:keyfind(<<"transfer-encoding">>,
					1, RespHeaders))
				andalso (Encodings =/= undefined)
				andalso (false =/= lists:keyfind(<<"gzip">>, 1, Encodings)),
			case CanGzip of
				true ->
					GzBody = zlib:gzip(Body),
					{_, Req3} = response(Status, Headers, RespHeaders, [
							{<<"content-length">>, integer_to_list(byte_size(GzBody))},
							{<<"content-encoding">>, <<"gzip">>},
							{<<"date">>, cowboy_clock:rfc1123()},
							{<<"server">>, <<"Cowboy">>}
						|HTTP11Headers],
						case Method of <<"HEAD">> -> <<>>; _ -> GzBody end,
						Req2),
					Req3;
				false ->
					reply_no_compress(Status, Headers, Body, Req,
						RespHeaders, HTTP11Headers, Method, BodySize)
			end;
		{error, badarg} ->
			reply_no_compress(Status, Headers, Body, Req,
				RespHeaders, HTTP11Headers, Method, BodySize)
	end.

reply_no_compress(Status, Headers, Body, Req,
		RespHeaders, HTTP11Headers, Method, BodySize) ->
	{_, Req2} = response(Status, Headers, RespHeaders, [
			{<<"content-length">>, integer_to_list(BodySize)},
			{<<"date">>, cowboy_clock:rfc1123()},
			{<<"server">>, <<"Cowboy">>}
		|HTTP11Headers],
		case Method of <<"HEAD">> -> <<>>; _ -> Body end,
		Req),
	Req2.

%% @equiv chunked_reply(Status, [], Req)
-spec chunked_reply(cowboy:http_status(), Req) -> {ok, Req} when Req::req().
chunked_reply(Status, Req) ->
	chunked_reply(Status, [], Req).

%% @doc Initiate the sending of a chunked reply to the client.
%% @see cowboy_req:chunk/2
-spec chunked_reply(cowboy:http_status(), cowboy:http_headers(), Req)
	-> {ok, Req} when Req::req().
chunked_reply(Status, Headers, Req) ->
	{_, Req2} = chunked_response(Status, Headers, Req),
	{ok, Req2}.

%% @doc Send a chunk of data.
%%
%% A chunked reply must have been initiated before calling this function.
-spec chunk(iodata(), req()) -> ok | {error, atom()}.
chunk(_Data, #http_req{method= <<"HEAD">>}) ->
	ok;
chunk(Data, #http_req{socket=Socket, transport=cowboy_spdy,
		resp_state=chunks}) ->
	cowboy_spdy:stream_data(Socket, Data);
chunk(Data, #http_req{socket=Socket, transport=Transport,
		resp_state=stream}) ->
	Transport:send(Socket, Data);
chunk(Data, #http_req{socket=Socket, transport=Transport,
		resp_state=chunks}) ->
	Transport:send(Socket, [integer_to_list(iolist_size(Data), 16),
		<<"\r\n">>, Data, <<"\r\n">>]).

%% @doc Finish the chunked reply.
%% @todo If ever made public, need to send nothing if HEAD.
-spec last_chunk(Req) -> Req when Req::req().
last_chunk(Req=#http_req{socket=Socket, transport=cowboy_spdy}) ->
	_ = cowboy_spdy:stream_close(Socket),
	Req#http_req{resp_state=done};
last_chunk(Req=#http_req{socket=Socket, transport=Transport}) ->
	_ = Transport:send(Socket, <<"0\r\n\r\n">>),
	Req#http_req{resp_state=done}.

%% @doc Send an upgrade reply.
%% @private
-spec upgrade_reply(cowboy:http_status(), cowboy:http_headers(), Req)
	-> {ok, Req} when Req::req().
upgrade_reply(Status, Headers, Req=#http_req{transport=Transport,
		resp_state=waiting, resp_headers=RespHeaders})
		when Transport =/= cowboy_spdy ->
	{_, Req2} = response(Status, Headers, RespHeaders, [
		{<<"connection">>, <<"Upgrade">>}
	], <<>>, Req),
	{ok, Req2#http_req{resp_state=done, resp_headers=[], resp_body= <<>>}}.

%% @doc Send a reply if one hasn't been sent already.
%%
%% Meant to be used internally for sending errors after crashes.
%% @private
-spec maybe_reply(cowboy:http_status(), req()) -> ok.
maybe_reply(Status, Req) ->
	receive
		{cowboy_req, resp_sent} -> ok
	after 0 ->
		_ = cowboy_req:reply(Status, Req),
		ok
	end.

%% @doc Ensure the response has been sent fully.
%% @private
-spec ensure_response(req(), cowboy:http_status()) -> ok.
%% The response has already been fully sent to the client.
ensure_response(#http_req{resp_state=done}, _) ->
	ok;
%% No response has been sent but everything apparently went fine.
%% Reply with the status code found in the second argument.
ensure_response(Req=#http_req{resp_state=RespState}, Status)
		when RespState =:= waiting; RespState =:= waiting_stream ->
	_ = reply(Status, [], [], Req),
	ok;
%% Terminate the chunked body for HTTP/1.1 only.
ensure_response(#http_req{method= <<"HEAD">>}, _) ->
	ok;
ensure_response(Req=#http_req{resp_state=chunks}, _) ->
	_ = last_chunk(Req),
	ok;
ensure_response(#http_req{}, _) ->
	ok.

%% Private setter/getter API.

%% @private
-spec append_buffer(binary(), Req) -> Req when Req::req().
append_buffer(Suffix, Req=#http_req{buffer=Buffer}) ->
	Req#http_req{buffer= << Buffer/binary, Suffix/binary >>}.

%% @private
-spec get(atom(), req()) -> any(); ([atom()], req()) -> any().
get(List, Req) when is_list(List) ->
	[g(Atom, Req) || Atom <- List];
get(Atom, Req) when is_atom(Atom) ->
	g(Atom, Req).

g(bindings, #http_req{bindings=Ret}) -> Ret;
g(body_state, #http_req{body_state=Ret}) -> Ret;
g(buffer, #http_req{buffer=Ret}) -> Ret;
g(connection, #http_req{connection=Ret}) -> Ret;
g(cookies, #http_req{cookies=Ret}) -> Ret;
g(headers, #http_req{headers=Ret}) -> Ret;
g(host, #http_req{host=Ret}) -> Ret;
g(host_info, #http_req{host_info=Ret}) -> Ret;
g(meta, #http_req{meta=Ret}) -> Ret;
g(method, #http_req{method=Ret}) -> Ret;
g(multipart, #http_req{multipart=Ret}) -> Ret;
g(onresponse, #http_req{onresponse=Ret}) -> Ret;
g(p_headers, #http_req{p_headers=Ret}) -> Ret;
g(path, #http_req{path=Ret}) -> Ret;
g(path_info, #http_req{path_info=Ret}) -> Ret;
g(peer, #http_req{peer=Ret}) -> Ret;
g(pid, #http_req{pid=Ret}) -> Ret;
g(port, #http_req{port=Ret}) -> Ret;
g(qs, #http_req{qs=Ret}) -> Ret;
g(qs_vals, #http_req{qs_vals=Ret}) -> Ret;
g(resp_body, #http_req{resp_body=Ret}) -> Ret;
g(resp_compress, #http_req{resp_compress=Ret}) -> Ret;
g(resp_headers, #http_req{resp_headers=Ret}) -> Ret;
g(resp_state, #http_req{resp_state=Ret}) -> Ret;
g(socket, #http_req{socket=Ret}) -> Ret;
g(transport, #http_req{transport=Ret}) -> Ret;
g(version, #http_req{version=Ret}) -> Ret.

%% @private
-spec set([{atom(), any()}], Req) -> Req when Req::req().
set([], Req) -> Req;
set([{bindings, Val}|Tail], Req) -> set(Tail, Req#http_req{bindings=Val});
set([{body_state, Val}|Tail], Req) -> set(Tail, Req#http_req{body_state=Val});
set([{buffer, Val}|Tail], Req) -> set(Tail, Req#http_req{buffer=Val});
set([{connection, Val}|Tail], Req) -> set(Tail, Req#http_req{connection=Val});
set([{cookies, Val}|Tail], Req) -> set(Tail, Req#http_req{cookies=Val});
set([{headers, Val}|Tail], Req) -> set(Tail, Req#http_req{headers=Val});
set([{host, Val}|Tail], Req) -> set(Tail, Req#http_req{host=Val});
set([{host_info, Val}|Tail], Req) -> set(Tail, Req#http_req{host_info=Val});
set([{meta, Val}|Tail], Req) -> set(Tail, Req#http_req{meta=Val});
set([{method, Val}|Tail], Req) -> set(Tail, Req#http_req{method=Val});
set([{multipart, Val}|Tail], Req) -> set(Tail, Req#http_req{multipart=Val});
set([{onresponse, Val}|Tail], Req) -> set(Tail, Req#http_req{onresponse=Val});
set([{p_headers, Val}|Tail], Req) -> set(Tail, Req#http_req{p_headers=Val});
set([{path, Val}|Tail], Req) -> set(Tail, Req#http_req{path=Val});
set([{path_info, Val}|Tail], Req) -> set(Tail, Req#http_req{path_info=Val});
set([{peer, Val}|Tail], Req) -> set(Tail, Req#http_req{peer=Val});
set([{pid, Val}|Tail], Req) -> set(Tail, Req#http_req{pid=Val});
set([{port, Val}|Tail], Req) -> set(Tail, Req#http_req{port=Val});
set([{qs, Val}|Tail], Req) -> set(Tail, Req#http_req{qs=Val});
set([{qs_vals, Val}|Tail], Req) -> set(Tail, Req#http_req{qs_vals=Val});
set([{resp_body, Val}|Tail], Req) -> set(Tail, Req#http_req{resp_body=Val});
set([{resp_headers, Val}|Tail], Req) -> set(Tail, Req#http_req{resp_headers=Val});
set([{resp_state, Val}|Tail], Req) -> set(Tail, Req#http_req{resp_state=Val});
set([{socket, Val}|Tail], Req) -> set(Tail, Req#http_req{socket=Val});
set([{transport, Val}|Tail], Req) -> set(Tail, Req#http_req{transport=Val});
set([{version, Val}|Tail], Req) -> set(Tail, Req#http_req{version=Val}).

%% @private
-spec set_bindings(cowboy_router:tokens(), cowboy_router:tokens(),
	cowboy_router:bindings(), Req) -> Req when Req::req().
set_bindings(HostInfo, PathInfo, Bindings, Req) ->
	Req#http_req{host_info=HostInfo, path_info=PathInfo,
		bindings=Bindings}.

%% Misc API.

%% @doc Compact the request data by removing all non-system information.
%%
%% This essentially removes the host and path info, query string, bindings,
%% headers and cookies.
%%
%% Use it when you really need to save up memory, for example when having
%% many concurrent long-running connections.
-spec compact(Req) -> Req when Req::req().
compact(Req) ->
	Req#http_req{host_info=undefined,
		path_info=undefined, qs_vals=undefined,
		bindings=undefined, headers=[],
		p_headers=[], cookies=[]}.

%% @doc Prevent any further responses.
%% @private
-spec lock(Req) -> Req when Req::req().
lock(Req) ->
	Req#http_req{resp_state=locked}.

%% @doc Convert the Req object to a list of key/values.
-spec to_list(req()) -> [{atom(), any()}].
to_list(Req) ->
	lists:zip(record_info(fields, http_req), tl(tuple_to_list(Req))).

%% Internal.

-spec chunked_response(cowboy:http_status(), cowboy:http_headers(), Req) ->
	{normal | hook, Req} when Req::req().
chunked_response(Status, Headers, Req=#http_req{
		transport=cowboy_spdy, resp_state=waiting,
		resp_headers=RespHeaders}) ->
	{RespType, Req2} = response(Status, Headers, RespHeaders, [
		{<<"date">>, cowboy_clock:rfc1123()},
		{<<"server">>, <<"Cowboy">>}
	], stream, Req),
	{RespType, Req2#http_req{resp_state=chunks,
		resp_headers=[], resp_body= <<>>}};
chunked_response(Status, Headers, Req=#http_req{
		version=Version, connection=Connection,
		resp_state=RespState, resp_headers=RespHeaders})
		when RespState =:= waiting; RespState =:= waiting_stream ->
	RespConn = response_connection(Headers, Connection),
	HTTP11Headers = if
		Version =:= 'HTTP/1.0' -> [];
		true ->
			MaybeTE = if
				RespState =:= waiting_stream -> [];
				true -> [{<<"transfer-encoding">>, <<"chunked">>}]
			end,
			[{<<"connection">>, atom_to_connection(Connection)}|MaybeTE]
	end,
	RespState2 = if
		Version =:= 'HTTP/1.1', RespState =:= 'waiting' -> chunks;
		true -> stream
	end,
	{RespType, Req2} = response(Status, Headers, RespHeaders, [
		{<<"date">>, cowboy_clock:rfc1123()},
		{<<"server">>, <<"Cowboy">>}
	|HTTP11Headers], <<>>, Req),
	{RespType, Req2#http_req{connection=RespConn, resp_state=RespState2,
			resp_headers=[], resp_body= <<>>}}.

-spec response(cowboy:http_status(), cowboy:http_headers(),
	cowboy:http_headers(), cowboy:http_headers(), stream | iodata(), Req)
	-> {normal | hook, Req} when Req::req().
response(Status, Headers, RespHeaders, DefaultHeaders, Body, Req=#http_req{
		socket=Socket, transport=Transport, version=Version,
		pid=ReqPid, onresponse=OnResponse}) ->
	FullHeaders = case OnResponse of
		already_called -> Headers;
		_ -> response_merge_headers(Headers, RespHeaders, DefaultHeaders)
	end,
	Body2 = case Body of stream -> <<>>; _ -> Body end,
	Req2 = case OnResponse of
		already_called -> Req;
		undefined -> Req;
		OnResponse ->
			OnResponse(Status, FullHeaders, Body2,
				%% Don't call 'onresponse' from the hook itself.
				Req#http_req{resp_headers=[], resp_body= <<>>,
					onresponse=already_called})
	end,
	ReplyType = case Req2#http_req.resp_state of
		waiting when Transport =:= cowboy_spdy, Body =:= stream ->
			cowboy_spdy:stream_reply(Socket, status(Status), FullHeaders),
			ReqPid ! {?MODULE, resp_sent},
			normal;
		waiting when Transport =:= cowboy_spdy ->
			cowboy_spdy:reply(Socket, status(Status), FullHeaders, Body),
			ReqPid ! {?MODULE, resp_sent},
			normal;
		RespState when RespState =:= waiting; RespState =:= waiting_stream ->
			HTTPVer = atom_to_binary(Version, latin1),
			StatusLine = << HTTPVer/binary, " ",
				(status(Status))/binary, "\r\n" >>,
			HeaderLines = [[Key, <<": ">>, Value, <<"\r\n">>]
				|| {Key, Value} <- FullHeaders],
			Transport:send(Socket, [StatusLine, HeaderLines, <<"\r\n">>, Body2]),
			ReqPid ! {?MODULE, resp_sent},
			normal;
		_ ->
			hook
	end,
	{ReplyType, Req2}.

-spec response_connection(cowboy:http_headers(), keepalive | close)
	-> keepalive | close.
response_connection([], Connection) ->
	Connection;
response_connection([{Name, Value}|Tail], Connection) ->
	case Name of
		<<"connection">> ->
			Tokens = parse_connection_before(Value, []),
			connection_to_atom(Tokens);
		_ ->
			response_connection(Tail, Connection)
	end.

-spec response_merge_headers(cowboy:http_headers(), cowboy:http_headers(),
	cowboy:http_headers()) -> cowboy:http_headers().
response_merge_headers(Headers, RespHeaders, DefaultHeaders) ->
	Headers2 = [{Key, Value} || {Key, Value} <- Headers],
	merge_headers(
		merge_headers(Headers2, RespHeaders),
		DefaultHeaders).

-spec merge_headers(cowboy:http_headers(), cowboy:http_headers())
	-> cowboy:http_headers().

%% Merge headers by prepending the tuples in the second list to the
%% first list. It also handles Set-Cookie properly, which supports
%% duplicated entries. Notice that, while the RFC2109 does allow more
%% than one cookie to be set per Set-Cookie header, we are following
%% the implementation of common web servers and applications which
%% return many distinct headers per each Set-Cookie entry to avoid
%% issues with clients/browser which may not support it.
merge_headers(Headers, []) ->
	Headers;
merge_headers(Headers, [{<<"set-cookie">>, Value}|Tail]) ->
	merge_headers([{<<"set-cookie">>, Value}|Headers], Tail);
merge_headers(Headers, [{Name, Value}|Tail]) ->
	Headers2 = case lists:keymember(Name, 1, Headers) of
		true -> Headers;
		false -> [{Name, Value}|Headers]
	end,
	merge_headers(Headers2, Tail).

-spec atom_to_connection(keepalive) -> <<_:80>>;
						(close) -> <<_:40>>.
atom_to_connection(keepalive) ->
	<<"keep-alive">>;
atom_to_connection(close) ->
	<<"close">>.

%% Optimized parsing functions for the Connection header.
parse_connection_before(<<>>, Acc) ->
	lists:reverse(Acc);
parse_connection_before(<< C, Rest/bits >>, Acc)
		when C =:= $,; C =:= $\s; C =:= $\t ->
	parse_connection_before(Rest, Acc);
parse_connection_before(Buffer, Acc) ->
	parse_connection(Buffer, Acc, <<>>).

%% An evil block of code appeared!
parse_connection(<<>>, Acc, <<>>) ->
	lists:reverse(Acc);
parse_connection(<<>>, Acc, Token) ->
	lists:reverse([Token|Acc]);
parse_connection(<< C, Rest/bits >>, Acc, Token)
		when C =:= $,; C =:= $\s; C =:= $\t ->
	parse_connection_before(Rest, [Token|Acc]);
parse_connection(<< C, Rest/bits >>, Acc, Token) ->
	case C of
		$A -> parse_connection(Rest, Acc, << Token/binary, $a >>);
		$B -> parse_connection(Rest, Acc, << Token/binary, $b >>);
		$C -> parse_connection(Rest, Acc, << Token/binary, $c >>);
		$D -> parse_connection(Rest, Acc, << Token/binary, $d >>);
		$E -> parse_connection(Rest, Acc, << Token/binary, $e >>);
		$F -> parse_connection(Rest, Acc, << Token/binary, $f >>);
		$G -> parse_connection(Rest, Acc, << Token/binary, $g >>);
		$H -> parse_connection(Rest, Acc, << Token/binary, $h >>);
		$I -> parse_connection(Rest, Acc, << Token/binary, $i >>);
		$J -> parse_connection(Rest, Acc, << Token/binary, $j >>);
		$K -> parse_connection(Rest, Acc, << Token/binary, $k >>);
		$L -> parse_connection(Rest, Acc, << Token/binary, $l >>);
		$M -> parse_connection(Rest, Acc, << Token/binary, $m >>);
		$N -> parse_connection(Rest, Acc, << Token/binary, $n >>);
		$O -> parse_connection(Rest, Acc, << Token/binary, $o >>);
		$P -> parse_connection(Rest, Acc, << Token/binary, $p >>);
		$Q -> parse_connection(Rest, Acc, << Token/binary, $q >>);
		$R -> parse_connection(Rest, Acc, << Token/binary, $r >>);
		$S -> parse_connection(Rest, Acc, << Token/binary, $s >>);
		$T -> parse_connection(Rest, Acc, << Token/binary, $t >>);
		$U -> parse_connection(Rest, Acc, << Token/binary, $u >>);
		$V -> parse_connection(Rest, Acc, << Token/binary, $v >>);
		$W -> parse_connection(Rest, Acc, << Token/binary, $w >>);
		$X -> parse_connection(Rest, Acc, << Token/binary, $x >>);
		$Y -> parse_connection(Rest, Acc, << Token/binary, $y >>);
		$Z -> parse_connection(Rest, Acc, << Token/binary, $z >>);
		C -> parse_connection(Rest, Acc, << Token/binary, C >>)
	end.

%% @doc Walk through a tokens list and return whether
%% the connection is keepalive or closed.
%%
%% We don't match on "keep-alive" since it is the default value.
-spec connection_to_atom([binary()]) -> keepalive | close.
connection_to_atom([]) ->
	keepalive;
connection_to_atom([<<"close">>|_]) ->
	close;
connection_to_atom([_|Tail]) ->
	connection_to_atom(Tail).

-spec status(cowboy:http_status()) -> binary().
status(100) -> <<"100 Continue">>;
status(101) -> <<"101 Switching Protocols">>;
status(102) -> <<"102 Processing">>;
status(200) -> <<"200 OK">>;
status(201) -> <<"201 Created">>;
status(202) -> <<"202 Accepted">>;
status(203) -> <<"203 Non-Authoritative Information">>;
status(204) -> <<"204 No Content">>;
status(205) -> <<"205 Reset Content">>;
status(206) -> <<"206 Partial Content">>;
status(207) -> <<"207 Multi-Status">>;
status(226) -> <<"226 IM Used">>;
status(300) -> <<"300 Multiple Choices">>;
status(301) -> <<"301 Moved Permanently">>;
status(302) -> <<"302 Found">>;
status(303) -> <<"303 See Other">>;
status(304) -> <<"304 Not Modified">>;
status(305) -> <<"305 Use Proxy">>;
status(306) -> <<"306 Switch Proxy">>;
status(307) -> <<"307 Temporary Redirect">>;
status(400) -> <<"400 Bad Request">>;
status(401) -> <<"401 Unauthorized">>;
status(402) -> <<"402 Payment Required">>;
status(403) -> <<"403 Forbidden">>;
status(404) -> <<"404 Not Found">>;
status(405) -> <<"405 Method Not Allowed">>;
status(406) -> <<"406 Not Acceptable">>;
status(407) -> <<"407 Proxy Authentication Required">>;
status(408) -> <<"408 Request Timeout">>;
status(409) -> <<"409 Conflict">>;
status(410) -> <<"410 Gone">>;
status(411) -> <<"411 Length Required">>;
status(412) -> <<"412 Precondition Failed">>;
status(413) -> <<"413 Request Entity Too Large">>;
status(414) -> <<"414 Request-URI Too Long">>;
status(415) -> <<"415 Unsupported Media Type">>;
status(416) -> <<"416 Requested Range Not Satisfiable">>;
status(417) -> <<"417 Expectation Failed">>;
status(418) -> <<"418 I'm a teapot">>;
status(422) -> <<"422 Unprocessable Entity">>;
status(423) -> <<"423 Locked">>;
status(424) -> <<"424 Failed Dependency">>;
status(425) -> <<"425 Unordered Collection">>;
status(426) -> <<"426 Upgrade Required">>;
status(428) -> <<"428 Precondition Required">>;
status(429) -> <<"429 Too Many Requests">>;
status(431) -> <<"431 Request Header Fields Too Large">>;
status(500) -> <<"500 Internal Server Error">>;
status(501) -> <<"501 Not Implemented">>;
status(502) -> <<"502 Bad Gateway">>;
status(503) -> <<"503 Service Unavailable">>;
status(504) -> <<"504 Gateway Timeout">>;
status(505) -> <<"505 HTTP Version Not Supported">>;
status(506) -> <<"506 Variant Also Negotiates">>;
status(507) -> <<"507 Insufficient Storage">>;
status(510) -> <<"510 Not Extended">>;
status(511) -> <<"511 Network Authentication Required">>;
status(B) when is_binary(B) -> B.

%% Tests.

-ifdef(TEST).

url_test() ->
	{undefined, _} =
		url(#http_req{transport=ranch_tcp, host= <<>>, port= undefined,
			path= <<>>, qs= <<>>, pid=self()}),
	{<<"http://localhost/path">>, _ } =
		url(#http_req{transport=ranch_tcp, host= <<"localhost">>, port=80,
			path= <<"/path">>, qs= <<>>, pid=self()}),
	{<<"http://localhost:443/path">>, _} =
		url(#http_req{transport=ranch_tcp, host= <<"localhost">>, port=443,
			path= <<"/path">>, qs= <<>>, pid=self()}),
	{<<"http://localhost:8080/path">>, _} =
		url(#http_req{transport=ranch_tcp, host= <<"localhost">>, port=8080,
			path= <<"/path">>, qs= <<>>, pid=self()}),
	{<<"http://localhost:8080/path?dummy=2785">>, _} =
		url(#http_req{transport=ranch_tcp, host= <<"localhost">>, port=8080,
			path= <<"/path">>, qs= <<"dummy=2785">>, pid=self()}),
	{<<"https://localhost/path">>, _} =
		url(#http_req{transport=ranch_ssl, host= <<"localhost">>, port=443,
			path= <<"/path">>, qs= <<>>, pid=self()}),
	{<<"https://localhost:8443/path">>, _} =
		url(#http_req{transport=ranch_ssl, host= <<"localhost">>, port=8443,
			path= <<"/path">>, qs= <<>>, pid=self()}),
	{<<"https://localhost:8443/path?dummy=2785">>, _} =
		url(#http_req{transport=ranch_ssl, host= <<"localhost">>, port=8443,
			path= <<"/path">>, qs= <<"dummy=2785">>, pid=self()}),
	ok.

parse_connection_test_() ->
	%% {Binary, Result}
	Tests = [
		{<<"close">>, [<<"close">>]},
		{<<"ClOsE">>, [<<"close">>]},
		{<<"Keep-Alive">>, [<<"keep-alive">>]},
		{<<"keep-alive, Upgrade">>, [<<"keep-alive">>, <<"upgrade">>]}
	],
	[{B, fun() -> R = parse_connection_before(B, []) end}
		|| {B, R} <- Tests].

connection_to_atom_test_() ->
	%% {Tokens, Result}
	Tests = [
		{[<<"close">>], close},
		{[<<"keep-alive">>], keepalive},
		{[<<"keep-alive">>, <<"upgrade">>], keepalive}
	],
	[{lists:flatten(io_lib:format("~p", [T])),
		fun() -> R = connection_to_atom(T) end} || {T, R} <- Tests].

merge_headers_test_() ->
	Tests = [
		{[{<<"content-length">>,<<"13">>},{<<"server">>,<<"Cowboy">>}],
		 [{<<"set-cookie">>,<<"foo=bar">>},{<<"content-length">>,<<"11">>}],
		 [{<<"set-cookie">>,<<"foo=bar">>},
		  {<<"content-length">>,<<"13">>},
		  {<<"server">>,<<"Cowboy">>}]},
		{[{<<"content-length">>,<<"13">>},{<<"server">>,<<"Cowboy">>}],
		 [{<<"set-cookie">>,<<"foo=bar">>},{<<"set-cookie">>,<<"bar=baz">>}],
		 [{<<"set-cookie">>,<<"bar=baz">>},
		  {<<"set-cookie">>,<<"foo=bar">>},
		  {<<"content-length">>,<<"13">>},
		  {<<"server">>,<<"Cowboy">>}]}
	],
	[fun() -> Res = merge_headers(L,R) end || {L, R, Res} <- Tests].

-endif.
