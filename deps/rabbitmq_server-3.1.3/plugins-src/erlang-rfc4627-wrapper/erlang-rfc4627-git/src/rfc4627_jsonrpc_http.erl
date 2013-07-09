%% JSON-RPC for HTTP transports (inets, mochiweb, yaws, ...)
%%---------------------------------------------------------------------------
%% @author Tony Garnock-Jones <tonygarnockjones@gmail.com>
%% @author LShift Ltd. <query@lshift.net>
%% @copyright 2007-2010, 2011, 2012 Tony Garnock-Jones and 2007-2010 LShift Ltd.
%% @license
%%
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use, copy,
%% modify, merge, publish, distribute, sublicense, and/or sell copies
%% of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
%% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
%% BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
%% ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
%% CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%% SOFTWARE.
%%---------------------------------------------------------------------------
%% @since 1.2.0
%%
%% @doc General JSON-RPC HTTP transport support.
%%
%% Used by the Inets HTTP transport library, {@link
%% rfc4627_jsonrpc_inets}.

-module(rfc4627_jsonrpc_http).
-include("rfc4627_jsonrpc.hrl").

-export([invoke_service_method/4]).

%% @spec (AliasPrefix, Path, RequestInfo, Body) -> Result
%% where AliasPrefix = default | string()
%%       Path = string()
%%       RequestInfo = rfc4627:jsonobj()
%%       Body = string() | binary()
%%       Result = no_match | {ok, string(), jsonobj()}
%%
%% @doc Uses `AliasPrefix', `RequestInfo', `Path' and `Body' to locate
%% and invoke a service procedure.
%%
%% If `AliasPrefix' is `default', the default prefix `"/jsonrpc"' is used.
%%
%% `Path' is expected to be the "path" part of the request URI: a string of the form "/.../prefix/objectname[/methodname]".
%%
%% `RequestInfo' is a JSON "object" containing HTTP-specific request information:
%%
%% <ul>
%% <li>`http_method' should be either `<<"GET">>' (the default) or `<<"POST">>'</li>
%% <li>`http_query_parameters' should be the query parameters (the part of the URL after "?")
%%     decoded into a JSON "object", with one field per parameter</li>
%% <li>`http_headers' should be a JSON "object" containing the HTTP request headers</li>
%% <li>`scheme' should be either `<<"http">>' or `<<"https">>'</li>
%% <li>`remote_port' should be the TCP port number of the remote peer</li>
%% <li>`remote_peername' should be a {@type binary()} containing the IP address of the
%%     remote peer, in a form acceptable to {@link inet:gethostbyaddr/1}</li>
%% </ul>
%%
%% All the fields in `RequestInfo' are optional.
%%
%% `Body' must be either a {@type binary()} or a {@type string()}
%% containing the HTTP request body.
%%
%% Operation proceeds as follows. `Path' is first matched against
%% `AliasPrefix'. If it does not match, `no_match' is
%% returned. Otherwise, the matching prefix is stripped from the path,
%% and extraction of the service name, method name, and parameters from
%% the HTTP request proceeds as per the JSON-RPC specification,
%% [JSON-RPC-1-1-WD-20060807.html#ProcedureCall].
%%
%% Once the service name, method name and parameters are known, The
%% service is looked up with {@link rfc4627_jsonrpc:lookup_service/1},
%% and the named method is invoked with {@link
%% rfc4627_jsonrpc:invoke_service_method/8}.
%%
%% The final result is encoded into a list of bytes using {@link
%% rfc4627:encode/1}, and returned along with the `ResponseInfo' to
%% our caller.
invoke_service_method(default, Path, RequestInfo, Body) ->
    invoke_service_method("/jsonrpc", Path, RequestInfo, Body);
invoke_service_method(AliasPrefix, Path, RequestInfo, Body) ->
    case parse_jsonrpc(AliasPrefix,
		       Path,
		       rfc4627:get_field(RequestInfo, "http_method", <<"GET">>),
		       rfc4627:get_field(RequestInfo, "http_query_parameters", {obj, []}),
		       Body) of
	no_match ->
	    no_match;
	{PostOrGet, Id, Service, Method, Args} ->
	    {_ResultOrError, Result, ResponseInfo} =
		case rfc4627_jsonrpc:lookup_service(Service) of
		    not_found ->
			Err = rfc4627_jsonrpc:error_response(404, "Service not found", Service),
			rfc4627_jsonrpc:expand_jsonrpc_reply(Id, Err);
		    ServiceRec ->
			HttpHeaders = rfc4627:get_field(RequestInfo, "http_headers", {obj, []}),
			Timeout = extract_timeout_header(HttpHeaders),
			EndpointAddress = service_address(AliasPrefix, RequestInfo, Service),
			rfc4627_jsonrpc:invoke_service_method(
			  ServiceRec, Id,
			  PostOrGet, RequestInfo, EndpointAddress, Method, Args, Timeout)
		end,
	    ResultEnc = lists:flatten(rfc4627:encode(Result)),
	    {ok, ResultEnc, ResponseInfo}
    end.

extract_timeout_header(HeadersJsonObj) ->
    case rfc4627:get_field(HeadersJsonObj, "x-json-rpc-timeout", <<"default">>) of
	<<"default">> ->
	    default;
	<<"infinity">> ->
	    infinity;
	Other ->
	    list_to_integer(binary_to_list(Other))
    end.

extract_object_and_method(AliasPrefix, Path) ->
    AliasPrefixRe = "^" ++ AliasPrefix ++ "/",
    case re:run(Path, AliasPrefixRe) of
	{match, [{0, Length}]} ->
	    ObjectMethod = string:substr(Path, Length + 1),
	    case lists:reverse(string:tokens(ObjectMethod, "/")) of
		[] -> {<<>>, <<>>};
		[Object] -> {list_to_binary(Object), <<>>};
		[Method1, Object | _] -> {list_to_binary(Object), list_to_binary(Method1)}
	    end;
	nomatch ->
	    no_match
    end.

parse_jsonrpc(AliasPrefix, Path, HttpMethod, QueryParametersObj, Body) ->
    case extract_object_and_method(AliasPrefix, Path) of
	no_match ->
	    no_match;
	{Object, PathMethod} ->
	    case HttpMethod of
		<<"POST">> ->
		    {ok, RequestObject, _} = rfc4627:decode(Body),
		    {post,
		     rfc4627:get_field(RequestObject, "id", null),
		     Object,
		     rfc4627:get_field(RequestObject, "method", undefined),
		     rfc4627:get_field(RequestObject, "params", [])};
		_ ->
		    %% GET, presumably. We don't really care, here.
		    {get,
		     null,
		     Object,
		     PathMethod,
		     QueryParametersObj}
	    end
    end.

service_address(AliasPrefix, RequestInfo, ServiceName) ->
    HttpHeaders = rfc4627:get_field(RequestInfo, "http_headers", {obj, []}),
    Host = case rfc4627:get_field(HttpHeaders, "host", undefined) of
	       undefined -> "";
	       Name -> "//" ++ binary_to_list(Name)
	   end,
    Scheme = case rfc4627:get_field(RequestInfo, "scheme", undefined) of
		 undefined -> "";
		 S -> binary_to_list(S) ++ ":"
	     end,
    list_to_binary(Scheme ++ Host ++ AliasPrefix ++ "/" ++ binary_to_list(ServiceName)).
