%% JSON-RPC for Mochiweb
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
%% @reference the <a href="http://code.google.com/p/mochiweb/">Mochiweb home page</a>
%%
%% @doc Support for serving JSON-RPC via Mochiweb.
%%
%% Familiarity with writing Mochiweb applications is assumed.
%%
%% == Basic Usage ==
%%
%% <ul>
%% <li>Register your JSON-RPC services as usual.</li>
%% <li>Decide on your `AliasPrefix' (see {@link rfc4627_jsonrpc_http:invoke_service_method/4}).</li>
%% <li>When a Mochiweb request arrives at your application, call {@link handle/2} with your `AliasPrefix' and the request.</li>
%% </ul>
%%
%% It's as simple as that - if the request's URI path matches the
%% `AliasPrefix', it will be decoded and the JSON-RPC service it names
%% will be invoked.

-module(rfc4627_jsonrpc_mochiweb).

-export([handle/2]).

normalize(X) when is_atom(X) ->
    string:to_lower(atom_to_list(X));
normalize(X) when is_binary(X) ->
    string:to_lower(binary_to_list(X));
normalize(X) when is_list(X) ->
    string:to_lower(X).

%% @spec (string(), #mochiweb_request{}) -> no_match | {ok, Response}
%% where Response = {Code, ReplyHeaders, ReplyBody}
%%       Code = integer()
%%       ReplyHeaders = [{string(), string()}]
%%       ReplyBody = string()
%%
%% @doc If the request matches `AliasPrefix', the corresponding
%% JSON-RPC service is invoked, and an `{ok, Response}' is returned;
%% otherwise, `no_match' is returned.
%%
%% Call this function from your Mochiweb application's `loop'
%% function, as follows:
%%
%% ```
%% case rfc4627_jsonrpc_mochiweb:handle("/rpc", Req) of
%%      no_match ->
%%          handle_non_jsonrpc_request(Req);
%%      {ok, Response} ->
%%          Req:respond(Response)
%% end
%% '''
%%
%% where `handle_non_jsonrpc_request' does the obvious thing for
%% non-JSON-RPC requests.
handle(AliasPrefix, Req) ->
    Path = Req:get(path),
    QueryObj = {obj, [{K, list_to_binary(V)} || {K,V} <- Req:parse_qs()]},
    HeaderObj = {obj, [{normalize(K), list_to_binary(V)}
		       || {K,V} <- mochiweb_headers:to_list(Req:get(headers))]},
    RequestInfo = {obj, [{"http_method", list_to_binary(atom_to_list(Req:get(method)))},
			 {"http_query_parameters", QueryObj},
			 {"http_headers", HeaderObj},
			 {"remote_peername", list_to_binary(Req:get(peer))},
			 {"scheme", <<"http">>}]},
    Body = Req:recv_body(),

    case rfc4627_jsonrpc_http:invoke_service_method(AliasPrefix,
						    Path,
						    RequestInfo,
						    Body) of
	no_match ->
	    no_match;
	{ok, ResultEnc, ResponseInfo} ->
            DefaultType = rfc4627:mime_type(),
            RespType = case Req:accepts_content_type(DefaultType) of
                           true -> DefaultType;
                           false ->
                               case Req:accepts_content_type("text/plain") of
                                   true -> "text/plain";
                                   false -> DefaultType
                               end
                       end,
	    {obj, ResponseHeaderFields} =
		rfc4627:get_field(ResponseInfo, "http_headers", {obj, []}),
            StatusCode =
		rfc4627:get_field(ResponseInfo, "http_status_code", 200),
	    Headers = [{K, binary_to_list(V)} || {K,V} <- ResponseHeaderFields],
	    {ok, {StatusCode, Headers ++ [{"Content-type", RespType}], ResultEnc}}
    end.
