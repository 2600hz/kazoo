%% JSON-RPC for Erlang's inets httpd
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
%% @doc An extension module for the Inets HTTP server, providing HTTP access to JSON-RPC services.
%%
%% == Configuring HTTP access to registered JSON-RPC services ==
%%
%% The inets httpd uses an `httpd.conf' file to configure itself. To
%% enable HTTP access to registered JSON-RPC services, two things need
%% to be added to the httpd configuration file:
%%
%% <ul>
%% <li>an entry for `rfc4627_jsonrpc_inets' in the `Modules' configuration directive (just after `mod_alias' and `mod_auth' will do)</li>
%% <li>a `JsonRpcAlias' directive, specifying a subspace of the URLs served by the httpd that will be mapped to JSON-RPC service requests (the value used as `AliasPrefix' in calls to {@link rfc4627_jsonrpc_http:invoke_service_method/4})</li>
%% </ul>
%%
%% Here's a complete `httpd.conf':
%%
%% ```
%% ServerName localhost
%% ServerRoot test/server_root
%% DocumentRoot test/server_root/htdocs
%% Port 5671
%% Modules mod_alias mod_auth rfc4627_jsonrpc_inets mod_actions mod_cgi mod_responsecontrol mod_trace mod_range mod_head mod_include mod_dir mod_get mod_log mod_disk_log
%% DirectoryIndex index.html
%% JsonRpcAlias /rpc
%% ErrorLog logs/error_log
%% TransferLog logs/access_log
%% SecurityLog logs/security_log</pre>
%% '''
%%
%% If an httpd server is started from this configuration, it will
%%
%% <ul>
%% <li>listen on port 5671</li>
%% <li>permit JSON-RPC access via URLs starting with `/rpc'</li>
%% </ul>
%%
%% The URL structure for JSON-RPC requests will be
%%
%% <pre>http://localhost:5671/rpc/<i>ServiceName</i></pre>
%%
%% where ServiceName is the {@link
%% rfc4627_jsonrpc:register_service/2}'d name of the service. For
%% instance, the example "hello world" service defined in
%% `test_jsonrpc_inets.erl' would be accessible at
%%
%% ``http://localhost:5671/rpc/test''
%%
%% The built-in service description method, `system.describe', is
%% accessible via a POST to that URL, or a GET to
%%
%% ``http://localhost:5671/rpc/test/system.describe''
%%
%% Similarly, any idempotent methods provided by a service may be
%% accessed via POST to the base URL for the service, or via GET to a
%% URL of the form
%%
%% <pre>http://localhost:5671/rpc/<i>ServiceName</i>/<i>MethodName</i>?<i>arg</i>=<i>value</i>&amp;<i>...</i></pre>
%%

-module(rfc4627_jsonrpc_inets).
-include("rfc4627_jsonrpc.hrl").

%% The path to httpd.hrl has changed with various OTP releases. Our
%% Makefile detects the changes for us, and supplies compile-time
%% macro definitions to allow us to adapt.
-ifdef(inets_pre_r14a).
-include_lib("inets/src/httpd.hrl").
-else.
-ifdef(inets_pre_r14b01).
-include_lib("inets/src/http_server/httpd.hrl").
-else.
-include_lib("inets/include/httpd.hrl").
-endif.
-endif.

-export([do/1, load/2]).

%% @spec (#mod{}) -> {proceed, term()}
%% @doc Implements the inets httpd main callback interface.
%%
%% Calls out to {@link rfc4627_jsonrpc_http:invoke_service_method/4}.
do(ModData = #mod{data = OldData}) ->
    case {proplists:get_value(status, OldData),
	  proplists:get_value(response, OldData)} of
	{undefined, undefined} ->
	    do_rpc(ModData);
	_ ->
	    {proceed, OldData}
    end.

%% @spec (Line::string(), AccIn::term()) -> {ok, AccOut::term(), {atom(), term()}}
%% @doc Parses the `"JsonRpcAlias"' configuration entry from the inets `httpd.conf' file.
load("JsonRpcAlias " ++ Alias, []) ->
    {ok, [], {json_rpc_alias, Alias}}.

do_rpc(#mod{init_data = #init_data{peername = {PeerPort, PeerName}},
	    config_db = ConfigDb,
	    socket_type = SocketType,
	    method = HttpMethod,
	    request_uri = PathAndQuery,
	    parsed_header = InetsHeaders,
	    entity_body = Body,
	    data = OldData}) ->
    AliasPrefix = httpd_util:lookup(ConfigDb, json_rpc_alias, default),
    {Path, QueryObj} = case string:tokens(PathAndQuery, "?") of
			   [] -> {"", {obj, []}};
			   [P] -> {P, {obj, []}};
			   [P, Q] -> {P, {obj, case httpd:parse_query(Q) of
						   [{"",""}] -> []; %% is this a bug in httpd?
						   KV -> [{K, list_to_binary(V)} || {K,V} <- KV]
					       end}}
		       end,
    HeaderObj = {obj, [{K, list_to_binary(V)} || {K,V} <- InetsHeaders]},
    SchemeFields = case SocketType of
		       ip_comm -> [{"scheme", <<"http">>}];
		       ssl -> [{"scheme", <<"https">>}];
		       _ -> []
		   end,

    RequestInfo = {obj, ([{"http_method", list_to_binary(HttpMethod)},
			  {"http_query_parameters", QueryObj},
			  {"http_headers", HeaderObj},
			  {"remote_port", PeerPort},
			  {"remote_peername", list_to_binary(PeerName)}]
			 ++ SchemeFields)},

    case rfc4627_jsonrpc_http:invoke_service_method(AliasPrefix,
						    Path,
						    RequestInfo,
						    Body) of
	no_match ->
	    {proceed, OldData};
	{ok, ResultEnc, ResponseInfo} ->
	    {obj, ResponseHeaderFields} =
		rfc4627:get_field(ResponseInfo, "http_headers", {obj, []}),
            StatusCode =
		rfc4627:get_field(ResponseInfo, "http_status_code", 200),
	    Headers = [{K, binary_to_list(V)} || {K,V} <- ResponseHeaderFields],
	    {proceed, [{response, {response,
				   [{code, StatusCode},
				    {content_length, integer_to_list(length(ResultEnc))},
				    {content_type, rfc4627:mime_type()}
				    | Headers],
				   ResultEnc}} | OldData]}
    end.
