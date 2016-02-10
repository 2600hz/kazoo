%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, 2600Hz
%%% @doc
%%% Kazoo HTTP client
%%% @end
%%% @contributors
%%%   Hesaam Farhang
%%%-------------------------------------------------------------------
-module(kz_http).

-export([req/1, req/2, req/3, req/4, req/5
         ,async_req/2, async_req/3, async_req/4, async_req/5, async_req/6
         ,get/1, get/2, get/3
         ,options/1, options/2, options/3
         ,head/1, head/2, head/3
         ,trace/1, trace/2, trace/3
         ,post/1, post/2, post/3, post/4
         ,put/1, put/2, put/3, put/4
         ,delete/1, delete/2, delete/3, delete/4
         ,handle_response/1
        ]).

-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_log.hrl").

-define(HTTP_OPTIONS, [timeout, connect_timeout, ssl
                       ,essl, autoredirect, proxy_auth
                       ,version, relaxed, url_encode]).
-define(OPTIONS, [sync, stream, body_format
                  ,full_result , headers_as_is, socket_opts
                  ,receiver, ipv6_host_with_brackets]).

-type http_body() :: string() | binary().
-type httpc_result() :: {term(), wh_proplist(), http_body()} |
                        {string(), string() |binary()} |
                        reference().
-type httpc_ret() :: {ok, httpc_result()} |
                     {ok, saved_to_file} |
                     {error, {connect_failed, term()} | {send_failed, term()} | term()}.
-type httpc_request() :: {string(), wh_proplist()} |
                         {string(), wh_proplist(), string(), http_body()}.

-type http_req_id() :: {'http_req_id', reference()} | {'ok', reference()} | reference().
-export_type([http_req_id/0]).

-type http_ret() :: {'ok', string(), wh_proplist(), string() | binary()} |
                       {'ok', 'saved_to_file'} |
                       {'error', any()} |
                       http_req_id().
-export_type([http_ret/0]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Send synchronous request
%% @end
%%--------------------------------------------------------------------
-spec get(string()) -> http_ret().
-spec get(string(), wh_proplist()) -> http_ret().
-spec get(string(), wh_proplist(), wh_proplist()) -> http_ret().
get(Url) ->
    req('get', Url, [], [], []).
get(Url, Headers) ->
    req('get', Url, Headers, [], []).
get(Url, Headers, Options) ->
    req('get', Url, Headers, [], Options).

-spec options(string()) -> http_ret().
-spec options(string(), wh_proplist()) -> http_ret().
-spec options(string(), wh_proplist(), wh_proplist()) -> http_ret().
options(Url) ->
    req('options', Url, [], [], []).
options(Url, Headers) ->
    req('options', Url, Headers, [], []).
options(Url, Headers, Options) ->
    req('options', Url, Headers, [], Options).

-spec head(string()) -> http_ret().
-spec head(string(), wh_proplist()) -> http_ret().
-spec head(string(), wh_proplist(), wh_proplist()) -> http_ret().
head(Url) ->
    req('head', Url, [], [], []).
head(Url, Headers) ->
    req('head', Url, Headers, [], []).
head(Url, Headers, Options) ->
    req('head', Url, Headers, [], Options).

-spec trace(string()) -> http_ret().
-spec trace(string(), wh_proplist()) -> http_ret().
-spec trace(string(), wh_proplist(), wh_proplist()) -> http_ret().
trace(Url) ->
    req('trace', Url, [], [], []).
trace(Url, Headers) ->
    req('trace', Url, Headers, [], []).
trace(Url, Headers, Options) ->
    req('trace', Url, Headers, [], Options).

-spec delete(string()) -> http_ret().
-spec delete(string(), wh_proplist()) -> http_ret().
-spec delete(string(), wh_proplist(), http_body()) -> http_ret().
-spec delete(string(), wh_proplist(), http_body(), wh_proplist()) -> http_ret().
delete(Url) ->
    req('delete', Url, [], [], []).
delete(Url, Headers) ->
    req('delete', Url, Headers, [], []).
delete(Url, Headers, Body) ->
    req('delete', Url, Headers, Body, []).
delete(Url, Headers, Body, Options) ->
    req('delete', Url, Headers, Body, Options).

-spec post(string()) -> http_ret().
-spec post(string(), wh_proplist()) -> http_ret().
-spec post(string(), wh_proplist(), http_body()) -> http_ret().
-spec post(string(), wh_proplist(), http_body(), wh_proplist()) -> http_ret().
post(Url) ->
    req('post', Url, [], [], []).
post(Url, Headers) ->
    req('post', Url, Headers, [], []).
post(Url, Headers, Body) ->
    req('post', Url, Headers, Body, []).
post(Url, Headers, Body, Options) ->
    req('post', Url, Headers, Body, Options).

-spec put(string()) -> http_ret().
-spec put(string(), wh_proplist()) -> http_ret().
-spec put(string(), wh_proplist(), http_body()) -> http_ret().
-spec put(string(), wh_proplist(), http_body(), wh_proplist()) -> http_ret().
put(Url) ->
    req('put', Url, [], [], []).
put(Url, Headers) ->
    req('put', Url, Headers, [], []).
put(Url, Headers, Body) ->
    req('put', Url, Headers, Body, []).
put(Url, Headers, Body, Options) ->
    req('put', Url, Headers, Body, Options).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Send a synchronous HTTP request
%% @end
%%--------------------------------------------------------------------
-spec req(string()) -> http_ret().
-spec req(atom(), string()) -> http_ret().
-spec req(atom(), string(), wh_proplist()) -> http_ret().
-spec req(atom(), string(), wh_proplist(), http_body()) -> http_ret().
-spec req(atom(), string(), wh_proplist(), http_body(), wh_proplist()) -> http_ret().
req(Url) ->
    req('get', Url, [], [], []).
req(Method, Url) ->
    req(Method, Url, [], [], []).
req(Method, Url, Headers) ->
    req(Method, Url, Headers, [], []).
req(Method, Url, Headers, Body) ->
    req(Method, Url, Headers, Body, []).
req(Method, Url, Hdrs, Body, Opts) ->
    {Headers, Options} = maybe_basic_auth(Hdrs, Opts),
    Request = build_request(Method, Url, Headers, Body),
    execute_request(Method, Request, Options).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Send a asynchronous HTTP request
%% @end
%%--------------------------------------------------------------------
-spec async_req(pid(), string()) -> http_ret().
-spec async_req(pid(), atom(), string()) -> http_ret().
-spec async_req(pid(), atom(), string(), wh_proplist()) -> http_ret().
-spec async_req(pid(), atom(), string(), wh_proplist(), http_body()) -> http_ret().
-spec async_req(pid(), atom(), string(), wh_proplist(), http_body(), wh_proplist()) -> http_ret().
async_req(Pid, Url) ->
    async_req(Pid, 'get', Url, [], [], []).
async_req(Pid, Method, Url) ->
    async_req(Pid, Method, Url, [], [], []).
async_req(Pid, Method, Url, Headers) ->
    async_req(Pid, Method, Url, Headers, []).
async_req(Pid, Method, Url, Headers, Body) ->
    async_req(Pid, Method, Url, Headers, Body, []).
async_req(Pid, Method, Url, Hdrs, Body, Opts) ->
    {Headers, Options} = maybe_basic_auth(Hdrs, Opts),
    Request = build_request(Method, Url, Headers, Body),
    execute_request(Method, Request, [{receiver, Pid}, {sync, false}, {stream, self} | Options]).

%%--------------------------------------------------------------------
%% @private
%% @doc Send request using httpc and handle its response
%% @end
%%--------------------------------------------------------------------
-spec execute_request(atom(), tuple(), wh_proplist()) -> http_ret().
execute_request(Method, Request, Opts) ->
    HTTPOptions = get_options(?HTTP_OPTIONS, Opts),
    Opts1 = get_options(?OPTIONS, Opts),
    Options = case props:get_value('body_format', Opts1) of
                'undefined' -> [{'body_format', 'binary'} | Opts1];
                _ -> Opts1
              end,
    handle_response(catch httpc:request(Method, Request, HTTPOptions, Options)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Response to caller in a proper manner
%% @end
%%--------------------------------------------------------------------
-spec handle_response(httpc_ret()) -> http_ret().
handle_response({'ok', 'saved_to_file'}) ->
    {'ok', 'saved_to_file'};
handle_response({'ok', ReqId}) when is_reference(ReqId) ->
    {'http_req_id', ReqId};
handle_response({'ok', {{_, StatusCode, _}, Headers, Body}}) ->
    {'ok', StatusCode, Headers, Body};
handle_response({'error', 'timeout'}) ->
    lager:debug("connection timeout"),
    {'error', 'timeout'};
handle_response({'error', {failed_connect,[{_, Address}, {_, _, nxdomain}]}}) ->
    lager:debug("non existent domain ~p", Address),
    {'error', {failed_connect, nxdomain}};
handle_response({'error', {failed_connect,[{_, Address}, {_, _, econnrefused}]}}) ->
    lager:debug("connection refused to ~p", Address),
    {'error', {failed_connect, econnrefused}};
handle_response({'error', {malformed_url, _, Url}}) ->
    lager:debug("failed to parse the URL ~p", Url),
    {'error', {malformed_url, Url}};
handle_response({'error', Error}) ->
    lager:debug("request failed with ~p", [Error]),
    Error.

%%--------------------------------------------------------------------
%% @private
%% @doc Build <code>Authorization</code> header using <code>basic_auth</code> option
%% @end
%%--------------------------------------------------------------------
-spec maybe_basic_auth(wh_proplist(), wh_proplist()) -> {wh_proplist(), wh_proplist()}.
maybe_basic_auth(Headers, Options) ->
    case props:get_value('basic_auth', Options) of
        'undefined' -> {Headers, Options};
        {Username, Password} ->
            BasicAuth = {"Authorization"
                         , "Basic " ++ base64:encode_to_string(<<Username/binary, ":", Password/binary>>)
                        },
            {[BasicAuth | Headers], props:delete('basic_auth', Options)}
    end.
%%--------------------------------------------------------------------
%% @private
%% @doc Build httpc request argument based on method
%% @end
%%--------------------------------------------------------------------
-spec build_request(atom(), string(), wh_proplist(), http_body()) -> httpc_request().
build_request(Method, Url, Headers, _Body) when (Method =:= 'options') orelse
                                                (Method =:= 'get') orelse
                                                (Method =:= 'head') orelse
                                                (Method =:= 'trace') ->
    {Url, Headers};
build_request(Method, Url, Headers, Body) when (Method =:= 'post') orelse
                                               (Method =:= 'put') orelse
                                               (Method =:= 'delete') ->
    ContentType = case props:get_value("Content-Type", Headers) of
                      'undefined' -> [];
                      C -> C
                  end,
    {Url, Headers, ContentType, Body}.

%%--------------------------------------------------------------------
%% @private
%% @doc Get options out of a propslist based on options type
%% Two <code>HTTP_OPTIONS</code> and <code>OPTIONS</code> macros are specify
%% which type of options should be returned.
%% @end
%%--------------------------------------------------------------------
-spec get_options(list(), wh_proplist()) -> wh_proplist().
get_options(Type, Options) ->
    [{K, V} || {K, V} <- Options, lists:member(K, Type)].


