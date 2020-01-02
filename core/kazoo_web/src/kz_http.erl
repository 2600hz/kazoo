%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc Kazoo HTTP client
%%% @author Hesaam Farhang
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_http).

-export([req/1, req/2, req/3, req/4, req/5
        ,async_req/2, async_req/3, async_req/4, async_req/5, async_req/6
        ,get/1, get/2, get/3
        ,options/1, options/2, options/3
        ,head/1, head/2, head/3
        ,trace/1, trace/2, trace/3
        ,post/1, post/2, post/3, post/4
        ,patch/1, patch/2, patch/3, patch/4
        ,put/1, put/2, put/3, put/4
        ,delete/1, delete/2, delete/3, delete/4
        ]).

-include("kz_web.hrl").

-type method() :: 'delete' |
                  'get' |
                  'head' |
                  'options' |
                  'post' |
                  'patch' |
                  'put' |
                  'trace'.

-type field() :: string().
-type value() :: string() | integer().
-type header() :: {field(), value()}.
-type headers() :: [header()].

-type http_body() :: iodata().

-type status_line() :: {string(), integer(), string()}.

-type httpc_result() :: {status_line(), headers(), http_body()} |
                        {string(), string() | binary()} |
                        reference().

-type httpc_ret() :: {'ok', httpc_result()} |
                     {'ok', 'saved_to_file'} |
                     {'error', {'connect_failed' | 'malformed_url', any()} |
                      {'send_failed', any()} |
                      any()
                     }.

-type httpc_request() :: {string(), headers()} |
                         {string(), headers(), string(), http_body()}.

-type req_id() :: {'http_req_id', reference()} |
                  {'ok', reference()} |
                  reference().

-type ret() :: {'ok', pos_integer(), headers(), kz_term:text()} |
               {'ok', 'saved_to_file'} |
               {'error', any()} |
               req_id().

-export_type([ret/0
             ,req_id/0
             ,field/0, value/0, header/0, headers/0
             ,method/0
             ,http_body/0
             ]).

-define(REQ_URL_INDEX, 1).

%%------------------------------------------------------------------------------
%% @doc Send synchronous request.
%% @end
%%------------------------------------------------------------------------------

-spec get(kz_term:text()) -> ret().
get(Url) ->
    req('get', Url, [], [], []).

-spec get(kz_term:text(), headers()) -> ret().
get(Url, Headers) ->
    req('get', Url, Headers, [], []).

-spec get(kz_term:text(), headers(), kz_term:proplist()) -> ret().
get(Url, Headers, Options) ->
    req('get', Url, Headers, [], Options).

-spec options(kz_term:text()) -> ret().
options(Url) ->
    req('options', Url, [], [], []).

-spec options(kz_term:text(), headers()) -> ret().
options(Url, Headers) ->
    req('options', Url, Headers, [], []).

-spec options(kz_term:text(), headers(), kz_term:proplist()) -> ret().
options(Url, Headers, Options) ->
    req('options', Url, Headers, [], Options).

-spec head(kz_term:text()) -> ret().
head(Url) ->
    req('head', Url, [], [], []).

-spec head(kz_term:text(), headers()) -> ret().
head(Url, Headers) ->
    req('head', Url, Headers, [], []).

-spec head(kz_term:text(), headers(), kz_term:proplist()) -> ret().
head(Url, Headers, Options) ->
    req('head', Url, Headers, [], Options).

-spec trace(kz_term:text()) -> ret().
trace(Url) ->
    req('trace', Url, [], [], []).

-spec trace(kz_term:text(), headers()) -> ret().
trace(Url, Headers) ->
    req('trace', Url, Headers, [], []).

-spec trace(kz_term:text(), headers(), kz_term:proplist()) -> ret().
trace(Url, Headers, Options) ->
    req('trace', Url, Headers, [], Options).

-spec delete(kz_term:text()) -> ret().
delete(Url) ->
    req('delete', Url, [], [], []).

-spec delete(kz_term:text(), headers()) -> ret().
delete(Url, Headers) ->
    req('delete', Url, Headers, [], []).

-spec delete(kz_term:text(), headers(), http_body()) -> ret().
delete(Url, Headers, Body) ->
    req('delete', Url, Headers, Body, []).

-spec delete(kz_term:text(), headers(), http_body(), kz_term:proplist()) -> ret().
delete(Url, Headers, Body, Options) ->
    req('delete', Url, Headers, Body, Options).

-spec post(kz_term:text()) -> ret().
post(Url) ->
    req('post', Url, [], [], []).

-spec post(kz_term:text(), headers()) -> ret().
post(Url, Headers) ->
    req('post', Url, Headers, [], []).

-spec post(kz_term:text(), headers(), http_body()) -> ret().
post(Url, Headers, Body) ->
    req('post', Url, Headers, Body, []).

-spec post(kz_term:text(), headers(), http_body(), kz_term:proplist()) -> ret().
post(Url, Headers, Body, Options) ->
    req('post', Url, Headers, Body, Options).

-spec patch(kz_term:text()) -> ret().
patch(Url) ->
    req('patch', Url, [], [], []).

-spec patch(kz_term:text(), headers()) -> ret().
patch(Url, Headers) ->
    req('patch', Url, Headers, [], []).

-spec patch(kz_term:text(), headers(), http_body()) -> ret().
patch(Url, Headers, Body) ->
    req('patch', Url, Headers, Body, []).

-spec patch(kz_term:text(), headers(), http_body(), kz_term:proplist()) -> ret().
patch(Url, Headers, Body, Options) ->
    req('patch', Url, Headers, Body, Options).

-spec put(kz_term:text()) -> ret().
put(Url) ->
    req('put', Url, [], [], []).

-spec put(kz_term:text(), headers()) -> ret().
put(Url, Headers) ->
    req('put', Url, Headers, [], []).

-spec put(kz_term:text(), headers(), http_body()) -> ret().
put(Url, Headers, Body) ->
    req('put', Url, Headers, Body, []).

-spec put(kz_term:text(), headers(), http_body(), kz_term:proplist()) -> ret().
put(Url, Headers, Body, Options) ->
    req('put', Url, Headers, Body, Options).

%%------------------------------------------------------------------------------
%% @doc Send a synchronous HTTP request.
%% @end
%%------------------------------------------------------------------------------

-spec req(kz_term:text()) -> ret().
req(Url) ->
    req('get', Url, [], [], []).

-spec req(method(), kz_term:text()) -> ret().
req(Method, Url) ->
    req(Method, Url, [], [], []).

-spec req(method(), kz_term:text(), headers()) -> ret().
req(Method, Url, Headers) ->
    req(Method, Url, Headers, [], []).

-spec req(method(), kz_term:text(), headers(), http_body()) -> ret().
req(Method, Url, Headers, Body) ->
    req(Method, Url, Headers, Body, []).

-spec req(method(), kz_term:text(), headers(), http_body(), kz_term:proplist()) -> ret().
req(Method, Url, Hdrs, Body, Opts) ->
    {Headers, Options} = maybe_basic_auth(Hdrs, Opts),
    Request = build_request(Method, Url, Headers, Body),
    execute_request(Method, Request, Options).

%%------------------------------------------------------------------------------
%% @doc Send an asynchronous HTTP request.
%% @end
%%------------------------------------------------------------------------------

-spec async_req(pid(), kz_term:text()) -> ret().
async_req(Pid, Url) ->
    async_req(Pid, 'get', Url, [], [], []).

-spec async_req(pid(), method(), kz_term:text()) -> ret().
async_req(Pid, Method, Url) ->
    async_req(Pid, Method, Url, [], [], []).

-spec async_req(pid(), method(), kz_term:text(), headers()) -> ret().
async_req(Pid, Method, Url, Headers) ->
    async_req(Pid, Method, Url, Headers, []).

-spec async_req(pid(), method(), kz_term:text(), headers(), http_body()) -> ret().
async_req(Pid, Method, Url, Headers, Body) ->
    async_req(Pid, Method, Url, Headers, Body, []).

-spec async_req(pid(), method(), kz_term:text(), headers(), http_body(), kz_term:proplist()) -> ret().
async_req(Pid, Method, Url, Hdrs, Body, Opts) ->
    {Headers, Options} = maybe_basic_auth(Hdrs, Opts),
    Request = build_request(Method, Url, Headers, Body),
    NewOptions = [{'receiver', Pid}
                 ,{'sync', 'false'}
                 ,{'stream', 'self'}
                  | Options
                 ],
    execute_request(Method, Request, NewOptions).

%%------------------------------------------------------------------------------
%% @doc Send request using httpc and handle its response.
%% @end
%%------------------------------------------------------------------------------
-spec execute_request(method(), httpc_request(), kz_term:proplist()) -> ret().
execute_request(Method, Request, Opts) ->
    HTTPOptions = get_options(?HTTP_OPTIONS, Opts),
    Opts1 = get_options(?OPTIONS, Opts),
    Options = props:insert_value('body_format', 'binary', Opts1),
    F = fun () ->
                {Method
                ,element(?REQ_URL_INDEX, Request)
                ,catch httpc:request(Method, Request, HTTPOptions, Options)
                }
        end,
    handle_timed_response(timer:tc(F)).

%%------------------------------------------------------------------------------
%% @doc Response to caller in a proper manner.
%% @end
%%------------------------------------------------------------------------------
-spec handle_timed_response({pos_integer(), {method(), kz_term:text(), httpc_ret()}}) -> ret().
handle_timed_response({Micros, {_Method, _Url, Resp}}) when is_integer(Micros) ->
    ElapsedMs = float_to_list(Micros / ?MILLISECONDS_IN_SECOND, [{'decimals', 2}, 'compact']),
    lager:debug("~sms: ~s ~s", [ElapsedMs, _Method, _Url]),
    handle_response(Resp).

-spec handle_response(httpc_ret()) -> ret().
handle_response({'ok', 'saved_to_file'}=Ok) -> Ok;
handle_response({'ok', ReqId})
  when is_reference(ReqId) ->
    {'http_req_id', ReqId};
handle_response({'ok', {{_, StatusCode, _}, Headers, Body}})
  when is_integer(StatusCode) ->
    {'ok', StatusCode, Headers, Body};
handle_response({'error', 'timeout'}=Err) ->
    lager:debug("connection timeout"),
    Err;
handle_response({'EXIT', {Error,_Trace}}) ->
    lager:debug("caught EXIT ~p: ~p", [Error, _Trace]),
    {'error', Error};
handle_response({'error', {'failed_connect',[{_, _Address}, {_, _, 'nxdomain'}]}}) ->
    lager:debug("non existent domain ~p", [_Address]),
    {'error', {'failed_connect', 'nxdomain'}};
handle_response({'error', {'failed_connect',[{_, _Address}, {_, _, 'econnrefused'}]}}) ->
    lager:debug("connection refused to ~p", [_Address]),
    {'error', {'failed_connect', 'econnrefused'}};
handle_response({'error', {'malformed_url', _, Url}}) ->
    lager:debug("failed to parse URL ~p", [Url]),
    {'error', {'malformed_url', Url}};
handle_response({'error', Error}=Err) ->
    lager:debug("request failed with ~p", [Error]),
    Err.

%%------------------------------------------------------------------------------
%% @doc Build Authorization header using basic_auth option.
%% @end
%%------------------------------------------------------------------------------
-spec maybe_basic_auth(kz_term:proplist(), kz_term:proplist()) -> {kz_term:proplist(), kz_term:proplist()}.
maybe_basic_auth(Headers, Options) ->
    case props:get_value('basic_auth', Options) of
        'undefined' -> {Headers, Options};
        {Username, Password} ->
            AuthString = "Basic " ++ base64:encode_to_string(<<Username/binary, ":", Password/binary>>),
            BasicAuth = {"Authorization", AuthString},
            {[BasicAuth|Headers], props:delete('basic_auth', Options)}
    end.
%%------------------------------------------------------------------------------
%% @doc Build httpc request argument based on method.
%% @end
%%------------------------------------------------------------------------------
-spec build_request(method(), kz_term:text(), kz_term:proplist(), http_body()) -> httpc_request().
build_request(Method, Url, Headers, _Body) when Method =:= 'options';
                                                Method =:= 'get';
                                                Method =:= 'head';
                                                Method =:= 'trace' ->
    {kz_term:to_list(Url), ensure_string_headers(Headers)};
build_request(Method, Url, Headers, Body) when Method =:= 'post';
                                               Method =:= 'put';
                                               Method =:= 'patch';
                                               Method =:= 'delete' ->
    ContentType = props:get_first_defined(["Content-Type"
                                          ,"content-type"
                                          ,'content_type'
                                          ,<<"Content-Type">>
                                          ,<<"content-type">>
                                          ,<<"content_type">>
                                          ]
                                         ,Headers
                                         ,""
                                         ),
    {kz_term:to_list(Url)
    ,ensure_string_headers(Headers)
    ,kz_term:to_list(ContentType)
    ,kz_term:to_binary(Body)
    }.

ensure_string_headers(Headers) ->
    [{kz_term:to_list(K), kz_term:to_list(V)} || {K,V} <- Headers].

%%------------------------------------------------------------------------------
%% @doc Get options out of a proplist based on options type
%% Two <code>HTTP_OPTIONS</code> and <code>OPTIONS</code> macros are specify
%% which type of options should be returned.
%% @end
%%------------------------------------------------------------------------------
-spec get_options(list(), kz_term:proplist()) -> kz_term:proplist().
get_options(Type, Options) ->
    [KV || KV = {K, _V} <- Options, lists:member(K, Type)].
