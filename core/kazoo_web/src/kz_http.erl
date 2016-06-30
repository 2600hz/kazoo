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

-include("kz_web.hrl").

-type method() :: 'delete' |
                  'get' |
                  'head' |
                  'options' |
                  'post' |
                  'put' |
                  'trace'.

-type http_body() :: string() | binary().

-type httpc_result() :: {any(), kz_proplist(), http_body()} |
                        {string(), string() | binary()} |
                        reference().

-type httpc_ret() :: {'ok', httpc_result()} |
                     {'ok', 'saved_to_file'} |
                     {'error', {'connect_failed', any()} |
                      {'send_failed', any()} |
                      any()
                     }.

-type httpc_request() :: {string(), kz_proplist()} |
                         {string(), kz_proplist(), string(), http_body()}.

-type req_id() :: {'http_req_id', reference()} |
                  {'ok', reference()} |
                  reference().

-type ret() :: {'ok', pos_integer(), kz_proplist(), string() | binary()} |
               {'ok', 'saved_to_file'} |
               {'error', any()} |
               req_id().

-export_type([ret/0]).
-export_type([req_id/0]).

%%--------------------------------------------------------------------
%% @public
%% @doc Send synchronous request
%%--------------------------------------------------------------------
-spec get(string()) -> ret().
-spec get(string(), kz_proplist()) -> ret().
-spec get(string(), kz_proplist(), kz_proplist()) -> ret().
get(Url) ->
    req('get', Url, [], [], []).
get(Url, Headers) ->
    req('get', Url, Headers, [], []).
get(Url, Headers, Options) ->
    req('get', Url, Headers, [], Options).

-spec options(string()) -> ret().
-spec options(string(), kz_proplist()) -> ret().
-spec options(string(), kz_proplist(), kz_proplist()) -> ret().
options(Url) ->
    req('options', Url, [], [], []).
options(Url, Headers) ->
    req('options', Url, Headers, [], []).
options(Url, Headers, Options) ->
    req('options', Url, Headers, [], Options).

-spec head(string()) -> ret().
-spec head(string(), kz_proplist()) -> ret().
-spec head(string(), kz_proplist(), kz_proplist()) -> ret().
head(Url) ->
    req('head', Url, [], [], []).
head(Url, Headers) ->
    req('head', Url, Headers, [], []).
head(Url, Headers, Options) ->
    req('head', Url, Headers, [], Options).

-spec trace(string()) -> ret().
-spec trace(string(), kz_proplist()) -> ret().
-spec trace(string(), kz_proplist(), kz_proplist()) -> ret().
trace(Url) ->
    req('trace', Url, [], [], []).
trace(Url, Headers) ->
    req('trace', Url, Headers, [], []).
trace(Url, Headers, Options) ->
    req('trace', Url, Headers, [], Options).

-spec delete(string()) -> ret().
-spec delete(string(), kz_proplist()) -> ret().
-spec delete(string(), kz_proplist(), http_body()) -> ret().
-spec delete(string(), kz_proplist(), http_body(), kz_proplist()) -> ret().
delete(Url) ->
    req('delete', Url, [], [], []).
delete(Url, Headers) ->
    req('delete', Url, Headers, [], []).
delete(Url, Headers, Body) ->
    req('delete', Url, Headers, Body, []).
delete(Url, Headers, Body, Options) ->
    req('delete', Url, Headers, Body, Options).

-spec post(string()) -> ret().
-spec post(string(), kz_proplist()) -> ret().
-spec post(string(), kz_proplist(), http_body()) -> ret().
-spec post(string(), kz_proplist(), http_body(), kz_proplist()) -> ret().
post(Url) ->
    req('post', Url, [], [], []).
post(Url, Headers) ->
    req('post', Url, Headers, [], []).
post(Url, Headers, Body) ->
    req('post', Url, Headers, Body, []).
post(Url, Headers, Body, Options) ->
    req('post', Url, Headers, Body, Options).

-spec put(string()) -> ret().
-spec put(string(), kz_proplist()) -> ret().
-spec put(string(), kz_proplist(), http_body()) -> ret().
-spec put(string(), kz_proplist(), http_body(), kz_proplist()) -> ret().
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
%% @doc Send a synchronous HTTP request
%%--------------------------------------------------------------------
-spec req(string()) -> ret().
-spec req(method(), string()) -> ret().
-spec req(method(), string(), kz_proplist()) -> ret().
-spec req(method(), string(), kz_proplist(), http_body()) -> ret().
-spec req(method(), string(), kz_proplist(), http_body(), kz_proplist()) -> ret().
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
%% @doc Send an asynchronous HTTP request
%%--------------------------------------------------------------------
-spec async_req(pid(), string()) -> ret().
-spec async_req(pid(), method(), string()) -> ret().
-spec async_req(pid(), method(), string(), kz_proplist()) -> ret().
-spec async_req(pid(), method(), string(), kz_proplist(), http_body()) -> ret().
-spec async_req(pid(), method(), string(), kz_proplist(), http_body(), kz_proplist()) -> ret().
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
    NewOptions = [{'receiver', Pid}
                 ,{'sync', 'false'}
                 ,{'stream', 'self'}
                  | Options
                 ],
    execute_request(Method, Request, NewOptions).

%%--------------------------------------------------------------------
%% @private
%% @doc Send request using httpc and handle its response
%%--------------------------------------------------------------------
-spec execute_request(method(), tuple(), kz_proplist()) -> ret().
execute_request(Method, Request, Opts) ->
    HTTPOptions = get_options(?HTTP_OPTIONS, Opts),
    Opts1 = get_options(?OPTIONS, Opts),
    Options = props:insert_value('body_format', 'binary', Opts1),

    handle_response(catch httpc:request(Method, Request, HTTPOptions, Options)).

%%--------------------------------------------------------------------
%% @public
%% @doc Response to caller in a proper manner
%%--------------------------------------------------------------------
-spec handle_response(httpc_ret()) -> ret().
handle_response({'ok', 'saved_to_file'}=Ok) ->
    Ok;
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

%%--------------------------------------------------------------------
%% @private
%% @doc Build Authorization header using basic_auth option
%%--------------------------------------------------------------------
-spec maybe_basic_auth(kz_proplist(), kz_proplist()) -> {kz_proplist(), kz_proplist()}.
maybe_basic_auth(Headers, Options) ->
    case props:get_value('basic_auth', Options) of
        'undefined' -> {Headers, Options};
        {Username, Password} ->
            AuthString = "Basic " ++ base64:encode_to_string(<<Username/binary, ":", Password/binary>>),
            BasicAuth = {"Authorization", AuthString},
            {[BasicAuth|Headers], props:delete('basic_auth', Options)}
    end.
%%--------------------------------------------------------------------
%% @private
%% @doc Build httpc request argument based on method
%%--------------------------------------------------------------------
-spec build_request(method(), string(), kz_proplist(), http_body()) -> httpc_request().
build_request(Method, Url, Headers, _Body) when (Method == 'options');
                                                (Method == 'get');
                                                (Method == 'head');
                                                (Method == 'trace') ->
    {kz_util:to_list(Url), ensure_string_headers(Headers)};
build_request(Method, Url, Headers, Body) when (Method == 'post');
                                               (Method == 'put');
                                               (Method == 'delete') ->
    ContentType = props:get_first_defined(["Content-Type"
                                          ,"content-type"
                                          ,'content_type'
                                          ,<<"Content-Type">>
                                          ,<<"content-type">>
                                          ,<<"content_type">>
                                          ]
                                         ,Headers
                                         ,""),
    {kz_util:to_list(Url), ensure_string_headers(Headers), kz_util:to_list(ContentType), Body}.

ensure_string_headers(Headers) ->
    [{kz_util:to_list(K), kz_util:to_list(V)} || {K,V} <- Headers].
%%--------------------------------------------------------------------
%% @private
%% @doc Get options out of a propslist based on options type
%% Two <code>HTTP_OPTIONS</code> and <code>OPTIONS</code> macros are specify
%% which type of options should be returned.
%% @end
%%--------------------------------------------------------------------
-spec get_options(list(), kz_proplist()) -> kz_proplist().
get_options(Type, Options) ->
    [KV || KV = {K, _V} <- Options, lists:member(K, Type)].
