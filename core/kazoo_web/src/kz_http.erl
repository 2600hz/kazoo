%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, 2600Hz
%%% @doc
%%% Kazoo HTTP client
%%% @end
%%% @contributors
%%%   Hesaam Farhang
%%%-------------------------------------------------------------------
-module(kz_http).

-export([req/3, req/4, req/5
         ,async_req/4, async_req/5, async_req/6
         ,handle_response/1
        ]).

-include_lib("whistle/include/wh_types.hrl").

-define(HTTP_OPTIONS, [timeout, connect_timeout, ssl
                       ,essl, autoredirect, proxy_auth
                       ,version, relaxed, url_encode]).
-define(OPTIONS, [sync, stream, body_format
                  ,full_result , headers_as_is, socket_opts
                  ,receiver, ipv6_host_with_brackets]).

-type httpc_result() :: {term(), wh_proplist(), string() | binary()} |
                        {string(), string() |binary()} |
                        reference().
-type httpc_ret() :: {ok, httpc_result()} |
                     {ok, saved_to_file} |
                     {error, {connect_failed, term()} | {send_failed, term()} | term()}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Send a synchronous HTTP request
%% @end
%%--------------------------------------------------------------------
-spec req(atom(), string(), wh_proplist()) -> kz_http_ret().
-spec req(atom(), string(), wh_proplist(), wh_proplist()) -> kz_http_ret().
-spec req(atom(), string(), wh_proplist(), wh_proplist(), string() | binary()) -> kz_http_ret().
req(Method, Url, Headers) ->
    req(Method, Url, Headers, [], []).
req(Method, Url, Headers, Options) ->
    req(Method, Url, Headers, Options, []).
req(Method, Url, Hdrs, Opts, Body) ->
    {Headers, Options} = maybe_basic_auth(Hdrs, Opts),
    Request = build_request(Method, Url, Headers, Body),

    execute_request(Method, Request, Options).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Send a asynchronous HTTP request
%% @end
%%--------------------------------------------------------------------
-spec async_req(pid(), atom(), string(), wh_proplist()) -> kz_http_ret().
-spec async_req(pid(), atom(), string(), wh_proplist(), wh_proplist()) -> kz_http_ret().
-spec async_req(pid(), atom(), string(), wh_proplist(), wh_proplist(), string() | binary()) -> kz_http_ret().
async_req(Pid, Method, Url, Headers) ->
    async_req(Pid, Method, Url, Headers, []).
async_req(Pid, Method, Url, Headers, Options) ->
    async_req(Pid, Method, Url, Headers, Options, []).
async_req(Pid, Method, Url, Hdrs, Opts, Body) ->
    {Headers, Options} = maybe_basic_auth(Hdrs, Opts),
    Request = build_request(Method, Url, Headers, Body),

    execute_request(Method, Request, [{receiver, Pid}, {sync, false}, {stream, self} | Options]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec execute_request(atom(), tuple(), wh_proplist()) -> kz_http_ret().
execute_request(Method, Request, Opts) ->
    HTTPOptions = get_options(?HTTP_OPTIONS, Opts),
    Options = get_options(?OPTIONS, Opts),
    handle_response(catch httpc:request(Method, Request, HTTPOptions, Options)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Return proper response to caller
%% @end
%%--------------------------------------------------------------------
-spec handle_response(httpc_ret()) -> kz_http_ret().
handle_response(Resp) ->
    case Resp of
        {'ok', 'saved_to_file'} ->
            {'ok', 'saved_to_file'};
        {'ok', ReqId} when is_reference(ReqId) ->
            {'http_req_id', ReqId};
        {'ok', {{_, StatusCode, _}, Headers, Body}} ->
            {'ok', StatusCode, Headers, Body};
        {'error', {Error, [_, {_, _, Reason}]}} ->
            {'error', {Error, {'error', Reason}}};
        {'error', {E, R1, R2}} ->
            {'error', {E, {R1, R2}}};
        {'error', R} ->
            {'error', R}
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec maybe_basic_auth(wh_proplist(), wh_proplist()) -> wh_proplist().
maybe_basic_auth(Headers, Options) ->
    case props:get_value("basic_auth", Options) of
        'undefined' -> {Headers, Options};
        {Username, Password} ->
            BasicAuth = {"Authorization"
                         , "Basic " ++ base64:encode_to_string(<<Username/binary, ":", Password/binary>>)
                        },
            {[BasicAuth | Headers], props:delete("basic_auth", Options)}
    end.
%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec build_request(atom(), string(), wh_proplist(), string() | binary())
                        -> {string(), wh_proplist()} |
                           {string(), wh_proplist(), string(), string() | binary()}.
build_request(Method, Url, Headers, _Body) when (Method =:= options) orelse
                                                (Method =:= get) orelse
                                                (Method =:= head) orelse
                                                (Method =:= trace) ->
    {Url, Headers};
build_request(Method, Url, Headers, Body) when (Method =:= post) orelse
                                               (Method =:= put) orelse
                                               (Method =:= delete) ->
    ContentType = case props:get_value("Content-Type", Headers) of
                      'undefined' -> [];
                      C -> C
                  end,
    {Url, Headers, ContentType, Body}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get_options(list(), wh_proplist()) -> wh_proplist().
get_options(Type, Options) ->
    [{K, V} || {K, V} <- Options, lists:member(K, Type)].


