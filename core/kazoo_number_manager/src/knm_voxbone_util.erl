%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2019, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(knm_voxbone_util).

-export([api_uri/0
        ,build_uri/2
        ,required_params/0
        ,to_voxbone_pattern/1
        ,voxbone_request/3
        ,voxbone_request/4
        ,purge_carts/0
        ]
       ).

-include("knm_voxbone.hrl").

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec api_uri() -> kz_term:ne_binary().
api_uri() ->
    <<"https://",(environment_url(?VOXBONE_ENVIRONMENT))/binary,"/ws-voxbone/services/rest">>.

%%------------------------------------------------------------------------------
%% @doc generate
%% @end
%%------------------------------------------------------------------------------
-spec auth() -> {'basic_auth', {kz_term:ne_binary(), kz_term:ne_binary()}}.
auth() ->
    {'basic_auth', {?VOXBONE_API_USERNAME, ?VOXBONE_API_PASSWORD}}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec build_uri(kz_term:ne_binaries(), qs_options()) -> kz_term:ne_binary().
build_uri(Resource, QueryString) ->
    QueryString1 = kz_term:to_binary(kz_http_util:props_to_querystring(QueryString)),
    <<(api_uri())/binary,"/",Resource/binary,"?",QueryString1/binary>>.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec default_headers() -> kz_term:proplist().
default_headers() ->
    [{"User-Agent", ?KNM_USER_AGENT}
    ,{"Accept", "application/json"}
    ,{"Content-Type", "application/json"}
    ].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
default_http_options() ->
    [auth()
    ,{'ssl', [{'verify', 'verify_none'}]}
    ,{'timeout', 180 * ?MILLISECONDS_IN_SECOND}
    ,{'connect_timeout', 180 * ?MILLISECONDS_IN_SECOND}
    ,{'body_format', 'string'}
    ].

%%------------------------------------------------------------------------------
%% @doc resolve the correct api host based on the configured environment
%% @end
%%------------------------------------------------------------------------------
-spec environment_url(kz_term:ne_binary()) -> kz_term:ne_binary().
environment_url(<<"production">>) -> ?VOXBONE_PRODUCTION_HOST;
environment_url(<<"sandbox">>) -> ?VOXBONE_SANDBOX_HOST;
environment_url(<<"beta">>) -> ?VOXBONE_BETA_HOST.

%%------------------------------------------------------------------------------
%% @doc format to voxbone wildcard pattern
%% @end
%%------------------------------------------------------------------------------
                                                %TODO add  prefix format
-spec to_voxbone_pattern(knm_phone_number:number()) -> kz_term:ne_binary().
to_voxbone_pattern(Number) ->
    <<"%",(knm_phone_number:number(Number))>>.

%%------------------------------------------------------------------------------
%% @doc sets required querystring params for api requests
%% @end
%%------------------------------------------------------------------------------
-spec required_params() -> qs_options().
required_params() ->
    [{'pageNumber', 0}
    ,{'pageSize', ?VOXBONE_API_PAGE_SIZE}
    ,{'showEmpty', false}
    ].

%%------------------------------------------------------------------------------
%% @doc generic abstraction for voxbone api requests to map to kz_http:req/5
%% @end
%%------------------------------------------------------------------------------
-spec voxbone_request(kz_term:atom(), kz_term:ne_binary(), kz_term:proplist()) -> {'ok', kz_json:object()} | {'error', kz_term:atom()}.
voxbone_request(Method, Resource, QueryString) ->
    voxbone_request(Method, Resource, QueryString, []).

-spec voxbone_request(kz_term:atom(), kz_term:ne_binary(), kz_term:proplist(), kz_json:object()) -> {'ok', kz_json:object()} | {'error', kz_term:atom()}.
voxbone_request(Method, Resource, QueryString, Body) ->
    URI = build_uri(Resource, QueryString),
    lager:debug("[~s] issuing ~p to ~p", [?MODULE, Method, URI]),
    Response = kz_http:req(Method, URI, default_headers(), Body, default_http_options()),
    handle_response(Response).

%%------------------------------------------------------------------------------
%% @doc utilties to clean up orphaned carts.
%% @end
%%------------------------------------------------------------------------------
-spec purge_carts() -> 'ok'.
purge_carts() ->
    purge_carts(?VOXBONE_ENVIRONMENT).

-spec purge_carts(kz_term:ne_binary()) -> 'ok'.
purge_carts(<<"sandbox">>) ->
    {'ok', JObj} = voxbone_request('get', <<"ordering/cart">>, required_params()),
    Carts = kz_json:get_value(<<"carts">>, JObj),
    maybe_delete_cart(Carts);
purge_carts(Environment) ->
    lager:debug("cart purge disabled for ~p.", [Environment]),
    'ok'.

-spec maybe_delete_cart(kz_json:objects()) -> 'ok'.
maybe_delete_cart([]) ->
    'ok';
maybe_delete_cart([Cart | Rest]) ->
    CartId = kz_json:get_value(<<"cartIdentifier">>, Cart),
    lager:debug("cleaning up cart ~p", [CartId]),
    voxbone_request('delete', <<"ordering/cart/",(kz_term:to_binary(CartId))/binary>>, [], []),
    maybe_delete_cart(Rest).

%%------------------------------------------------------------------------------
%% @doc handle api request response body or error
%% @end
%%------------------------------------------------------------------------------
-spec handle_response(kz_http:ret()) -> {'ok', kz_json:object()} | {'error', any()}.
handle_response({Result, Code, Props, Response})
  when is_binary(Response) ->
    handle_response({Result, Code, Props, kz_term:to_list(Response)});
handle_response({'ok', 400, _Props, Response}) ->
    parse_error(Response),
    {'error', 'bad_request'};
handle_response({'ok', 401, _Props, Response}) ->
    parse_error(Response),
    {'error', 'authentication'};
handle_response({'ok', 403, _Props, Response}) ->
    parse_error(Response),
    {'error', 'authorization'};
handle_response({'ok', 404, _Props, _Response}) ->
    {'error', 'not_found'};
handle_response({'ok', 405, _Props, Response}) ->
    parse_error(Response),
    {'error', 'method_not_allowed'};
handle_response({'ok', 429, _Props, Response}) ->
    parse_error(Response),
    {'error', 'too_many_requests'};
handle_response({'ok', 509, _Props, Response}) ->
    parse_error(Response),
    {'error', 'bandwidth_exceeded'};
handle_response({'ok', Code, _Props, Response}) ->
    lager:debug("voxbone api request completed with ~p.", [Code]),
    {'ok', kz_json:decode(Response)};
handle_response({'error', _}=Error) ->
    lager:debug("voxbone request error: ~p", [Error]),
    Error.

%%------------------------------------------------------------------------------
%% @doc pretty print lager message with HTTP Code and Voxbone API error message
%% @end
%%------------------------------------------------------------------------------
-spec parse_error(kz_term:text()) -> none().
parse_error(Response) ->
    Body = kz_json:decode(Response),
    ErrorBlock = kz_json:get_value(<<"errors">>, Body),
    Error = kz_json:get_value(<<"1">>, ErrorBlock),
    lager:debug("voxbone api request returned ~p: ~s", [kz_json:get_value(<<"httpStatusCode">>, Body), kz_json:get_value(<<"apiErrorMessage">>, Error)]).
