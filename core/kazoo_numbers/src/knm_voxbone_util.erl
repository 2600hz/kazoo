%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
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
%% Generate the API url based on the currently configured environment.
%% @end
%%------------------------------------------------------------------------------
-spec api_uri() -> kz_term:ne_binary().
api_uri() ->
    <<"https://",(environment_url(?VOXBONE_ENVIRONMENT))/binary,"/ws-voxbone/services/rest">>.

%%------------------------------------------------------------------------------
%% @doc
%% Generate the API url based on the currently configured environment.
%% @end
%%------------------------------------------------------------------------------
-ifndef(TEST).
-spec auth() -> {'basic_auth', {kz_term:ne_binary(), kz_term:ne_binary()}}.
auth() ->
    {'basic_auth', {?VOXBONE_API_USERNAME, ?VOXBONE_API_PASSWORD}}.
-endif.

%%------------------------------------------------------------------------------
%% @doc
%% Generate default HTTP headers
%% @end
%%------------------------------------------------------------------------------
-spec build_uri(kz_term:ne_binary(), qs_options()) -> kz_term:ne_binary().
build_uri(Resource, QueryString) ->
    QueryString1 = kz_term:to_binary(kz_http_util:props_to_querystring(QueryString)),
    <<(api_uri())/binary, "/", Resource/binary,"?", QueryString1/binary>>.

%%------------------------------------------------------------------------------
%% @doc
%% Generate default HTTP headers
%% @end
%%------------------------------------------------------------------------------
-ifndef(TEST).
-spec default_headers() -> kz_term:proplist().
default_headers() ->
    [{"User-Agent", ?KNM_USER_AGENT}
    ,{"Accept", "application/json"}
    ,{"Content-Type", "application/json"}
    ].
-endif.

%%------------------------------------------------------------------------------
%% @doc
%% Generate default HTTP options for the underlying HTTP client
%% @end
%%------------------------------------------------------------------------------
-ifndef(TEST).
default_http_options() ->
    [auth()
    ,{'ssl', [{'verify', 'verify_none'}]}
    ,{'timeout', 180 * ?MILLISECONDS_IN_SECOND}
    ,{'connect_timeout', 180 * ?MILLISECONDS_IN_SECOND}
    ,{'body_format', 'string'}
    ].
-endif.

%%------------------------------------------------------------------------------
%% @doc
%% Resolve the correct api host based on the configured environment
%% @end
%%------------------------------------------------------------------------------
-spec environment_url(kz_term:ne_binary()) -> kz_term:ne_binary().
environment_url(<<"production">>) -> ?VOXBONE_PRODUCTION_HOST;
environment_url(<<"sandbox">>) -> ?VOXBONE_SANDBOX_HOST;
environment_url(<<"beta">>) -> ?VOXBONE_BETA_HOST.

%%------------------------------------------------------------------------------
%% @doc
%% Format to voxbone wildcard pattern
%% @end
%%------------------------------------------------------------------------------
-spec to_voxbone_pattern(kz_term:ne_binary()) -> kz_term:ne_binary().
to_voxbone_pattern(<<Number/binary>>) ->
    <<"%2B", Number/binary>>.

%%------------------------------------------------------------------------------
%% @doc
%% Sets required querystring params for api requests
%% @end
%%------------------------------------------------------------------------------
-spec required_params() -> qs_options().
required_params() ->
    [{'pageNumber', 0}
    ,{'pageSize', ?VOXBONE_API_PAGE_SIZE}
    ,{'showEmpty', 'false'}
    ].

%%------------------------------------------------------------------------------
%% @doc
%% Generic abstraction for voxbone api requests to map to kz_http:req/5
%% @end
%%------------------------------------------------------------------------------
-spec voxbone_request(kz_http:method(), kz_term:ne_binary(), qs_options()) -> {'ok', kz_json:object()} |
          {'error', atom()}.
-ifndef(TEST).
voxbone_request(Method, <<Resource/binary>>, QueryString) ->
    voxbone_request(Method, Resource, QueryString, []).
-else.
voxbone_request('get', <<"inventory/did">>, _QueryString) ->
    Data = knm_util:fixture("voxbone_order_reference.json"),
    handle_response({'ok', 200, [], Data});
voxbone_request('get', <<"ordering/cart/3312757/checkout">>, _) ->
    Data = knm_util:fixture("voxbone_checkout.json"),
    handle_response({'ok', 200, [], Data});
voxbone_request('get', <<"inventory/didgroup">>, _) ->
    Data = knm_util:fixture("voxbone_inventory_search_results.json"),
    handle_response({'ok', 200, [], Data});
voxbone_request('get', <<"ordering/accountbalance">>, _) ->
    Data = knm_util:fixture("voxbone_account_balance.json"),
    handle_response({'ok', 200, [], Data});
voxbone_request('delete', <<"ordering/cart/3312757">>, _) ->
    Data = knm_util:fixture("voxbone_delete_cart.json"),
    handle_response({'ok', 200, [], Data}).
-endif.

-spec voxbone_request(atom(), kz_term:ne_binary(), qs_options(), iodata()) ->
          {'ok', kz_json:object()} |
          {'error', atom()}.
-ifndef(TEST).
voxbone_request(Method, <<Resource/binary>>, QueryString, Body) ->
    URI = build_uri(Resource, QueryString),
    lager:debug("[~s] issuing ~p to ~p", [?MODULE, Method, URI]),
    Response = kz_http:req(Method, URI, default_headers(), Body, default_http_options()),
    handle_response(Response).
-else.
voxbone_request('put', <<"ordering/cart">>, _QueryString, _Body) ->
    Data = knm_util:fixture("voxbone_new_cart.json"),
    handle_response({'ok', 200, [], Data});
voxbone_request('post', <<"ordering/cart/3312757/product">>, _, _) ->
    Data = knm_util:fixture("voxbone_add_to_cart.json"),
    handle_response({'ok', 200, [], Data});
voxbone_request('post', <<"ordering/cancel">>, _, _) ->
    Data = knm_util:fixture("voxbone_disconnect.json"),
    handle_response({'ok', 200, [], Data}).
-endif.

%%------------------------------------------------------------------------------
%% @doc
%% Utilties to clean up orphaned carts.
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

%%------------------------------------------------------------------------------
%% @doc
%% Issue cart deletion API request
%% @end
%%------------------------------------------------------------------------------
-spec maybe_delete_cart(kz_json:objects()) -> 'ok'.
maybe_delete_cart([]) -> 'ok';
maybe_delete_cart([Cart | Rest]) ->
    CartId = kz_json:get_value(<<"cartIdentifier">>, Cart),
    lager:debug("cleaning up cart ~p", [CartId]),
    _ = voxbone_request('delete', <<"ordering/cart/",(kz_term:to_binary(CartId))/binary>>, [], []),
    maybe_delete_cart(Rest).

%%------------------------------------------------------------------------------
%% @doc
%% Handle api request response body or error
%% @end
%%------------------------------------------------------------------------------
-spec handle_response(kz_http:ret()) -> {'ok', kz_json:object()} |
          {'error', atom()} .
handle_response({Result, Code, Props, Response})
  when is_binary(Response) ->
    handle_response({Result, Code, Props, kz_term:to_list(Response)});
handle_response({'ok', 400, _Props, _Response}) -> {'error', 'bad_request'};
handle_response({'ok', 401, _Props, _Response}) -> {'error', 'authentication'};
handle_response({'ok', 403, _Props, _Response}) -> {'error', 'authorization'};
handle_response({'ok', 404, _Props, _Response}) -> {'error', 'not_found'};
handle_response({'ok', 405, _Props, _Response}) -> {'error', 'method_not_allowed'};
handle_response({'ok', 429, _Props, _Response}) -> {'error', 'too_many_requests'};
handle_response({'ok', 500, _Props, _Response}) -> {'error', 'internal_server_error'};
handle_response({'ok', 509, _Props, _Response}) -> {'error', 'bandwidth_exceeded'};
handle_response({'ok', _Code, _Props, Response}) -> {'ok', kz_json:decode(Response)};
handle_response({'error', _}=Error) -> Error.
