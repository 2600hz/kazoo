%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz INC
%%% @doc
%%%
%%% A Number Manager module for carrier: telnyx.com
%%%
%%% @end
%%% @contributors
%%%   Pierre Fenoll
%%%-------------------------------------------------------------------
-module(knm_telnyx).
-behaviour(knm_gen_carrier).

-export([is_local/0]).
-export([find_numbers/3]).
-export([acquire_number/1]).
-export([disconnect_number/1]).
-export([is_number_billable/1]).
-export([should_lookup_cnam/0]).

-include("knm.hrl").

-define(MOD_CONFIG_CAT, <<(?KNM_CONFIG_CAT)/binary, ".telnyx">>).

-define(DEBUG, kapps_config:get_is_true(?MOD_CONFIG_CAT, <<"debug">>, 'false')).
-define(DEBUG_FILE, "/tmp/telnyx.json").
-define(DEBUG_WRITE(Format, Args),
        _ = ?DEBUG
        andalso file:write_file(?DEBUG_FILE, io_lib:format(Format, Args))
       ).
-define(DEBUG_APPEND(Format, Args),
        _ = ?DEBUG
        andalso file:write_file(?DEBUG_FILE, io_lib:format(Format, Args), ['append'])
       ).

-define(IS_SANDBOX_PROVISIONING_TRUE,
        kapps_config:get_is_true(?MOD_CONFIG_CAT, <<"sandbox_provisioning">>, 'false')).
-define(IS_PROVISIONING_ENABLED,
        kapps_config:get_is_true(?MOD_CONFIG_CAT, <<"enable_provisioning">>, 'true')).

-define(DOMAIN, "api.telnyx.com").
-define(URL(Path), "https://" ?DOMAIN "/origination/" ++ filename:join(Path)).

-define(SHOULD_KEEP_BEST_EFFORT,
        kapps_config:get_is_true(?MOD_CONFIG_CAT, <<"should_keep_best_effort">>, false)).

-define(USER, kapps_config:get_ne_binary(?MOD_CONFIG_CAT, <<"user">>)).
-define(TOKEN, kapps_config:get_ne_binary(?MOD_CONFIG_CAT, <<"token">>)).


%%% API

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Is this carrier handling numbers local to the system?
%% Note: a non-local (foreign) carrier module makes HTTP requests.
%% @end
%%--------------------------------------------------------------------
-spec is_local() -> boolean().
is_local() -> 'false'.

%% @public
-spec is_number_billable(knm_number:knm_number()) -> boolean().
is_number_billable(_Number) -> 'true'.


%%--------------------------------------------------------------------
%% @public
%% @doc
%% Query the system for a quantity of available numbers in a rate center
%% @end
%%--------------------------------------------------------------------
-spec find_numbers(ne_binary(), pos_integer(), knm_carriers:options()) ->
                          {'ok', knm_number:knm_numbers()} |
                          {'error', any()}.
find_numbers(<<"+1", Prefix:3/binary, _/binary>>, Quantity, Options)
  when ?IS_US_TOLLFREE(Prefix) ->
    SearchId = search_id(tollfree, Quantity, Prefix, undefined),
    {ok, numbers(SearchId, Options)};

find_numbers(<<"+1", NPA:3/binary, _/binary>>=Num, Quantity, Options) ->
    NXX = case byte_size(Num) >= 2+3+3 of
              true -> binary:part(Num, 2+3, 3);
              false -> undefined
          end,
    SearchId = search_id(npa, Quantity, NPA, NXX),
    {ok, numbers(SearchId, Options)}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Acquire a given number from the carrier
%% @end
%%--------------------------------------------------------------------
-spec acquire_number(knm_number:knm_number()) -> knm_number:knm_number().
acquire_number(Number) ->
    Debug = ?IS_SANDBOX_PROVISIONING_TRUE,
    case ?IS_PROVISIONING_ENABLED of
        'false' when Debug ->
            lager:debug("allowing sandbox provisioning"),
            Number;
        'false' ->
            knm_errors:unspecified('provisioning_disabled', Number);
        'true' ->
            PhoneNumber = knm_number:phone_number(Number),
            Num = knm_phone_number:number(PhoneNumber),
            Req = kz_json:from_list([{<<"requested_numbers">>, [Num]}
                                    ]),
            Rep = req(post, ["number_orders"], Req),
            case kz_json:get_ne_binary_value(<<"id">>, Rep) of
                undefined ->
                    lager:debug("order failure: ~s", [kz_json:encode(Rep)]),
                    Reason = kz_json:get_ne_binary_value(<<"message">>, Rep),
                    knm_errors:by_carrier(?MODULE, Reason, Num);
                OrderId ->
                    Data = kz_json:from_list([{<<"order_id">>, OrderId}]),
                    PN = knm_phone_number:update_carrier_data(PhoneNumber, Data),
                    knm_number:set_phone_number(Number, PN)
            end
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Release a number from the routing table
%% @end
%%--------------------------------------------------------------------
-spec disconnect_number(knm_number:knm_number()) ->
                               knm_number:knm_number().
disconnect_number(Number) ->
    Debug = ?IS_SANDBOX_PROVISIONING_TRUE,
    case ?IS_PROVISIONING_ENABLED of
        'false' when Debug ->
            lager:debug("allowing sandbox provisioning"),
            Number;
        'false' ->
            knm_errors:unspecified('provisioning_disabled', Number);
        'true' ->
            Number
    end.

%% @public
-spec should_lookup_cnam() -> boolean().
should_lookup_cnam() -> 'true'.


%%% Internals

-spec search_id(npa | tollfree, pos_integer(), ne_binary(), api_ne_binary()) -> nonempty_string().
search_id(SearchKind, Quantity, Prefix, NXX) ->
    Descriptor = kz_json:from_list(search_prefix(SearchKind, Prefix, NXX)),
    SearchIdReq = kz_json:from_list(
                    [{<<"search_type">>, search_kind(SearchKind)}
                    ,{<<"search_descriptor">>, Descriptor}
                    ,{<<"limit">>, Quantity}
                    ]),
    SearchIdRep = req(post, ["number_searches"], SearchIdReq),
    SearchId = kz_json:get_ne_binary_value(<<"id">>, SearchIdRep),
    binary_to_list(SearchId).

-spec numbers(nonempty_string(), knm_carriers:options()) -> knm_number:knm_numbers().
numbers(SearchId, Options) ->
    Path = ["number_searches", SearchId],
    AccountId = knm_carriers:account_id(Options),
    [Number
     || Data <- kz_json:get_value(<<"result">>, req(get, Path)),
        Num <- [kz_json:get_ne_binary_value(<<"number_e164">>, Data)],
        {ok, Number} <- [knm_carriers:create_found(Num, ?MODULE, AccountId, Data)]
    ].

search_kind(npa) -> 1;
search_kind(tollfree) -> 3.

search_prefix(tollfree, Prefix, _) ->
    [{<<"prefix">>, Prefix}];
search_prefix(npa, NPA, undefined) ->
    [{<<"npa">>, NPA}];
search_prefix(npa, NPA, NXX) ->
    [{<<"nxx">>, NXX}
     |search_prefix(npa, NPA, undefined)
    ].

-spec req(atom(), [nonempty_string()]) -> kz_json:object().
-spec req(atom(), [nonempty_string()], kz_json:object()) -> kz_json:object().
req(Method, Path) ->
    req(Method, Path, kz_json:new()).

-ifdef(TEST).
req(post, ["number_searches"], JObj) ->
    case kz_json:get_value([<<"search_descriptor">>, <<"prefix">>], JObj) of
        <<"800">> -> rep_fixture("telnyx_tollfree_search_12.json");
        _ -> rep_fixture("telnyx_npa_search_12.json")
    end;
req(get, ["number_searches", "411384989406463698"], _) ->
    rep_fixture("telnyx_tollfree_search_22.json");
req(get, ["number_searches", "411381763818915536"], _) ->
    rep_fixture("telnyx_npa_search_22.json");
req(post, ["number_orders"], _) ->
    rep_fixture("telnyx_order.json").

rep_fixture(Fixture) ->
    rep({ok, 200, [], list_to_binary(knm_util:fixture(Fixture))}).

-else.
req(get=_Method, Path, EmptyJObj) ->
    Url = ?URL(Path),
    Headers = http_headers(EmptyJObj),
    ?DEBUG_APPEND("Request:~n~s ~s~n~p~n", [_Method, Url, Headers]),
    Resp = kz_http:get(Url, Headers, http_options()),
    rep(Resp);
req(post=_Method, Path, JObj) ->
    Url = ?URL(Path),
    Headers = http_headers(JObj),
    Body = kz_json:encode(JObj),
    ?DEBUG_APPEND("Request:~n~s ~s~n~p~n~s~n", [_Method, Url, Headers, Body]),
    Resp = kz_http:post(Url, Headers, Body, http_options()),
    rep(Resp).

http_headers(BodyJObj) ->
    [{"Accept", "application/json"}
    ,{"x-api-user", binary_to_list(?USER)}
    ,{"x-api-token", binary_to_list(?TOKEN)}
    ,{"User-Agent", ?KNM_USER_AGENT}
     | [{"Content-Type", "application/json"} || not kz_json:is_empty(BodyJObj)]
    ].

http_options() ->
    [{'ssl', [{'verify', 'verify_none'}]}
    ,{'timeout', 180 * ?MILLISECONDS_IN_SECOND}
    ,{'connect_timeout', 180 * ?MILLISECONDS_IN_SECOND}
    ].
-endif.

-spec rep(kz_http:ret()) -> kz_json:object().
rep({'ok', 200=Code, _Headers, <<"{",_/binary>>=Response}) ->
    ?DEBUG_APPEND("Response:~n~p~n~p~n~s~n", [Code, _Headers, Response]),
    lager:debug("received response"),
    maybe_remove_best_effort(?SHOULD_KEEP_BEST_EFFORT, kz_json:decode(Response));
rep({'ok', Code, _Headers, _Response}) ->
    ?DEBUG_APPEND("Response:~n~p~n~p~n~s~n", [Code, _Headers, _Response]),
    Reason = http_code(Code),
    lager:debug("request error: ~p (~s)", [Code, Reason]),
    knm_errors:by_carrier(?MODULE, Reason, <<>>);
rep({'error', R}=_E) ->
    lager:debug("request error: ~p", [_E]),
    knm_errors:by_carrier(?MODULE, kz_util:to_binary(R), <<>>).

-spec http_code(pos_integer()) -> atom().
http_code(400) -> 'bad_request';
http_code(401) -> 'unauthenticated';
http_code(403) -> 'unauthorized';
http_code(404) -> 'not_found';
http_code(Code) when Code >= 500 -> 'server_error';
http_code(_Code) -> 'empty_response'.

-spec maybe_remove_best_effort(boolean(), kz_json:object()) -> kz_json:object().
maybe_remove_best_effort(true, JObj) -> JObj;
maybe_remove_best_effort(false, JObj) ->
    case kz_json:is_true(<<"any_best_effort">>, JObj) of
        false -> JObj;
        true ->
            Results = [Result
                       || Result <- kz_json:get_value(<<"result">>, JObj, []),
                          true =/= kz_json:is_true(<<"best_effort">>, Result)
                      ],
            kz_json:set_value(<<"result">>, Results, JObj)
    end.

%%% End of Module
