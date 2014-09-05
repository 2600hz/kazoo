%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2013, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   SIPLABS, LLC (Maksim Krzhemenevskiy)
%%%-------------------------------------------------------------------
-module(kamdb_handlers).

-export([handle_rate_req/2
        ]).

-include("kamdb.hrl").

-define(DEFAULT_RATES, <<"device-default-rate-limits">>).
-define(MINUTE, <<"per_minute">>).
-define(SECOND, <<"per_second">>).

-spec method_to_name() -> wh_proplist().
method_to_name() ->
    [{<<"REGISTER">>, <<"registrations">>}
     ,{<<"INVITE">>, <<"invites">>}
     ,{<<"TOTAL">>, <<"total">>}
    ].

-spec resolve_method(ne_binary()) -> ne_binary().
resolve_method(Method) ->
    props:get_value(Method, method_to_name()).

-spec handle_rate_req(wh_json:object(), wh_proplist()) -> any().
handle_rate_req(JObj, _Props) ->
    _ = get_ratelimits(JObj).

-spec get_ratelimits(wh_json:object()) -> list().
get_ratelimits(JObj) ->
    'true' = wapi_kamdb:ratelimits_req_v(JObj),
    RespStub = wh_json:from_list([{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
                                  | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                                 ]),
    ServerID = wh_json:get_value(<<"Server-ID">>, JObj),
    Entity = wh_json:get_value(<<"Entity">>, JObj),
    Method = wh_json:get_value(<<"Method">>, JObj),
    Keys = binary:split(Entity, <<"@">>),
    {Realm, ViewOpts} = case Keys of
                            [User, OnRealm] ->
                                lager:info("Lookup for ~s@~s", [User, OnRealm]),
                                V = case wh_json:is_true(<<"With-Realm">>, JObj) of
                                        'true' ->
                                            lager:info("Lookup ~s", [OnRealm]),
                                            [{'keys',[[OnRealm, resolve_method(Method)]
                                                      ,[?DEFAULT_RATES, resolve_method(Method)]
                                                      ,[User, resolve_method(Method)]
                                                     ]}];
                                        _ ->
                                            [{'keys',[[?DEFAULT_RATES, resolve_method(Method)]
                                                      ,[User, resolve_method(Method)]
                                                     ]}]
                                    end,
                                {OnRealm, V};
                            [JustRealm] ->
                                V = [{'key',[JustRealm, resolve_method(Method)]}],
                                {JustRealm, V}
                        end,
    DBase = get_dbase(Realm),
    {'ok', Results} = couch_mgr:get_results(DBase, <<"rate_limits/crossbar_listing">>, ViewOpts),
    Resp = lists:foldl(fun inject_rate_limits/2, RespStub, Results),
    wapi_kamdb:publish_ratelimits_resp(ServerID, Resp).

-spec get_dbase(ne_binary()) -> {ne_binary(), ne_binary()}.
get_dbase(Realm) ->
    lager:info(Realm),
    ViewOpts = [{'key', Realm}],
    {'ok', [JObj]} = couch_mgr:get_results(<<"accounts">>, <<"accounts/listing_by_realm">>, ViewOpts),
    wh_json:get_value([<<"value">>, <<"account_db">>], JObj).

-spec inject_rate_limits(wh_json:object(), wh_json:object()) -> wh_json:object().
inject_rate_limits(Result, Acc) ->
    [Name, _] = wh_json:get_value(<<"key">>, Result),
    JVal = wh_json:get_value(<<"value">>, Result),
    case wh_json:get_value(<<"type">>, JVal) of
        <<"device">> ->
            case Name of
                ?DEFAULT_RATES -> inject_if_not_exists(JVal, Acc);
                _ -> inject(JVal, Acc)
            end;
        <<"account">> -> inject(JVal, Acc)
    end.

-spec get_key_value(wh_json:object()) -> {wh_json:key(), term()}.
get_key_value(JVal) ->
    Type = case wh_json:get_value(<<"type">>, JVal) of
               <<"account">> -> <<"Realm">>;
               <<"device">> -> <<"Device">>
           end,
    {Period, Time} = case wh_json:get_value(?MINUTE, JVal) of
                         'undefined' -> {<<"Sec">>, wh_json:get_value(?SECOND, JVal)};
                         T -> {<<"Min">>, T}
                     end,
    {[Type, Period], Time}.

-spec inject(wh_json:object(), wh_json:object()) -> wh_json:object().
inject(JVal, Acc) ->
    {K, V} = get_key_value(JVal),
    wh_json:set_value(K, V, Acc).

-spec inject_if_not_exists(wh_json:object(), wh_json:object()) -> wh_json:object().
inject_if_not_exists(JVal, Acc) ->
    {K, V} = get_key_value(JVal),
    case wh_json:get_value(K, Acc) of
        'undefined' -> wh_json:set_value(K, V, Acc);
        _ -> Acc
    end.
