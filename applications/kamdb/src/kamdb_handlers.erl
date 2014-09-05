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

-define(DEVICE_DEFAULT_RATES, <<"device-default-rate-limits">>).
-define(ACCOUNT_DEFAULT_RATES, <<"account-default-rate-limits">>).

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

-spec get_reqest_info(ne_binaries(), ne_binary(), boolean()) -> {ne_binary(), wh_proplist()}.
get_reqest_info(Keys, Name, WithRealm) ->
    case Keys of
        [User, OnRealm] ->
            lager:info("Lookup for ~s@~s", [User, OnRealm]),
            V = case wh_util:is_true(WithRealm) of
                    'true' ->
                        lager:info("Lookup ~s", [OnRealm]),
                        [{'keys', [[OnRealm, Name]
                            , [?DEVICE_DEFAULT_RATES, Name]
                            , [User, resolve_method(Name)]
                        ]}];
                    _ ->
                        [{'keys', [[?DEVICE_DEFAULT_RATES, Name]
                            , [User, Name]
                        ]}]
                end,
            {OnRealm, V};
        [JustRealm] ->
            V = [{'key', [JustRealm, Name]}],
            {JustRealm, V}
    end.

-spec get_sysconfig_rates(ne_binaries(), ne_binary(), boolean()) -> wh_json:object().
get_sysconfig_rates(Keys, Name, WithRealm) ->
    Limits = whapps_config:get(?APP_NAME, <<"rate_limits">>),
    case Keys of
        [_, _] ->
            DeviceMinutes = wh_json:get_value([<<"device">>, ?MINUTE, Name], Limits),
            DeviceSeconds = wh_json:get_value([<<"device">>, ?SECOND, Name], Limits),
            JObj = wh_json:set_value(<<"Device">>
                                     ,wh_json:from_list([{<<"Min">>, DeviceMinutes}
                                                         ,{<<"Sec">>, DeviceSeconds}
                                                        ])
                                     ,wh_json:new()
                                    ),
            case wh_util:is_true(WithRealm) of
               'true' ->
                   AccountMinutes = wh_json:get_value([<<"account">>, ?MINUTE, Name], Limits),
                   AccountSeconds = wh_json:get_value([<<"account">>, ?SECOND, Name], Limits),
                   wh_json:set_value(<<"Realm">>
                                           ,wh_json:from_list([{<<"Min">>, AccountMinutes}
                                                               ,{<<"Sec">>, AccountSeconds}
                                                              ])
                                           ,JObj
                                          );
                _ -> JObj
            end;
        [_] ->
            AccountMinutes = wh_json:get_value([<<"account">>, ?MINUTE, Name], Limits),
            AccountSeconds = wh_json:get_value([<<"account">>, ?SECOND, Name], Limits),
            wh_json:set_value(<<"Realm">>
                              ,wh_json:from_list([{<<"Min">>, AccountMinutes}
                                                  ,{<<"Sec">>, AccountSeconds}
                                                 ])
                              ,wh_json:new()
                             )
    end.

-spec get_ratelimits(wh_json:object()) -> list().
get_ratelimits(JObj) ->
    'true' = wapi_kamdb:ratelimits_req_v(JObj),
    RespStub = wh_json:from_list([{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
                                  | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                                 ]),
    ServerID = wh_json:get_value(<<"Server-ID">>, JObj),
    Entity = wh_json:get_value(<<"Entity">>, JObj),
    Name = resolve_method(wh_json:get_value(<<"Method">>, JObj)),
    Keys = binary:split(Entity, <<"@">>),
    {Realm, ViewOpts} = get_reqest_info(Keys, Name, wh_json:get_value(<<"With-Realm">>, JObj)),
    JSysRates = get_sysconfig_rates(Keys, Name, wh_json:get_value(<<"With-Realm">>, JObj)),
    DBase = get_dbase(Realm),
    {'ok', Results} = couch_mgr:get_results(DBase, <<"rate_limits/crossbar_listing">>, ViewOpts),
    Resp = lists:foldl(fun inject_rate_limits/2, RespStub, Results),
    wapi_kamdb:publish_ratelimits_resp(ServerID, wh_json:merge_recursive([JSysRates, Resp])).

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
                ?DEVICE_DEFAULT_RATES -> inject_if_not_exists(JVal, Acc);
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
