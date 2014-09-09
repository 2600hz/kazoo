%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2013, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   SIPLABS, LLC (Maksim Krzhemenevskiy)
%%%-------------------------------------------------------------------
-module(kamdb_handle_rate).

-export([handle_rate_req/2
        ]).

-include("kamdb.hrl").

-define(DEVICE_DEFAULT_RATES, <<"device-default-rate-limits">>).
-define(ACCOUNT_FALLBACK, <<"fallback">>).

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
    Entity = wh_json:get_value(<<"Entity">>, JObj),
    case whapps_util:get_account_by_realm(kamdb_utils:extract_realm(Entity)) of
        {'ok', _} -> send_response(JObj);
        _ -> deny(JObj)
    end.

-spec deny(wh_json:object()) -> any().
deny(JObj) ->
    Entity = wh_json:get_value(<<"Entity">>, JObj),
    RespStub = wh_json:from_list([{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
                                  | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                                 ]),
    ServerID = wh_json:get_value(<<"Server-ID">>, JObj),
    Section = case Entity =/= kamdb_utils:extract_realm(Entity) of
                  'true' -> <<"Device">>;
                  _ -> <<"Realm">>
              end,
    DenySubObj = wh_json:from_list([{<<"Min">>, -1}
                                    ,{<<"Sec">>, -1}
                                   ]),
    DenyObj = wh_json:from_list([{Section, DenySubObj}]),
    Resp = case wh_json:is_true(<<"With-Realm">>, JObj) of
               'true' ->
                   RealmDeny = wh_json:from_list([{<<"Realm">>, DenySubObj}]),
                   wh_json:merge_jobjs(DenyObj, RealmDeny);
               _ -> DenyObj
           end,
    wapi_kamdb:publish_ratelimits_resp(ServerID, wh_json:merge_jobjs(Resp, RespStub)).

-spec send_response(wh_json:object()) -> any().
send_response(JObj) ->
    'true' = wapi_kamdb:ratelimits_req_v(JObj),
    RespStub = wh_json:from_list([{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
                                  | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                                 ]),
    ServerID = wh_json:get_value(<<"Server-ID">>, JObj),
    Name = resolve_method(wh_json:get_value(<<"Method">>, JObj)),
    Entity = wh_json:get_value(<<"Entity">>, JObj),
    Keys = binary:split(Entity, <<"@">>),
    Limits = case Keys of
                 [_, _] ->
                     case wh_json:is_true(<<"With-Realm">>, JObj) of
                         'true' -> wh_json:merge_jobjs(get_realm_limits(JObj)
                                                       ,get_user_limits(JObj)
                                                      );
                         _ -> get_user_limits(JObj)
                     end;
                 [_] -> get_realm_limits(JObj)
             end,
    Resp = wh_json:merge_jobjs(RespStub, Limits),
    JSysRates = get_sysconfig_rates(Keys, Name, wh_json:get_value(<<"With-Realm">>, JObj)),
    wapi_kamdb:publish_ratelimits_resp(ServerID, wh_json:merge_recursive([JSysRates, Resp])).

-spec get_user_limits(wh_json:object()) -> wh_json:object().
get_user_limits(JObj) ->
    Entity = wh_json:get_value(<<"Entity">>, JObj),
    [User, Realm] = binary:split(Entity, <<"@">>),
    Name = resolve_method(wh_json:get_value(<<"Method">>, JObj)),
    {'ok', UserDb} = whapps_util:get_account_by_realm(Realm),
    ViewOpts = [{'keys',[[User, Name]
                         ,[?DEVICE_DEFAULT_RATES, Name]
                        ]}],
    {'ok', Results} = couch_mgr:get_results(UserDb, <<"rate_limits/crossbar_listing">>, ViewOpts),
    lists:foldl(fun inject_device_rate_limits/2, wh_json:new(), Results).

-spec get_realm_limits(wh_json:object()) -> wh_json:object().
get_realm_limits(JObj) ->
    Entity = wh_json:get_value(<<"Entity">>, JObj),
    Realm = kamdb_utils:extract_realm(Entity),
    Name = resolve_method(wh_json:get_value(<<"Method">>, JObj)),
    ViewOpts = [{'keys',[[Realm, Name]
                         ,[Realm, ?ACCOUNT_FALLBACK]
                        ]}],
    {'ok', Results} = couch_mgr:get_results(<<"accounts">>, <<"rate_limits/crossbar_listing">>, ViewOpts),
    {Rates, MaybeFallback} = lists:partition(fun is_rate_limit/1, Results),
    RespCandidate = lists:foldl(fun (Rate, Acc) -> inject(wh_json:get_value(<<"value">>, Rate), Acc) end, wh_json:new(), Rates),
    Parents = case MaybeFallback of
                  [Fallback] -> wh_json:get_value([<<"value">>, <<"parents">>], Fallback);
                  [] -> []
              end,
    maybe_run_through_fallbacks(RespCandidate, lists:reverse(Parents), Name).

-spec maybe_run_through_fallbacks(wh_json:object(), list(), ne_binary()) -> wh_json:object().
maybe_run_through_fallbacks(JObj, Parents, Name) ->
    case wh_json:get_integer_value([<<"Realm">>, <<"Min">>], JObj) =:= 'undefined'
        orelse wh_json:get_integer_value([<<"Realm">>, <<"Sec">>], JObj) =:= 'undefined'
    of
        'true' -> run_through_fallbacks(JObj, Parents, Name);
        _ -> JObj
    end.

-spec run_through_fallbacks(wh_json:object(), list(), ne_binary()) -> wh_json:object().
run_through_fallbacks(JObj, Parents, Name) ->
    case lists:dropwhile(fun not_limited/1, Parents) of
        [Limiter | _] ->
            {'ok', JDoc} = couch_mgr:open_cache_doc(<<"accounts">>, Limiter),
            PerMinute = wh_json:get_value([<<"rate_limits">>, ?MINUTE, Name], JDoc),
            PerSecond = wh_json:get_value([<<"rate_limits">>, ?SECOND, Name], JDoc),
            wh_json:set_values([{[<<"Realm">>, <<"Min">>], PerMinute}
                                ,{[<<"Realm">>, <<"Sec">>], PerSecond}
                               ], JObj);
        [] -> JObj
    end.

-spec not_limited(ne_binary()) -> boolean().
not_limited(AccountId) ->
    {'ok', JDoc} = couch_mgr:open_cache_doc(<<"accounts">>, AccountId),
    wh_json:get_value(<<"rate_limits">>, JDoc) =/= 'undefuned'.

-spec is_rate_limit(wh_json:object()) -> boolean().
is_rate_limit(JObj) ->
    [_, Type] = wh_json:get_value(<<"key">>, JObj),
    Type =/= ?ACCOUNT_FALLBACK.

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

-spec inject_device_rate_limits(wh_json:object(), wh_json:object()) -> wh_json:object().
inject_device_rate_limits(Result, Acc) ->
    [Name, _] = wh_json:get_value(<<"key">>, Result),
    JVal = wh_json:get_value(<<"value">>, Result),
    case Name of
        ?DEVICE_DEFAULT_RATES -> inject_if_not_exists(JVal, Acc);
        _ -> inject(JVal, Acc)
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
