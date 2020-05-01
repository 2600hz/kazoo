%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% @author SIPLABS, LLC (Maksim Krzhemenevskiy)
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(frontier_handle_rate).

-export([handle_rate_req/2
        ,lookup_rate_limit_records/1
        ,names/0
        ,name_to_method/1
        ]).

-include("frontier.hrl").

-define(DEVICE_DEFAULT_RATES, <<"device-default-rate-limits">>).
-define(RATES_CROSSBAR_LISTING, <<"rate_limits/crossbar_listing">>).
-define(RATES_LISTING_BY_OWNER, <<"rate_limits/list_by_owner">>).

-spec is_device_defaults(kz_json:object()) -> boolean().
is_device_defaults(JObj) ->
    IsDefaultRates = case kz_json:get_value(<<"key">>, JObj) of
                         [?DEVICE_DEFAULT_RATES, _] -> 'true';
                         _ -> 'false'
                     end,
    frontier_utils:is_device(JObj)
        andalso IsDefaultRates.

-spec names() -> kz_term:ne_binaries().
names() ->
    [<<"registrations">>
    ,<<"invites">>
    ,<<"total_packets">>
    ].

-spec methods() -> kz_term:ne_binaries().
methods() ->
    [<<"REGISTER">>
    ,<<"INVITE">>
    ,<<"TOTAL">>
    ].

-spec name_to_method(kz_term:ne_binary()) -> kz_term:ne_binary().
name_to_method(Name) ->
    Props = lists:zip(names(), methods()),
    props:get_value(Name, Props).

-spec resolve_method(kz_term:ne_binary()) -> kz_term:ne_binary().
resolve_method(Method) ->
    props:get_value(Method, lists:zip(methods(), names())).

-spec handle_rate_req(kz_json:object(), kz_term:proplist()) -> any().
handle_rate_req(JObj, _Props) ->
    Entity = kz_json:get_value(<<"Entity">>, JObj),
    IncludeRealm = kz_json:is_true(<<"With-Realm">>, JObj, 'false'),
    MethodList = lookup_methods(JObj),
    lager:debug("handle rate limits request for ~s", [Entity]),
    Limits = lookup_rate_limit_records(Entity, IncludeRealm, MethodList),
    send_response(Limits, JObj).

-spec lookup_methods(kz_json:object()) -> kz_term:api_binaries().
lookup_methods(JObj) ->
    Method = kz_json:get_value(<<"Method">>, JObj),
    MethodName = case Method of
                     'undefined' -> 'undefined';
                     _ -> [resolve_method(Method)]
                 end,
    Methods = kz_json:get_value(<<"Method-List">>, JObj),
    MethodNames = case is_list(Methods) of
                      'true' -> [resolve_method(M) || M <- Methods];
                      'false' -> []
                  end,
    case is_list(MethodName) of
        'false' -> MethodNames;
        'true' -> MethodName
    end.

-spec send_response(kz_json:object(), kz_json:object()) -> 'ok'.
send_response(Limits, Reqest) ->
    RespStub = kz_json:from_list([{<<"Msg-ID">>, kz_json:get_value(<<"Msg-ID">>, Reqest)}
                                  | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
                                 ]),
    Srv = kz_json:get_value(<<"Server-ID">>, Reqest),
    kapi_frontier:publish_ratelimits_resp(Srv, kz_json:merge_jobjs(Limits, RespStub)).


-spec lookup_rate_limit_records(kz_term:ne_binary()) -> kz_json:object().
lookup_rate_limit_records(Entity) ->
    lookup_rate_limit_records(Entity, 'true', names()).

-spec lookup_rate_limit_records(kz_term:ne_binary(), boolean(), kz_term:ne_binaries()) -> kz_json:object().
lookup_rate_limit_records(Entity, IncludeRealm, MethodList) ->
    lager:info("handle rate limit request for ~s", [Entity]),
    Realm = frontier_utils:extract_realm(Entity),
    Responses = case kapps_util:get_account_by_realm(Realm) of
                    {'ok', AccountDB} ->
                        lager:info("found realm, try to send response"),
                        run_rate_limits_query(Entity, AccountDB, IncludeRealm, MethodList);
                    _ ->
                        lager:info("can't find realm ~s. Throttle him.", [Realm]),
                        make_deny_rates(Entity, IncludeRealm, MethodList)
                end,
    lists:foldl(fun fold_responses/2, kz_json:new(), lists:flatten(Responses)).

-spec build_list_of_querynames(kz_term:ne_binary(), boolean()) -> kz_term:ne_binaries().
build_list_of_querynames(Entity, IncludeRealm) ->
    Realm = frontier_utils:extract_realm(Entity),
    Username = frontier_utils:extract_username(Entity),
    QueryName = case Realm =/= Entity of
                    'true' -> Username;
                    'false' -> Realm
                end,
    case IncludeRealm
        andalso Realm =/= Entity
    of
        'true' -> [Realm, Username];
        'false' -> [QueryName]
    end.

-spec run_rate_limits_query(kz_term:ne_binary(), kz_term:ne_binary(), boolean(), kz_term:ne_binaries()) -> kz_json:objects().
run_rate_limits_query(Entity, AccountDB, IncludeRealm, MethodList) ->
    EntityList = build_list_of_querynames(Entity, IncludeRealm),
    Rates = fetch_rates(EntityList, IncludeRealm, MethodList, AccountDB),
    Realm = frontier_utils:extract_realm(Entity),
    FromSysCOnfig =
        case Realm =/= EntityList
            andalso IncludeRealm
        of
            'true' ->
                fetch_rates_from_sys_config(Realm, <<"realm">>, MethodList)
                    ++ fetch_rates_from_sys_config(Entity, <<"device">>, MethodList);
            'false' ->
                Type = frontier_utils:get_entity_type(Entity),
                fetch_rates_from_sys_config(Entity, Type, MethodList)
        end,
    Rates ++ FromSysCOnfig.

-spec to_json_key(kz_term:ne_binary()) -> kz_term:ne_binary().
to_json_key(Token) ->
    Tokens = binary:split(Token, <<"_">>),
    kz_binary:join([kz_binary:ucfirst(T) || T <- Tokens], <<"-">>).

-spec fold_responses(kz_json:object(), kz_json:object()) -> kz_json:object().
fold_responses(Record, Acc) ->
    Type = to_json_key(kz_json:get_value([<<"value">>, <<"type">>], Record)),
    [Entity, MethodName] = kz_json:get_value(<<"key">>, Record),
    JsonMethod = name_to_method(MethodName),
    RPM = kz_json:get_value([<<"value">>, ?MINUTE], Record),
    RPS = kz_json:get_value([<<"value">>, ?SECOND], Record),
    Section = kz_json:get_value(Type, Acc, kz_json:new()),
    S1 = case RPM =/= 'undefined'
             andalso kz_json:get_value([<<"Minute">>, JsonMethod], Section)
         of
             'undefined' -> kz_json:set_value([<<"Minute">>, JsonMethod], RPM, Section);
             _ -> Section
         end,
    S2 = case RPS =/= 'undefined'
             andalso kz_json:get_value([<<"Second">>, JsonMethod], Section)
         of
             'undefined' -> kz_json:set_value([<<"Second">>, JsonMethod], RPS, S1);
             _ -> S1
         end,
    S3 = case Entity =/= ?DEVICE_DEFAULT_RATES of
             'true' -> kz_json:set_value(<<"Name">>, Entity, S2);
             'false' -> S2
         end,
    kz_json:set_value(Type, S3, Acc).

-spec make_deny_rates(kz_term:ne_binary(), boolean(), kz_term:ne_binaries()) -> kz_json:objects().
make_deny_rates(Entity, IncludeRealm, MethodList) ->
    Rates = deny_rates_for_entity(Entity, MethodList),
    Realm = frontier_utils:extract_realm(Entity),
    case Entity =/= Realm
        andalso IncludeRealm
    of
        'true' -> Rates ++ deny_rates_for_entity(Realm, MethodList);
        'false' -> Rates
    end.

%% Kamailio expects -1 for unknown entities
-spec deny_rates_for_entity(kz_term:ne_binary(), kz_term:ne_binaries()) -> kz_json:objects().
deny_rates_for_entity(Entity, MethodList) ->
    lists:flatmap(fun(Method) ->
                          construct_records(Method, Entity, -1, -1)
                  end
                 ,MethodList
                 ).

-spec construct_records(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary() | integer(), kz_term:ne_binary() | integer()) ->
          kz_json:objects().
construct_records(Method, Entity, RPM, RPS) ->
    {Name, Type} = case binary:split(Entity, <<"@">>) of
                       [User, _] -> {User, <<"device">>};
                       [Realm] -> {Realm, <<"realm">>}
                   end,

    RPMObject = kz_json:from_list([{<<"type">>, Type}
                                  ,{?MINUTE, RPM}
                                  ]),
    RPSObject = kz_json:from_list([{<<"type">>, Type}
                                  ,{?SECOND, RPS}
                                  ]),
    Record = kz_json:from_list([{<<"key">>, [Name, Method]}]),

    [kz_json:set_value(<<"value">>, JObj, Record)
     || JObj <- [RPMObject, RPSObject]
    ].

-spec section_type(kz_term:ne_binary()) -> kz_term:ne_binary().
section_type(<<"realm">>) -> <<"account">>;
section_type(<<"device">>) -> <<"device">>.

-spec fetch_rates_from_sys_config(kz_term:ne_binary() | kz_term:ne_binaries(), kz_term:ne_binary(), kz_term:ne_binaries()) ->
          kz_json:objects().
fetch_rates_from_sys_config(_, _, []) ->
    lager:info("sysconfig: Empty request - empty response"),
    [];
fetch_rates_from_sys_config(<<_/binary>> = Entity, Type, MethodList) ->
    Section = section_type(Type),
    AllRates = kapps_config:get_json(?APP_NAME, <<"rate_limits">>),
    TargetRates = kz_json:get_value(Section, AllRates, kz_json:new()),

    lists:foldl(fun(Method, Acc) ->
                        RPM = kz_json:get_value([?MINUTE, Method], TargetRates),
                        RPS = kz_json:get_value([?SECOND, Method], TargetRates),
                        case RPM =:= 'undefined'
                            orelse RPS =:= 'undefined'
                        of
                            'true' -> Acc;
                            'false' -> construct_records(Method, Entity, RPM, RPS) ++ Acc
                        end
                end, [], MethodList).

-spec fetch_rates(kz_term:ne_binaries(), boolean(), kz_term:ne_binaries(), kz_term:ne_binary()) -> kz_json:objects().
fetch_rates(_, _, [], _) ->
    lager:info("document: Empty request - empty response"),
    [];
fetch_rates(EntityList, IncludeRealm, MethodList, AccountDB) ->
    lager:info("run db query..."),
    Keys = [[E, M] || E <- [?DEVICE_DEFAULT_RATES | EntityList], M <- MethodList],
    ViewOpts = [{'keys', Keys}],
    Results = case kz_datamgr:get_results(AccountDB, ?RATES_CROSSBAR_LISTING, ViewOpts) of
                  {'ok', JObjs} ->
                      lager:info("got ~p records for entities ~p from db document", [length(JObjs), EntityList]),
                      JObjs;
                  _ ->
                      lager:info("can not get device rates from db"),
                      []
              end,
    Status = handle_db_response(Results, IncludeRealm),
    [H | _] = EntityList,
    Realm = frontier_utils:extract_realm(H),
    case Status of
        {'ok', Ret} -> Ret;
        {_, Partial} -> Partial ++ fetch_from_parents(AccountDB, MethodList, Realm)
    end.

-spec fetch_from_parents(kz_term:ne_binary(), kz_term:ne_binaries(), kz_term:ne_binary()) -> kz_json:objects().
fetch_from_parents(AccountDb, MethodList, Realm) ->
    case kzd_accounts:fetch(AccountDb) of
        {'ok', JObj} ->
            Tree = lists:reverse(kzd_accounts:tree(JObj)),
            check_fallbacks(Tree, MethodList, Realm);
        {'error', _Reason} ->
            lager:info("can't access to db: ~p", [_Reason])
    end.

-spec check_fallbacks(kz_term:ne_binaries(), kz_term:ne_binaries(), kz_term:ne_binary()) -> kz_json:objects().
check_fallbacks(Tree, MethodList, Realm) ->
    Result = lists:foldl(fun (X, Acc) -> check_fallback(X, Acc, MethodList, Realm) end, 'empty', Tree),
    case Result of
        'empty' -> [];
        _ -> Result
    end.

-spec check_fallback(kz_term:ne_binary(), atom() | kz_json:objects(), kz_term:ne_binaries(), kz_term:ne_binary()) ->
          atom() | kz_json:objects().
check_fallback(AccountId, 'empty', MethodList, Realm) ->
    AccountDB = kzs_util:format_account_db(AccountId),
    ViewOpts = [{'key', AccountId}],
    case kz_datamgr:get_results(AccountDB, ?RATES_LISTING_BY_OWNER, ViewOpts) of
        {'ok', []} -> 'empty';
        {'ok', [JObj]} ->
            Fallback = kz_doc:id(JObj),
            build_results(kz_datamgr:open_cache_doc(AccountDB, Fallback), MethodList, Realm);
        {'ok', _JObjs} ->
            lager:error("found many results, please check account rate limits for ~s", [AccountDB]),
            'empty';
        {'error', _Reason} ->
            lager:error("can't fetch data from db: ~p", [_Reason]),
            'empty'
    end;
check_fallback(_, Acc, _, _) ->
    Acc.

-type couch_ret() :: {'ok', kz_json:object()} | {'error', any()}.
-spec build_results(couch_ret(), kz_term:ne_binaries(), kz_term:ne_binary()) -> kz_json:objects().
build_results({'error', _Reason}, _, _) ->
    lager:error("can't fetch data from db: ~p", [_Reason]),
    [];
build_results({'ok', JObj}, MethodList, Realm) ->
    Limits = kz_json:get_value(<<"account">>, JObj),
    lists:foldl(fun (M, Acc) -> build(M, Acc, Limits, Realm) end,[],MethodList).

-spec build(kz_term:ne_binary(), kz_json:objects(), kz_json:object(), kz_term:ne_binary()) -> kz_json:objects().
build(Method, Acc, JObj, Realm) ->
    PerMinute = kz_json:get_value([?MINUTE, Method], JObj),
    PerSecond = kz_json:get_value([?SECOND, Method], JObj),
    case PerMinute =:= 'undefined'
        orelse PerSecond =:= 'undefined'
    of
        'true' -> Acc;
        'false' -> construct_records(Method, Realm, PerMinute, PerSecond)
    end.

-type status() :: 'ok' | 'need_account'.
-type rates_ret() :: {status(), kz_json:objects()}.

-spec handle_db_response(kz_json:objects(), boolean()) -> rates_ret().
handle_db_response(JObjs, IncludeRealm) ->
    {DefaultRates, OtherRates} = lists:partition(fun is_device_defaults/1, JObjs),
    {AccountRates, DeviceRates} = lists:partition(fun frontier_utils:is_realm/1, OtherRates),

    handle_db_response(AccountRates, DeviceRates, DefaultRates, IncludeRealm).

-spec handle_db_response(kz_json:objects(), kz_json:objects(), kz_json:objects(), boolean()) -> rates_ret().
handle_db_response(AccountRates, [], DefaultRates, IncludeRealm) ->
    lager:debug("using default rates for the device"),
    handle_db_response(AccountRates, DefaultRates, IncludeRealm);
handle_db_response(AccountRates, DeviceRates, _DefaultRates, IncludeRealm) ->
    lager:debug("found rates in the device doc"),
    handle_db_response(AccountRates, DeviceRates, IncludeRealm).

-spec handle_db_response(kz_json:objects(), kz_json:objects(), boolean()) -> rates_ret().
handle_db_response([], DeviceRates, 'true') ->
    {'need_account', DeviceRates};
handle_db_response(AccountRates, DeviceRates, _IncludeRealm) ->
    {'ok', AccountRates ++ DeviceRates}.
