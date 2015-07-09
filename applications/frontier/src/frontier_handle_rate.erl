%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2013, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   SIPLABS, LLC (Maksim Krzhemenevskiy)
%%%-------------------------------------------------------------------
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

-spec is_device_defaults(wh_json:object()) -> boolean().
is_device_defaults(JObj) ->
    IsDefaultRates = case wh_json:get_value(<<"key">>, JObj) of
                         [?DEVICE_DEFAULT_RATES, _] -> 'true';
                         _ -> 'false'
                     end,
    frontier_utils:is_device(JObj) andalso IsDefaultRates.

-spec names() -> ne_binaries().
names() ->
    [<<"registrations">>
     ,<<"invites">>
     ,<<"total_packets">>
    ].

-spec methods() -> ne_binaries().
methods() ->
    [<<"REGISTER">>
     ,<<"INVITE">>
     ,<<"TOTAL">>
    ].

-spec name_to_method(ne_binary()) -> ne_binary().
name_to_method(Name) ->
    Props = lists:zip(names(), methods()),
    props:get_value(Name, Props).

-spec resolve_method(ne_binary()) -> ne_binary().
resolve_method(Method) ->
    props:get_value(Method, lists:zip(methods(), names())).

-spec handle_rate_req(wh_json:object(), wh_proplist()) -> _.
handle_rate_req(JObj, _Props) ->
    Entity = wh_json:get_value(<<"Entity">>, JObj),
    IncludeRealm = wh_json:is_true(<<"With-Realm">>, JObj, 'false'),
    MethodList = lookup_methods(JObj),
    lager:debug("Handle rate limits request for ~s", [Entity]),
    Limits = lookup_rate_limit_records(Entity, IncludeRealm, MethodList),
    send_response(Limits, JObj).

-spec lookup_methods(wh_json:object()) -> api_binaries().
lookup_methods(JObj) ->
    Method = wh_json:get_value(<<"Method">>, JObj),
    MethodName = case Method of
                     'undefined' -> 'undefuned';
                     _ -> [resolve_method(Method)]
                 end,
    Methods = wh_json:get_value(<<"Method-List">>, JObj),
    MethodNames = case is_list(Methods) of
                      'true' -> lists:map(fun resolve_method/1, Methods);
                      'false' -> []
                  end,
    case is_list(MethodName) of
        'false' -> MethodNames;
        'true' -> MethodName
    end.

-spec send_response(wh_json:object(), wh_json:object()) -> 'ok'.
send_response(Limits, Reqest) ->
    RespStub = wh_json:from_list([{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, Reqest)}
                                   | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                                  ]),
    Srv = wh_json:get_value(<<"Server-ID">>, Reqest),
    wapi_frontier:publish_ratelimits_resp(Srv, wh_json:merge_jobjs(Limits, RespStub)).


-spec lookup_rate_limit_records(ne_binary()) -> wh_json:object().
lookup_rate_limit_records(Entity) ->
    lookup_rate_limit_records(Entity, 'true', names()).

-spec lookup_rate_limit_records(ne_binary(), boolean(), ne_binaries()) -> wh_json:object().
lookup_rate_limit_records(Entity, IncludeRealm, MethodList) ->
    lager:info("Handle rate limit request for ~s", [Entity]),
    Realm = frontier_utils:extract_realm(Entity),
    Responses = case whapps_util:get_account_by_realm(Realm) of
                    {'ok', AccountDB} ->
                        lager:info("Found realm, try to send response"),
                        run_rate_limits_query(Entity, AccountDB, IncludeRealm, MethodList);
                    _ ->
                        lager:info("Can't find realm ~s. Throttle him.", [Realm]),
                        make_deny_rates(Entity, IncludeRealm, MethodList)
                end,
    lists:foldl(fun fold_responses/2, wh_json:new(), lists:flatten(Responses)).

-spec build_list_of_querynames(ne_binary(), boolean()) -> ne_binaries().
build_list_of_querynames(Entity, IncludeRealm) ->
    Realm = frontier_utils:extract_realm(Entity),
    Username = frontier_utils:extract_username(Entity),
    QueryName = case Realm =/= Entity of
                    'true' -> Username;
                    'false' -> Realm
                end,
    case IncludeRealm andalso Realm =/= Entity of
        'true' -> [Realm, Username];
    'false' -> [QueryName]
    end.

-spec run_rate_limits_query(ne_binary(), ne_binary(), boolean(), ne_binaries()) -> wh_json:objects().
run_rate_limits_query(Entity, AccountDB, IncludeRealm, MethodList) ->
    EntityList = build_list_of_querynames(Entity, IncludeRealm),
    Rates = fetch_rates(EntityList, IncludeRealm, MethodList, AccountDB),
    Realm = frontier_utils:extract_realm(Entity),
    FromSysCOnfig = case Realm =/= EntityList andalso IncludeRealm of
                        'true' ->
                            fetch_rates_from_sys_config(Realm, <<"realm">>, MethodList)
                                ++ fetch_rates_from_sys_config(Entity, <<"device">>, MethodList);
                        'false' ->
                            Type = frontier_utils:get_entity_type(Entity),
                            fetch_rates_from_sys_config(Entity, Type, MethodList)
                    end,
    Rates ++ FromSysCOnfig.

-spec to_json_key(ne_binary()) -> ne_binary().
to_json_key(Token) ->
    Tokens = binary:split(Token, <<"_">>),
    wh_util:join_binary([wh_util:ucfirst_binary(T) || T <- Tokens], <<"-">>).

-spec fold_responses(wh_json:object(), wh_json:object()) -> wh_json:object().
fold_responses(Record, Acc) ->
    Type = to_json_key(wh_json:get_value([<<"value">>, <<"type">>], Record)),
    [Entity, MethodName] = wh_json:get_value(<<"key">>, Record),
    JsonMethod = name_to_method(MethodName),
    RPM = wh_json:get_value([<<"value">>, ?MINUTE], Record),
    RPS = wh_json:get_value([<<"value">>, ?SECOND], Record),
    Section = wh_json:get_value(Type, Acc, wh_json:new()),
    S1 = case RPM =/= 'undefined' andalso wh_json:get_value([<<"Minute">>, JsonMethod], Section) of
             'undefined' -> wh_json:set_value([<<"Minute">>, JsonMethod], RPM, Section);
             _ -> Section
         end,
    S2 = case RPS =/= 'undefined' andalso wh_json:get_value([<<"Second">>, JsonMethod], Section) of
             'undefined' -> wh_json:set_value([<<"Second">>, JsonMethod], RPS, S1);
             _ -> S1
         end,
    S3 = case Entity =/= ?DEVICE_DEFAULT_RATES of
             'true' -> wh_json:set_value(<<"Name">>, Entity, S2);
             'false' -> S2
         end,
    wh_json:set_value(Type, S3, Acc).

-spec make_deny_rates(ne_binary(), boolean(), ne_binaries()) -> wh_json:objects().
make_deny_rates(Entity, IncludeRealm, MethodList) ->
    Rates = deny_rates_for_entity(Entity, MethodList),
    Realm = frontier_utils:extract_realm(Entity),
    case Entity =/= Realm
         andalso IncludeRealm
    of
        'true' -> Rates ++ deny_rates_for_entity(Realm, MethodList);
        'false' -> Rates
    end.

%% Kamailio expects -1 for unkown entities
-spec deny_rates_for_entity(ne_binary(), ne_binaries()) -> wh_json:objects().
deny_rates_for_entity(Entity, MethodList) ->
    lists:flatmap(fun(Method) ->
                          construct_records(Method, Entity, -1, -1)
                  end
                  ,MethodList
                 ).

-spec construct_records(ne_binary(), ne_binary(), ne_binary() | integer(), ne_binary() | integer()) ->
                               wh_json:objects().
construct_records(Method, Entity, RPM, RPS) ->
    {Name, Type} = case binary:split(Entity, <<"@">>) of
                       [User, _] -> {User, <<"device">>};
                       [Realm] -> {Realm, <<"realm">>}
                   end,
    RPMObject = wh_json:from_list([{<<"type">>, Type}
                                   ,{?MINUTE, RPM}
                                  ]),
    RPSObject = wh_json:from_list([{<<"type">>, Type}
                                   ,{?SECOND, RPS}
                                  ]),
    Record = wh_json:from_list([{<<"id">>, 'undefined'}
                                ,{<<"key">>, [Name, Method]}
                               ]),
    lists:map(fun(JObj) ->
                      wh_json:set_value(<<"value">>, JObj, Record)
              end
              ,[RPMObject, RPSObject]
             ).

-spec section_type(ne_binary()) -> ne_binary().
section_type(<<"realm">>) -> <<"account">>;
section_type(<<"device">>) -> <<"device">>.

-spec fetch_rates_from_sys_config(ne_binary() | ne_binaries(), ne_binary(), ne_binaries()) ->
                                         wh_json:objects().
fetch_rates_from_sys_config(_, _, []) ->
    lager:info("sysconfig: Empty request - empty response"),
    [];
fetch_rates_from_sys_config(<<_/binary>> = Entity, Type, MethodList) ->
    Section = section_type(Type),
    AllRates = whapps_config:get(?APP_NAME, <<"rate_limits">>),
    TargetRates = wh_json:get_value(Section, AllRates, wh_json:new()),

    lists:foldl(fun(Method, Acc) ->
                        RPM = wh_json:get_value([?MINUTE, Method], TargetRates),
                        RPS = wh_json:get_value([?SECOND, Method], TargetRates),
                        case RPM =:= 'undefined' orelse RPS =:= 'undefined' of
                            'true' -> Acc;
                            'false' -> construct_records(Method, Entity, RPM, RPS) ++ Acc
                         end
                end, [], MethodList).

-spec fetch_rates(ne_binaries(), boolean(), ne_binaries(), ne_binary()) -> wh_json:objects().
fetch_rates(_, _, [], _) ->
    lager:info("document: Empty request - empty response"),
    [];
fetch_rates(EntityList, IncludeRealm, MethodList, AccountDB) ->
    lager:info("Run db query..."),
    Keys = [[E, M] || E <- [?DEVICE_DEFAULT_RATES | EntityList], M <- MethodList],
    ViewOpts = [{'keys', Keys}],
    Results = case couch_mgr:get_results(AccountDB, ?RATES_CROSSBAR_LISTING, ViewOpts) of
                  {'ok', JObjs} ->
                      lager:info("Got ~p records for entities ~p from db document", [length(JObjs), EntityList]),
                      JObjs;
                  _ ->
                      lager:info("Can not get device rates drom db"),
                      []
              end,
    Status = handle_db_response(Results, IncludeRealm),
    [H | _] = EntityList,
    Realm = frontier_utils:extract_realm(H),
    case Status of
        {'ok', Ret} -> Ret;
        {_, Partial} -> Partial ++ fetch_from_parents(AccountDB, MethodList, Realm)
    end.

-spec fetch_from_parents(ne_binary(), ne_binaries(), ne_binary()) -> wh_json:objects().
fetch_from_parents(AccountDb, MethodList, Realm) ->
    case kz_account:fetch(AccountDb) of
        {'ok', JObj} ->
            Tree = lists:reverse(kz_account:tree(JObj)),
            check_fallbacks(Tree, MethodList, Realm);
        {'error', _Reason} ->
            lager:info("Cant't access to db: ~p", [_Reason])
    end.

-spec check_fallbacks(ne_binaries(), ne_binaries(), ne_binary()) -> wh_json:objects().
check_fallbacks(Tree, MethodList, Realm) ->
    Result = lists:foldl(fun (X, Acc) -> check_fallback(X, Acc, MethodList, Realm) end, 'empty', Tree),
    case Result of
        'empty' -> [];
        _ -> Result
    end.

-spec check_fallback(ne_binary(), atom() | wh_json:objects(), ne_binaries(), ne_binary()) -> atom() | wh_json:objects().
check_fallback(AccountId, 'empty', MethodList, Realm) ->
    AccountDB = wh_util:format_account_id(AccountId, 'encoded'),
    ViewOpts = [{'key', AccountId}],
    case couch_mgr:get_results(AccountDB, ?RATES_LISTING_BY_OWNER, ViewOpts) of
        {'ok', []} -> 'empty';
        {'ok', [JObj]} ->
            Fallback = wh_doc:id(JObj),
            build_results(couch_mgr:open_cache_doc(AccountDB, Fallback), MethodList, Realm);
        {'ok', _JObjs} ->
            lager:error("found many results, please check account rate limits for ~s", [AccountDB]),
            'empty';
        {'error', _Reason} ->
            lager:error("Can't fetch data from db: ~p", [_Reason]),
            'empty'
    end;
check_fallback(_, Acc, _, _) ->
    Acc.

-type couch_ret() :: {'ok', wh_json:object()} | {'error', any()}.
-spec build_results(couch_ret(), ne_binaries(), ne_binary()) -> wh_json:objects().
build_results({'error', _Reason}, _, _) ->
    lager:error("Can't fetch data from db: ~p", [_Reason]),
    [];
build_results({'ok', JObj}, MethodList, Realm) ->
    Limits = wh_json:get_value(<<"account">>, JObj),
    lists:foldl(fun (M, Acc) -> build(M, Acc, Limits, Realm) end,[],MethodList).

-spec build(ne_binary(), wh_json:objects(), wh_json:object(), ne_binary()) -> wh_json:objects().
build(Method, Acc, JObj, Realm) ->
    PerMinute = wh_json:get_value([?MINUTE, Method], JObj),
    PerSecond = wh_json:get_value([?SECOND, Method], JObj),
    case PerMinute =:= 'undefined' orelse PerSecond =:= 'undefined' of
        'true' -> Acc;
        'false' -> construct_records(Method, Realm, PerMinute, PerSecond)
    end.

-type status() :: 'ok' | 'need_account'.
-type rates_ret() :: {status(), wh_json:objects()}.

-spec handle_db_response(wh_json:objects(), boolean()) -> rates_ret().
-spec handle_db_response(wh_json:objects(), wh_json:objects(), boolean()) -> rates_ret().
-spec handle_db_response(wh_json:objects(), wh_json:objects(), wh_json:objects(), boolean()) -> rates_ret().
handle_db_response(JObjs, IncludeRealm) ->
    {DefaultRates, OtherRates} = lists:partition(fun is_device_defaults/1, JObjs),
    {AccountRates, DeviceRates} = lists:partition(fun frontier_utils:is_realm/1, OtherRates),

    handle_db_response(AccountRates, DeviceRates, DefaultRates, IncludeRealm).

handle_db_response(AccountRates, [], DefaultRates, IncludeRealm) ->
    lager:debug("using default rates for the device"),
    handle_db_response(AccountRates, DefaultRates, IncludeRealm);
handle_db_response(AccountRates, DeviceRates, _DefaultRates, IncludeRealm) ->
    lager:debug("found rates in the device doc"),
    handle_db_response(AccountRates, DeviceRates, IncludeRealm).

handle_db_response([], DeviceRates, 'true') ->
    {'need_account', DeviceRates};
handle_db_response(AccountRates, DeviceRates, _IncludeRealm) ->
    {'ok', AccountRates ++ DeviceRates}.
