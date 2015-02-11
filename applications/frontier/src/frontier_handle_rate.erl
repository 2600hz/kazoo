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
-define(ACCOUNT_FALLBACK, <<"fallback">>).
-define(RATES_CROSSBAR_LISTING, <<"rate_limits/crossbar_listing">>).

-spec is_fallback(wh_json:object()) -> boolean().
is_fallback(JObj) ->
    IsFallback = case wh_json:get_value(<<"key">>, JObj) of
                     [_, ?ACCOUNT_FALLBACK] -> 'true';
                     _ -> 'false'
                 end,
    frontier_utils:is_realm(JObj) andalso  IsFallback.

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

-spec handle_rate_req(wh_json:object(), wh_proplist()) -> any().
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

-spec run_rate_limits_query(ne_binary(), ne_binary(), boolean(), ne_binary()) -> wh_json:objects().
run_rate_limits_query(Entity, AccountDB, IncludeRealm, MethodList) ->
    Self = self(),
    lager:info("Building query list"),
    Type = case binary:split(Entity, <<"@">>) of
               [_, _] -> <<"device">>;
               _ -> <<"realm">>
           end,
    DeviceList = case Type of
                     <<"device">> -> [fun (Ref) ->
                                          fetch_rates_from_document(Self, Entity, Type, Ref, MethodList, AccountDB)
                                      end
                                      ,fun (Ref) ->
                                           fetch_rates_from_sys_config(Self, Entity, Type, Ref, MethodList)
                                       end
                                     ];
                     _ -> []
                 end,
    QueryList = case Type =:= <<"realm">> orelse IncludeRealm of
                    'true' ->
                        Realm = frontier_utils:extract_realm(Entity),
                        [fun (Ref) ->
                             fetch_rates_from_document(Self, Realm, <<"realm">>, Ref, MethodList, AccountDB)
                         end
                         ,fun (Ref) ->
                              fetch_rates_from_sys_config(Self, Realm, <<"realm">>, Ref, MethodList)
                          end
                        ] ++ DeviceList;
                    'false' -> DeviceList
                end,
    lager:info("Running queries"),
    Refs = lists:map(fun (F) ->
                         Ref = erlang:make_ref(),
                         spawn(fun () -> F(Ref) end),
                         Ref
                     end, QueryList),
    lager:info("Collecting results"),
    collect_responses(Refs, []).

-spec to_first_upper(ne_binary()) -> ne_binary().
to_first_upper(<<L, Ls/binary>>) ->
    list_to_binary([string:to_upper(L), Ls]).

-spec key_join(ne_binary(), ne_binary()) -> ne_binary().
key_join(X, Acc) ->
    list_to_binary([Acc, "-", X]).

-spec to_json_key(ne_binary()) -> ne_binary().
to_json_key(Token) ->
    Tokens = binary:split(Token, <<"_">>),
    [H | Tail] = lists:map(fun to_first_upper/1, Tokens),
    lists:foldl(fun key_join/2, H, Tail).

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

-spec collect_responses([reference()], []) -> wh_json:objects().
collect_responses([], Acc) ->
    lists:reverse(Acc);
collect_responses([Ref | Refs], Acc) ->
    R = receive
            {Ref, Rates} -> Rates
        after
            400 ->
                lager:info("Can't wait!"),
                []
        end,
    collect_responses(Refs, [R | Acc]).

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
-spec deny_rates_for_entity(ne_binary(), ne_binaries()) -> list().
deny_rates_for_entity(Entity, MethodList) ->
    X = lists:map(fun (M) -> construct_records(M, Entity, -1, -1) end, MethodList),
    lists:flatten(X).

-spec construct_records(ne_binary(), ne_binary(), ne_binary(), ne_binary()) -> wh_json:object().
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
    lists:map(fun (Obj) -> wh_json:set_value(<<"value">>, Obj, Record) end, [RPMObject, RPSObject]).


-spec fetch_rates_from_sys_config(pid(), ne_binary(), ne_binary(), reference(), ne_binaries()) -> 'ok'.
fetch_rates_from_sys_config(Srv, _, _, Ref, []) ->
    lager:info("sysconfig: Empty request - empty response"),
    Srv ! {Ref, []};
fetch_rates_from_sys_config(Srv, Entity, Type, Ref, MethodList) ->
    Section = case Type of
                  <<"device">> -> <<"device">>;
                  <<"realm">> -> <<"account">>
              end,
    AllRates = whapps_config:get(?APP_NAME, <<"rate_limits">>),
    TargetRates = wh_json:get_value(Section, AllRates, wh_json:new()),
    Payload = lists:foldl( fun (Method, Acc) ->
                                    RPM = wh_json:get_value([?MINUTE, Method], TargetRates, 0),
                                    RPS = wh_json:get_value([?SECOND, Method], TargetRates, 0),
                                    construct_records(Method, Entity, RPM, RPS) ++ Acc
                                end, [], MethodList),
    Srv ! {Ref, Payload}.

-spec fetch_rates_from_document(pid(), ne_binary(), ne_binary(), reference(), ne_binaries(), ne_binary()) -> 'ok'.
fetch_rates_from_document(Srv, Entity, Type, Ref, MethodList, AccountDB) ->
    Srv ! {Ref, fetch_rates(Entity, Type, MethodList, AccountDB)}.

-spec maybe_run_throug_fallbacks(wh_json:objects(), ne_binary(), ne_binaries()) -> wh_json:objects().
maybe_run_throug_fallbacks([], _, _) ->
    [];
maybe_run_throug_fallbacks([Fallback], Entity, MethodList) ->
    ReversedFallbacks = wh_json:get_value([<<"value">>, <<"parents">>], Fallback),
    run_throug_fallbacks(lists:reverse(ReversedFallbacks), Entity, MethodList).


-spec fetch_rates(ne_binary(), ne_binary(), ne_binaries(), ne_binary()) -> wh_json:objects().
fetch_rates(_, _, [], _) ->
    lager:info("document: Empty request - empty response"),
    [];
fetch_rates(Entity, <<"realm">>, MethodList, _) ->
    Keys = [[Entity, Method] || Method <- [?ACCOUNT_FALLBACK | MethodList]],
    ViewOpts = [{'keys', Keys}],
    case couch_mgr:get_results(?WH_ACCOUNTS_DB, ?RATES_CROSSBAR_LISTING, ViewOpts) of
        {'ok', JObjs} ->
            {Fallbacks, AccountRates} = lists:partition(fun is_fallback/1, JObjs),
            case length(AccountRates) > 0 of
                'true' -> AccountRates;
                'false' ->
                    maybe_run_throug_fallbacks(Fallbacks, Entity, MethodList)
            end;
        _ ->
            lager:info("Can not get account rates from db"),
            []
    end;
fetch_rates(Entity, <<"device">>, MethodList, AccountDB) ->
    lager:info("Run db query..."),
    User = frontier_utils:extract_username(Entity),
    Keys = [[E, M] || E <- [User, ?DEVICE_DEFAULT_RATES], M <- MethodList],
    ViewOpts = [{'keys', Keys}],
    case couch_mgr:get_results(AccountDB, ?RATES_CROSSBAR_LISTING, ViewOpts) of
        {'ok', JObjs} ->
            lager:info("Got ~p records for device ~s from db document", [length(JObjs), Entity]),
            {DefaultRates, DeviceRates} = lists:partition(fun is_device_defaults/1, JObjs),
            case length(DeviceRates) > 0 of
                'true' ->
                    lager:info("Found rates in the device doc"),
                    DeviceRates;
                'false' ->
                    lager:info("Found default rates for the device"),
                    DefaultRates
            end;
        _ ->
            lager:info("Can not get device rates drom db"),
            []
    end;
fetch_rates(_Entity, _Type, _Method, _AccountDB) ->
    lager:info("Unkown type for rate limits: ~p", _Type),
    [].

-spec not_limited(ne_binary()) -> boolean().
not_limited(AccountId) ->
    {'ok', JDoc} = couch_mgr:open_cache_doc(<<"accounts">>, AccountId),
    wh_json:get_value(<<"rate_limits">>, JDoc) =/= 'undefuned'.

%TODO
-spec run_throug_fallbacks(ne_binaries(), ne_binary(), ne_binaries()) -> wh_json:objects().
run_throug_fallbacks(Fallback, Entity, MethodList) ->
    case lists:dropwhile(fun not_limited/1, Fallback) of
        [Limiter | _] ->
            lager:debug("Try to use limits from ~s", [Limiter]),
            {'ok', JDoc} = couch_mgr:open_cache_doc(<<"accounts">>, Limiter),
            Limits = wh_json:get_value(<<"rate_limits">>, JDoc),
            AccountLimits = wh_json:get_first_defined([<<"account">>, <<"own">>], Limits, wh_json:new()),
            lists:foldl(fun (M, Acc) ->
                            RPM = wh_json:get_value([?MINUTE, M], AccountLimits),
                            RPS = wh_json:get_value([?SECOND, M], AccountLimits),
                            construct_records(M, Entity, RPM, RPS) ++ Acc
                        end, [], MethodList);
        [] ->
            lager:debug("Can't find parent with limits"),
            []
    end.
