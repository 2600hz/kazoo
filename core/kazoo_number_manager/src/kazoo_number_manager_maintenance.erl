%%%-------------------------------------------------------------------
%%% @copyright (C) 2016-2017, 2600Hz INC
%%% @doc
%%%
%%%
%%% @end
%%% @contributors
%%%   Pierre Fenoll
%%%-------------------------------------------------------------------
-module(kazoo_number_manager_maintenance).

-include("knm.hrl").

-export([app_using/2]).
-export([carrier_module_usage/0
        ,carrier_module_usage/1
        ]).
-export([convert_carrier_module/2
        ,convert_carrier_module/3
        ,convert_carrier_module_number/2
        ]).
-export([refresh_numbers_dbs/0
        ,refresh_numbers_db/1
        ]).
-export([fix_number/3
        ,fix_account_numbers/1
        ,fix_accounts_numbers/1
        ]).
-export([migrate/0, migrate/1
        ,migrate_unassigned_numbers/0, migrate_unassigned_numbers/1
        ]).
-export([generate_numbers/4]).
-export([delete/1]).
-export([purge_discovery/0, purge_discovery/1]).
-export([purge_deleted/0, purge_deleted/1]).
-export([update_number_services_view/1]).

-export([all_features/0]).
-export([feature_permissions_on_number/1]).
-export([add_allowed_feature_on_number/2
        ,remove_allowed_feature_on_number/2
        ,add_denied_feature_on_number/2
        ,remove_denied_feature_on_number/2
        ]).
-export([feature_permissions_on_reseller_of/1]).
-export([add_allowed_feature_on_reseller_of/2
        ,remove_allowed_feature_on_reseller_of/2
        ,add_denied_feature_on_reseller_of/2
        ,remove_denied_feature_on_reseller_of/2
        ]).
-export([feature_permissions_on_system_config/0]).
-export([add_allowed_feature_on_system_config/1
        ,remove_allowed_feature_on_system_config/1
        ,reset_allowed_features_to_defaults_on_system_config/0
        ]).
-export([ensure_adminonly_features_are_reachable/0]).

-define(TIME_BETWEEN_ACCOUNTS_MS
       ,kapps_config:get_pos_integer(?KNM_CONFIG_CAT, <<"time_between_accounts_ms">>, ?MILLISECONDS_IN_SECOND)).

-define(TIME_BETWEEN_NUMBERS_MS
       ,kapps_config:get_pos_integer(?KNM_CONFIG_CAT, <<"time_between_numbers_ms">>, ?MILLISECONDS_IN_SECOND)).

-define(PARALLEL_JOBS_COUNT,
        kapps_config:get_pos_integer(?KNM_CONFIG_CAT, <<"parallel_jobs_count">>, 1)).

-define(LOG(Format, Args)
       ,begin
            lager:debug(Format, Args),
            io:format(Format ++ "\n", Args)
        end
       ).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec carrier_module_usage() -> 'ok'.
carrier_module_usage() ->
    carrier_module_usage(knm_util:get_all_number_dbs(), dict:new()).

-spec carrier_module_usage(text()) -> 'ok'.
carrier_module_usage(Prefix) ->
    Database = knm_converters:to_db(Prefix),
    carrier_module_usage([Database], dict:new()).

-spec carrier_module_usage(ne_binaries(), dict:dict()) -> 'ok'.
carrier_module_usage([], Totals) ->
    io:format("Totals:~n", []),
    F = fun (Module, Count) -> io:format("    ~s: ~p~n", [Module, Count]) end,
    _ = dict:map(F, Totals),
    ok;
carrier_module_usage([Database|Databases], Totals0) ->
    Totals1 = get_carrier_module_usage(Database, Totals0),
    carrier_module_usage(Databases, Totals1).

-spec get_carrier_module_usage(ne_binary(), dict:dict()) -> dict:dict().
get_carrier_module_usage(Database, Totals) ->
    io:format("~s:~n", [Database]),
    ViewOptions = ['reduce', 'group'],
    {'ok', JObjs} = kz_datamgr:get_results(Database, <<"numbers/module_name">>, ViewOptions),
    log_carrier_module_usage(JObjs, Database, Totals).

-spec log_carrier_module_usage(kz_json:objects(), ne_binary(), dict:dict()) -> dict:dict().
log_carrier_module_usage([], _, Totals) -> Totals;
log_carrier_module_usage([JObj|JObjs], Database, Totals0) ->
    Module = kz_json:get_value(<<"key">>, JObj),
    Count = kz_json:get_value(<<"value">>, JObj),
    io:format("    ~s: ~p~n", [Module, Count]),
    Totals1 = dict:update_counter(Module, Count, Totals0),
    log_carrier_module_usage(JObjs, Database, Totals1).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec convert_carrier_module(ne_binary(), ne_binary()) -> 'ok'.
convert_carrier_module(Source, Target) ->
    convert_carrier_module_database(Source, Target, knm_util:get_all_number_dbs()).

-spec convert_carrier_module(ne_binary(), ne_binary(), ne_binary()) -> 'ok'.
convert_carrier_module(Source, Target, Prefix) ->
    Database = knm_converters:to_db(Prefix),
    convert_carrier_module_database(Source, Target, [Database]).

-spec convert_carrier_module_database(ne_binary(), ne_binary(), ne_binaries()) -> 'ok'.
convert_carrier_module_database(_, _, []) -> 'ok';
convert_carrier_module_database(Source, Target, [Database|Databases]) ->
    io:format("attempt to convert numbers with carrier module ~s to ~s in database ~s~n"
             ,[Source, Target, Database]),
    ViewOptions = [{'reduce', 'false'}, {'key', Source}],
    {'ok', JObjs} = kz_datamgr:get_results(Database, <<"numbers/module_name">>, ViewOptions),
    convert_carrier_module_numbers([kz_doc:id(JObj) || JObj <- JObjs], Target),
    convert_carrier_module_database(Source, Target, Databases).

-spec convert_carrier_module_numbers(ne_binaries(), ne_binary()) -> ok.
convert_carrier_module_numbers(Nums, Target) ->
    case lists:member(Target, knm_carriers:all_modules()) of
        false -> io:format("Bad carrier module: ~s\n", [Target]);
        true ->
            Routines = [{fun knm_phone_number:set_module_name/2, Target}],
            #{ok := Ns, ko := KOs} = knm_numbers:update(Nums, Routines),
            io:format("updated carrier module to ~s for ~p:\n", [Target, length(Ns)]),
            F = fun (N) -> io:format("\t~s\n", [knm_phone_number:number(knm_number:phone_number(N))]) end,
            lists:foreach(F, Ns),
            io:format("updating carrier module failed for ~p:\n", [maps:size(KOs)]),
            G = fun (Num, R) -> io:format("\t~s: ~p\n", [Num, R]) end,
            _ = maps:map(G, KOs),
            ok
    end.

-spec convert_carrier_module_number(ne_binary(), ne_binary()) -> ok.
convert_carrier_module_number(Num, Target) ->
    convert_carrier_module_numbers([Num], Target).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec refresh_numbers_dbs() -> 'ok'.
refresh_numbers_dbs() ->
    {'ok', Databases} = kz_datamgr:db_info(),
    NumberDbs = [Db
                 || Db <- Databases,
                    kzs_util:db_classification(Db) =:= 'numbers'
                        orelse kzs_util:db_classification(Db) =:= 'system_numbers'
                ],
    refresh_numbers_dbs(NumberDbs, length(NumberDbs)).

-spec refresh_numbers_dbs(ne_binaries(), non_neg_integer()) -> 'ok'.
refresh_numbers_dbs([], _) -> 'ok';
refresh_numbers_dbs([NumberDb|NumberDbs], Total) ->
    ?LOG("(~p/~p) updating number db ~s", [length(NumberDbs) + 1, Total, NumberDb]),
    _ = refresh_numbers_db(NumberDb),
    refresh_numbers_dbs(NumberDbs, Total).

-spec refresh_numbers_db(ne_binary()) -> 'ok'.
refresh_numbers_db(<<?KNM_DB_PREFIX_ENCODED, _/binary>> = NumberDb) ->
    {'ok',_} = kz_datamgr:revise_doc_from_file(NumberDb
                                              ,'kazoo_number_manager'
                                              ,<<"views/numbers.json">>
                                              ),
    'ok';
refresh_numbers_db(<<?KNM_DB_PREFIX, Suffix/binary>>) ->
    NumberDb = <<?KNM_DB_PREFIX_ENCODED, Suffix/binary>>,
    refresh_numbers_db(NumberDb);
refresh_numbers_db(<<"+", _/binary>> = Num) ->
    refresh_numbers_db(knm_converters:to_db(Num));
refresh_numbers_db(_Thing) ->
    ?LOG("skipping badly formed ~s", [_Thing]).

%% @public
-spec update_number_services_view(ne_binary()) -> no_return.
update_number_services_view(?MATCH_ACCOUNT_RAW(AccountId)) ->
    update_number_services_view(kz_util:format_account_db(AccountId));
update_number_services_view(?MATCH_ACCOUNT_ENCODED(_)=AccountDb) ->
    ClassifiersJObj = knm_converters:available_classifiers(), %%TODO: per-account classifiers.
    Pairs = [{Classification, kz_json:get_value([Classification, <<"regex">>], ClassifiersJObj)}
             || Classification <- kz_json:get_keys(ClassifiersJObj)
            ],
    {Classifications, Regexs} = lists:unzip(Pairs),
    MapView = number_services_map(Classifications, Regexs),
    RedView = number_services_red(),
    ViewName = <<"_design/numbers">>,
    View = case kz_datamgr:open_doc(AccountDb, ViewName) of
               {ok, JObj} -> JObj;
               {error, _R} ->
                   lager:debug("reading account view ~s from disk (~p)", [ViewName, _R]),
                   {ViewName,JObj} = kapps_util:get_view_json('crossbar', <<"account/numbers.json">>),
                   JObj
           end,
    PathMap = [<<"views">>, <<"reconcile_services">>, <<"map">>],
    PathRed = [<<"views">>, <<"reconcile_services">>, <<"reduce">>],
    case kz_json:are_equal(MapView, kz_json:get_ne_binary_value(PathMap, View))
        andalso kz_json:are_equal(RedView, kz_json:get_ne_binary_value(PathRed, View))
    of
        true -> no_return;
        false ->
            NewView = kz_json:set_values([{PathMap, MapView}
                                         ,{PathRed, RedView}
                                         ]
                                        ,View
                                        ),
            true = kz_datamgr:db_view_update(AccountDb, [{ViewName, NewView}]),
            ?LOG("View updated for ~s!", [AccountDb])
    end.

%% @public
-spec fix_accounts_numbers([ne_binary()]) -> 'ok'.
-spec fix_account_numbers(ne_binary()) -> 'ok'.
fix_accounts_numbers(Accounts) ->
    AccountDbs = lists:usort([kz_util:format_account_db(Account) || Account <- Accounts]),
    _ = purge_discovery(),
    _ = purge_deleted(),
    foreach_pause_in_between(?TIME_BETWEEN_ACCOUNTS_MS, fun fix_account_numbers/1, AccountDbs).

fix_account_numbers(AccountDb = ?MATCH_ACCOUNT_ENCODED(A,B,Rest)) ->
    ?LOG("########## fixing [~s] ##########", [AccountDb]),
    ?LOG("[~s] getting numbers from account db", [AccountDb]),
    DisplayPNs = get_DIDs(AccountDb, <<"phone_numbers/crossbar_listing">>),
    put(callflow_DIDs, get_DIDs_callflow(AccountDb)),
    put(trunkstore_DIDs, get_DIDs_trunkstore(AccountDb)),
    AccountId = ?MATCH_ACCOUNT_RAW(A, B, Rest),
    Leftovers =
        lists:foldl(fun (NumberDb, Leftovers) ->
                            Fixer = fun (DID) -> fix_docs(AccountDb, NumberDb, DID) end,
                            ?LOG("[~s] getting numbers from ~s", [AccountDb, NumberDb]),
                            AuthoritativePNs = get_DIDs_assigned_to(NumberDb, AccountId),
                            ?LOG("[~s] start fixing ~s", [AccountDb, NumberDb]),
                            foreach_pause_in_between(?TIME_BETWEEN_NUMBERS_MS
                                                    ,Fixer
                                                    ,gb_sets:to_list(AuthoritativePNs)
                                                    ),
                            ?LOG("[~s] done fixing ~s", [AccountDb, NumberDb]),
                            timer:sleep(?TIME_BETWEEN_ACCOUNTS_MS),
                            gb_sets:subtract(Leftovers, AuthoritativePNs)
                    end
                   ,DisplayPNs
                   ,knm_util:get_all_number_dbs()
                   ),
    ToRm0 = gb_sets:to_list(Leftovers),
    lists:foreach(fun (_DID) -> log_alien(AccountDb, _DID) end, ToRm0),
    ToRm = [DID
            || DID <- ToRm0,
               false =:= is_assigned_to(AccountDb, DID, AccountId),
               ok =:= ?LOG("########## will remove [~s] doc: ~s ##########", [AccountDb, DID])
           ],
    _ = kz_datamgr:del_docs(AccountDb, ToRm),
    ?LOG("########## updating view [~s] ##########", [AccountDb]),
    update_number_services_view(AccountDb),
    erase(callflow_DIDs),
    erase(trunkstore_DIDs),
    ?LOG("########## done fixing [~s] ##########", [AccountDb]);
fix_account_numbers(Account = ?NE_BINARY) ->
    fix_account_numbers(kz_util:format_account_db(Account)).

log_alien(_AccountDb, _DID) ->
    ?LOG("########## found alien [~s] doc: ~s ##########", [_AccountDb, _DID]).

-spec fix_number(ne_binary(), ne_binary(), ne_binary()) -> knm_number_return().
fix_number(Num, AuthBy, AccountDb) ->
    UsedBy = app_using(knm_converters:normalize(Num), AccountDb),
    Routines = [{fun knm_phone_number:set_used_by/2, UsedBy}
               ,fun knm_phone_number:remove_denied_features/1
               ],
    Options = [{auth_by, AuthBy}
              ,{dry_run, false}
              ,{batch_run, false}
              ],
    knm_number:update(Num, Routines, Options).

-spec migrate() -> 'ok'.
migrate() ->
    ensure_adminonly_features_are_reachable(),
    _ = refresh_numbers_dbs(),
    pforeach(fun migrate/1, kapps_util:get_all_accounts()),
    migrate_unassigned_numbers().

-spec migrate(ne_binary()) -> 'ok'.
migrate(Account) ->
    AccountDb = kz_util:format_account_db(Account),
    fix_account_numbers(AccountDb),
    _ = kz_datamgr:del_doc(AccountDb, <<"phone_numbers">>),
    'ok'.

-spec migrate_unassigned_numbers() -> 'ok'.
-spec migrate_unassigned_numbers(ne_binary(), integer()) -> 'ok'.
migrate_unassigned_numbers() ->
    ?LOG("********** fixing unassigned numbers **********", []),
    pforeach(fun migrate_unassigned_numbers/1, knm_util:get_all_number_dbs()),
    ?LOG("********** finished fixing unassigned numbers **********", []).

-spec migrate_unassigned_numbers(ne_binary()) -> ok.
migrate_unassigned_numbers(<<?KNM_DB_PREFIX_ENCODED, _/binary>> = NumberDb) ->
    ?LOG("########## start fixing ~s ##########", [NumberDb]),
    migrate_unassigned_numbers(NumberDb, 0),
    ?LOG("########## done fixing ~s ##########", [NumberDb]);
migrate_unassigned_numbers(<<?KNM_DB_PREFIX_encoded, Suffix/binary>>) ->
    migrate_unassigned_numbers(<<?KNM_DB_PREFIX_ENCODED, Suffix/binary>>);
migrate_unassigned_numbers(<<?KNM_DB_PREFIX, Suffix/binary>>) ->
    migrate_unassigned_numbers(<<?KNM_DB_PREFIX_ENCODED, Suffix/binary>>);
migrate_unassigned_numbers(Number) ->
    migrate_unassigned_numbers(knm_converters:to_db(Number)).

migrate_unassigned_numbers(NumberDb, Offset) ->
    ViewOptions = [{limit, kz_datamgr:max_bulk_insert()}
                  ,{skip, Offset}
                  ],
    ?LOG("[~s] checking for unassigned numbers with offset ~b", [NumberDb, Offset]),
    case kz_datamgr:get_results(NumberDb, <<"numbers/unassigned">>, ViewOptions) of
        {ok, []} -> 'ok';
        {ok, JObjs} ->
            Length = length(JObjs),
            ?LOG("[~s] fixing ~b docs", [NumberDb, Length]),
            foreach_pause_in_between(?TIME_BETWEEN_NUMBERS_MS
                                    ,fun fix_unassign_doc/1
                                    ,[kz_doc:id(JObj) || JObj <- JObjs]
                                    ),
            timer:sleep(?TIME_BETWEEN_ACCOUNTS_MS),
            migrate_unassigned_numbers(NumberDb, Offset + Length);
        {error, _R} ->
            ?LOG("failed to get unassign DIDs from ~s: ~p", [NumberDb, _R])
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

escape(?NE_BINARY=Bin0) ->
    StartSz = byte_size(Start= <<"<<">>),
    EndSz   = byte_size(End  = <<">>">>),
    Bin = iolist_to_binary(io_lib:format("~p", [Bin0])),
    SizeOfWhatIWant = byte_size(Bin) - (StartSz + EndSz),
    <<Start:StartSz/binary, Escaped:SizeOfWhatIWant/binary, End:EndSz/binary>> = Bin,
    Escaped.

number_services_map(Classifications, Regexs) ->
    iolist_to_binary(
      ["function(doc) {"
       "  if (doc.pvt_type != 'number' || doc.pvt_deleted) return;"
       "  var e164 = doc._id;"
       %% "log('+14157125234'.match(",escape(<<"\\d+">>),"));"
       "  var resM = {};"
       "  resM[doc.pvt_module_name] = 1;"
       "  var resC = {};"
       "  if (false) return;"
      ,[["  else if (e164.match(", escape(R), "))"
         "    resC['", Class, "'] = resM;"
        ]
        || {Class, R} <- lists:zip(Classifications, Regexs)
       ]
      ,"  var resF = {};"
       "  var used = doc.pvt_features || {};"
       "  for (var feature in used)"
       "    if (used.hasOwnProperty(feature))"
       "      resF[feature] = 1;"
       "  emit(doc._id, {'classifications':resC, 'features':resF});"
       "}"
      ]).

number_services_red() ->
    iolist_to_binary(
      ["function(Keys, Values, _Rereduce) {"
       "  var incr = function (o, k, v) {"
       "    o[k] = v + ((o[k] === undefined) ? 0 : o[k]);"
       "    return o;"
       "  };"
       "  var acc = function (Oout, Oin) {"
       "    for (var Ofield in Oin)"
       "      if (Oin.hasOwnProperty(Ofield))"
       "        Oout = incr(Oout, Ofield, Oin[Ofield]);"
       "    return Oout;"
       "  };"
       "  var resF = {};"
       "  var resC = {};"
       "  for (var i in Values) {"
       "    var Value = Values[i];"
       "    resF = acc(resF, Value['features'] || {});"
       "    var Classifications = Value['classifications'] || {};"
       "    for (var klass in Classifications) {"
       "      if (Classifications.hasOwnProperty(klass))"
       "        resC[klass] = acc(resC[klass] || {}, Classifications[klass]);"
       "    }"
       "  }"
       "  return {'classifications':resC, 'features':resF};"
       "}"
      ]).

-spec foreach_pause_in_between(non_neg_integer(), fun(), list()) -> 'ok'.
foreach_pause_in_between(_, _, []) -> 'ok';
foreach_pause_in_between(_, Fun, [Element]) ->
    _ = Fun(Element),
    'ok';
foreach_pause_in_between(Time, Fun, [Element|Elements]) ->
    _ = Fun(Element),
    timer:sleep(Time),
    foreach_pause_in_between(Time, Fun, Elements).

-spec pforeach(fun((A) -> any()), [A]) -> ok.
pforeach(Fun, Arg1s)
  when is_function(Fun, 1),
       is_list(Arg1s) ->
    Malt = [%% Each worker process gets to do only 1 item before dying
            1
            %% A processes count of 1 is equivalent to lists:foreach-ordering
            %% with each Fun being applied on its own (different) process
           ,{processes, ?PARALLEL_JOBS_COUNT}
           ],
    plists:foreach(Fun, Arg1s, Malt).

-spec fix_docs(ne_binary(), ne_binary(), ne_binary()) -> ok.
fix_docs(AccountDb, NumberDb, DID) ->
    Res = kz_datamgr:open_doc(AccountDb, DID),
    fix_docs(Res, AccountDb, NumberDb, DID).

fix_docs({error, timeout}, _AccountDb, _, _DID) ->
    ?LOG("getting ~s from ~s timed out, skipping", [_DID, _AccountDb]);
fix_docs({error, _R}, AccountDb, _, DID) ->
    ?LOG("failed to get ~s from ~s (~p), creating it", [DID, AccountDb, _R]),
    Updates = [{fun knm_phone_number:set_used_by/2, app_using(DID, AccountDb)}
              ,fun knm_phone_number:remove_denied_features/1
              ],
    %% knm_number:update/2,3 ensures creation of doc in AccountDb
    case knm_number:update(DID, Updates, options()) of
        {ok, _} -> ok;
        {error, _E} -> ?LOG("creating ~s failed: ~p", [DID, _E])
    end;
fix_docs({ok, Doc}, AccountDb, NumberDb, DID) ->
    Res = kz_datamgr:open_doc(NumberDb, DID),
    fix_docs(Res, Doc, AccountDb, NumberDb, DID).

fix_docs({error, timeout}, _, _, _NumberDb, _DID) ->
    ?LOG("getting ~s from ~s timed out, skipping", [_DID, _NumberDb]);
fix_docs({error, _R}, _, _, _NumberDb, _DID) ->
    ?LOG("~s disappeared from ~s (~p), skipping", [_DID, _NumberDb]);
fix_docs({ok, NumDoc}, Doc, _AccountDb, NumberDb, DID) ->
    AccountDb = account_db_from_number_doc(NumDoc),
    ShouldEnsureDocIsInRightAccountDb = _AccountDb =/= AccountDb,
    ShouldEnsureDocIsInRightAccountDb
        andalso ?LOG("[~s] ~s should be in ~s instead", [_AccountDb, DID, AccountDb]),
    UsedBy = app_using(DID, AccountDb),
    case not ShouldEnsureDocIsInRightAccountDb
        andalso UsedBy =:= kz_json:get_ne_binary_value(?PVT_USED_BY, NumDoc)
        andalso have_same_pvt_values(NumDoc, Doc)
    of
        true -> ?LOG("~s already synced", [DID]);
        false ->
            JObj = kz_json:merge_jobjs(kz_doc:public_fields(NumDoc)
                                      ,kz_doc:public_fields(Doc)
                                      ),
            ?LOG("syncing ~s", [DID]),
            Routines = [{fun knm_phone_number:set_used_by/2, UsedBy}
                       ,{fun knm_phone_number:reset_doc/2, JObj}
                       ,fun knm_phone_number:remove_denied_features/1
                       ],
            try knm_number:update(DID, Routines, options()) of
                {ok, _} -> ok;
                {error, _R} -> ?LOG("sync of ~s failed: ~p", [DID, _R])
            catch error:function_clause ->
                    kz_util:log_stacktrace(),
                    ?LOG("failed to sync ~s", [DID])
            end
    end.

options() ->
    [{auth_by, ?KNM_DEFAULT_AUTH_BY}
     %% No caching + bulk doc writes
    ,{batch_run, true}
    ].

account_db_from_number_doc(NumDoc) ->
    case kz_json:get_ne_binary_value(?PVT_ASSIGNED_TO, NumDoc) of
        undefined -> undefined;
        AccountId -> kz_util:format_account_db(AccountId)
    end.

-spec fix_unassign_doc(ne_binary()) -> 'ok'.
fix_unassign_doc(DID) ->
    Setters = [{fun knm_phone_number:set_used_by/2, undefined}
              ,fun knm_phone_number:remove_denied_features/1
              ],
    case knm_number:update(DID, Setters, options()) of
        {ok, _} -> ok;
        {error, _R} -> ?LOG("failed fixing unassigned ~s: ~p", [DID, _R])
    end.

-type dids() :: gb_sets:set(ne_binary()).
-spec get_DIDs(ne_binary(), ne_binary()) -> dids().
get_DIDs(AccountDb, View) ->
    get_DIDs(AccountDb, View, []).
-spec get_DIDs(ne_binary(), ne_binary(), kz_proplist()) -> dids().
get_DIDs(AccountDb, View, ViewOptions) ->
    ?LOG("[~s] getting numbers from ~s", [AccountDb, View]),
    case kz_datamgr:get_result_keys(AccountDb, View, ViewOptions) of
        {'ok', DIDs} -> gb_sets:from_list(DIDs);
        {'error', _R} ->
            ?LOG("failed to get ~s DIDs from ~s: ~p", [View, AccountDb, _R]),
            gb_sets:new()
    end.

get_DIDs_callflow(AccountDb) ->
    get_DIDs_callflow(AccountDb, []).
get_DIDs_trunkstore(AccountDb) ->
    get_DIDs_trunkstore(AccountDb, []).
get_DIDs_callflow(AccountDb, ViewOptions) ->
    get_DIDs(AccountDb, <<"callflows/listing_by_number">>, ViewOptions).
get_DIDs_trunkstore(AccountDb, ViewOptions) ->
    get_DIDs(AccountDb, <<"trunkstore/lookup_did">>, ViewOptions).

-spec get_DIDs_assigned_to(ne_binary(), ne_binary()) -> dids().
get_DIDs_assigned_to(NumberDb, AssignedTo) ->
    ViewOptions = [{startkey, [AssignedTo]}
                  ,{endkey, [AssignedTo, kz_json:new()]}
                  ],
    case kz_datamgr:get_results(NumberDb, <<"numbers/assigned_to">>, ViewOptions) of
        {ok, JObjs} -> gb_sets:from_list([kz_doc:id(JObj) || JObj <- JObjs]);
        {error, _R} ->
            lager:debug("failed to get ~s DIDs from ~s: ~p", [AssignedTo, NumberDb, _R]),
            gb_sets:new()
    end.

-spec have_same_pvt_values(kz_json:object(), kz_json:object()) -> boolean().
have_same_pvt_values(NumDoc0, Doc0) ->
    NumDoc = cleanse(kz_doc:private_fields(NumDoc0)),
    Doc = cleanse(kz_doc:private_fields(Doc0)),
    kz_json:are_equal(NumDoc, Doc).

-spec cleanse(kz_json:object()) -> kz_json:object().
cleanse(JObj) ->
    kz_json:delete_keys([<<"id">>, <<"_id">>
                        ,<<"_rev">>
                        ,?PVT_AUTH_BY
                        ,?PVT_STATE_LEGACY
                        ,?PVT_MODIFIED
                        ,?PVT_CREATED
                        ]
                       ,JObj
                       ).


-spec app_using(ne_binary(), ne_binary()) -> api_ne_binary().
-ifdef(TEST).
app_using(?TEST_OLD7_NUM, ?CHILD_ACCOUNT_DB) -> <<"trunkstore">>;
app_using(?NE_BINARY, ?MATCH_ACCOUNT_ENCODED(_)) -> undefined.
-else.
app_using(Num, AccountDb) ->
    app_using(Num, AccountDb, get(callflow_DIDs), get(trunkstore_DIDs)).

-type api_dids() :: undefined | dids().
-spec app_using(ne_binary(), ne_binary(), api_dids(), api_dids()) -> api_ne_binary().
app_using(Num, AccountDb, undefined, TrunkstoreNums) ->
    CallflowNums = get_DIDs_callflow(AccountDb, [{key, Num}]),
    app_using(Num, AccountDb, CallflowNums, TrunkstoreNums);
app_using(Num, AccountDb, CallflowNums, undefined) ->
    TrunkstoreNums = get_DIDs_trunkstore(AccountDb, [{key, Num}]),
    app_using(Num, AccountDb, CallflowNums, TrunkstoreNums);
app_using(Num, _, CallflowNums, TrunkstoreNums) ->
    case gb_sets:is_element(Num, CallflowNums) of
        true -> <<"callflow">>;
        false ->
            case gb_sets:is_element(Num, TrunkstoreNums) of
                true -> <<"trunkstore">>;
                false -> undefined
            end
    end.
-endif.

-spec is_assigned_to(ne_binary(), ne_binary(), ne_binary()) -> boolean().
is_assigned_to(AccountDb, DID, AccountId) ->
    case kz_datamgr:open_doc(AccountDb, DID) of
        {error, _R} ->
            lager:debug("~s's ~s temporarily unavailable, skipping", [AccountDb, DID]),
            true;
        {ok, Doc} ->
            AccountId =/= kz_json:get_ne_binary_value(?PVT_ASSIGNED_TO, Doc)
    end.


-spec generate_numbers(ne_binary(), ne_binary(), pos_integer(), non_neg_integer()) -> 'ok'.
generate_numbers(Type, AccountId, StartingNumber, Quantity) ->
    M = kz_term:to_atom(<<"knm_", Type/binary>>, 'true'),
    M:generate_numbers(AccountId, kz_term:to_integer(StartingNumber), kz_term:to_integer(Quantity)).


%% @public
-spec delete(ne_binary()) -> 'no_return'.
delete(Num) ->
    case knm_number:delete(Num, knm_number_options:default()) of
        {'ok', _} -> io:format("Removed ~s\n", [Num]);
        {'error', _R} -> io:format("ERROR: ~p\n", [_R])
    end,
    'no_return'.


%% @public
-spec purge_discovery() -> 'no_return'.
purge_discovery() ->
    Purge = fun (NumberDb) -> purge_number_db(NumberDb, ?NUMBER_STATE_DISCOVERY) end,
    pforeach(Purge, knm_util:get_all_number_dbs()),
    'no_return'.

%% @public
-spec purge_deleted(ne_binary()) -> 'no_return'.
purge_deleted(Prefix) ->
    purge_number_db(<<?KNM_DB_PREFIX_ENCODED, Prefix/binary>>, ?NUMBER_STATE_DELETED),
    'no_return'.

%% @public
-spec purge_deleted() -> 'no_return'.
purge_deleted() ->
    Purge = fun (NumberDb) -> purge_number_db(NumberDb, ?NUMBER_STATE_DELETED) end,
    pforeach(Purge, knm_util:get_all_number_dbs()),
    'no_return'.

%% @public
-spec purge_discovery(ne_binary()) -> 'no_return'.
purge_discovery(Prefix) ->
    purge_number_db(<<?KNM_DB_PREFIX_ENCODED, Prefix/binary>>, ?NUMBER_STATE_DISCOVERY),
    'no_return'.

-spec purge_number_db(ne_binary(), ne_binary()) -> 'ok'.
purge_number_db(NumberDb, State) ->
    ViewOptions = [{'startkey', [State]}
                  ,{'endkey', [State, kz_json:new()]}
                  ,'include_docs'
                  ,{'limit', 500}
                  ],
    case kz_datamgr:get_results(NumberDb, <<"numbers/status">>, ViewOptions) of
        {'ok', []} -> 'ok';
        {'ok', Numbers} ->
            io:format("removing ~p numbers in state '~s' from ~s~n", [length(Numbers), State, NumberDb]),
            JObjs = [JObj || Number <- Numbers,
                             JObj <- [kz_json:get_value(<<"doc">>, Number)],
                             State =:= kz_json:get_value(?PVT_STATE, JObj)
                    ],
            kz_datamgr:del_docs(NumberDb, JObjs),
            purge_number_db(NumberDb, State)
    end.


%% @private
-spec is_feature_valid(any()) -> boolean().
is_feature_valid(Thing) ->
    lists:member(Thing, ?ALL_KNM_FEATURES).

%% @private
-spec invalid_feature(ne_binary()) -> no_return.
invalid_feature(Feature) ->
    io:format("Feature '~s' is not a known feature.\n", [Feature]),
    all_features().

%% @public
-spec all_features() -> no_return.
all_features() ->
    io:format("Known features:\n\t~s\n", [list_features(?ALL_KNM_FEATURES)]),
    no_return.

%% @private
-spec list_features(ne_binaries()) -> iodata().
list_features(Features) ->
    kz_util:iolist_join($\s, Features).

%% @private
-spec error_with_number(ne_binary(), any()) -> no_return.
error_with_number(Num, Error) ->
    Reason = case kz_json:is_json_object(Error) of
                 false -> Error;
                 true -> knm_errors:error(Error)
             end,
    io:format("Error with number ~s: ~s\n", [Num, Reason]),
    no_return.

%% @private
-spec print_feature_permissions(ne_binaries(), ne_binaries()) -> no_return.
print_feature_permissions(Allowed, Denied) ->
    io:format("\tFeatures allowed: ~s\n"
              "\tFeatures denied: ~s\n"
             ,[list_features(Allowed), list_features(Denied)]
             ),
    no_return.

%% @private
-spec list_number_feature_permissions(knm_number:knm_number()) -> no_return.
list_number_feature_permissions(N) ->
    PN = knm_number:phone_number(N),
    Num = knm_phone_number:number(PN),
    Allowed = knm_phone_number:features_allowed(PN),
    Denied = knm_phone_number:features_denied(PN),
    io:format("Feature permissions on ~s:\n", [Num]),
    print_feature_permissions(Allowed, Denied).

%% @private
-spec edit_feature_permissions_on_number(ne_binary(), fun(), ne_binary()) -> no_return.
edit_feature_permissions_on_number(Num, Fun, Feature) ->
    case is_feature_valid(Feature) of
        false -> invalid_feature(Feature);
        true ->
            Updates = [{Fun, Feature}],
            case knm_number:update(Num, Updates) of
                {ok, N} -> list_number_feature_permissions(N);
                {error, Error} -> error_with_number(Num, Error)
            end
    end.

%% @public
-spec feature_permissions_on_number(ne_binary()) -> no_return.
feature_permissions_on_number(Num) ->
    case knm_number:get(Num) of
        {error, Error} -> error_with_number(Num, Error);
        {ok, N} -> list_number_feature_permissions(N)
    end.

%% @public
-spec add_allowed_feature_on_number(ne_binary(), ne_binary()) -> no_return.
add_allowed_feature_on_number(?NE_BINARY=Feature, ?NE_BINARY=Num) ->
    edit_feature_permissions_on_number(Num, fun knm_phone_number:add_allowed_feature/2, Feature).

%% @public
-spec remove_allowed_feature_on_number(ne_binary(), ne_binary()) -> no_return.
remove_allowed_feature_on_number(?NE_BINARY=Feature, ?NE_BINARY=Num) ->
    edit_feature_permissions_on_number(Num, fun knm_phone_number:remove_allowed_feature/2, Feature).

%% @public
-spec add_denied_feature_on_number(ne_binary(), ne_binary()) -> no_return.
add_denied_feature_on_number(?NE_BINARY=Feature, ?NE_BINARY=Num) ->
    edit_feature_permissions_on_number(Num, fun knm_phone_number:add_denied_feature/2, Feature).

%% @public
-spec remove_denied_feature_on_number(ne_binary(), ne_binary()) -> no_return.
remove_denied_feature_on_number(?NE_BINARY=Feature, ?NE_BINARY=Num) ->
    edit_feature_permissions_on_number(Num, fun knm_phone_number:remove_denied_feature/2, Feature).

%% @public
-spec feature_permissions_on_reseller_of(ne_binary()) -> no_return.
feature_permissions_on_reseller_of(?MATCH_ACCOUNT_RAW(AccountId)) ->
    Allowed = empty_list_when_undefined(?FEATURES_ALLOWED_RESELLER(AccountId)),
    Denied = empty_list_when_undefined(?FEATURES_DENIED_RESELLER(AccountId)),
    ResellerId = kz_services:find_reseller_id(AccountId),
    io:format("Feature permissions on reseller of ~s (~s):\n", [AccountId, ResellerId]),
    print_feature_permissions(Allowed, Denied).

%% @private
-spec empty_list_when_undefined(api_list()) -> ne_binaries().
empty_list_when_undefined(undefined) -> [];
empty_list_when_undefined(NeBinaries) -> NeBinaries.

%% @private
-spec edit_allowed_feature_permissions_on_reseller_of(ne_binary(), fun(), ne_binary()) -> no_return.
edit_allowed_feature_permissions_on_reseller_of(AccountId, Fun, Feature) ->
    case is_feature_valid(Feature) of
        false -> invalid_feature(Feature);
        true ->
            Allowed = empty_list_when_undefined(?FEATURES_ALLOWED_RESELLER(AccountId)),
            NewFeatures = lists:usort(Fun(Feature, Allowed)),
            ResellerId = kz_services:find_reseller_id(AccountId),
            _ = kapps_account_config:set(ResellerId, ?KNM_CONFIG_CAT, ?KEY_FEATURES_ALLOW, NewFeatures),
            feature_permissions_on_reseller_of(AccountId)
    end.

%% @private
-spec edit_denied_feature_permissions_on_reseller_of(ne_binary(), fun(), ne_binary()) -> no_return.
edit_denied_feature_permissions_on_reseller_of(AccountId, Fun, Feature) ->
    case is_feature_valid(Feature) of
        false -> invalid_feature(Feature);
        true ->
            Denied = empty_list_when_undefined(?FEATURES_DENIED_RESELLER(AccountId)),
            NewFeatures = lists:usort(Fun(Feature, Denied)),
            ResellerId = kz_services:find_reseller_id(AccountId),
            _ = kapps_account_config:set(ResellerId, ?KNM_CONFIG_CAT, ?KEY_FEATURES_DENY, NewFeatures),
            feature_permissions_on_reseller_of(AccountId)
    end.

%% @public
-spec add_allowed_feature_on_reseller_of(ne_binary(), ne_binary()) -> no_return.
add_allowed_feature_on_reseller_of(?NE_BINARY=Feature, ?MATCH_ACCOUNT_RAW(AccountId)) ->
    Cons = fun (AFeature, Features) -> [AFeature|Features] end,
    edit_allowed_feature_permissions_on_reseller_of(AccountId, Cons, Feature).

%% @public
-spec remove_allowed_feature_on_reseller_of(ne_binary(), ne_binary()) -> no_return.
remove_allowed_feature_on_reseller_of(?NE_BINARY=Feature, ?MATCH_ACCOUNT_RAW(AccountId)) ->
    edit_allowed_feature_permissions_on_reseller_of(AccountId, fun lists:delete/2, Feature).

%% @public
-spec add_denied_feature_on_reseller_of(ne_binary(), ne_binary()) -> no_return.
add_denied_feature_on_reseller_of(?NE_BINARY=Feature, ?MATCH_ACCOUNT_RAW(AccountId)) ->
    Cons = fun (AFeature, Features) -> [AFeature|Features] end,
    edit_denied_feature_permissions_on_reseller_of(AccountId, Cons, Feature).

%% @public
-spec remove_denied_feature_on_reseller_of(ne_binary(), ne_binary()) -> no_return.
remove_denied_feature_on_reseller_of(?NE_BINARY=Feature, ?MATCH_ACCOUNT_RAW(AccountId)) ->
    edit_denied_feature_permissions_on_reseller_of(AccountId, fun lists:delete/2, Feature).

%% @public
-spec feature_permissions_on_system_config() -> no_return.
feature_permissions_on_system_config() ->
    Allowed = knm_providers:system_allowed_features(),
    io:format("Features allowed on system config document:\n\t~s\n", [list_features(Allowed)]),
    no_return.

%% @public
-spec reset_allowed_features_to_defaults_on_system_config() -> no_return.
reset_allowed_features_to_defaults_on_system_config() ->
    set_features_on_system_config(?DEFAULT_FEATURES_ALLOWED_SYSTEM).

%% @private
-spec set_features_on_system_config(ne_binaries()) -> no_return.
set_features_on_system_config(Features) ->
    _ = kapps_config:set(?KNM_CONFIG_CAT, ?KEY_FEATURES_ALLOW, lists:usort(Features)),
    feature_permissions_on_system_config().

%% @private
-spec edit_allowed_feature_permissions_on_system_config(fun(), ne_binary()) -> no_return.
edit_allowed_feature_permissions_on_system_config(Fun, Feature) ->
    case is_feature_valid(Feature) of
        false -> invalid_feature(Feature);
        true ->
            Allowed = knm_providers:system_allowed_features(),
            set_features_on_system_config(Fun(Feature, Allowed))
    end.

%% @public
-spec add_allowed_feature_on_system_config(ne_binary()) -> no_return.
add_allowed_feature_on_system_config(?NE_BINARY=Feature) ->
    Cons = fun (AFeature, Features) -> [AFeature|Features] end,
    edit_allowed_feature_permissions_on_system_config(Cons, Feature).

%% @public
-spec remove_allowed_feature_on_system_config(ne_binary()) -> no_return.
remove_allowed_feature_on_system_config(?NE_BINARY=Feature) ->
    edit_allowed_feature_permissions_on_system_config(fun lists:delete/2, Feature).

%% @public
-spec ensure_adminonly_features_are_reachable() -> no_return.
ensure_adminonly_features_are_reachable() ->
    Configured = knm_providers:system_allowed_features(),
    case lists:usort(?ADMIN_ONLY_FEATURES) -- Configured of
        [] -> no_return;
        ToAdd ->
            io:format("Adding the following admin-only number features to system_config: ~s"
                     ,[list_features(ToAdd)]),
            set_features_on_system_config(ToAdd ++ Configured)
    end.
