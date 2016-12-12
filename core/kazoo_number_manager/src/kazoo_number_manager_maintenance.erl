%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, 2600Hz INC
%%% @doc
%%%
%%%
%%% @end
%%% @contributors
%%%   Pierre Fenoll
%%%-------------------------------------------------------------------
-module(kazoo_number_manager_maintenance).

-include("knm.hrl").

-export([refresh_numbers_dbs/0
        ,refresh_numbers_db/1
        ]).
-export([fix_account_numbers/1
        ,fix_accounts_numbers/1
        ]).
-export([migrate/0, migrate/1
        ,migrate_unassigned_numbers/0, migrate_unassigned_numbers/1
        ]).
-export([generate_numbers/4]).
-export([delete/1]).
-export([purge_discovery/0
        ,purge_discovery/1
        ]).
-export([update_number_services_view/1]).

-define(TIME_BETWEEN_ACCOUNTS_MS
       ,kapps_config:get_integer(?KNM_CONFIG_CAT, <<"time_between_accounts_ms">>, ?MILLISECONDS_IN_SECOND)).

-define(TIME_BETWEEN_NUMBERS_MS
       ,kapps_config:get_integer(?KNM_CONFIG_CAT, <<"time_between_numbers_ms">>, ?MILLISECONDS_IN_SECOND)).

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
refresh_numbers_db(<<"+", Suffix/binary>>) ->
    refresh_numbers_db(Suffix);
refresh_numbers_db(Suffix) ->
    NumberDb = <<?KNM_DB_PREFIX_ENCODED, Suffix/binary>>,
    refresh_numbers_db(NumberDb).

%% @public
-spec update_number_services_view(ne_binary()) -> ok.
update_number_services_view(?MATCH_ACCOUNT_RAW(AccountId)) ->
    update_number_services_view(kz_util:format_account_db(AccountId));
update_number_services_view(?MATCH_ACCOUNT_ENCODED(_)=AccountDb) ->
    JObj = knm_converters:available_classifiers(), %%TODO: per-account classifiers.
    Pairs = [{Classification, kz_json:get_value([Classification, <<"regex">>], JObj)}
             || Classification <- kz_json:get_keys(JObj)
            ],
    {Classifications, Regexs} = lists:unzip(Pairs),
    MapView = number_services_map(Classifications, Regexs),
    RedView = number_services_red(),
    ViewName = <<"_design/numbers">>,
    {ok, View} = kz_datamgr:open_doc(AccountDb, ViewName),
    NewView = kz_json:set_values([{[<<"views">>, <<"reconcile_services">>, <<"map">>], MapView}
                                 ,{[<<"views">>, <<"reconcile_services">>, <<"reduce">>], RedView}
                                 ]
                                ,View
                                ),
    case kz_json:are_identical(View, NewView) of
        true -> ?LOG("View is up to date.", []);
        false ->
            true = kz_datamgr:db_view_update(AccountDb, [{ViewName, NewView}]),
            ?LOG("View updated!", [])
    end.

%% @public
-spec fix_accounts_numbers([ne_binary()]) -> 'ok'.
-spec fix_account_numbers(ne_binary()) -> 'ok'.
fix_accounts_numbers(Accounts) ->
    AccountDbs = lists:usort([kz_util:format_account_db(Account) || Account <- Accounts]),
    foreach_pause_in_between(?TIME_BETWEEN_ACCOUNTS_MS, fun fix_account_numbers/1, AccountDbs).

fix_account_numbers(AccountDb = ?MATCH_ACCOUNT_ENCODED(A,B,Rest)) ->
    kz_util:put_callid(?MODULE),
    ?LOG("########## fixing [~s] ##########", [AccountDb]),
    ?LOG("[~s] getting numbers from account db", [AccountDb]),
    DisplayPNs = get_DIDs(AccountDb, <<"phone_numbers/crossbar_listing">>),
    ?LOG("[~s] getting numbers from callflow", [AccountDb]),
    put(callflow_DIDs, get_DIDs(AccountDb, <<"callflows/listing_by_number">>)),
    ?LOG("[~s] getting numbers from trunkstore", [AccountDb]),
    put(trunkstore_DIDs, get_DIDs(AccountDb, <<"trunkstore/lookup_did">>)),
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
    _ = knm_phone_number:push_stored(), %% Bulk doc writes
    ToRm0 = gb_sets:to_list(Leftovers),
    lists:foreach(fun (DID) ->
                          ?LOG("########## found alien [~s] doc: ~s ##########", [AccountDb, DID])
                  end
                 ,ToRm0
                 ),
    ToRm = [DID
            || DID <- ToRm0,
               false =:= is_assigned_to(AccountDb, DID, AccountId),
               ok =:= ?LOG("########## will remove [~s] doc: ~s ##########", [AccountDb, DID])
           ],
    _ = kz_datamgr:del_docs(AccountDb, ToRm),
    ?LOG("########## updating view [~s] ##########", [AccountDb]),
    update_number_services_view(AccountDb),
    ?LOG("########## done fixing [~s] ##########", [AccountDb]);
fix_account_numbers(Account = ?NE_BINARY) ->
    fix_account_numbers(kz_util:format_account_db(Account)).

-spec migrate() -> 'ok'.
migrate() ->
    _ = refresh_numbers_dbs(),
    AccountDbs = kapps_util:get_all_accounts(),
    foreach_pause_in_between(?TIME_BETWEEN_ACCOUNTS_MS, fun migrate/1, AccountDbs),
    erase(callflow_DIDs),
    erase(trunkstore_DIDs),
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
    NumberDbs = knm_util:get_all_number_dbs(),
    foreach_pause_in_between(?TIME_BETWEEN_ACCOUNTS_MS
                            ,fun migrate_unassigned_numbers/1
                            ,NumberDbs
                            ),
    ?LOG("********** finished fixing unassigned numbers **********", []).

-spec migrate_unassigned_numbers(ne_binary()) -> ok.
migrate_unassigned_numbers(<<?KNM_DB_PREFIX_ENCODED, _/binary>> = NumberDb) ->
    ?LOG("########## start fixing ~s ##########", [NumberDb]),
    migrate_unassigned_numbers(NumberDb, 0),
    _ = knm_phone_number:push_stored(), %% Bulk doc writes
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
                                    ,lists:map(fun kz_doc:id/1, JObjs)
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
       "  var resCB = {}, resCnB = {};"
       %% "log('+14157125234'.match(",escape(<<"\\d+">>),"));"
       "  var is_billable = (true === doc.pvt_is_billable);" %% If undefined, defaults to false.
       "  if (false) return;"
      ,[["  else if (e164.match(", escape(R), ")) {"
         "    if (is_billable)"
         "      resCB['", Class, "'] = 1;"
         "    else"
         "      resCnB['", Class, "'] = 1;"
         "  }"
        ]
        || {Class, R} <- lists:zip(Classifications, Regexs)
       ]
      ,"  var resF = {};"
       "  var used = doc.pvt_features || {};"
       "  for (var feature in used)"
       "    if (used.hasOwnProperty(feature))"
       "      resF[feature] = 1;"
       "  var resM = {};"
       "  resM[doc.pvt_module_name] = 1;"
       "  emit(doc._id, {'classifications':{'billable':resCB, 'non_billable':resCnB}, 'features':resF, 'modules':resM});"
       "}"
      ]).

number_services_red() ->
    iolist_to_binary(
      ["function(Keys, Values, _Rereduce) {"
       "  var incr = function (o, k, v) {"
       "    if (o[k] === undefined)"
       "      o[k] = v;"
       "    else"
       "      o[k] += v;"
       "    return o;"
       "  };"
       "  var acc = function (Oout, Oin) {"
       "    for (var Ofield in Oin)"
       "      if (Oin.hasOwnProperty(Ofield))"
       "        Oout = incr(Oout, Ofield, Oin[Ofield]);"
       "    return Oout;"
       "  };"
       "  var resCB = {}, resCnB = {};"
       "  var resF = {};"
       "  var resM = {};"
       "  for (var i in Values) {"
       "    var Value = Values[i];"
       "    resCB = acc(resCB, Value['classifications']['billable'] || {});"
       "    resCnB = acc(resCnB, Value['classifications']['non_billable'] || {});"
       "    resF = acc(resF, Value['features'] || {});"
       "    resM = acc(resM, Value['modules'] || {});"
       "  }"
       "  return {'classifications':{'billable':resCB, 'non_billable':resCnB}, 'features':resF, 'modules':resM};"
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

-spec fix_docs(ne_binary(), ne_binary(), ne_binary()) -> ok.
fix_docs(AccountDb, NumberDb, DID) ->
    Res = kz_datamgr:open_doc(AccountDb, DID),
    fix_docs(Res, AccountDb, NumberDb, DID).

fix_docs({error, timeout}, _AccountDb, _, _DID) ->
    ?LOG("getting ~s from ~s timed out, skipping", [_DID, _AccountDb]);
fix_docs({error, _R}, _AccountDb, _, DID) ->
    ?LOG("failed to get ~s from ~s (~p), creating it", [DID, _AccountDb, _R]),
    %% knm_number:update/2,3 ensures creation of doc in AccountDb
    case knm_number:update(DID, [{fun knm_phone_number:set_used_by/2, app_using(DID)}]) of
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
fix_docs({ok, NumDoc}, Doc, AccountDb, NumberDb, DID) ->
    case app_using(DID) =:= kz_json:get_ne_binary_value(?PVT_USED_BY, NumDoc)
        andalso have_same_pvt_values(NumDoc, Doc)
        andalso are_features_available_synced(NumDoc)
        andalso is_billable_a_boolean(NumDoc)
    of
        true -> ?LOG("~s already synced", [DID]);
        false ->
            JObj = kz_json:merge_jobjs(kz_json:public_fields(NumDoc)
                                      ,kz_json:public_fields(Doc)
                                      ),
            ?LOG("syncing ~s", [DID]),
            Routines = [{fun knm_phone_number:set_used_by/2, app_using(DID)}
                       ,{fun knm_phone_number:update_doc/2, JObj}
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

-spec fix_unassign_doc(ne_binary()) -> 'ok'.
fix_unassign_doc(DID) ->
    Setters = [{fun knm_phone_number:set_used_by/2, undefined}],
    case knm_number:update(DID, Setters, options()) of
        {ok, _} -> ok;
        {error, _R} -> ?LOG("failed fixing unassigned ~s: ~p", [DID, _R])
    end.

-type dids() :: gb_sets:set(ne_binary()).
-spec get_DIDs(ne_binary(), ne_binary()) -> dids().
get_DIDs(AccountDb, View) ->
    case kz_datamgr:get_result_keys(AccountDb, View, []) of
        {'ok', DIDs} -> gb_sets:from_list(DIDs);
        {'error', _R} ->
            ?LOG("failed to get ~s DIDs from ~s: ~p", [View, AccountDb, _R]),
            gb_sets:new()
    end.

-spec get_DIDs_assigned_to(ne_binary(), ne_binary()) -> dids().
get_DIDs_assigned_to(NumberDb, AssignedTo) ->
    ViewOptions = [{startkey, [AssignedTo]}
                  ,{endkey, [AssignedTo, kz_json:new()]}
                  ],
    case kz_datamgr:get_results(NumberDb, <<"numbers/assigned_to">>, ViewOptions) of
        {ok, JObjs} -> gb_sets:from_list(lists:map(fun kz_doc:id/1, JObjs));
        {error, _R} ->
            lager:debug("failed to get ~s DIDs from ~s: ~p", [AssignedTo, NumberDb, _R]),
            gb_sets:new()
    end.

-spec have_same_pvt_values(kz_json:object(), kz_json:object()) -> boolean().
have_same_pvt_values(NumDoc0, Doc0) ->
    NumDoc = cleanse(kz_json:private_fields(NumDoc0)),
    Doc = cleanse(kz_json:private_fields(Doc0)),
    NumDoc == Doc.

-spec are_features_available_synced(kz_json:object()) -> boolean().
are_features_available_synced(NumDoc) ->
    kz_json:get_value(?PVT_FEATURES_AVAILABLE, NumDoc) =:=
        knm_phone_number:features_available(
          knm_phone_number:from_json(NumDoc)
         ).

-spec is_billable_a_boolean(kz_json:object()) -> boolean().
is_billable_a_boolean(NumDoc) ->
    is_boolean(kz_json:get_value(?PVT_IS_BILLABLE, NumDoc)).

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

-spec app_using(ne_binary()) -> api_ne_binary().
app_using(DID) ->
    case gb_sets:is_element(DID, get(callflow_DIDs)) of
        true -> <<"callflow">>;
        false ->
            case gb_sets:is_element(DID, get(trunkstore_DIDs)) of
                true -> <<"trunkstore">>;
                false -> undefined
            end
    end.

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
    M = kz_util:to_atom(<<"knm_", Type/binary>>, 'true'),
    M:generate_numbers(AccountId, kz_util:to_integer(StartingNumber), kz_util:to_integer(Quantity)).


-spec delete(ne_binary()) -> 'no_return'.
delete(Num) ->
    case knm_number:delete(Num, knm_number_options:default()) of
        {'ok', _} -> io:format("Removed ~s\n", [Num]);
        {'error', _R} -> io:format("ERROR: ~p\n", [_R])
    end,
    'no_return'.


-spec purge_discovery() -> 'no_return'.
purge_discovery() ->
    Purge = fun (NumberDb) -> purge_number_db(NumberDb, ?NUMBER_STATE_DISCOVERY) end,
    lists:foreach(Purge, knm_util:get_all_number_dbs()),
    'no_return'.

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
