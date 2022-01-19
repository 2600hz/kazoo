%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2022, 2600Hz
%%% @doc
%%% @author Sergey Korobkov
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kt_rates).

%% behaviour: tasks_provider

-export([init/0
        ,help/1, help/2, help/3
        ,output_header/1
        ,finish/2
        ]).

%% Verifiers
-export([direction/1
        ]).

%% Appliers
-export([export/2
        ,import/3
        ,delete/3
        ]).

-include("tasks.hrl").
-include("modules/kt_rates.hrl").

-define(CATEGORY, "rates").
-define(ACTIONS, [<<"export">>
                 ,<<"import">>
                 ,<<"delete">>
                 ]).
-define(RATES_VIEW, <<"rates/lookup">>).
-define(BULK_LIMIT, 500).

-define(DOLLAR_SIGN, 36). % = $\$ but makes fmt wonky atm

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    _ = tasks_bindings:bind(<<"tasks.help">>, ?MODULE, 'help'),
    _ = tasks_bindings:bind(<<"tasks."?CATEGORY".output_header">>, ?MODULE, 'output_header'),
    _ = tasks_bindings:bind(<<"tasks."?CATEGORY".direction">>, ?MODULE, 'direction'),
    _ = tasks_bindings:bind(<<"tasks."?CATEGORY".finish">>, ?MODULE, 'finish'),
    tasks_bindings:bind_actions(<<"tasks."?CATEGORY>>, ?MODULE, ?ACTIONS).

-spec output_header(kz_term:ne_binary()) -> kz_tasks:output_header().
output_header(<<"export">>) -> ?DOC_FIELDS.

-spec help(kz_json:object()) -> kz_json:object().
help(JObj) -> help(JObj, <<?CATEGORY>>).

-spec help(kz_json:object(), kz_term:ne_binary()) -> kz_json:object().
help(JObj, <<?CATEGORY>>=Category) ->
    lists:foldl(fun(Action, J) -> help(J, Category, Action) end, JObj, ?ACTIONS).

-spec help(kz_json:object(), kz_term:ne_binary(), kz_term:ne_binary()) -> kz_json:object().
help(JObj, <<?CATEGORY>>=Category, Action) ->
    kz_json:set_value([Category, Action], kz_json:from_list(action(Action)), JObj).

-spec action(kz_term:ne_binary()) -> kz_term:proplist().
action(<<"export">>) ->
    [{<<"description">>, <<"Export ratedeck">>}
    ,{<<"doc">>, <<"Export rates from the supplied ratedeck">>}
    ];

action(<<"import">>) ->
    %% prefix & cost are mandatory fields
    Mandatory = ?MANDATORY_FIELDS,
    Optional = ?DOC_FIELDS -- Mandatory,

    [{<<"description">>, <<"Bulk-import rates to a specified ratedeck">>}
    ,{<<"doc">>, <<"Creates rates from file">>}
    ,{<<"expected_content">>, <<"text/csv">>}
    ,{<<"mandatory">>, Mandatory}
    ,{<<"optional">>, Optional}
    ];

action(<<"delete">>) ->
    %% prefix is mandatory field
    Mandatory = [<<"prefix">>],
    Optional = ?DOC_FIELDS -- Mandatory,
    [{<<"description">>, <<"Bulk-remove rates where they match with all defined fields in CSV row">>}
    ,{<<"doc">>, <<"Delete rates from file">>}
    ,{<<"expected_content">>, <<"text/csv">>}
    ,{<<"mandatory">>, Mandatory}
    ,{<<"optional">>, Optional}
    ].

%%% Verifiers

-spec direction(kz_term:ne_binary()) -> boolean().
direction(<<"inbound">>) -> 'true';
direction(<<"outbound">>) -> 'true';
direction(_) -> 'false'.

%%% Appliers

-spec export(kz_tasks:extra_args(), kz_tasks:iterator()) -> kz_tasks:iterator().
export(ExtraArgs, 'init') ->
    case is_allowed(ExtraArgs) of
        'true' ->
            State = [{'db', get_ratedeck_db(ExtraArgs)}
                    ,{'options', [{'limit', ?BULK_LIMIT + 1}
                                 ,'include_docs'
                                 ]
                     }
                    ],
            export(ExtraArgs, State);
        'false' ->
            lager:warning("rates exporting is forbidden for account ~s, auth account ~s"
                         ,[maps:get('account_id', ExtraArgs)
                          ,maps:get('auth_account_id', ExtraArgs)
                          ]
                         ),
            {<<"task execution is forbidden">>, 'stop'}
    end;
export(_ExtraArgs, 'stop') -> 'stop';
export(_ExtraArgs, State) ->
    Db = props:get_value('db', State),
    Options = props:get_value('options', State),
    Limit = props:get_value(['options', 'limit'], State),
    case kz_datamgr:get_results(Db, ?RATES_VIEW, Options) of
        {'ok', []} -> 'stop';
        {'ok', Results} when length(Results) >= Limit ->
            {Head, Last} = split_results(Results),
            Rows = [to_csv_row(R) || R <- Head],
            NewOptions = props:set_values([{'startkey', kz_json:get_value(<<"key">>, Last)}
                                          ,{'startkey_docid', kz_json:get_value(<<"id">>, Last)}
                                          ]
                                         ,Options
                                         ),
            NewState = props:set_value('options', NewOptions, State),
            {Rows, NewState};
        {'ok', Results} ->
            Rows = [to_csv_row(R) || R <- Results],
            {Rows, 'stop'}
    end.

-spec import(kz_tasks:extra_args(), map() | kz_tasks:iterator(), kz_tasks:args()) -> kz_tasks:iterator().
import(ExtraArgs, 'init', Args) ->
    case is_allowed(ExtraArgs) of
        'true' ->
            lager:info("import is allowed, continuing"),
            import(ExtraArgs, #{}, Args);
        'false' ->
            lager:warning("rates importing is forbidden for account ~s, auth account ~s"
                         ,[maps:get('account_id', ExtraArgs)
                          ,maps:get('auth_account_id', ExtraArgs)
                          ]
                         ),
            {<<"task execution is forbidden">>, 'stop'}
    end;
import(_ExtraArgs, Map, Args) ->
    Rate = generate_row(Args, 'true'),
    Db = ratedeck_db(Rate),

    BulkLimit = kz_datamgr:max_bulk_insert(),

    case maps:get(Db, Map, 'undefined') of
        'undefined' ->
            lager:debug("adding rate ~s (prefix ~p) to ratedeck '~s'"
                       ,[kz_doc:id(Rate), kzd_rates:prefix(Rate), Db]
                       ),
            {'ok', Map#{Db => {1, [Rate]}}};
        {BulkLimit, Rates} ->
            lager:info("saving ~b rates to '~s'", [BulkLimit, Db]),
            kz_datamgr:suppress_change_notice(),
            save_rates(Db, [Rate | Rates]),
            kz_datamgr:enable_change_notice(),
            {'ok', Map#{Db => {0, []}}};
        {Size, Rates} ->
            {'ok', Map#{Db => {Size+1, [Rate | Rates]}}}
    end.

-spec delete(kz_tasks:extra_args(), kz_tasks:iterator(), kz_tasks:args()) -> kz_tasks:iterator().
delete(ExtraArgs, 'init', Args) ->
    kz_datamgr:suppress_change_notice(),
    case is_allowed(ExtraArgs) of
        'true' ->
            State = [{'limit', ?BULK_LIMIT}
                    ,{'count', 0}
                    ,{'map', #{}}
                    ],
            delete(ExtraArgs, State, Args);
        'false' ->
            lager:warning("rates deleting is forbidden for account ~s, auth account ~s"
                         ,[maps:get('account_id', ExtraArgs)
                          ,maps:get('auth_account_id', ExtraArgs)
                          ]
                         ),
            {<<"task execution is forbidden">>, 'stop'}
    end;
delete(_ExtraArgs, State, Args) ->
    RateJObj = generate_row(Args, 'false'),
    Db = ratedeck_db(RateJObj),

    Limit = props:get_value('limit', State),
    Count = props:get_value('count', State) + 1,

    Map = props:get_value('map', State),
    NewMap = Map#{Db => [RateJObj | maps:get(Db, Map, [])]},

    case Count rem Limit of
        0 ->
            delete_rates(NewMap),
            {'ok'
            ,props:set_values([{'count', Count}
                              ,{'map', #{}}
                              ]
                             ,State
                             )
            };
        _Rem ->
            lager:debug("adding rate ~s (prefix ~p) to bulk delete list"
                       ,[kz_doc:id(RateJObj), kzd_rates:prefix(RateJObj)]
                       ),
            {'ok'
            ,props:set_values([{'count', Count}
                              ,{'map', NewMap}
                              ]
                             ,State
                             )
            }
    end.

-spec finish(kz_term:ne_binary(), any()) -> any().
finish(<<"import">>, 'init') ->
    'ok';
finish(<<"import">>, Dict) ->
    _Size = maps:size(Dict),
    lager:info("importing ~p ratedeck~s", [_Size, maybe_plural(_Size)]),
    _ = maps:map(fun import_rates_into_ratedeck/2, Dict);
finish(<<"delete">>, 'init') ->
    kz_datamgr:enable_change_notice(),
    'ok';
finish(<<"delete">>, State) ->
    lager:debug("finishing delete"),
    Map = props:get_value('map', State, #{}),
    delete_rates(Map),
    kz_datamgr:enable_change_notice(),
    _ = [kzs_publish:publish_db(Db, <<"edited">>) || Db <- maps:keys(Map)],
    'ok'.

-spec import_rates_into_ratedeck(kz_term:ne_binary(), {non_neg_integer(), kz_json:objects()}) -> 'true'.
import_rates_into_ratedeck(RatedeckDb, {0, []}) ->
    kz_datamgr:enable_change_notice(),
    lager:debug("importing into ratedeck '~s' complete", [RatedeckDb]),
    kzs_publish:publish_db(RatedeckDb, <<"edited">>);
import_rates_into_ratedeck(RatedeckDb, {_C, Rates}) ->
    lager:debug("importing ~p rate~s into ratedeck '~s'"
               ,[_C, maybe_plural(_C), RatedeckDb]
               ),

    kz_datamgr:suppress_change_notice(),
    save_rates(RatedeckDb, Rates),
    import_rates_into_ratedeck(RatedeckDb, {0, []}).

-spec maybe_plural(pos_integer()) -> string().
maybe_plural(1) -> "";
maybe_plural(_) -> "s".

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec split_results(kz_json:objects()) -> {kz_json:objects(), kz_json:object()}.
split_results([_|_] = JObjs) ->
    {Head, [Last]} = lists:split(length(JObjs)-1, JObjs),
    %% !!!
    %% workaround until https://github.com/benoitc/couchbeam/pull/160
    %% !!!
    case kz_json:get_value(<<"key">>, lists:last(Head)) =:= kz_json:get_value(<<"key">>, Last) of
        'true' -> split_results(Head);
        'false' -> {Head, Last}
    end.

-spec is_allowed(kz_tasks:extra_args()) -> boolean().
is_allowed(ExtraArgs) ->
    AuthAccountId = maps:get('auth_account_id', ExtraArgs),
    AccountId = maps:get('account_id', ExtraArgs),
    {'ok', AccountDoc} = kzd_accounts:fetch(AccountId),
    {'ok', AuthAccountDoc} = kzd_accounts:fetch(AuthAccountId),
    kzd_accounts:is_in_account_hierarchy(AuthAccountId, AccountId, 'true')
    %% Serve request for reseller rates
        andalso kzd_accounts:is_reseller(AccountDoc)
    %% or serve requests from SuperAdmin
        orelse kzd_accounts:is_superduper_admin(AuthAccountDoc).

-spec get_ratedeck_db(kz_tasks:extra_args()) -> kz_term:ne_binary().
get_ratedeck_db(_ExtraArgs) ->
    %% TODO: per account DB?
    ?KZ_RATES_DB.

-spec to_csv_row(kz_json:object()) -> kz_csv:row().
to_csv_row(Row) ->
    Doc = kz_json:get_json_value(<<"doc">>, Row),
    [kz_json:get_binary_value(Key, Doc) || Key <- ?DOC_FIELDS].

generate_row(Args, 'true') ->
    RateJObj = kzd_rates:from_map(Args),
    validate_row(RateJObj, ?DOC_FIELDS);
generate_row(Args, 'false') ->
    validate_row(kzd_rates:from_map(Args), ?DOC_FIELDS).

-spec validate_row(kzd_rates:doc(), kz_json:keys()) -> kzd_rates:doc().
validate_row(RateJObj, [Key|Keys]) -> validate_row(validate_field(RateJObj, Key), Keys);
validate_row(RateJObj, []) -> RateJObj.

-spec validate_field(kzd_rates:doc(), kz_json:key()) -> kzd_rates:doc().
validate_field(RateJObj, <<"rate_name">>) ->
    Value = maybe_generate_name(RateJObj),
    kzd_rates:set_rate_name(RateJObj, Value);
validate_field(RateJObj, <<"weight">>) ->
    Value = maybe_generate_weight(RateJObj),
    kzd_rates:set_weight(RateJObj, Value);
validate_field(RateJObj, <<"caller_id_numbers">>) ->
    case maybe_generate_caller_id_numbers(RateJObj) of
        'undefined' ->
            kz_json:delete_key(<<"caller_id_numbers">>, RateJObj);
        Value -> kzd_rates:set_caller_id_numbers(RateJObj, kz_binary:join(Value, <<":">>))
    end;
validate_field(RateJObj, <<"routes">>) ->
    Value = maybe_generate_routes(RateJObj),
    kzd_rates:set_routes(RateJObj, Value);
validate_field(RateJObj, Key) ->
    Getter = binary_to_atom(<<Key/binary>>, 'utf8'),
    Setter = binary_to_atom(<<"set_", Key/binary>>, 'utf8'),
    case kzd_rates:Getter(RateJObj) of
        'undefined' ->
            kz_json:delete_key(Key, RateJObj);
        Value ->
            kzd_rates:Setter(RateJObj, Value)
    end.

-spec save_rates(kz_term:ne_binary(), kzd_rates:docs()) -> 'ok'.
save_rates(Db, Rates) ->
    JObjs = maybe_override_rate(Db, Rates),
    case kz_datamgr:save_docs(Db, JObjs) of
        {'ok', _Result} ->
            refresh_selectors_index(Db);
        {'error', 'not_found'} ->
            lager:debug("failed to find database ~s", [Db]),
            init_db(Db),
            case kz_datamgr:save_docs(Db, JObjs) of
                {'ok', _} ->
                    refresh_selectors_index(Db);
                {'error', _Reason} ->
                    lager:debug("failed to saved ~b rates to reatedeck ~s", [length(Rates), Db])
            end;
        {'error', _Reason} ->
            lager:debug("failed to saved ~b rates to reatedeck ~s", [length(Rates), Db])
    end.

-spec maybe_override_rate(kz_term:ne_binary(), kz_json:objects()) -> kz_json:objects().
maybe_override_rate(Db, Rates) ->
    Ids = [kz_doc:id(JObj) || JObj <- Rates],
    case kz_datamgr:open_docs(Db, Ids) of
        {'ok', Result} ->
            %% if a rate was soft-deleted allow to unsoft-deleted it
            DbJObjs = maps:from_list(
                        [{kz_doc:id(JObj), kz_json:delete_key(<<"pvt_deleted">>, kz_json:get_value(<<"doc">>, JObj))}
                         || JObj <- Result,
                            %% open_docs could include `null' as doc without even have `error' when the document is hard
                            %% deleted
                            kz_json:is_json_object(kz_json:get_value(<<"doc">>, JObj))
                        ]
                       ),
            [kz_json:merge(maps:get(kz_doc:id(JObj), DbJObjs, kz_json:new()), update_pvt(Db, JObj))
             || JObj <- Rates
            ];
        {'error', _Reason} ->
            lager:debug("failed with error ~p to fetch and merge rates from ~s, hoping for the best on saving"
                       ,[_Reason, Db]
                       ),
            Rates
    end.

-spec update_pvt(kz_term:ne_binary(), kz_json:object()) -> kz_json:object().
update_pvt(Db, JObj0) ->
    JObj = kz_json:set_value(<<"pvt_kt_rates_modified">>, kz_time:now_s(), JObj0),
    kz_doc:set_account_db(kz_doc:update_pvt_parameters(JObj, 'undefined'), Db).

-spec delete_rates(map()) -> 'ok'.
delete_rates(Map) ->
    lager:debug("deleteing rates from ~b database(s)", [maps:size(Map)]),
    maps:fold(fun delete_rates/3, 'ok', Map).

-spec delete_rates(kz_term:ne_binary(), kz_json:objects(), 'ok') -> 'ok'.
delete_rates(Db, JObjs, 'ok') ->
    PrefixGrouped = group_by_prefix(JObjs),
    Keys = maps:keys(PrefixGrouped),
    Options = [{'keys', Keys}
              ,'include_docs'
              ],
    case kz_datamgr:get_results(Db, ?RATES_VIEW, Options) of
        {'ok', []} ->
            lager:debug("no requested rates were found in db ~s", [Db]);
        {'ok', Results} ->
            {NotFound, Docs} = lists:foldl(fun maybe_delete_rate/2
                                          ,{PrefixGrouped, []}
                                          ,Results
                                          ),
            delete_rates(Db, Docs),
            maps:fold(fun log_not_found/3, 'ok', NotFound);
        {'error', Reason} ->
            lager:debug("failed to get results from db ~s: ~p", [Db, Reason])
    end.

-spec group_by_prefix(kz_json:objects()) -> map().
group_by_prefix(JObjs) ->
    lists:foldl(fun(JObj, Acc) ->
                        Prefix = kzd_rates:prefix(JObj),
                        Acc#{kz_term:to_integer(Prefix) => [JObj | maps:get(Prefix, Acc, [])]}
                end
               ,#{}
               ,JObjs
               ).

-spec log_not_found(integer(), kz_json:objects(), 'ok') -> 'ok'.
log_not_found(_Prefix, JObjs, 'ok') ->
    lists:foreach(fun log_not_found/1, JObjs).

-spec log_not_found(kz_json:object()) -> 'ok'.
log_not_found(JObj) ->
    lager:debug("requested rate ~s (prefix: ~p) (id: ~s) was not found"
               ,[kzd_rates:rate_name(JObj)
                ,kzd_rates:prefix(JObj)
                ,kz_doc:id(JObj)
                ]
               ).

-spec delete_rates(kz_term:ne_binary(), kz_json:objects()) -> 'ok'.
delete_rates(_, []) ->
    lager:debug("nothing to delete");
delete_rates(Db, Docs) ->
    case kz_datamgr:del_docs(Db, Docs) of
        {'ok', _} ->
            lager:debug("deleted ~b rates from db ~s", [length(Docs), Db]);
        {'error', Reason} ->
            lager:debug("failed to delete ~b doc(s) from db ~s: ~p", [length(Docs), Db, Reason])
    end.

-spec refresh_selectors_index(kz_term:ne_binary()) -> 'ok'.
refresh_selectors_index(Db) ->
    _ = kz_datamgr:all_docs(Db, [{'limit', 1}]),
    _ = kz_datamgr:get_results(Db, ?RATES_VIEW, [{'limit', 1}]),
    'ok'.

-spec init_db(kz_term:ne_binary()) -> 'ok'.
init_db(Db) ->
    _Created = kz_datamgr:db_create(Db),
    lager:debug("created ~s: ~s", [Db, _Created]),
    _ = kapps_maintenance:refresh(Db),
    lager:info("initialized new ratedeck ~s", [Db]).

-spec maybe_delete_rate(kz_json:object(), {map(), kz_json:objects()}) ->
          {map(), kz_json:objects()}.
maybe_delete_rate(JObj, {Map, Docs}) ->
    Prefix = kz_term:to_integer(kz_json:get_value(<<"key">>, JObj)),
    Doc = kz_json:get_value(<<"doc">>, JObj),

    ReqRates = maps:get(Prefix, Map),

    %% Delete docs only if its match with all defined fields in CSV row
    case should_delete_rate(ReqRates, Doc)  of
        {'true', NewReqRates} ->
            {Map#{Prefix => NewReqRates}, [Doc | Docs]};
        'false' ->
            {Map, Docs}
    end.

-spec should_delete_rate(kz_json:objects(), kz_json:object()) -> 'false' | {'true', kz_json:objects()}.
should_delete_rate([], _) ->
    'false';
should_delete_rate([ReqRate | ReqRates], Doc) ->
    case kz_json:all(fun({_, <<>>}) ->
                             'true';
                        ({ReqKey, ReqValue}) ->
                             kz_json:get_value(ReqKey, Doc, <<>>) =:= ReqValue
                     end
                    ,ReqRate
                    )
    of
        'true' -> {'true', ReqRates};
        'false' -> should_delete_rate(ReqRates, Doc)
    end.

-spec maybe_generate_name(kzd_rates:doc()) -> kz_term:ne_binary().
maybe_generate_name(RateJObj) ->
    maybe_generate_name(RateJObj, kzd_rates:rate_name(RateJObj)).

-spec maybe_generate_name(kzd_rates:doc(), kz_term:api_ne_binary()) -> kz_term:ne_binary().
maybe_generate_name(RateJObj, 'undefined') ->
    generate_name(kzd_rates:prefix(RateJObj)
                 ,kzd_rates:iso_country_code(RateJObj, <<"XX">>)
                 ,kzd_rates:direction(RateJObj, [])
                 );
maybe_generate_name(_RateJObj, Name) -> Name.

-spec generate_name(kz_term:ne_binary() | pos_integer(), kz_term:api_ne_binary(), kz_term:ne_binaries()) -> kz_term:ne_binary().
generate_name(Prefix, ISO, Directions) when is_integer(Prefix) ->
    generate_name(kz_term:to_binary(Prefix), ISO, Directions);
generate_name(Prefix, 'undefined', []) when is_binary(Prefix) ->
    Prefix;
generate_name(Prefix, ISO, []) ->
    <<ISO/binary, "_", Prefix/binary>>;
generate_name(Prefix, 'undefined', Directions) ->
    Direction = kz_binary:join(Directions, <<"_">>),
    <<Direction/binary, "_", Prefix/binary>>;
generate_name(Prefix, ISO, Directions) ->
    Direction = kz_binary:join(Directions, <<"_">>),
    <<Direction/binary, "_", ISO/binary, "_", Prefix/binary>>.

-spec maybe_generate_weight(kzd_rates:doc()) -> integer().
maybe_generate_weight(RateJObj) ->
    maybe_generate_weight(RateJObj, kzd_rates:weight(RateJObj)).

-spec maybe_generate_weight(kzd_rates:doc(), kz_term:api_integer()) -> integer().
maybe_generate_weight(RateJObj, 'undefined') ->
    generate_weight(kzd_rates:prefix(RateJObj)
                   ,kzd_rates:rate_cost(RateJObj, 0)
                   ,kzd_rates:private_cost(RateJObj)
                   );
maybe_generate_weight(_RateJObj, Weight) -> kzd_rates:constrain_weight(Weight).

-spec generate_weight(kz_term:ne_binary() | pos_integer(), kz_currency:units(), kz_currency:units()) ->
          kzd_rates:weight_range().
generate_weight(Prefix, UnitCost, UnitIntCost) when is_integer(Prefix) ->
    generate_weight(kz_term:to_binary(Prefix), UnitCost, UnitIntCost);
generate_weight(?NE_BINARY = Prefix, UnitCost, UnitIntCost) ->
    UnitCostToUse = maybe_default(UnitIntCost, UnitCost),
    CostToUse = kz_currency:units_to_dollars(UnitCostToUse),

    Weight = (byte_size(Prefix) * 10) - trunc(CostToUse * 100),
    kzd_rates:constrain_weight(Weight).

-spec maybe_default(kz_currency:units(), kz_currency:units()) -> kz_currency:units().
maybe_default(0, Default) -> Default;
maybe_default(Value, _Default) -> Value.

-spec maybe_generate_caller_id_numbers(kzd_rates:doc()) -> kz_term:api_ne_binaries().
maybe_generate_caller_id_numbers(RateJObj) ->
    maybe_generate_caller_id_numbers(RateJObj, kzd_rates:caller_id_numbers(RateJObj)).

-spec maybe_generate_caller_id_numbers(kzd_rates:doc(), kz_term:ne_binary()) -> kz_term:api_ne_binaries().
maybe_generate_caller_id_numbers(_RateJObj, CIDNumbers)  when is_binary(CIDNumbers) ->
    lists:map(fun(X) -> <<"^\\+?", X/binary, ".+", ?DOLLAR_SIGN>> end
             ,binary:split(CIDNumbers, <<":">>, ['global'])
             );
maybe_generate_caller_id_numbers(_RateJObj, _CIDNumbers) ->
    'undefined'.

-spec maybe_generate_routes(kzd_rates:doc()) -> kz_term:api_ne_binaries().
maybe_generate_routes(RateJObj) ->
    %% don't change this to accessor, csv could be a binary not a list
    Routes = kz_json:get_value(<<"routes">>, RateJObj),
    case kz_term:is_ne_binary(Routes) of
        'true' ->
            NewRoutes = try kz_json:unsafe_decode(Routes)
                        catch _:_ -> Routes
                        end,
            maybe_generate_routes(RateJObj, NewRoutes);
        'false' ->
            maybe_generate_routes(RateJObj, Routes)
    end.

-spec maybe_generate_routes(kzd_rates:doc(), binary() | [binary()] | any()) -> kz_term:api_ne_binaries().
maybe_generate_routes(_RateJObj, Routes) when is_list(Routes) ->
    [Route || Route <- Routes,
              kz_term:is_ne_binary(Route)
    ];
maybe_generate_routes(RateJObj, Route) when is_binary(Route) ->
    case kz_term:is_ne_binary(Route) of
        'true' -> [Route];
        'false' -> maybe_generate_routes(RateJObj, 'undefined')
    end;
maybe_generate_routes(RateJObj, _Routes) ->
    lager:debug("no valid routes, generating default route using prefix"),
    kz_json:get_value(<<"routes">>, kzd_rates:set_default_route(RateJObj)).

-spec ratedeck_db(kzd_rates:doc()) -> kz_term:ne_binary().
ratedeck_db(RateJObj) ->
    RatedeckId = kzd_rates:ratedeck_id(RateJObj),
    case kz_term:is_ne_binary(RatedeckId) of
        'true' ->
            kzd_ratedeck:format_ratedeck_db(RatedeckId);
        'false' ->
            ?KZ_RATES_DB
    end.
