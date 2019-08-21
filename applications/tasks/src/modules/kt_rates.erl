%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2019, 2600Hz
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
    [{<<"description">>, <<"Bulk-remove rates">>}
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

-spec import(kz_tasks:extra_args(), dict:dict() | kz_tasks:iterator(), kz_tasks:args()) -> kz_tasks:iterator().
import(ExtraArgs, 'init', Args) ->
    case is_allowed(ExtraArgs) of
        'true' ->
            lager:info("import is allowed, continuing"),
            import(ExtraArgs, dict:new(), Args);
        'false' ->
            lager:warning("rates importing is forbidden for account ~s, auth account ~s"
                         ,[maps:get('account_id', ExtraArgs)
                          ,maps:get('auth_account_id', ExtraArgs)
                          ]
                         ),
            {<<"task execution is forbidden">>, 'stop'}
    end;
import(_ExtraArgs, Dict, Args) ->
    Rate = generate_row(Args),
    Db = kzd_ratedeck:format_ratedeck_db(kzd_rates:ratedeck_id(Rate, ?KZ_RATES_DB)),

    BulkLimit = kz_datamgr:max_bulk_insert(),

    case dict:find(Db, Dict) of
        'error' ->
            lager:debug("adding prefix ~p to ratedeck '~s'", [kzd_rates:prefix(Rate), Db]),
            {'ok', dict:store(Db, {1, [Rate]}, Dict)};
        {'ok', {BulkLimit, Rates}} ->
            lager:info("saving ~b rates to '~s'", [BulkLimit, Db]),
            kz_datamgr:suppress_change_notice(),
            save_rates(Db, [Rate | Rates]),
            kz_datamgr:enable_change_notice(),
            {'ok', dict:store(Db, {0, []}, Dict)};
        {'ok', {Size, Rates}} ->
            {'ok', dict:store(Db, {Size+1, [Rate | Rates]}, Dict)}
    end.

-spec delete(kz_tasks:extra_args(), kz_tasks:iterator(), kz_tasks:args()) -> kz_tasks:iterator().
delete(ExtraArgs, 'init', Args) ->
    kz_datamgr:suppress_change_notice(),
    case is_allowed(ExtraArgs) of
        'true' ->
            State = [{'db', get_ratedeck_db(ExtraArgs)}
                    ,{'limit', ?BULK_LIMIT}
                    ,{'count', 0}
                    ,{'keys', []}
                    ,{'dict', dict:new()}
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
    Rate = kzd_rates:from_map(Args),

    Limit = props:get_value('limit', State),
    Count = props:get_value('count', State) + 1,
    P = kzd_rates:prefix(Rate),

    %% override account-ID from task props
    Dict = dict:append(P, Rate, props:get_value('dict', State)),
    Keys = [P | props:get_value('keys', State)],
    case Count rem Limit of
        0 ->
            Db = props:get_value('db', State),
            delete_rates(Db, Keys, Dict),
            {[], props:set_values([{'count', Count}
                                  ,{'keys', []}
                                  ,{'dict', dict:new()}
                                  ]
                                 ,State
                                 )
            };
        _Rem ->
            {[], props:set_values([{'count', Count}
                                  ,{'keys', Keys}
                                  ,{'dict', Dict}
                                  ]
                                 ,State
                                 )
            }
    end.

-spec finish(kz_term:ne_binary(), any()) -> any().
finish(<<"import">>, Dict) ->
    _Size = dict:size(Dict),
    lager:info("importing ~p ratedeck~s", [_Size, maybe_plural(_Size)]),
    _ = dict:map(fun import_rates_into_ratedeck/2, Dict);
finish(<<"delete">>, State) ->
    Db = props:get_value('db', State),
    Keys = props:get_value('keys', State),
    Dict = props:get_value('dict', State),
    delete_rates(Db, Keys, Dict),
    kz_datamgr:enable_change_notice(),
    kzs_publish:publish_db(Db, <<"edited">>).

-spec import_rates_into_ratedeck(kz_term:ne_binary(), {non_neg_integer(), kz_json:objects()}) -> 'true'.
import_rates_into_ratedeck(Ratedeck, {0, []}) ->
    RatedeckDb = kzd_ratedeck:format_ratedeck_db(Ratedeck),
    kz_datamgr:enable_change_notice(),
    lager:debug("importing into ratedeck '~s' complete", [RatedeckDb]),
    kzs_publish:publish_db(RatedeckDb, <<"edited">>);
import_rates_into_ratedeck(Ratedeck, {_C, Rates}) ->
    RatedeckDb = kzd_ratedeck:format_ratedeck_db(Ratedeck),
    lager:debug("importing ~p rate~s into ratedeck '~s'"
               ,[_C, maybe_plural(_C), RatedeckDb]
               ),

    kz_datamgr:suppress_change_notice(),
    save_rates(RatedeckDb, Rates),
    import_rates_into_ratedeck(Ratedeck, {0, []}).

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

-spec maybe_override_rate(kz_tasks:args()) -> kzd_rates:doc().
maybe_override_rate(Args) ->
    RateJObj = kzd_rates:from_map(Args),
    Id = kz_doc:id(RateJObj),
    Db = kzd_ratedeck:format_ratedeck_db(kzd_rates:ratedeck_id(RateJObj, ?KZ_RATES_DB)),

    case kz_datamgr:open_cache_doc(Db, Id) of
        {'ok', ExistingJObj} ->
            lager:debug("updating existing rate ~s(~s) in ~s", [Id, kz_doc:revision(ExistingJObj), Db]),
            kz_json:merge(ExistingJObj, RateJObj);
        {'error', 'not_found'} -> RateJObj
    end.

-spec generate_row(kz_tasks:args()) -> kzd_rates:doc().
generate_row(Args) ->
    RateJObj = maybe_override_rate(Args),
    Prefix = kz_term:to_binary(kzd_rates:prefix(RateJObj)),
    lager:debug("create rate for prefix ~s(~s)", [Prefix, kz_doc:id(RateJObj)]),
    validate_row(RateJObj, ?DOC_FIELDS).

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
        'undefined' -> RateJObj;
        Value -> kzd_rates:set_caller_id_numbers(RateJObj, kz_binary:join(Value, <<":">>))
    end;
validate_field(RateJObj, <<"routes">>) ->
    Value = maybe_generate_routes(RateJObj),
    kzd_rates:set_routes(RateJObj, Value);
validate_field(RateJObj, Key) ->
    Getter = binary_to_atom(<<Key/binary>>, 'utf8'),
    Setter = binary_to_atom(<<"set_", Key/binary>>, 'utf8'),
    Value = kzd_rates:Getter(RateJObj),
    kzd_rates:Setter(RateJObj, Value).

-spec save_rates(kz_term:ne_binary(), kzd_rates:docs()) -> 'ok'.
save_rates(Db, Rates) ->
    case kz_datamgr:save_docs(Db, Rates) of
        {'ok', _Result} ->
            refresh_selectors_index(Db);
        {'error', 'not_found'} ->
            lager:debug("failed to find database ~s", [Db]),
            init_db(Db),
            save_rates(Db, Rates);
        %% Workaround, need to fix!
        %% We assume that is everything ok and try to refresh index
        {'error', 'timeout'} -> refresh_selectors_index(Db)
    end.

-spec delete_rates(kz_term:ne_binary(), list(), dict:dict()) -> 'ok'.
delete_rates(Db, Keys, Dict) ->
    Options = [{'keys', Keys}
              ,'include_docs'
              ],
    case kz_datamgr:get_results(Db, ?RATES_VIEW, Options) of
        {'ok', []} -> 'ok';
        {'ok', Results} ->
            Docs = lists:filtermap(fun(R) -> maybe_delete_rate(R, Dict) end, Results),
            do_delete_rates(Db, Docs)
    end.

-spec do_delete_rates(kz_term:ne_binary(), kz_json:objects()) -> 'ok'.
do_delete_rates(_Db, []) -> 'ok';
do_delete_rates(Db, Docs) ->
    {Head, Rest} = case length(Docs) > ?BULK_LIMIT
                       andalso lists:split(?BULK_LIMIT, Docs)
                   of
                       'false' -> {Docs, []};
                       {H, T} -> {H, T}
                   end,
    case kz_datamgr:del_docs(Db, Head) of
        {'ok', _} -> refresh_selectors_index(Db);
        %% Workaround, need to fix!
        %% We assume that is everything ok and try to refresh index
        {'error', 'timeout'} -> refresh_selectors_index(Db)
    end,
    do_delete_rates(Db, Rest).

-spec refresh_selectors_index(kz_term:ne_binary()) -> 'ok'.
refresh_selectors_index(Db) ->
    {'ok', _} = kz_datamgr:all_docs(Db, [{'limit', 1}]),
    {'ok', _} = kz_datamgr:get_results(Db, <<"rates/lookup">>, [{'limit', 1}]),
    'ok'.

-spec init_db(kz_term:ne_binary()) -> 'ok'.
init_db(Db) ->
    _Created = kz_datamgr:db_create(Db),
    lager:debug("created ~s: ~s", [Db, _Created]),
    _ = kapps_maintenance:refresh(Db),
    lager:info("initialized new ratedeck ~s", [Db]).

-spec maybe_delete_rate(kz_json:object(), dict:dict()) -> kz_json:object() | 'false'.
maybe_delete_rate(JObj, Dict) ->
    Prefix = kz_term:to_integer(kz_json:get_value(<<"key">>, JObj)),
    Doc = kz_json:get_value(<<"doc">>, JObj),
    ReqRates = dict:fetch(Prefix, Dict),
    %% Delete docs only if its match with all defined fields in CSV row
    case lists:any(fun(ReqRate) ->
                           lists:all(fun({ReqKey, ReqValue}) ->
                                             kz_json:get_value(ReqKey, Doc) =:= ReqValue
                                     end
                                    ,ReqRate
                                    )
                   end
                  ,ReqRates
                  )
    of
        'true' -> {'true', Doc};
        'false' -> 'false'
    end.

-spec maybe_default(kz_currency:units(), kz_currency:units()) -> kz_currency:units().
maybe_default(0, Default) -> Default;
maybe_default(Value, _Default) -> Value.

-spec maybe_generate_name(kzd_rates:doc()) -> kz_term:ne_binary().
maybe_generate_name(RateJObj) ->
    maybe_generate_name(RateJObj, kzd_rates:rate_name(RateJObj)).

-spec maybe_generate_name(kzd_rates:doc(), kz_term:api_ne_binary()) -> kz_term:ne_binary().
maybe_generate_name(RateJObj, 'undefined') ->
    generate_name(kzd_rates:prefix(RateJObj)
                 ,kzd_rates:iso_country_code(RateJObj)
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
    maybe_generate_weight(RateJObj, kzd_rates:weight(RateJObj, 'undefined')).

-spec maybe_generate_weight(kzd_rates:doc(), kz_term:api_integer()) -> integer().
maybe_generate_weight(RateJObj, 'undefined') ->
    generate_weight(kzd_rates:prefix(RateJObj)
                   ,kzd_rates:rate_cost(RateJObj)
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

-spec maybe_generate_caller_id_numbers(kzd_rates:doc()) -> kz_term:api_ne_binaries().
maybe_generate_caller_id_numbers(RateJObj) ->
    maybe_generate_caller_id_numbers(RateJObj, kz_json:get_ne_binary_value(<<"caller_id_numbers">>, RateJObj)).

-spec maybe_generate_caller_id_numbers(kzd_rates:doc(), kz_term:ne_binary()) -> kz_term:api_ne_binaries().
maybe_generate_caller_id_numbers(_RateJObj, CIDNumbers)  when is_binary(CIDNumbers) ->
    lists:map(fun(X) -> <<"^\\+?", X/binary, ".+", ?DOLLAR_SIGN>> end
             ,binary:split(CIDNumbers, <<":">>, ['global'])
             );
maybe_generate_caller_id_numbers(_RateJObj, _CIDNumbers) ->
    'undefined'.

-spec maybe_generate_routes(kzd_rates:doc()) -> kz_term:api_ne_binaries().
maybe_generate_routes(RateJObj) ->
    Routes = kz_json:get_value(<<"routes">>, RateJObj),
    case is_binary(Routes) of
        'true' ->
            NewRoutes = try kz_json:unsafe_decode(Routes)
                        catch _:_ -> Routes
                        end,
            maybe_generate_routes(RateJObj, NewRoutes);
        'false' ->
            maybe_generate_routes(RateJObj, Routes)
    end.

-spec maybe_generate_routes(kzd_rates:doc(), kz_term:ne_binary()) -> kz_term:api_ne_binaries().
maybe_generate_routes(_RateJObj, Routes) when is_list(Routes) ->
    Routes;
maybe_generate_routes(_RateJObj, Routes) when is_binary(Routes) ->
    [Routes];
maybe_generate_routes(RateJObj, _Routes) ->
    lager:debug("no valid routes, generating default route using prefix"),
    kz_json:get_value(<<"routes">>, kzd_rates:set_default_route(RateJObj)).
