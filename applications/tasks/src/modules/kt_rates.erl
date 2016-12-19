%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2016, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Sergey Korobkov
%%%-------------------------------------------------------------------
-module(kt_rates).
%% behaviour: tasks_provider

-export([init/0
        ,help/1, help/2, help/3
        ,output_header/1
        ,cleanup/2
        ]).

%% Verifiers
-export([direction/1
        ]).

%% Appliers
-export([export/2
        ,import/18
        ,delete/18
        ]).

-include("tasks.hrl").

-define(CATEGORY, "rates").
-define(ACTIONS, [<<"export">>
                 ,<<"import">>
                 ,<<"delete">>
                 ]).
-define(RATES_VIEW, <<"rates/lookup">>).
-define(BULK_LIMIT, 500).

%% All this macros MUST be synchronized to each other
-define(VARS, Prefix, Cost, RatedeckName, AccountId, ISO,
        Desc, Name, Surcharge, Increment, Minimum,
        Direction, IntCost, IntSurcharge, NoCharge, Weight,
        Version).
-define(SPEC_FIELDS, api_binary(), api_binary(), api_binary(), api_binary(), api_binary(),
        api_binary(), api_binary(), api_binary(), api_binary(), api_binary(),
        api_binary(), api_binary(), api_binary(), api_binary(), api_binary(),
        api_binary()).
-define(DOC_FIELDS, <<"prefix">>, <<"rate_cost">>, <<"ratedeck_name">>, <<"account_id">>, <<"iso_country_code">>,
        <<"description">>, <<"rate_name">>, <<"rate_surcharge">>, <<"rate_increment">>, <<"rate_minimum">>,
        <<"direction">>, <<"pvt_rate_cost">>, <<"pvt_rate_surcharge">>, <<"rate_nocharge_time">>, <<"weight">>,
        <<"rate_version">>).

%%%===================================================================
%%% API
%%%===================================================================

-spec init() -> 'ok'.
init() ->
    _ = tasks_bindings:bind(<<"tasks.help">>, ?MODULE, 'help'),
    _ = tasks_bindings:bind(<<"tasks."?CATEGORY".output_header">>, ?MODULE, 'output_header'),
    _ = tasks_bindings:bind(<<"tasks."?CATEGORY".direction">>, ?MODULE, 'direction'),
    _ = tasks_bindings:bind(<<"tasks."?CATEGORY".cleanup">>, ?MODULE, 'cleanup'),
    tasks_bindings:bind_actions(<<"tasks."?CATEGORY>>, ?MODULE, ?ACTIONS).

-spec output_header(ne_binary()) -> kz_csv:row().
output_header(<<"export">>) -> [?DOC_FIELDS].

-spec help(kz_json:object()) -> kz_json:object().
help(JObj) -> help(JObj, <<?CATEGORY>>).

-spec help(kz_json:object(), ne_binary()) -> kz_json:object().
help(JObj, <<?CATEGORY>>=Category) ->
    lists:foldl(fun(Action, J) -> help(J, Category, Action) end, JObj, ?ACTIONS).

-spec help(kz_json:object(), ne_binary(), ne_binary()) -> kz_json:object().
help(JObj, <<?CATEGORY>>=Category, Action) ->
    kz_json:set_value([Category, Action], kz_json:from_list(action(Action)), JObj).

-spec action(ne_binary()) -> kz_proplist().
action(<<"export">>) ->
    [{<<"description">>, <<"Export ratedeck">>}
    ,{<<"doc">>, <<"Export rates from \"ratedeck\" DB">>}
    ];

action(<<"import">>) ->
    %% prefix & cost is mandatory fields
    {Mandatory, Optional} = lists:split(2, [?DOC_FIELDS]),
    [{<<"description">>, <<"Bulk-import rates">>}
    ,{<<"doc">>, <<"Creates rates from file">>}
    ,{<<"expected_content">>, <<"text/csv">>}
    ,{<<"mandatory">>, Mandatory}
    ,{<<"optional">>, Optional}
    ];

action(<<"delete">>) ->
    %% prefix is mandatory field
    {Mandatory, Optional} = lists:split(1, [?DOC_FIELDS]),
    [{<<"description">>, <<"Bulk-remove rates">>}
    ,{<<"doc">>, <<"Delete rates from file">>}
    ,{<<"expected_content">>, <<"text/csv">>}
    ,{<<"mandatory">>, Mandatory}
    ,{<<"optional">>, Optional}
    ].

%%% Verifiers

-spec direction(ne_binary()) -> boolean().
direction(<<"inbound">>) -> 'true';
direction(<<"outbound">>) -> 'true';
direction(_) -> 'false'.

%%% Appliers

-spec export(kz_proplist(), task_iterator()) -> task_iterator().
export(Props, 'init') ->
    case is_allowed(Props) of
        'true' -> State = [{'db', get_ratedeck_db(Props)}
                          ,{'options', [{'limit', ?BULK_LIMIT + 1}
                                       ,'include_docs'
                                       ]}
                          ],
                  export(Props, State);
        'false' ->
            lager:warning("rates exporting is forbidden for account ~s, auth account ~s"
                         ,[props:get_value('account_id', Props)
                          ,props:get_value('auth_account_id', Props)
                          ]
                         ),
            {<<"task execution is forbidden">>, 'stop'}
    end;
export(_Props, 'stop') -> 'stop';
export(_Props, State) ->
    Db = props:get_value('db', State),
    Options = props:get_value('options', State),
    Limit = props:get_value(['options', 'limit'], State),
    case kz_datamgr:get_results(Db, ?RATES_VIEW, Options) of
        {'ok', []} -> 'stop';
        {'ok', Results} when erlang:length(Results) >= Limit ->
            {Head, Last} = split_results(Results),
            Rows = [to_csv_row(R) || R <- Head],
            NewOptions = [{'startkey', kz_json:get_value(<<"key">>, Last)}
                         ,{'startkey_docid', kz_json:get_value(<<"id">>, Last)}
                          | props:delete_keys(['startkey', 'startkey_docid'], Options)],
            NewState = props:set_value('options', NewOptions, State),
            {Rows, NewState};
        {'ok', Results} ->
            Rows = [to_csv_row(R) || R <- Results],
            {Rows, 'stop'}
    end.

-spec import(kz_proplist(), task_iterator(), ?SPEC_FIELDS) -> task_iterator().
import(Props, 'init', ?VARS) ->
    kz_datamgr:suppress_change_notice(),
    case is_allowed(Props) of
        'true' ->
            State = [{'db', get_ratedeck_db(Props)}
                    ,{'limit', ?BULK_LIMIT}
                    ,{'count', 0}
                    ,{'docs', []}
                    ],
            import(Props, State, ?VARS);
        'false' ->
            lager:warning("rates importing is forbidden for account ~s, auth account ~s"
                         ,[props:get_value('account_id', Props)
                          ,props:get_value('auth_account_id', Props)
                          ]
                         ),
            {<<"task execution is forbidden">>, 'stop'}
    end;
import(Props, State, ?VARS) ->
    Limit = props:get_value('limit', State),
    Count = props:get_value('count', State) + 1,
    JObj = generate_row([?VARS], Props),
    Docs = props:get_value('docs', State),
    NewDocs = case Count rem Limit =:= 0 of
                  'true' ->
                      Db = props:get_value('db', State),
                      save_rates(Db, [JObj | Docs]),
                      [];
                  'false' -> [JObj | Docs]
              end,
    {[], props:set_values([{'count', Count}, {'docs', NewDocs}], State)}.

-spec delete(kz_proplist(), task_iterator(), ?SPEC_FIELDS) -> task_iterator().
delete(Props, 'init', ?VARS) ->
    kz_datamgr:suppress_change_notice(),
    case is_allowed(Props) of
        'true' ->
            State = [{'db', get_ratedeck_db(Props)}
                    ,{'limit', ?BULK_LIMIT}
                    ,{'count', 0}
                    ,{'keys', []}
                    ,{'dict', dict:new()}
                    ],
            delete(Props, State, ?VARS);
        'false' ->
            lager:warning("rates deleting is forbidden for account ~s, auth account ~s"
                         ,[props:get_value('account_id', Props)
                          ,props:get_value('auth_account_id', Props)
                          ]
                         ),
            {<<"task execution is forbidden">>, 'stop'}
    end;
delete(Props, State, ?VARS) ->
    Limit = props:get_value('limit', State),
    Count = props:get_value('count', State) + 1,
    P = kz_util:to_integer(Prefix),
    Row = lists:zip([?DOC_FIELDS], [?VARS]),
    %% override account-ID from task props
    Update = [{<<"account_id">>, props:get_value('account_id', Props)}],
    DictRow = props:filter_undefined(props:set_values(Update, Row)),
    Dict = dict:append(P, DictRow, props:get_value('dict', State)),
    Keys = [P | props:get_value('keys', State)],
    case Count rem Limit =:= 0 of
        'true' ->
            Db = props:get_value('db', State),
            delete_rates(Db, Keys, Dict),
            {[], props:set_values([{'count', Count}, {'keys', []}, {'dict', dict:new()}], State)};
        'false' ->
            {[], props:set_values([{'count', Count}, {'keys', Keys}, {'dict', Dict}], State)}
    end.

-spec cleanup(ne_binary(), any()) -> any().
cleanup(<<"import">>, State) ->
    Db = props:get_value('db', State),
    Docs = props:get_value('docs', State),
    save_rates(Db, Docs),
    kz_datamgr:enable_change_notice(),
    kzs_publish:maybe_publish_db(Db, <<"edited">>);
cleanup(<<"delete">>, State) ->
    Db = props:get_value('db', State),
    Keys = props:get_value('keys', State),
    Dict = props:get_value('dict', State),
    delete_rates(Db, Keys, Dict),
    kz_datamgr:enable_change_notice(),
    kzs_publish:maybe_publish_db(Db, <<"edited">>).

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec split_results(kz_json:objects()) -> {kz_json:objects(), kz_json:object()}.
split_results([_|_] = JObjs) ->
    {Head, [Last]} = lists:split(erlang:length(JObjs)-1, JObjs),
    %% !!!
    %% workaround untill https://github.com/benoitc/couchbeam/pull/160
    %% !!!
    case kz_json:get_value(<<"key">>, lists:last(Head)) =:= kz_json:get_value(<<"key">>, Last) of
        'true' -> split_results(Head);
        'false' -> {Head, Last}
    end.

-spec is_allowed(kz_proplist()) -> boolean().
is_allowed(Props) ->
    AuthAccountId = props:get_value('auth_account_id', Props),
    AccountId = props:get_value('account_id', Props),
    {'ok', AccountDoc} = kz_account:fetch(AccountId),
    {'ok', AuthAccountDoc} = kz_account:fetch(AuthAccountId),
    kz_util:is_in_account_hierarchy(AuthAccountId, AccountId, 'true')
    %% Serve request for reseller rates
        andalso kz_account:is_reseller(AccountDoc)
    %% or serve requests from SuperAdmin
        orelse kz_account:is_superduper_admin(AuthAccountDoc).

-spec get_ratedeck_db(kz_proplist()) -> ne_binary().
get_ratedeck_db(_Props) ->
    %% TODO: per account DB?
    ?KZ_RATES_DB.

-spec to_csv_row(kz_json:object()) -> kz_csv:row().
to_csv_row(Row) ->
    Doc = kz_json:get_json_value(<<"doc">>, Row),
    [kz_json:get_binary_value(Key, Doc) || Key <- [?DOC_FIELDS]].

-spec generate_row(api_binaries(), kz_proplist()) -> kz_json:object().
generate_row([?VARS], Props) ->
    List = lists:zip([?DOC_FIELDS], [?VARS]),
    Update = [{<<"pvt_type">>, <<"rate">>}
             ,{<<"rate_name">>, maybe_generate_name(Name, Prefix, ISO, Direction)}
             ,{<<"weight">>, maybe_generate_weight(Weight, Prefix, Cost, IntCost)}
             ,{<<"routes">>, [<<"^\\+?", Prefix/binary, ".+$">>]}
             %% override account-ID from task props
             ,{<<"account_id">>, props:get_value('account_id', Props)}
                                ,{<<"pvt_auth_account_id">>, props:get_value('auth_account_id', Props)}
                                ],
    kz_json:from_list(props:filter_undefined(props:set_values(Update, List))).

-spec save_rates(ne_binary(), kz_json:objects()) -> 'ok'.
save_rates(Db, Docs) ->
    case kz_datamgr:save_docs(Db, Docs) of
        {'ok', _Result} -> refresh_selectors_index(Db);
        {'error', 'not_found'} ->
            init_db(Db),
            save_rates(Db, Docs);
        %% Workaround, need to fix!
        %% We assume that is everything ok and try to refresh index
        {'error', 'timeout'} -> refresh_selectors_index(Db)
    end.

-spec delete_rates(ne_binary(), list(), dict:dict()) -> 'ok'.
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

-spec do_delete_rates(ne_binary(), kz_json:objects()) -> 'ok'.
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

-spec refresh_selectors_index(ne_binary()) -> 'ok'.
refresh_selectors_index(Db) ->
    {'ok', _} = kz_datamgr:all_docs(Db, [{limit, 1}]),
    {'ok', _} = kz_datamgr:get_results(Db, <<"rates/lookup">>, [{'limit', 1}]),
    'ok'.

-spec init_db(ne_binary()) -> 'ok'.
init_db(Db) ->
    'true' = kz_datamgr:db_create(Db),
    {'ok', _} = kz_datamgr:revise_doc_from_file(Db, 'crossbar', "views/rates.json"),
    'ok'.

-spec maybe_delete_rate(kz_json:object(), dict:dict()) -> kz_json:object() | 'false'.
maybe_delete_rate(JObj, Dict) ->
    Prefix = kz_util:to_integer(kz_json:get_value(<<"key">>, JObj)),
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

-spec maybe_default(any(), any()) -> any().
maybe_default('undefined', Default) -> Default;
maybe_default(Value, _Default) -> Value.

-spec maybe_generate_name(api_binary(), api_binary(), api_binary(), api_binary()) -> api_binary().
maybe_generate_name('undefined', Prefix, 'undefined', 'undefined') ->
    <<Prefix/binary>>;
maybe_generate_name('undefined', Prefix, ISO, 'undefined') ->
    <<ISO/binary, "_", Prefix/binary>>;
maybe_generate_name('undefined', Prefix, 'undefined', Direction) ->
    <<Direction/binary, "_", Prefix/binary>>;
maybe_generate_name('undefined', Prefix, ISO, Direction) ->
    <<Direction/binary, "_", ISO/binary, "_", Prefix/binary>>;
maybe_generate_name(Name, _Prefix, _ISO, _Direction) -> Name.

-spec maybe_generate_weight(api_binary() | api_integer(), ne_binary(), ne_binary(), api_binary()) -> api_binary().
maybe_generate_weight('undefined', Prefix, Cost, IntCost) ->
    R = kz_util:to_float(maybe_default(IntCost, Cost)),
    maybe_generate_weight(byte_size(kz_util:to_binary(Prefix)) * 10 - trunc(R * 100), Prefix, Cost, IntCost);
maybe_generate_weight(Weight, _, _, _) ->
    case kz_util:to_integer(Weight) of
        X when X =< 0 -> <<"1">>;
        X when X >= 100 -> <<"100">>;
        X -> kz_util:to_binary(X)
    end.
