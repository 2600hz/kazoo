%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2019-, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(pqc_cb_ledgers).

%% API functions
-export([fetch/2, fetch/3
        ,fetch_by_source/3, fetch_by_source/4
        ,credit/3, debit/3
        ]).

%% Manual test functions
-export([seq/0
        ,cleanup/0
        ]).

-include("kazoo_proper.hrl").

-define(ACCOUNT_NAMES, [<<"account_for_ledgers">>]).

%% API functions
-spec fetch(pqc_cb_api:state(), kz_term:ne_binary()) -> pqc_cb_api:response().
fetch(API, ?NE_BINARY=AccountId) ->
    fetch(API, AccountId, <<"application/json">>).

-spec fetch(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary()) -> pqc_cb_api:response().
fetch(API, ?NE_BINARY=AccountId, ?NE_BINARY=AcceptType) ->
    LedgersURL = ledgers_url(AccountId),
    RequestHeaders = pqc_cb_api:request_headers(API, [{<<"accept">>, AcceptType}]),

    Expectations = #{'response_codes' => [200]
                    ,'response_headers' => [{<<"content-type">>, AcceptType}]
                    },

    pqc_cb_api:make_request(Expectations
                           ,fun kz_http:get/2
                           ,LedgersURL
                           ,RequestHeaders
                           ).

-spec fetch_by_source(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary()) ->
                             pqc_cb_api:response().
fetch_by_source(API, ?NE_BINARY=AccountId, ?NE_BINARY=SourceType) ->
    fetch_by_source(API, AccountId, SourceType, <<"application/json">>).

-spec fetch_by_source(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) ->
                             pqc_cb_api:response().
fetch_by_source(API, ?NE_BINARY=AccountId, SourceType, ?NE_BINARY=AcceptType) ->
    LedgersURL = ledgers_source_url(AccountId, SourceType),
    RequestHeaders = pqc_cb_api:request_headers(API, [{<<"accept">>, AcceptType}]),

    Expectations = [#{'response_codes' => [200]
                     ,'response_headers' => [{<<"content-type">>, AcceptType}]
                     }
                   ,#{'response_codes' => [204]}
                   ],

    pqc_cb_api:make_request(Expectations
                           ,fun kz_http:get/2
                           ,LedgersURL
                           ,RequestHeaders
                           ).

-spec credit(pqc_cb_api:state(), kz_term:ne_binary(), kzd_ledger:doc()) ->
                    pqc_cb_api:response().
credit(API, ?NE_BINARY=AccountId, Ledger) ->
    LedgersURL = ledgers_credit_url(AccountId),
    RequestHeaders = pqc_cb_api:request_headers(API),

    Expectations = #{'response_codes' => [201]},

    Envelope = pqc_cb_api:create_envelope(Ledger),

    pqc_cb_api:make_request(Expectations
                           ,fun kz_http:put/3
                           ,LedgersURL
                           ,RequestHeaders
                           ,kz_json:encode(Envelope)
                           ).

-spec debit(pqc_cb_api:state(), kz_term:ne_binary(), kzd_ledger:doc()) ->
                   pqc_cb_api:response().
debit(API, ?NE_BINARY=AccountId, Ledger) ->
    LedgersURL = ledgers_debit_url(AccountId),
    RequestHeaders = pqc_cb_api:request_headers(API),

    Expectations = #{'response_codes' => [201]},

    Envelope = pqc_cb_api:create_envelope(Ledger),

    pqc_cb_api:make_request(Expectations
                           ,fun kz_http:put/3
                           ,LedgersURL
                           ,RequestHeaders
                           ,kz_json:encode(Envelope)
                           ).

-spec ledgers_url(kz_term:ne_binary()) -> string().
ledgers_url(AccountId) ->
    string:join([pqc_cb_accounts:account_url(AccountId), "ledgers"], "/").

-spec ledgers_source_url(kz_term:ne_binary(), kz_term:ne_binary()) -> string().
ledgers_source_url(AccountId, SourceType) ->
    string:join([pqc_cb_accounts:account_url(AccountId), "ledgers", kz_term:to_list(SourceType)], "/").

-spec ledgers_credit_url(kz_term:ne_binary()) -> string().
ledgers_credit_url(AccountId) ->
    string:join([pqc_cb_accounts:account_url(AccountId), "ledgers", "credit"], "/").

-spec ledgers_debit_url(kz_term:ne_binary()) -> string().
ledgers_debit_url(AccountId) ->
    string:join([pqc_cb_accounts:account_url(AccountId), "ledgers", "debit"], "/").

-spec initial_state() -> pqc_kazoo_model:model().
initial_state() ->
    _ = init_system(),
    API = pqc_cb_api:authenticate(),
    pqc_kazoo_model:new(API).

init_system() ->
    TestId = kz_binary:rand_hex(5),
    kz_util:put_callid(TestId),

    _ = kz_data_tracing:clear_all_traces(),
    _ = [kapps_controller:start_app(App) ||
            App <- ['crossbar']
        ],
    _ = [crossbar_maintenance:start_module(Mod) ||
            Mod <- ['cb_ledgers']
        ],
    lager:info("INIT FINISHED").

-spec seq() -> 'ok'.
seq() ->
    Model = initial_state(),
    API = pqc_kazoo_model:api(Model),

    PriorChunkSize = kapps_config:get_pos_integer(<<"crossbar">>, <<"load_view_chunk_size">>),
    {'ok', _} = kapps_config:set_default(<<"crossbar">>, <<"load_view_chunk_size">>, 1),

    AccountResp = pqc_cb_accounts:create_account(API, hd(?ACCOUNT_NAMES)),
    lager:info("created account: ~s", [AccountResp]),

    AccountId = kz_json:get_value([<<"data">>, <<"id">>], kz_json:decode(AccountResp)),

    EmptySourceFetchCSV = fetch_by_source(API, AccountId, <<?MODULE_STRING>>, <<"text/csv">>),
    lager:info("empty source fetch CSV: ~s", [EmptySourceFetchCSV]),

    Ledger2 = ledger_doc(<<"baseball">>),
    CreditResp2 = credit(API, AccountId, Ledger2),
    lager:info("credit: ~s", [CreditResp2]),

    %% make sure ledgers have diff pvt_created
    timer:sleep(?MILLISECONDS_IN_SECOND),

    %% put the non-custom ledger later, since sorting is descending
    Ledger = ledger_doc(),
    CreditResp = credit(API, AccountId, Ledger),
    lager:info("credit: ~s", [CreditResp]),

    SourceFetch = fetch_by_source(API, AccountId, <<?MODULE_STRING>>),
    lager:info("source fetch: ~s", [SourceFetch]),
    Data = kz_json:get_value(<<"data">>, kz_json:decode(SourceFetch)),
    _ = ledgers_exist(Data, [Ledger, Ledger2]),

    SourceFetchCSV = fetch_by_source(API, AccountId, <<?MODULE_STRING>>, <<"text/csv">>),
    lager:info("source fetch CSV: ~s", [SourceFetchCSV]),
    'true' = ledgers_exist_in_csv(SourceFetchCSV, [Ledger, Ledger2]),

    {'ok', _} = kapps_config:set_default(<<"crossbar">>, <<"load_view_chunk_size">>, PriorChunkSize),

    cleanup(API),
    lager:info("FINISHED").

-spec cleanup() -> 'ok'.
cleanup() ->
    _ = pqc_cb_accounts:cleanup_accounts(?ACCOUNT_NAMES),
    cleanup_system().

cleanup(API) ->
    lager:info("CLEANUP TIME, EVERYBODY HELPS"),
    _ = pqc_cb_accounts:cleanup_accounts(API, ?ACCOUNT_NAMES),
    _ = pqc_cb_api:cleanup(API),
    cleanup_system().

cleanup_system() -> 'ok'.

ledgers_exist(_Data, []) -> 'true';
ledgers_exist(Data, [Ledger | Ledgers]) ->
    lists:any(fun(Datum) -> ledger_matches(Datum, Ledger) end, Data)
        andalso ledgers_exist(Data, Ledgers).

ledger_matches(Datum, Ledger) ->
    kzd_ledgers:source_id(Datum) =:= kzd_ledgers:source_id(Ledger).

ledgers_exist_in_csv(CSV, Ledgers) ->
    {Header, Rows} = kz_csv:take_row(CSV),
    MetaPos = header_has_column_name(Header, <<"metadata_bonus">>),
    SourceIdPos = header_has_column_name(Header, <<"Source ID">>),
    is_integer(MetaPos)
        andalso is_integer(SourceIdPos)
        andalso ledgers_in_rows({MetaPos, SourceIdPos}, Rows, Ledgers).

header_has_column_name(Headers, ColumnName) ->
    header_has_column_name(Headers, ColumnName, 1).

header_has_column_name([], _ColumnName, _N) ->
    lager:info("failed to find column '~s'", [_ColumnName]),
    'undefined';
header_has_column_name([ColumnName | _Headers], ColumnName, N) ->
    lager:debug("found column '~s' at ~p", [ColumnName, N]),
    N;
header_has_column_name([_Header | Headers], ColumnName, N) ->
    header_has_column_name(Headers, ColumnName, N+1).

ledgers_in_rows(Pos, Rows, Ledgers) ->
    ledgers_in_row(Pos, kz_csv:take_row(Rows), Ledgers).

ledgers_in_row({_MetaPos, _SourceIdPos}, 'eof', []) -> 'true';
ledgers_in_row({_MetaPos, _SourceIdPos}, 'eof', _Ledgers) ->
    lager:info("failed to find ledgers ~p", [_Ledgers]),
    'false';
ledgers_in_row({MetaPos, SourceIdPos}, {Row, Rows}, Ledgers) ->
    %% remove ledger matching Row
    RowSourceId = lists:nth(SourceIdPos, Row),
    RowMetaValue = try lists:nth(MetaPos, Row) catch _:_ -> <<>> end,

    Ls = [Ledger
          || Ledger <- Ledgers,
             kzd_ledgers:source_id(Ledger) =/= RowSourceId,
             metadata_bonus(Ledger) =/= RowMetaValue
         ],
    ledgers_in_row({MetaPos, SourceIdPos}, kz_csv:take_row(Rows), Ls).

metadata_bonus(Ledger) ->
    Metadata = kzd_ledgers:metadata(Ledger, kz_json:new()),
    kz_json:get_value(<<"bonus">>, Metadata, <<>>).

-spec ledger_doc() -> kzd_ledgers:doc().
ledger_doc() ->
    lists:foldl(fun({F, V}, D) -> F(D, V) end
               ,kzd_ledgers:new()
               ,[{fun kzd_ledgers:set_source_service/2, <<?MODULE_STRING>>}
                ,{fun kzd_ledgers:set_source_id/2, kz_binary:rand_hex(4)}
                ,{fun kzd_ledgers:set_usage_quantity/2, 1}
                ,{fun kzd_ledgers:set_usage_unit/2, <<"Hz">>}
                ,{fun kzd_ledgers:set_usage_type/2, <<"cycle">>}
                ]
               ).

-spec ledger_doc(kz_term:ne_binary()) -> kzd_ledgers:doc().
ledger_doc(Value) ->
    kz_json:set_value([<<"metadata">>, <<"bonus">>], Value, ledger_doc()).
