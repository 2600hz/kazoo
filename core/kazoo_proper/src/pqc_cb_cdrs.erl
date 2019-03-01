%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2019-, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(pqc_cb_cdrs).

%% Manual testing
-export([seq/0
        ,cleanup/0
        ]).

%% API Shims
-export([summary/2, summary/3
        ,fetch/3
        ]).

-export([interactions/2
        ,legs/3
        ]).

-include_lib("proper/include/proper.hrl").
-include("kazoo_proper.hrl").
-include_lib("kazoo_stdlib/include/kz_databases.hrl").

-define(ACCOUNT_NAMES, [<<"account_for_cdrs">>]).

-spec summary(pqc_cb_api:state(), kz_term:ne_binary()) -> pqc_cb_api:response().
summary(API, AccountId) ->
    summary(API, AccountId, <<"application/json">>).

-spec summary(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary()) -> pqc_cb_api:response().
summary(API, AccountId, Accept) ->
    URL = cdrs_url(AccountId),
    RequestHeaders = pqc_cb_api:request_headers(API, [{<<"accept">>, Accept}]),

    Expectations = [#{'response_codes' => [200]
                     ,'response_headers' => [{"content-type", kz_term:to_list(Accept)}]
                     }
                   ,#{'response_codes' => [204]}
                   ],

    pqc_cb_api:make_request(Expectations
                           ,fun kz_http:get/2
                           ,URL
                           ,RequestHeaders
                           ).

-spec fetch(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary()) -> pqc_cb_api:response().
fetch(API, AccountId, CDRId) ->
    URL = cdr_url(AccountId, CDRId),
    RequestHeaders = pqc_cb_api:request_headers(API),
    pqc_cb_api:make_request([200]
                           ,fun kz_http:get/2
                           ,URL
                           ,RequestHeaders
                           ).

-spec interactions(pqc_cb_api:state(), kz_term:ne_binary()) -> pqc_cb_api:response().
interactions(API, AccountId) ->
    URL = interactions_url(AccountId),
    RequestHeaders = pqc_cb_api:request_headers(API),
    pqc_cb_api:make_request([200]
                           ,fun kz_http:get/2
                           ,URL
                           ,RequestHeaders
                           ).

-spec legs(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary()) -> pqc_cb_api:response().
legs(API, AccountId, InteractionId) ->
    URL = legs_url(AccountId, InteractionId),
    RequestHeaders = pqc_cb_api:request_headers(API),
    pqc_cb_api:make_request([200]
                           ,fun kz_http:get/2
                           ,URL
                           ,RequestHeaders
                           ).

legs_url(AccountId, InteractionId) ->
    string:join([pqc_cb_accounts:account_url(AccountId), "cdrs", "legs", kz_term:to_list(InteractionId)], "/").

interactions_url(AccountId) ->
    string:join([pqc_cb_accounts:account_url(AccountId), "cdrs", "interaction"], "/").

cdr_url(AccountId, CDRId) ->
    string:join([pqc_cb_accounts:account_url(AccountId), "cdrs", kz_term:to_list(CDRId)], "/").

cdrs_url(AccountId) ->
    string:join([pqc_cb_accounts:account_url(AccountId), "cdrs"], "/").

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
            Mod <- ['cb_cdrs']
        ],

    ?INFO("INIT FINISHED").

-spec seq() -> 'ok'.
seq() ->
    API = init_api(),
    AccountId = create_account(API),

    EmptySummaryResp = summary(API, AccountId),
    ?INFO("empty summary resp: ~s", [EmptySummaryResp]),
    [] = kz_json:get_list_value(<<"data">>, kz_json:decode(EmptySummaryResp)),

    EmptyCSVResp = summary(API, AccountId, <<"text/csv">>),
    ?INFO("empty CSV resp: ~s", [EmptyCSVResp]),

    CDRs = seed_cdrs(AccountId),
    ?INFO("CDRs: ~p~n", [CDRs]),

    SummaryResp = summary(API, AccountId),
    ?INFO("summary resp: ~s", [SummaryResp]),
    'true' = cdrs_exist(CDRs, kz_json:get_list_value(<<"data">>, kz_json:decode(SummaryResp))),
    ?INFO("all cdrs found in response"),

    CSVResp = summary(API, AccountId, <<"text/csv">>),
    ?INFO("CSV resp: ~s", [CSVResp]),

    InteractionsResp = interactions(API, AccountId),
    ?INFO("interactions resp: ~s", [InteractionsResp]),

    lists:foreach(fun(CDR) -> seq_cdr(API, AccountId, CDR) end, CDRs),

    cleanup(API),
    ?INFO("FINISHED").

seq_cdr(API, AccountId, CDR) ->
    CDRId = kz_doc:id(CDR),
    InteractionId = kzd_cdrs:interaction_id(CDR),

    FetchResp = fetch(API, AccountId, CDRId),
    ?INFO("~s: fetch resp ~s", [CDRId, FetchResp]),
    'true' = cdrs_exist([CDR], [kz_json:get_json_value(<<"data">>, kz_json:decode(FetchResp))]),

    LegsResp = legs(API, AccountId, CDRId),
    ?INFO("~s: legs by id resp: ~s", [CDRId, LegsResp]),
    'true' = cdrs_exist([CDR], kz_json:get_list_value(<<"data">>, kz_json:decode(LegsResp))),

    InteractionResp = legs(API, AccountId, InteractionId),
    ?INFO("~s: legs by interaction resp: ~s", [CDRId, InteractionResp]),
    'true' = cdrs_exist([CDR], kz_json:get_list_value(<<"data">>, kz_json:decode(InteractionResp))).

-spec cdrs_exist(kz_json:objects(), kz_json:object()) -> boolean().
cdrs_exist([], []) -> 'true';
cdrs_exist(CDRs, []) ->
    IDs = [kz_doc:id(CDR) || CDR <- CDRs],
    ?INFO("  failed to find CDR(s) ~s", [kz_binary:join(IDs, <<", ">>)]),
    'false';
cdrs_exist([_|_]=CDRs, [API|APIs]) ->
    cdrs_exist([CDR || CDR <- CDRs, kz_doc:id(CDR) =/= kz_doc:id(API)]
              ,APIs
              ).

create_account(API) ->
    AccountResp = pqc_cb_accounts:create_account(API, hd(?ACCOUNT_NAMES)),
    ?INFO("created account: ~s", [AccountResp]),

    kz_json:get_value([<<"data">>, <<"id">>], kz_json:decode(AccountResp)).

init_api() ->
    Model = initial_state(),
    pqc_kazoo_model:api(Model).

-spec cleanup() -> 'ok'.
cleanup() ->
    _ = pqc_cb_accounts:cleanup_accounts(?ACCOUNT_NAMES),
    cleanup_system().

cleanup(API) ->
    ?INFO("CLEANUP TIME, EVERYBODY HELPS"),
    _ = pqc_cb_accounts:cleanup_accounts(API, ?ACCOUNT_NAMES),
    _ = pqc_cb_api:cleanup(API),
    cleanup_system().

cleanup_system() -> 'ok'.

seed_cdrs(AccountId) ->
    {Year, Month, _} = erlang:date(),
    seed_cdrs(AccountId, Year, Month).

seed_cdrs(AccountId, Year, Month) ->
    {'ok', CDR1} = seed_cdr(AccountId, Year, Month),
    {PrevY, PrevM} = kazoo_modb_util:prev_year_month(Year, Month),
    {'ok', CDR2} = seed_cdr(AccountId, PrevY, PrevM),
    [CDR1, CDR2].

seed_cdr(AccountId, Year, Month) ->
    CallId = kz_binary:rand_hex(6),
    CDRId = kzd_cdrs:create_doc_id(CallId, Year, Month),

    InteractionTime = interaction_time(Year, Month),
    InteractionKey = kz_binary:rand_hex(4),
    InteractionId = list_to_binary([integer_to_binary(InteractionTime), "-", InteractionKey]),

    JObj = kz_json:from_list([{<<"_id">>, CDRId}
                             ,{<<"call_id">>, CallId}

                             ,{<<"interaction_id">>, InteractionId}
                             ,{<<"interaction_key">>, InteractionKey}
                             ,{<<"interaction_time">>, InteractionTime}

                             ,{<<"custom_channel_vars">>, kz_json:new()}

                             ,{<<"call_direction">>, <<"inbound">>}

                             ,{<<"request">>, <<"2600@hertz.com">>}
                             ,{<<"to">>, <<"capt@crunch.com">>}
                             ,{<<"from">>, <<"cereal@killer.com">>}

                             ,{<<"ringing_seconds">>, 3}
                             ,{<<"billing_seconds">>, 6}
                             ,{<<"duration_seconds">>, 9}
                             ,{<<"timestamp">>, InteractionTime}
                             ]),

    AccountMODb = kz_util:format_account_id(AccountId, InteractionTime),

    Props = [{'type', <<"cdr">>}
            ,{'account_id', AccountId}
            ,{'now', InteractionTime}
            ],
    CDR = kz_doc:update_pvt_parameters(JObj, AccountMODb, Props),
    ?INFO("creating ~s in ~s", [CDRId, AccountMODb]),

    _ = kazoo_modb:create(AccountMODb),

    kazoo_modb:save_doc(AccountMODb, CDR).

interaction_time(Year, Month) ->
    {{_, _, D}, HMS} = calendar:universal_time(),
    calendar:datetime_to_gregorian_seconds({{Year, Month, D}, HMS}).
