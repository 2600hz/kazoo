%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2022, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(pqc_cb_callflows).

-export([list_callflows/2
        ,create_callflow/3
        ,fetch_callflow/3
        ,delete_callflow/3
        ]).

-export([seq/0, seq_url/0
        ,cleanup/0
        ]).

-include("kazoo_proper.hrl").

-define(ACCOUNT_NAMES, [<<?MODULE_STRING>>]).

-spec list_callflows(pqc_cb_api:state(), pqc_cb_accounts:account_id()) ->
          pqc_cb_api:response().
list_callflows(API, AccountId) ->
    pqc_cb_api:make_request(#{'response_codes' => [200]}
                           ,fun kz_http:get/2
                           ,callflows_url(AccountId)
                           ,pqc_cb_api:request_headers(API)
                           ).

-spec fetch_callflow(pqc_cb_api:state(), pqc_cb_accounts:account_id(), kz_term:ne_binary()) ->
          pqc_cb_api:response().
fetch_callflow(API, AccountId, CallflowId) ->
    pqc_cb_api:make_request(#{'response_codes' => [200]}
                           ,fun kz_http:get/2
                           ,callflows_url(AccountId, CallflowId)
                           ,pqc_cb_api:request_headers(API)
                           ).

-spec create_callflow(pqc_cb_api:state(), pqc_cb_accounts:account_id(), kzd_callflows:doc()) ->
          pqc_cb_api:response().
create_callflow(API, AccountId, CallflowDoc) ->
    URL = callflows_url(AccountId),
    RequestHeaders = pqc_cb_api:request_headers(API),
    RequestEnvelope  = pqc_cb_api:create_envelope(CallflowDoc),

    pqc_cb_api:make_request(#{'response_codes' => [201]}
                           ,fun kz_http:put/3
                           ,URL
                           ,RequestHeaders
                           ,kz_json:encode(RequestEnvelope)
                           ).

-spec create_callflow_doc() -> kzd_callflows:doc().
create_callflow_doc() ->
    lists:foldl(fun({F, V}, Doc) -> F(Doc, V) end
               ,kzd_callflows:new()
               ,[{fun kzd_callflows:set_numbers/2, [<<"123">>]}
                ,{fun kzd_callflows:set_flow/2
                 ,kz_json:from_list([{<<"module">>, <<"response">>}
                                    ,{<<"data">>, kz_json:from_list([{<<"code">>, 200}])}
                                    ])
                 }
                ]
               ).

-spec delete_callflow(pqc_cb_api:state(), pqc_cb_accounts:account_id(), kz_term:ne_binary()) ->
          pqc_cb_api:response().
delete_callflow(API, AccountId, CallflowId) ->
    pqc_cb_api:make_request(#{'response_codes' => [200]}
                           ,fun kz_http:delete/2
                           ,callflows_url(AccountId, CallflowId)
                           ,pqc_cb_api:request_headers(API)
                           ).

callflows_url(AccountId) ->
    string:join([pqc_cb_accounts:account_url(AccountId), "callflows"], "/").

callflows_url(AccountId, CallflowId) ->
    string:join([callflows_url(AccountId), kz_term:to_list(CallflowId)], "/").

-spec seq() -> 'ok'.
seq() ->
    _ = seq_crud(),
    seq_url().

seq_crud() ->
    _ = init(),
    Model = initial_state(),
    API = pqc_kazoo_model:api(Model),

    AccountResp = pqc_cb_accounts:create_account(API, hd(?ACCOUNT_NAMES)),
    AccountId = kz_json:get_value([<<"data">>, <<"id">>], kz_json:decode(AccountResp)),
    lager:info("created account ~s", [AccountId]),

    EmptySummaryResp = list_callflows(API, AccountId),
    lager:info("empty summary: ~s", [EmptySummaryResp]),
    [] = kz_json:get_list_value(<<"data">>, kz_json:decode(EmptySummaryResp)),

    CreateResp = create_callflow(API, AccountId, create_callflow_doc()),
    lager:info("create resp ~p", [CreateResp]),
    CallflowDoc = kz_json:get_json_value(<<"data">>, kz_json:decode(CreateResp)),
    <<CallflowId:32/binary>> = kz_doc:id(CallflowDoc),

    SummaryResp = list_callflows(API, AccountId),
    lager:info("summary resp: ~s", [SummaryResp]),
    [SummaryJObj] = kz_json:get_list_value(<<"data">>, kz_json:decode(SummaryResp)),
    CallflowId = kz_doc:id(SummaryJObj),

    FetchResp = fetch_callflow(API, AccountId, CallflowId),
    lager:info("fetched resp: ~s", [FetchResp]),
    FetchJObj = kz_json:get_json_value(<<"data">>, kz_json:decode(FetchResp)),
    CallflowId = kz_doc:id(FetchJObj),

    DeleteResp = delete_callflow(API, AccountId, CallflowId),
    lager:info("delete resp: ~s", [DeleteResp]),
    CallflowId = kz_doc:id(kz_json:get_json_value(<<"data">>, kz_json:decode(DeleteResp))),

    EmptyAgainResp = list_callflows(API, AccountId),
    lager:info("empty again resp: ~s", [EmptyAgainResp]),
    [] = kz_json:get_list_value(<<"data">>, kz_json:decode(EmptyAgainResp)),

    lager:info("COMPLETED SUCCESSFULLY!"),
    _ = cleanup(API).

-spec seq_url() -> any().
seq_url() ->
    _ = init(),
    Model = initial_state(),
    API = pqc_kazoo_model:api(Model),

    AccountResp = pqc_cb_accounts:create_account(API, hd(?ACCOUNT_NAMES)),
    AccountId = kz_json:get_value([<<"data">>, <<"id">>], kz_json:decode(AccountResp)),
    lager:info("created account ~s", [AccountId]),

    {'error', ErrorResp} = create_callflow(API, AccountId, pivot_url_callflow()),
    lager:info("create resp failed with internal url: ~s", [ErrorResp]),

    lager:info("COMPLETED SUCCESSFULLY!"),
    _ = cleanup(API).

pivot_url_callflow() ->
    PivotData = kz_json:from_list([{<<"voice_url">>, <<"http://localhost/123">>}]),
    Flow = kz_json:from_list([{<<"module">>, <<"pivot">>}
                             ,{<<"data">>, PivotData}
                             ]),
    kz_doc:setters(kzd_callflows:new()
                  ,[{fun kzd_callflows:set_numbers/2, [<<"345">>]}
                   ,{fun kzd_callflows:set_flow/2, Flow}
                   ]).

init() ->
    _ = kz_data_tracing:clear_all_traces(),
    _ = [kapps_controller:start_app(App) ||
            App <- ['crossbar']
        ],
    _ = [crossbar_maintenance:start_module(Mod) ||
            Mod <- ['cb_callflows', 'cb_accounts']
        ],
    lager:info("INIT FINISHED").

-spec initial_state() -> pqc_kazoo_model:model().
initial_state() ->
    API = pqc_cb_api:authenticate(),
    lager:info("state initialized to ~p", [API]),
    pqc_kazoo_model:new(API).

-spec cleanup() -> any().
cleanup() ->
    lager:info("CLEANUP ALL THE THINGS"),
    kz_data_tracing:clear_all_traces(),
    cleanup(pqc_cb_api:authenticate()).

-spec cleanup(pqc_cb_api:state()) -> any().
cleanup(API) ->
    lager:info("CLEANUP TIME, EVERYBODY HELPS"),
    _ = pqc_cb_accounts:cleanup_accounts(API, ?ACCOUNT_NAMES),
    pqc_cb_api:cleanup(API).
