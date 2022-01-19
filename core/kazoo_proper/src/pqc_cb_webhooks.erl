%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2022, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(pqc_cb_webhooks).

-export([list_webhooks/2
        ,create_webhook/3
        ,fetch_webhook/3
        ,delete_webhook/3
        ]).

-export([seq/0, seq_url/0
        ,cleanup/0
        ]).

-include("kazoo_proper.hrl").

-define(ACCOUNT_NAMES, [<<?MODULE_STRING>>]).

-spec list_webhooks(pqc_cb_api:state(), pqc_cb_accounts:account_id()) ->
          pqc_cb_api:response().
list_webhooks(API, AccountId) ->
    pqc_cb_api:make_request(#{'response_codes' => [200]}
                           ,fun kz_http:get/2
                           ,webhooks_url(AccountId)
                           ,pqc_cb_api:request_headers(API)
                           ).

-spec fetch_webhook(pqc_cb_api:state(), pqc_cb_accounts:account_id(), kz_term:ne_binary()) ->
          pqc_cb_api:response().
fetch_webhook(API, AccountId, WebhookId) ->
    pqc_cb_api:make_request(#{'response_codes' => [200]}
                           ,fun kz_http:get/2
                           ,webhooks_url(AccountId, WebhookId)
                           ,pqc_cb_api:request_headers(API)
                           ).

-spec create_webhook(pqc_cb_api:state(), pqc_cb_accounts:account_id(), kzd_webhooks:doc()) ->
          pqc_cb_api:response().
create_webhook(API, AccountId, WebhookDoc) ->
    URL = webhooks_url(AccountId),
    RequestHeaders = pqc_cb_api:request_headers(API),
    RequestEnvelope  = pqc_cb_api:create_envelope(WebhookDoc),

    pqc_cb_api:make_request(#{'response_codes' => [201]}
                           ,fun kz_http:put/3
                           ,URL
                           ,RequestHeaders
                           ,kz_json:encode(RequestEnvelope)
                           ).

-spec create_webhook_doc() -> kzd_webhooks:doc().
create_webhook_doc() ->
    lists:foldl(fun({F, V}, Doc) -> F(Doc, V) end
               ,kzd_webhooks:new()
               ,[{fun kzd_webhooks:set_uri/2, <<"https://webhook.host/here">>}
                ,{fun kzd_webhooks:set_name/2, <<?MODULE_STRING>>}
                ,{fun kzd_webhooks:set_hook/2, <<"channel_create">>}
                ]
               ).

-spec delete_webhook(pqc_cb_api:state(), pqc_cb_accounts:account_id(), kz_term:ne_binary()) ->
          pqc_cb_api:response().
delete_webhook(API, AccountId, WebhookId) ->
    pqc_cb_api:make_request(#{'response_codes' => [200]}
                           ,fun kz_http:delete/2
                           ,webhooks_url(AccountId, WebhookId)
                           ,pqc_cb_api:request_headers(API)
                           ).

webhooks_url(AccountId) ->
    string:join([pqc_cb_accounts:account_url(AccountId), "webhooks"], "/").

webhooks_url(AccountId, WebhookId) ->
    string:join([webhooks_url(AccountId), kz_term:to_list(WebhookId)], "/").

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

    EmptySummaryResp = list_webhooks(API, AccountId),
    lager:info("empty summary: ~s", [EmptySummaryResp]),
    [] = kz_json:get_list_value(<<"data">>, kz_json:decode(EmptySummaryResp)),

    CreateResp = create_webhook(API, AccountId, create_webhook_doc()),
    lager:info("create resp ~p", [CreateResp]),
    WebhookDoc = kz_json:get_json_value(<<"data">>, kz_json:decode(CreateResp)),
    <<WebhookId:32/binary>> = kz_doc:id(WebhookDoc),

    SummaryResp = list_webhooks(API, AccountId),
    lager:info("summary resp: ~s", [SummaryResp]),
    [SummaryJObj] = kz_json:get_list_value(<<"data">>, kz_json:decode(SummaryResp)),
    WebhookId = kz_doc:id(SummaryJObj),

    FetchResp = fetch_webhook(API, AccountId, WebhookId),
    lager:info("fetched resp: ~s", [FetchResp]),
    FetchJObj = kz_json:get_json_value(<<"data">>, kz_json:decode(FetchResp)),
    WebhookId = kz_doc:id(FetchJObj),

    DeleteResp = delete_webhook(API, AccountId, WebhookId),
    lager:info("delete resp: ~s", [DeleteResp]),
    WebhookId = kz_doc:id(kz_json:get_json_value(<<"data">>, kz_json:decode(DeleteResp))),

    EmptyAgainResp = list_webhooks(API, AccountId),
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

    {'error', ErrorResp} = create_webhook(API, AccountId, pivot_url_webhook()),
    lager:info("create resp failed with internal url: ~s", [ErrorResp]),

    lager:info("COMPLETED SUCCESSFULLY!"),
    _ = cleanup(API).

pivot_url_webhook() ->
    kz_doc:setters(kzd_webhooks:new()
                  ,[{fun kzd_webhooks:set_uri/2, <<"https://localhost:345/webhook">>}
                   ,{fun kzd_webhooks:set_name/2, <<?MODULE_STRING>>}
                   ,{fun kzd_webhooks:set_hook/2, <<"channel_destroy">>}
                   ]).

init() ->
    _ = kz_data_tracing:clear_all_traces(),
    _ = [kapps_controller:start_app(App) ||
            App <- ['crossbar']
        ],
    _ = [crossbar_maintenance:start_module(Mod) ||
            Mod <- ['cb_webhooks', 'cb_accounts']
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
