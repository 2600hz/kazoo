%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2019-, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(pqc_cb_webhooks).

%% API functions
-export([list_available/1
        ,samples/1, sample/2

         %% Account operations
        ,summary/2
        ,create/3
        ,delete/3
        ]).

%% Manual test functions
-export([seq/0, seq_recv_events/0
        ,cleanup/0
        ]).

-include("kazoo_proper.hrl").

-define(ACCOUNT_NAMES, [<<"account_for_webhooks">>]).

-spec list_available(pqc_cb_api:state()) -> pqc_cb_api:response().
list_available(API) ->
    URL = base_webhooks_url(),
    RequestHeaders = pqc_cb_api:request_headers(API),

    Expectations = [#expectation{response_codes = [200]}],

    pqc_cb_api:make_request(Expectations
                           ,fun kz_http:get/2
                           ,URL
                           ,RequestHeaders
                           ).

-spec summary(pqc_cb_api:state(), kz_term:ne_binary()) -> pqc_cb_api:response().
summary(API, AccountId) ->
    WebhooksURL = webhooks_url(AccountId),
    RequestHeaders = pqc_cb_api:request_headers(API),

    Expectations = [#expectation{response_codes = [200]}],

    pqc_cb_api:make_request(Expectations
                           ,fun kz_http:get/2
                           ,WebhooksURL
                           ,RequestHeaders
                           ).

-spec create(pqc_cb_api:state(), kz_term:ne_binary(), kz_json:object()) -> pqc_cb_api:response().
create(API, AccountId, WebhookData) ->
    URL = webhooks_url(AccountId),
    RequestHeaders = pqc_cb_api:request_headers(API),
    Expectations = [#expectation{response_codes = [201]
                                ,response_headers = [{"content-type", "application/json"}]
                                }
                   ],
    pqc_cb_api:make_request(Expectations
                           ,fun kz_http:put/3
                           ,URL
                           ,RequestHeaders
                           ,kz_json:encode(pqc_cb_api:create_envelope(WebhookData))
                           ).

-spec delete(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary()) -> pqc_cb_api:response().
delete(API, AccountId, WebhookId) ->
    URL = webhook_url(AccountId, WebhookId),
    RequestHeaders = pqc_cb_api:request_headers(API),
    Expectations = [#expectation{response_codes = [200]
                                ,response_headers = [{"content-type", "application/json"}]
                                }
                   ],
    pqc_cb_api:make_request(Expectations
                           ,fun kz_http:delete/2
                           ,URL
                           ,RequestHeaders
                           ).

-spec samples(pqc_cb_api:state()) -> pqc_cb_api:response().
samples(API) ->
    URL = samples_url(),
    RequestHeaders = pqc_cb_api:request_headers(API),

    Expectations = [#expectation{response_codes = [200]}],

    pqc_cb_api:make_request(Expectations
                           ,fun kz_http:get/2
                           ,URL
                           ,RequestHeaders
                           ).

-spec sample(pqc_cb_api:state(), kz_term:ne_binary()) -> pqc_cb_api:response().
sample(API, SampleId) ->
    URL = sample_url(SampleId),
    RequestHeaders = pqc_cb_api:request_headers(API),

    Expectations = [#expectation{response_codes = [200]}],

    pqc_cb_api:make_request(Expectations
                           ,fun kz_http:get/2
                           ,URL
                           ,RequestHeaders
                           ).

-spec base_webhooks_url() -> string().
base_webhooks_url() ->
    string:join([pqc_cb_api:v2_base_url(), "webhooks"], "/").

-spec webhooks_url(kz_term:ne_binary()) -> string().
webhooks_url(AccountId) ->
    string:join([pqc_cb_accounts:account_url(AccountId), "webhooks"], "/").

-spec webhook_url(kz_term:ne_binary(), kz_term:ne_binary()) -> string().
webhook_url(AccountId, WebhookId) ->
    string:join([pqc_cb_accounts:account_url(AccountId), "webhooks", kz_term:to_list(WebhookId)], "/").

%% -spec webhook_url(kz_term:ne_binary(), kz_term:ne_binary()) -> string().
%% webhook_url(AccountId, WebhookId) ->
%%     string:join([pqc_cb_accounts:account_url(AccountId), "webhooks", kz_term:to_list(WebhookId)], "/").

-spec samples_url() -> string().
samples_url() ->
    string:join([base_webhooks_url(), "samples"], "/").

-spec sample_url(kz_term:ne_binary()) -> string().
sample_url(SampleId) ->
    string:join([base_webhooks_url(), "samples", kz_term:to_list(SampleId)], "/").

-spec seq() -> 'ok'.
seq() ->
    seq_samples(),
    seq_recv_events().

-spec seq_recv_events() -> 'ok'.
seq_recv_events() ->
    API = initial_state(),

    AccountId = create_account(API),

    EmptySummaryResp = summary(API, AccountId),
    lager:info("empty summary: ~s", [EmptySummaryResp]),
    'true' = ([] =:= kz_json:get_list_value([<<"data">>], kz_json:decode(EmptySummaryResp))),

    CreateResp = create(API, AccountId, user_webhook()),
    lager:info("created hook: ~s", [CreateResp]),
    WebhookId = kz_json:get_ne_binary_value([<<"data">>, <<"id">>], kz_json:decode(CreateResp)),

    SummaryResp = summary(API, AccountId),
    lager:info("summary resp: ~s", [SummaryResp]),
    [WebhookSummary] = kz_json:get_list_value([<<"data">>], kz_json:decode(SummaryResp)),
    WebhookId = kz_doc:id(WebhookSummary),

    UserPid = kz_process:spawn_link(fun create_user_and_wait/3, [self(), API, AccountId]),
    lager:info("managing user CRUD in ~p", [UserPid]),

    CreatedHookQS = pqc_httpd:fetch_req([<<?MODULE_STRING>>], 2000),
    lager:info("created event: ~s", [CreatedHookQS]),
    CreatedHookProps = kz_http_util:parse_query_string(CreatedHookQS),
    UserId = props:get_value(<<"id">>, CreatedHookProps),
    'true' = <<"user">> =:= props:get_value(<<"type">>, CreatedHookProps),
    'true' = <<"doc_created">> =:= props:get_value(<<"action">>, CreatedHookProps),

    UserPid ! {'patch', kz_json:from_list([{<<"patch">>, kz_binary:rand_hex(4)}])},
    lager:info("patching user"),

    PatchedHookQS = pqc_httpd:fetch_req([<<?MODULE_STRING>>], 2000),
    lager:info("update event: ~s", [PatchedHookQS]),
    PatchedHookProps = kz_http_util:parse_query_string(PatchedHookQS),

    'true' = UserId =:= props:get_value(<<"id">>, PatchedHookProps),
    'true' = <<"user">> =:= props:get_value(<<"type">>, PatchedHookProps),
    'true' = <<"doc_edited">> =:= props:get_value(<<"action">>, PatchedHookProps),

    UserPid ! 'delete',
    lager:info("deleting user"),

    DeletedHookQS = pqc_httpd:fetch_req([<<?MODULE_STRING>>], 2000),
    lager:info("delete event: ~s", [DeletedHookQS]),
    DeletedHookProps = kz_http_util:parse_query_string(DeletedHookQS),
    'true' = <<"user">> =:= props:get_value(<<"type">>, DeletedHookProps),
    'true' = <<"doc_deleted">> =:= props:get_value(<<"action">>, DeletedHookProps),
    'true' = UserId =:= props:get_value(<<"id">>, DeletedHookProps),

    DeleteResp = delete(API, AccountId, WebhookId),
    lager:info("delete webhook: ~s", [DeleteResp]),

    EmptyAgainResp = summary(API, AccountId),
    lager:info("empty again summary: ~s", [EmptyAgainResp]),
    'true' = ([] =:= kz_json:get_list_value([<<"data">>], kz_json:decode(EmptyAgainResp))),

    cleanup(API).

seq_samples() ->
    API = initial_state(),

    AvailableResp = list_available(API),
    lager:info("available: ~s", [AvailableResp]),
    Available = kz_json:get_list_value(<<"data">>, kz_json:decode(AvailableResp)),
    'true' = ([] =/= Available),

    SamplesResp = samples(API),
    lager:info("samples: ~s", [SamplesResp]),
    Samples = [_|_] = kz_json:get_list_value(<<"data">>, kz_json:decode(SamplesResp)),

    lists:all(fun(SampleId) -> can_fetch_sample(API, SampleId) end, Samples),

    AccountId = create_account(API),

    SummaryResp = summary(API, AccountId),
    lager:info("summary: ~s", [SummaryResp]),
    [] = kz_json:get_list_value(<<"data">>, kz_json:decode(SummaryResp)),

    cleanup(API),
    lager:info("FINISHED").

-spec initial_state() -> pqc_cb_api:state().
initial_state() ->
    API = pqc_cb_api:init_api(['crossbar', 'webhooks']
                             ,['cb_webhooks', 'cb_users']
                             ),
    _HTTPD = pqc_httpd:start_link(kz_log:get_callid()),
    ?INFO("HTTPD started: ~p", [_HTTPD]),
    API.

-spec cleanup() -> 'ok'.
cleanup() ->
    _ = pqc_cb_accounts:cleanup_accounts(?ACCOUNT_NAMES),
    cleanup_system().

cleanup(API) ->
    lager:info("CLEANUP TIME, EVERYBODY HELPS"),
    _ = pqc_cb_accounts:cleanup_accounts(API, ?ACCOUNT_NAMES),
    _ = pqc_cb_api:cleanup(API),
    cleanup_system().

cleanup_system() ->
    pqc_httpd:stop().

can_fetch_sample(API, SampleId) ->
    SampleResp = sample(API, SampleId),
    lager:info("sample for ~s: ~s", [SampleId, SampleResp]),
    [] =/= kz_json:get_list_value(<<"data">>, kz_json:decode(SampleResp)).

create_account(API) ->
    AccountResp = pqc_cb_accounts:create_account(API, hd(?ACCOUNT_NAMES)),
    ?INFO("created account: ~s", [AccountResp]),
    kz_json:get_value([<<"data">>, <<"id">>], kz_json:decode(AccountResp)).

user_webhook() ->
    URL = <<(pqc_httpd:base_url())/binary, ?MODULE_STRING>>,

    ModifiedUserDocs = [{<<"type">>, kzd_users:type()}
                       ,{<<"action">>, <<"all">>}
                       ],

    Webhook = kz_json:exec_first([{fun kzd_webhooks:set_uri/2, URL}
                                 ,{fun kzd_webhooks:set_name/2, <<?MODULE_STRING, "_user">>}
                                 ,{fun kzd_webhooks:set_http_verb/2, <<"post">>}
                                 ,{fun kzd_webhooks:set_hook/2, <<"object">>}
                                 ,{fun kzd_webhooks:set_custom_data/2, kz_json:from_list(ModifiedUserDocs)}
                                 ]
                                ,kzd_webhooks:new()
                                ),
    kz_doc:public_fields(Webhook).

create_user_and_wait(ParentPid, API, AccountId) ->
    UserDoc = pqc_cb_users:new_user(),
    UserResp = pqc_cb_users:create(API, AccountId, UserDoc),
    lager:info("user resp: ~s", [UserResp]),
    UserId = kz_json:get_value([<<"data">>, <<"id">>], kz_json:decode(UserResp)),

    ParentPid ! {'user_id', UserId},
    wait_for_instructions(ParentPid, API, AccountId, UserId).

wait_for_instructions(ParentPid, API, AccountId, UserId) ->
    receive
        {'patch', PatchJObj} ->
            PatchResp = pqc_cb_users:patch(API, AccountId, UserId, PatchJObj),
            lager:info("patched: ~s", [PatchResp]),
            ParentPid ! {'patched', kz_json:get_ne_binary_value(<<"revision">>, kz_json:decode(PatchResp))},
            wait_for_instructions(ParentPid, API, AccountId, UserId);
        'delete' ->
            DeleteResp = pqc_cb_users:delete(API, AccountId, UserId),
            lager:info("deleted: ~s", [DeleteResp]),
            ParentPid ! {'deleted', kz_json:get_ne_binary_value(<<"revision">>, kz_json:decode(DeleteResp))};
        'stop' -> 'ok'
    end.
