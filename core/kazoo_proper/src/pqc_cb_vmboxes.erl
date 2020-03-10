%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2018-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(pqc_cb_vmboxes).

-export([new_message/5
        ,fetch_message_metadata/4
        ,fetch_message_binary/4
        ,create_box/3
        ,delete_box/3
        ]).

-export([seq/0
        ,cleanup/0
        ]).

-include("kazoo_proper.hrl").

-define(ACCOUNT_NAMES, [<<?MODULE_STRING>>]).

-spec new_message(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object(), binary()) ->
          pqc_cb_api:response().
new_message(API, AccountId, BoxId, MessageJObj, MessageBin) ->
    MessagesURL = messages_url(AccountId, BoxId),

    Boundary = kz_http_util:create_boundary(),
    Body = create_body(MessageJObj, MessageBin, Boundary),

    RequestHeaders = pqc_cb_api:request_headers(API
                                               ,[{<<"content-type">>, "multipart/mixed; boundary=" ++ kz_term:to_list(Boundary)}
                                                ,{<<"content-length">>, iolist_size(Body)}
                                                ]
                                               ),

    Expectations = [#expectation{response_codes = [201]}],
    pqc_cb_api:make_request(Expectations
                           ,fun kz_http:put/3
                           ,MessagesURL
                           ,RequestHeaders
                           ,Body
                           ).

-spec create_body(kz_json:object(), binary(), kz_term:ne_binary()) -> binary().
create_body(MessageJObj, MessageBin, Boundary) ->
    kz_http_util:encode_multipart([{kz_json:encode(pqc_cb_api:create_envelope(MessageJObj))
                                   ,[{<<"content-type">>, <<"application/json">>}]
                                   }
                                  ,{MessageBin
                                   ,[{<<"content-type">>, <<"audio/mp3">>}]
                                   }
                                  ]
                                 ,Boundary
                                 ).

-spec fetch_message_metadata(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> pqc_cb_api:response().
fetch_message_metadata(API, AccountId, BoxId, MessageId) ->
    MessageURL = message_url(AccountId, BoxId, MessageId),

    RequestHeaders = pqc_cb_api:request_headers(API),

    Expectations = [#expectation{response_codes = [200]}],
    pqc_cb_api:make_request(Expectations
                           ,fun kz_http:get/2
                           ,MessageURL
                           ,RequestHeaders
                           ).

-spec fetch_message_binary(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> pqc_cb_api:response().
fetch_message_binary(API, AccountId, BoxId, MessageId) ->
    MessageURL = message_bin_url(AccountId, BoxId, MessageId),

    RequestHeaders = pqc_cb_api:request_headers(API, [{<<"accept">>, "audio/mp3"}]),

    Expectations = [#expectation{response_codes = [200]}],
    pqc_cb_api:make_request(Expectations
                           ,fun kz_http:get/2
                           ,MessageURL
                           ,RequestHeaders
                           ).

-spec create_box(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary()) -> pqc_cb_api:response().
create_box(API, AccountId, BoxName) ->
    BoxesURL = boxes_url(AccountId),
    RequestHeaders = pqc_cb_api:request_headers(API, [{<<"content-type">>, "application/json"}]),

    Data = kz_json:from_list([{<<"name">>, BoxName}
                             ,{<<"mailbox">>, BoxName}
                             ]),
    Req = pqc_cb_api:create_envelope(Data),
    Expectations = [#expectation{response_codes = [201]}],

    pqc_cb_api:make_request(Expectations
                           ,fun kz_http:put/3
                           ,BoxesURL
                           ,RequestHeaders
                           ,kz_json:encode(Req)
                           ).

-spec delete_box(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary()) -> pqc_cb_api:response().
delete_box(API, AccountId, BoxId) ->
    RequestHeaders = pqc_cb_api:request_headers(API),
    BoxURL = box_url(AccountId, BoxId),

    pqc_cb_api:make_request([#expectation{response_codes = [200]}]
                           ,fun kz_http:delete/2
                           ,BoxURL
                           ,RequestHeaders
                           ).

boxes_url(AccountId) ->
    string:join([pqc_cb_accounts:account_url(AccountId), "vmboxes"], "/").

box_url(AccountId, BoxId) ->
    string:join([pqc_cb_accounts:account_url(AccountId), "vmboxes", kz_term:to_list(BoxId)], "/").

messages_url(AccountId, BoxId) ->
    string:join([pqc_cb_accounts:account_url(AccountId), "vmboxes", kz_term:to_list(BoxId), "messages"], "/").

message_url(AccountId, BoxId, MessageId) ->
    string:join([pqc_cb_accounts:account_url(AccountId), "vmboxes", kz_term:to_list(BoxId), "messages", kz_term:to_list(MessageId)], "/").

message_bin_url(AccountId, BoxId, MessageId) ->
    string:join([pqc_cb_accounts:account_url(AccountId), "vmboxes", kz_term:to_list(BoxId), "messages", kz_term:to_list(MessageId), "raw"], "/").

-spec seq() -> 'ok'.
seq() ->
    Fs = [fun seq_kzoo_52/0],
    lists:foreach(fun run/1, Fs).

run(F) -> F().

seq_kzoo_52() ->
    Model = initial_state(),
    API = pqc_kazoo_model:api(Model),

    AccountResp = pqc_cb_accounts:create_account(API, hd(?ACCOUNT_NAMES)),
    lager:info("created account: ~s", [AccountResp]),

    AccountId = kz_json:get_value([<<"data">>, <<"id">>], kz_json:decode(AccountResp)),

    BoxName = kz_binary:rand_hex(4),
    CreateResp = create_box(API, AccountId, BoxName),
    lager:info("create resp: ~s", [CreateResp]),
    CreatedBox = kz_json:get_json_value(<<"data">>, kz_json:decode(CreateResp)),
    BoxId = kz_doc:id(CreatedBox),
    BoxName = kzd_vmboxes:name(CreatedBox),

    DeleteResp = delete_box(API, AccountId, BoxId),
    lager:info("delete resp: ~s", [DeleteResp]),
    DeletedBox = kz_json:get_json_value(<<"data">>, kz_json:decode(DeleteResp)),
    BoxId = kz_doc:id(DeletedBox),
    BoxName = kzd_vmboxes:name(DeletedBox),
    'true' = kz_json:is_true([<<"_read_only">>, <<"deleted">>], DeletedBox),

    cleanup(API),
    lager:info("FINISHED SEQ").

-spec initial_state() -> pqc_kazoo_model:model().
initial_state() ->
    _ = init_system(),
    API = pqc_cb_api:authenticate(),
    pqc_kazoo_model:new(API).

init_system() ->
    TestId = kz_binary:rand_hex(5),
    kz_log:put_callid(TestId),

    _ = kz_data_tracing:clear_all_traces(),
    _ = [kapps_controller:start_app(App) ||
            App <- ['crossbar']
        ],
    _ = [crossbar_maintenance:start_module(Mod) ||
            Mod <- ['cb_vmboxes']
        ],
    lager:info("INIT FINISHED").

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
