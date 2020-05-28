-module(pqc_cb_vmboxes).

-export([new_message/5
        ,fetch_message_metadata/4
        ,fetch_message_binary/4
        ,list_messages/3, list_messages/4
        ,create_box/3
        ,delete_box/3
        ]).

-export([seq/0, seq_kcro_24/0
        ,cleanup/0
        ]).

-include_lib("kazoo_stdlib/include/kz_types.hrl").

-define(ACCOUNT_NAMES, [<<?MODULE_STRING>>]).

-spec new_message(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object(), binary()) ->
          pqc_cb_api:response().
new_message(API, AccountId, BoxId, MessageJObj, MessageBin) ->
    MessagesURL = messages_url(AccountId, BoxId),

    Boundary = kz_http_util:create_boundary(),
    Body = kz_http_util:encode_multipart([{kz_json:encode(pqc_cb_api:create_envelope(MessageJObj))
                                          ,[{<<"content-type">>, <<"application/json">>}]
                                          }
                                         ,{MessageBin
                                          ,[{<<"content-type">>, <<"audio/mp3">>}]
                                          }
                                         ]
                                        ,Boundary
                                        ),

    RequestHeaders = pqc_cb_api:request_headers(API
                                               ,[{<<"content-type">>, <<"multipart/mixed; boundary=", Boundary/binary>>}
                                                ,{<<"content-length">>, iolist_size(Body)}
                                                ]
                                               ),

    pqc_cb_api:make_request([201]
                           ,fun kz_http:put/3
                           ,MessagesURL
                           ,RequestHeaders
                           ,Body
                           ).

-spec fetch_message_metadata(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> pqc_cb_api:response().
fetch_message_metadata(API, AccountId, BoxId, MessageId) ->
    MessageURL = message_url(AccountId, BoxId, MessageId),

    RequestHeaders = pqc_cb_api:request_headers(API),

    pqc_cb_api:make_request([200]
                           ,fun kz_http:get/2
                           ,MessageURL
                           ,RequestHeaders
                           ).

-spec fetch_message_binary(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> pqc_cb_api:response().
fetch_message_binary(API, AccountId, BoxId, MessageId) ->
    MessageURL = message_bin_url(AccountId, BoxId, MessageId),

    RequestHeaders = pqc_cb_api:request_headers(API, [{<<"accept">>, <<"audio/mp3">>}]),

    pqc_cb_api:make_request([200]
                           ,fun kz_http:get/2
                           ,MessageURL
                           ,RequestHeaders
                           ).

-spec list_messages(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary()) -> pqc_cb_api:response().
list_messages(API, AccountId, BoxId) ->
    list_messages(API, AccountId, BoxId, []).

-spec list_messages(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()) -> pqc_cb_api:response().
list_messages(API, AccountId, BoxId, QuerystringParams) ->
    MessagesURL = messages_url(AccountId, BoxId, QuerystringParams),

    pqc_cb_api:make_request([200]
                           ,fun kz_http:get/2
                           ,MessagesURL
                           ,pqc_cb_api:request_headers(API)
                           ).

-spec create_box(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary()) -> pqc_cb_api:response().
create_box(API, AccountId, BoxName) ->
    BoxesURL = boxes_url(AccountId),
    RequestHeaders = pqc_cb_api:request_headers(API, [{<<"content-type">>, <<"application/json">>}]),

    Data = kz_json:from_list([{<<"name">>, BoxName}
                             ,{<<"mailbox">>, BoxName}
                             ]),
    Req = pqc_cb_api:create_envelope(Data),

    pqc_cb_api:make_request([201]
                           ,fun kz_http:put/3
                           ,BoxesURL
                           ,RequestHeaders
                           ,kz_json:encode(Req)
                           ).

-spec delete_box(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary()) -> pqc_cb_api:response().
delete_box(API, AccountId, BoxId) ->
    RequestHeaders = pqc_cb_api:request_headers(API),
    BoxURL = box_url(AccountId, BoxId),

    pqc_cb_api:make_request([200]
                           ,fun kz_http:delete/2
                           ,BoxURL
                           ,RequestHeaders
                           ).

boxes_url(AccountId) ->
    string:join([pqc_cb_accounts:account_url(AccountId), "vmboxes"], "/").

box_url(AccountId, BoxId) ->
    string:join([pqc_cb_accounts:account_url(AccountId), "vmboxes", kz_term:to_list(BoxId)], "/").

messages_url(AccountId, BoxId) ->
    messages_url(AccountId, BoxId, []).

messages_url(AccountId, BoxId, QuerystringParams) ->
    [string:join([pqc_cb_accounts:account_url(AccountId), "vmboxes", kz_term:to_list(BoxId), "messages"], "/")
    ,"?", kz_http_util:props_to_querystring(QuerystringParams)
    ].

message_url(AccountId, BoxId, MessageId) ->
    string:join([pqc_cb_accounts:account_url(AccountId), "vmboxes", kz_term:to_list(BoxId), "messages", kz_term:to_list(MessageId)], "/").

message_bin_url(AccountId, BoxId, MessageId) ->
    string:join([pqc_cb_accounts:account_url(AccountId), "vmboxes", kz_term:to_list(BoxId), "messages", kz_term:to_list(MessageId), "raw"], "/").

-spec seq() -> 'ok'.
seq() ->
    Fs = [fun seq_kzoo_52/0
         ,fun seq_kcro_24/0
         ],
    lists:foreach(fun run/1, Fs).

run(F) -> F().

-spec seq_kcro_24() -> 'ok'.
seq_kcro_24() ->
    Model = initial_state(),
    API = pqc_kazoo_model:api(Model),

    AccountResp = pqc_cb_accounts:create_account(API, hd(?ACCOUNT_NAMES)),
    lager:info("created account: ~s", [AccountResp]),

    AccountId = kz_json:get_value([<<"data">>, <<"id">>], kz_json:decode(AccountResp)),

    BoxName = kz_binary:rand_hex(4),
    CreateResp = create_box(API, AccountId, BoxName),
    lager:info("created box resp: ~s", [CreateResp]),

    CreatedBox = kz_json:get_json_value(<<"data">>, kz_json:decode(CreateResp)),
    BoxId = kz_doc:id(CreatedBox),

    %% create messages
    {{Year, Month, Day}, _} = calendar:universal_time(),
    CurrentMonth = {{Year, Month, Day}
                   ,CurrentMsgs = [{<<"0-new">>, <<"new">>}     %% oldest
                                  ,{<<"1-saved">>, <<"saved">>}
                                  ,{<<"2-new">>, <<"new">>}
                                  ,{<<"3-saved">>, <<"saved">>}
                                  ,{<<"4-saved">>, <<"saved">>}
                                  ,{<<"5-new">>, <<"new">>}
                                  ,{<<"6-saved">>, <<"saved">>}
                                  ,{<<"7-new">>, <<"new">>}
                                  ,{<<"8-saved">>, <<"saved">>} %% newest
                                  ]
                   },
    create_messages(API, AccountId, BoxId, CurrentMonth),

    {PrevYear, PrevMonth, PrevDay} = kz_date:normalize({Year, Month-1, Day}),

    LastMonth = {{PrevYear, PrevMonth, PrevDay}
                ,OlderMsgs = [{<<"10-new">>, <<"new">>}
                             ,{<<"11-saved">>, <<"saved">>}
                             ,{<<"12-new">>, <<"new">>}
                             ,{<<"13-saved">>, <<"saved">>}
                             ]
                },
    create_messages(API, AccountId, BoxId, LastMonth),

    ExpectedCDRs = lists:reverse(CurrentMsgs) ++ lists:reverse(OlderMsgs),
    ExpectedCDRIds = [CDRId || {CDRId, _} <- ExpectedCDRs],
    NewCDRIds = [CDRId || {CDRId, <<"new">>} <- ExpectedCDRs],

    lager:info("expected CDR IDs: ~p", [ExpectedCDRIds]),

    %% The main principle to reproduce the issue seems to be to "tune"
    %% page_size in such a way that it would be less than db size but
    %% there are requested items in the rest of the db that satisfy
    %% the filter. These items will be missed in the response.

    %% For example, the current MODB contains 4 new(n) items and 5
    %% saved(s) messages - "n s n s s n s n s", the order does matter.

    %% If we request new messages with "page_size=5" and filter for
    %% non-saved messages, we will get only 2 "new" messages from this db,
    %% as the code takes the next db because the page_size is reached,
    %% independant on the fact that next_start_key is available and
    %% the filtered out result less that page_size.
    %% ------------------------------------------------------------------------
    ListedResp = list_messages(API, AccountId, BoxId
                              ,[{<<"page_size">>, 5}
                               ,{<<"filter_not_folder">>, <<"saved">>}
                               ]),
    lager:info("listed messages ~s", [ListedResp]),
    ListedJObj = kz_json:decode(ListedResp),
    <<"success">> = kz_json:get_ne_binary_value(<<"status">>, ListedJObj),
    5 = kz_json:get_integer_value(<<"page_size">>, ListedJObj),

    lager:info("expecting ~p", [NewCDRIds]),
    Listing = kz_json:get_list_value(<<"data">>, ListedJObj),
    UpdatedCDRIds = remove_expected_messages(NewCDRIds, Listing),

    NextStartKey = kz_json:get_ne_binary_value(<<"next_start_key">>, ListedJObj),

    SecondPageResp = list_messages(API, AccountId, BoxId
                                  ,[{<<"page_size">>, 5}
                                   ,{<<"filter_not_folder">>, <<"saved">>}
                                   ,{<<"start_key">>, NextStartKey}
                                   ]
                                  ),
    lager:info("second page: ~s", [SecondPageResp]),
    SecondPage = kz_json:decode(SecondPageResp),
    <<"success">> = kz_json:get_ne_binary_value(<<"status">>, SecondPage),
    1 = kz_json:get_integer_value(<<"page_size">>, SecondPage),

    lager:info("expecting second page ~p", [UpdatedCDRIds]),
    SecondListing = kz_json:get_list_value(<<"data">>, SecondPage),
    [] = remove_expected_messages(UpdatedCDRIds, SecondListing),

    AllNewOnePageResp = list_messages(API, AccountId, BoxId
                                     ,[{<<"filter_not_folder">>, <<"saved">>}]
                                     ),
    lager:info("all new messages ~s", [AllNewOnePageResp]),
    AllNewJObj = kz_json:decode(AllNewOnePageResp),
    <<"success">> = kz_json:get_ne_binary_value(<<"status">>, AllNewJObj),
    'true' = length(NewCDRIds) =:= kz_json:get_integer_value(<<"page_size">>, AllNewJObj),
    AllNewListing = kz_json:get_list_value(<<"data">>, AllNewJObj),
    [] = remove_expected_messages(NewCDRIds, AllNewListing),

    AllMessagesResp = list_messages(API, AccountId, BoxId),
    lager:info("all messages ~s", [AllMessagesResp]),
    AllJObj = kz_json:decode(AllMessagesResp),
    <<"success">> = kz_json:get_ne_binary_value(<<"status">>, AllJObj),

    'true' = length(ExpectedCDRIds) =:= kz_json:get_integer_value(<<"page_size">>, AllJObj),
    AllListing = kz_json:get_list_value(<<"data">>, AllJObj),
    [] = remove_expected_messages(ExpectedCDRIds, AllListing),

    %% no pagination enabled
    NoPagingResp = list_messages(API, AccountId, BoxId, [{<<"paginate">>, 'false'}]),
    lager:info("no paging messages ~s", [NoPagingResp]),
    NoPagingJObj = kz_json:decode(NoPagingResp),
    <<"success">> = kz_json:get_ne_binary_value(<<"status">>, NoPagingJObj),

    NoPagingListing = kz_json:get_list_value(<<"data">>, NoPagingJObj),
    'true' = length(ExpectedCDRIds) =:= length(NoPagingListing),
    [] = remove_expected_messages(ExpectedCDRIds, NoPagingListing),

    %% no pagination enabled but with filter
    NoPagingFilterResp = list_messages(API, AccountId, BoxId, [{<<"paginate">>, 'false'}
                                                              ,{<<"filter_not_folder">>, <<"saved">>}
                                                              ]),
    lager:info("no paging but filtered messages ~s", [NoPagingFilterResp]),
    NoPagingFilterJObj = kz_json:decode(NoPagingFilterResp),
    <<"success">> = kz_json:get_ne_binary_value(<<"status">>, NoPagingFilterJObj),

    NoPagingFilterListing = kz_json:get_list_value(<<"data">>, NoPagingFilterJObj),
    'true' = length(NewCDRIds) =:= length(NoPagingFilterListing),
    [] = remove_expected_messages(NewCDRIds, NoPagingFilterListing),


    DeleteResp = delete_box(API, AccountId, BoxId),
    lager:info("delete resp: ~s", [DeleteResp]),
    DeletedBox = kz_json:get_json_value(<<"data">>, kz_json:decode(DeleteResp)),
    BoxId = kz_doc:id(DeletedBox),
    BoxName = kzd_vmboxes:name(DeletedBox),
    'true' = kz_json:is_true([<<"_read_only">>, <<"deleted">>], DeletedBox),

    cleanup(API),
    lager:info("FINISHED KCRO-24").

remove_expected_messages(CDRIds, []) -> CDRIds;
remove_expected_messages([CDRId | CDRIds], [Message | Messages]) ->
    CDRId = kz_json:get_ne_binary_value(<<"call_id">>, Message),
    remove_expected_messages(CDRIds, Messages).

seq_kzoo_52() ->
    Model = initial_state(),
    API = pqc_kazoo_model:api(Model),

    AccountResp = pqc_cb_accounts:create_account(API, hd(?ACCOUNT_NAMES)),
    lager:info("created account: ~s", [AccountResp]),

    AccountId = kz_json:get_value([<<"data">>, <<"id">>], kz_json:decode(AccountResp)),

    BoxName = kz_binary:rand_hex(4),
    CreateResp = create_box(API, AccountId, BoxName),
    lager:info("created box resp: ~s", [CreateResp]),
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
    lager:info("FINISHED KZOO-52").

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

create_messages(API, AccountId, BoxId, {{Year, Month, Day}, Folders}) ->
    MODB = kz_util:format_account_id(AccountId, Year, Month),
    'true' = kazoo_modb:create(MODB),

    {'ok', MP3} = file:read_file(filename:join([code:priv_dir('kazoo_proper'), "mp3.mp3"])),

    StartTime = calendar:datetime_to_gregorian_seconds({{Year, Month, Day}, {0, 0, 0}}),

    {_StartTime, Messages} = lists:foldl(fun create_voicemail/2
                                        ,{StartTime, []}
                                        ,Folders
                                        ),
    lists:foreach(fun(M) ->
                          <<_/binary>> = new_message(API, AccountId, BoxId, M, MP3)
                  end
                 ,Messages
                 ).

create_voicemail(Folder, {Timestamp, Messages}) ->
    Message = create_message(Folder, Timestamp),
    {Timestamp + ?SECONDS_IN_MINUTE, [Message | Messages]}.

create_message({CallId, Folder}, Timestamp) ->
    kz_json:from_list([{<<"call_id">>, CallId}
                      ,{<<"timestamp">>, Timestamp}
                      ,{<<"folder">>, Folder}
                      ,{<<"metadata">>
                       ,kz_json:from_list([{<<"call_id">>, CallId}
                                          ,{<<"caller_id_name">>, <<?MODULE_STRING>>}
                                          ,{<<"caller_id_number">>, <<?MODULE_STRING>>}
                                          ,{<<"length">>, 0}
                                          ,{<<"folder">>, Folder}
                                          ,{<<"timestamp">>, Timestamp}
                                          ])
                       }
                      ]).
