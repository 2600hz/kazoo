%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2018-2019, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(pqc_cb_vmboxes).

-export([new_message/5
        ,create_box/3
        ]).

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

boxes_url(AccountId) ->
    string:join([pqc_cb_accounts:account_url(AccountId), "vmboxes"], "/").

messages_url(AccountId, BoxId) ->
    string:join([pqc_cb_accounts:account_url(AccountId), "vmboxes", kz_term:to_list(BoxId), "messages"], "/").
