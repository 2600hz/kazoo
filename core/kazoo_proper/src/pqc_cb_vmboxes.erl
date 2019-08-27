%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2018-2019, 2600Hz
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
        ]).

-include("kazoo_proper.hrl").

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

boxes_url(AccountId) ->
    string:join([pqc_cb_accounts:account_url(AccountId), "vmboxes"], "/").

messages_url(AccountId, BoxId) ->
    string:join([pqc_cb_accounts:account_url(AccountId), "vmboxes", kz_term:to_list(BoxId), "messages"], "/").

message_url(AccountId, BoxId, MessageId) ->
    string:join([pqc_cb_accounts:account_url(AccountId), "vmboxes", kz_term:to_list(BoxId), "messages", kz_term:to_list(MessageId)], "/").

message_bin_url(AccountId, BoxId, MessageId) ->
    string:join([pqc_cb_accounts:account_url(AccountId), "vmboxes", kz_term:to_list(BoxId), "messages", kz_term:to_list(MessageId), "raw"], "/").
