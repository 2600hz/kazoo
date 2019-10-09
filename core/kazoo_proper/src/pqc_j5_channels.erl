%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2019-, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(pqc_j5_channels).

-export([seq/0
        ,cleanup/0
        ]).

-include("kazoo_proper.hrl").

-define(ACCOUNT_NAMES, [<<?MODULE_STRING>>]).
-define(WAIT_AFTER_DELETE, 1500).
-define(WAIT_AFTER_UPDATE, 1500).

-spec seq() -> 'ok'.
seq() ->
    API = pqc_cb_api:init_api(['crossbar', 'jonny5']
                             ,['cb_accounts', 'cb_limits_v2']
                             ),
    AccountId = create_account(API),
    ResellerId = reseller_id(API, AccountId),

    _ = update_limits(API, pqc_cb_api:auth_account_id(API), 2),
    _ = update_limits(API, AccountId, 2),

    j5_channels:flush(),

    CallId1 = kz_binary:rand_hex(5),
    CallId2 = kz_binary:rand_hex(5),
    CallId3 = kz_binary:rand_hex(5),
    CallId4 = kz_binary:rand_hex(5),

    _ = send_channel_create(AccountId, ResellerId, CallId1),
    {'ok', Resp1} = send_authz_req(AccountId, ResellerId, CallId1),
    'true' = is_authorized(Resp1),
    1 = query_limits(AccountId),

    _ = send_channel_create(AccountId, ResellerId, CallId2),
    {'ok', Resp2} = send_authz_req(AccountId, ResellerId, CallId2),
    'true' = is_authorized(Resp2),
    2 = query_limits(AccountId),

    _ = send_channel_create(AccountId, ResellerId, CallId3),
    {'ok', Resp3} = send_authz_req(AccountId, ResellerId, CallId3),
    'false' = is_authorized(Resp3),
    3 = query_limits(AccountId),
    _ = send_channel_destroy(AccountId, ResellerId, CallId3),
    _ = timer:sleep(?WAIT_AFTER_DELETE),
    2 = query_limits(AccountId),
    _ = send_channel_destroy(AccountId, ResellerId, CallId1),
    _ = send_channel_destroy(AccountId, ResellerId, CallId2),
    _ = timer:sleep(?WAIT_AFTER_DELETE),
    0 = query_limits(AccountId),

    _ = update_limits(API, AccountId, 0),
    _ = timer:sleep(?WAIT_AFTER_UPDATE),

    _ = send_channel_create(AccountId, ResellerId, CallId4),
    {'ok', Resp4} = send_authz_req(AccountId, ResellerId, CallId4),
    'false' = is_authorized(Resp4),
    1 = query_limits(AccountId),
    _ = send_channel_destroy(AccountId, ResellerId, CallId4),
    _ = timer:sleep(?WAIT_AFTER_DELETE),

    0 = query_limits(AccountId),

    cleanup(API).

is_authorized(JObj) ->
    kz_json:get_boolean_value(<<"Is-Authorized">>, JObj).

create_account(API) ->
    AccountResp = pqc_cb_accounts:create_account(API, hd(?ACCOUNT_NAMES)),
    ?INFO("created account: ~s", [AccountResp]),

    kz_json:get_value([<<"data">>, <<"id">>], kz_json:decode(AccountResp)).

reseller_id(API, AccountId) ->
    FetchResp = pqc_cb_accounts:fetch_account(API, AccountId),
    ?INFO("account fetched: ~s", [FetchResp]),

    kz_json:get_value([<<"data">>, <<"reseller_id">>], kz_json:decode(FetchResp)).

send_authz_req(AccountId, ResellerId, CallId) ->
    Req = [{<<"Call-Direction">>, <<"inbound">>}
          ,{<<"Call-ID">>, CallId}
          ,{<<"Caller-ID-Name">>, <<?MODULE_STRING>>}
          ,{<<"Caller-ID-Number">>, <<"19998887777">>}
          ,{<<"From">>, <<?MODULE_STRING>>}
          ,{<<"Request">>, <<"12223334444@realm.com">>}
          ,{<<"To">>, <<"12223334444@realm.com">>}
          ,{<<"Custom-Channel-Vars">>, kz_json:from_list([{<<"Account-ID">>, AccountId}
                                                         ,{<<"Reseller-ID">>, ResellerId}
                                                         ])}
           | kz_api:default_headers(<<?MODULE_STRING>>, <<"5.0">>)
          ],
    kz_amqp_worker:call(Req
                       ,fun kapi_authz:publish_authz_req/1
                       ,fun kapi_authz:authz_resp_v/1
                       ,3 * ?MILLISECONDS_IN_SECOND
                       ).

send_channel_destroy(AccountId, ResellerId, CallId) ->
    Event = [{<<"Call-Direction">>, <<"inbound">>}
            ,{<<"Call-ID">>, CallId}
            ,{<<"Caller-ID-Name">>, <<?MODULE_STRING>>}
            ,{<<"Caller-ID-Number">>, <<"19998887777">>}
            ,{<<"From">>, <<?MODULE_STRING>>}
            ,{<<"Request">>, <<"12223334444@realm.com">>}
            ,{<<"To">>, <<"12223334444@realm.com">>}
            ,{<<"Timestamp">>, kz_time:now_s()}
            ,{<<"Custom-Channel-Vars">>, kz_json:from_list([{<<"Account-ID">>, AccountId}
                                                           ,{<<"Reseller-ID">>, ResellerId}
                                                           ])}
             | kz_api:default_headers(<<"call_event">>, <<"CHANNEL_DESTROY">>, <<?MODULE_STRING>>, <<"5.0">>)
            ],
    kz_amqp_worker:cast(Event, fun kapi_call:publish_event/1).

send_channel_create(AccountId, ResellerId, CallId) ->
    Event = [{<<"Call-Direction">>, <<"inbound">>}
            ,{<<"Call-ID">>, CallId}
            ,{<<"Caller-ID-Name">>, <<?MODULE_STRING>>}
            ,{<<"Caller-ID-Number">>, <<"19998887777">>}
            ,{<<"From">>, <<?MODULE_STRING>>}
            ,{<<"Request">>, <<"12223334444@realm.com">>}
            ,{<<"To">>, <<"12223334444@realm.com">>}
            ,{<<"Timestamp">>, kz_time:now_s()}
            ,{<<"Custom-Channel-Vars">>, kz_json:from_list([{<<"Account-ID">>, AccountId}
                                                           ,{<<"Reseller-ID">>, ResellerId}
                                                           ])}
             | kz_api:default_headers(<<"call_event">>, <<"CHANNEL_CREATE">>, <<?MODULE_STRING>>, <<"5.0">>)
            ],
    kz_amqp_worker:cast(Event, fun kapi_call:publish_event/1).

query_limits(AccountId) ->
    j5_channels:total_calls(AccountId).

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

update_limits(API, AccountId, Trunks) ->
    _Update = pqc_cb_limits:update(API
                                  ,AccountId
                                  ,kz_json:from_list([{<<"twoway_trunks">>, Trunks}
                                                     ,{<<"accept_charges">>, 'true'}
                                                     ]
                                                    )
                                  ),
    ?INFO("update limits: ~s", [_Update]).
