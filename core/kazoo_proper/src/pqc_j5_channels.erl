%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2021-2022, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(pqc_j5_channels).

-export([seq/0
        ,cleanup/0
        ]).

-include("kazoo_proper.hrl").

-define(ACCOUNT_NAMES, [<<?MODULE_STRING>>]).

-spec seq() -> 'ok'.
seq() ->
    API = pqc_cb_api:init_api(['crossbar', 'jonny5']
                             ,['cb_accounts', 'cb_limits_v2']
                             ),
    AccountId = create_account(API),
    _ = update_limits(API, pqc_cb_api:auth_account_id(API)),
    _ = update_limits(API, AccountId),

    j5_channels:flush(),

    CallId = kz_binary:rand_hex(5),

    %% We expect a channel destroy processed before an authz response
    %% to cause the authz response to not be sent

    _ = send_channel_destroy(AccountId, CallId),
    {'error', 'timeout'} = send_authz_req(AccountId, CallId),
    0 = query_limits(AccountId),
    cleanup(API).

create_account(API) ->
    AccountResp = pqc_cb_accounts:create_account(API, hd(?ACCOUNT_NAMES)),
    ?INFO("created account: ~s", [AccountResp]),

    kz_json:get_value([<<"data">>, <<"id">>], kz_json:decode(AccountResp)).

send_authz_req(AccountId, CallId) ->
    Req = [{<<"Call-Direction">>, <<"inbound">>}
          ,{<<"Call-ID">>, CallId}
          ,{<<"Caller-ID-Name">>, <<?MODULE_STRING>>}
          ,{<<"Caller-ID-Number">>, <<"19998887777">>}
          ,{<<"From">>, <<?MODULE_STRING>>}
          ,{<<"Request">>, <<"12223334444@realm.com">>}
          ,{<<"To">>, <<"12223334444@realm.com">>}
          ,{<<"Custom-Channel-Vars">>, kz_json:from_list([{<<"Account-ID">>, AccountId}])}
           | kz_api:default_headers(<<?MODULE_STRING>>, <<"5.0">>)
          ],
    kz_amqp_worker:call(Req
                       ,fun kapi_authz:publish_authz_req/1
                       ,fun kapi_authz:authz_resp_v/1
                       ,3 * ?MILLISECONDS_IN_SECOND
                       ).

send_channel_destroy(AccountId, CallId) ->
    Event = [{<<"Call-Direction">>, <<"inbound">>}
            ,{<<"Call-ID">>, CallId}
            ,{<<"Caller-ID-Name">>, <<?MODULE_STRING>>}
            ,{<<"Caller-ID-Number">>, <<"19998887777">>}
            ,{<<"From">>, <<?MODULE_STRING>>}
            ,{<<"Request">>, <<"12223334444@realm.com">>}
            ,{<<"To">>, <<"12223334444@realm.com">>}
            ,{<<"Timestamp">>, kz_time:now_s()}
            ,{<<"Custom-Channel-Vars">>, kz_json:from_list([{<<"Account-ID">>, AccountId}])}
             | kz_api:default_headers(<<"call_event">>, <<"CHANNEL_DESTROY">>, <<?MODULE_STRING>>, <<"5.0">>)
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

update_limits(API, AccountId) ->
    _Update = pqc_cb_limits:update(API
                                  ,AccountId
                                  ,kz_json:from_list([{<<"twoway_trunks">>, 2}
                                                     ,{<<"accept_charges">>, 'true'}
                                                     ]
                                                    )
                                  ),
    ?INFO("update limits: ~s", [_Update]).
