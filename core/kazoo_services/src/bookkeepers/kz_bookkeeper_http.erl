%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2018, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_bookkeeper_http).
-behaviour(kz_gen_bookkeeper).

-export([is_good_standing/2]).
-export([sync/2]).
-export([transactions/3]).
-export([commit_transactions/2]).
-export([charge_transactions/2]).

-include("services.hrl").

-define(DEFAULT_SYNC_CONTENT_TYPE, ?DEFAULT_CONTENT_TYPE).

-record(sync, {id :: kz_term:api_ne_binary()
              ,account_id :: kz_term:api_ne_binary()
              ,items :: kz_service_items:items()
              ,url :: kz_term:api_ne_binary()
              ,method :: kz_term:api_ne_binary()
              ,content_type = ?DEFAULT_SYNC_CONTENT_TYPE :: kz_term:ne_binary() | '_'
              }).
-type sync() :: #sync{}.

-define(MOD_CONFIG_CAT, <<(?CONFIG_CAT)/binary, ".http_sync">>).

-define(CONNECT_TIMEOUT_MS
       ,kapps_config:get_integer(?MOD_CONFIG_CAT, <<"connect_timeout_ms">>, 10 * ?MILLISECONDS_IN_SECOND)
       ).
-define(HTTP_URL
       ,kapps_config:get_ne_binary(?MOD_CONFIG_CAT, <<"http_url">>, <<>>)
       ).
-define(AUTH_HEADER
       ,kapps_config:get_ne_binary(?MOD_CONFIG_CAT, <<"authorization_header">>, <<>>)
       ).

-define(HTTP_OPTS, [{'connect_timeout', ?CONNECT_TIMEOUT_MS}]).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec is_good_standing(kz_term:ne_binary(), kz_term:ne_binary()) -> boolean().
is_good_standing(_AccountId, Status) ->
    Status =:= kzd_services:status_good().

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec sync(kz_service_items:items(), kz_term:ne_binary()) -> bookkeeper_sync_result().
sync(Items, AccountId) ->
    Sync = #sync{id = get_sync_id(AccountId)
                ,account_id = AccountId
                ,items = Items
                ,url = ?HTTP_URL
                ,method = <<"post">>
                },
    kz_util:put_callid(Sync#sync.id),
    http_request(Sync).

-spec http_request(sync()) -> bookkeeper_sync_result().
http_request(#sync{url = 'undefined'}) ->
    lager:info("http sync URL is empty - skipping");
http_request(#sync{method = <<"post">>, url = Url} = Sync) ->
    Headers = [{"Content-Type", kz_term:to_list(Sync#sync.content_type)}
               | http_headers(Sync)
              ],
    Payload = http_payload(Sync),
    lager:debug("attempting http billing sync with ~s: ~s", [Url, Payload]),
    handle_resp(kz_http:post(Url, Headers, Payload, ?HTTP_OPTS), Sync).

-spec handle_resp(kz_http:ret(), sync()) -> bookkeeper_sync_result().
handle_resp({'ok', 200, _, _}, _Sync) ->
    lager:debug("http billing sync received 200 reply");
handle_resp({'ok', 402, _, _}, _Sync) ->
    lager:debug("http billing sync received 402 reply"),
    'delinquent';
handle_resp({'ok', RespCode, _, _}, _Sync) ->
    lager:debug("http billing sync received an unexpected response code: ~p", [RespCode]),
    'retry';
handle_resp({'error', _E}, _Sync) ->
    lager:debug("http billing sync failed: ~p", [_E]),
    'retry'.

-spec get_sync_id(kz_term:ne_binary()) -> kz_term:ne_binary().
get_sync_id(AccountId) ->
    {'ok', JObj} = kz_services:fetch_services_doc(AccountId),
    kz_doc:revision(JObj).

-spec http_payload(sync()) -> iolist().
http_payload(#sync{content_type = <<"application/json">>} = Sync) ->
    lager:debug("creating application/json payload for http billing sync"),
    kz_json:encode(
      kz_json:from_list(
        [{<<"account_id">>, Sync#sync.account_id}
        ,{<<"sync_id">>, Sync#sync.id}
        ,{<<"items">>, kz_service_items:public_json(Sync#sync.items)}
        ])).

-spec http_headers(sync()) -> kz_term:proplist().
http_headers(Sync) ->
    props:filter_empty(
      [{"X-Sync-ID", to_list(Sync#sync.id)}
      ,{"X-Account-ID", to_list(Sync#sync.account_id)}
      ,{"Authorization", to_list(?AUTH_HEADER)}
      ]
     ).

-spec to_list(kz_term:api_binary()) -> 'undefined' | list().
to_list('undefined') -> 'undefined';
to_list(Value) ->
    case kz_term:is_empty(Value) of
        'true' -> 'undefined';
        'false' -> kz_term:to_list(Value)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec commit_transactions(kz_term:ne_binary(), kz_transactions:kz_transactions()) -> ok | error.
commit_transactions(_BillingId, Transactions) ->
    kz_transactions:save(Transactions),
    'ok'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec charge_transactions(kz_term:ne_binary(), kz_json:objects()) -> [].
charge_transactions(_BillingId, _Transactions) -> [].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec transactions(kz_term:ne_binary(), kz_time:gregorian_seconds(), kz_time:gregorian_seconds()) ->
                          {'ok', kz_transaction:transactions()} |
                          {'error', atom()}.
transactions(AccountId, From, To) ->
    case kz_transactions:fetch_local(AccountId, From, To) of
        {'error', _Reason}=Error -> Error;
        {'ok', Transactions}=Res ->
            handle_topup(AccountId, Transactions),
            Res
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec handle_topup(kz_term:ne_binary(), kz_transactions:transactions()) -> 'ok'.
handle_topup(_, []) -> 'ok';
handle_topup(BillingId, [Transaction|Transactions]) ->
    case kz_transaction:code(Transaction) =:= ?CODE_TOPUP of
        'false' ->
            handle_topup(BillingId, Transactions);
        'true' ->
            send_topup_notification(BillingId, Transaction)
    end.

-spec send_topup_notification(kz_term:ne_binary(), kz_transaction:transaction()) -> 'ok'.
send_topup_notification(BillingId, Transaction) ->
    Props = [{<<"Account-ID">>, BillingId}
            ,{<<"Amount">>, wht_util:units_to_dollars(kz_transaction:amount(Transaction))}
            ,{<<"Response">>, <<"Authorized">>}
            ,{<<"Success">>, <<"true">>}
            ,{<<"ID">>, kz_transaction:id(Transaction)}
            ,{<<"Timestamp">>, kz_time:now_s()}
             | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
            ],
    kapps_notify_publisher:cast(Props, fun kapi_notifications:publish_topup/1).
