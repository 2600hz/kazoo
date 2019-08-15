%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc Jonny5 module (worker) for disconnect calls when account
%%% balance drops below zero.
%%%
%%% @author Dinkor (Sergey Korobkov)
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(ecallmgr_balance_crawler_worker).

-export([start/0]).

-include("ecallmgr.hrl").

-define(SERVER, ?MODULE).
-define(INTERACCOUNT_DELAY_MS, kapps_config:get_integer(?APP_NAME, <<"balance_crawler_interaccount_delay_ms">>, 10)).
-define(FETCH_TIMEOUT_MS, kapps_config:get_integer(?APP_NAME, <<"balance_crawler_fetch_timeout_ms">>, 10000)).

-spec start() -> no_return().
start() ->
    case ecallmgr_fs_channels:per_minute_accounts() of
        [] -> exit('no_accounts');
        Accounts ->
            lager:debug("get balance for ~p account(s)", [length(Accounts)]),
            DisconnectAccounts = send_req(Accounts),
            'ok' = disconnect_accounts(DisconnectAccounts),
            exit('work_done')
    end.

-spec send_req(kz_term:ne_binaries()) -> kz_term:ne_binaries().
send_req(Accounts) ->
    ReqResp = kz_amqp_worker:call(balance_check_req(Accounts)
                                 ,fun kapi_authz:publish_balance_check_req/1
                                 ,fun kapi_authz:balance_check_resp_v/1
                                 ,?FETCH_TIMEOUT_MS
                                 ),
    case ReqResp of
        {'error', _R} ->
            lager:debug("request for balance check lookup failed: ~p", [_R]),
            [];
        {'ok', JObj} ->
            balance_check_response(JObj)
    end.

-spec balance_check_req(kz_term:ne_binaries()) -> kz_term:proplist().
balance_check_req(Accounts) ->
    props:filter_undefined(
      [{<<"Accounts">>, Accounts}
       | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
      ]).

-spec balance_check_response(kz_json:object()) -> kz_term:ne_binaries().
balance_check_response(JObj) ->
    Balances = kz_json:get_json_value(<<"Balances">>, JObj),
    kz_json:foldl(fun maybe_add_key/3, [], Balances).

-spec maybe_add_key(kz_json:key(), boolean(), kz_json:keys()) -> kz_json:keys().
maybe_add_key(Key, 'false', Acc) -> [Key | Acc];
maybe_add_key(_Key, 'true', Acc) -> Acc.

-spec disconnect_accounts(kz_term:ne_binaries()) -> 'ok'.
disconnect_accounts([]) -> 'ok';
disconnect_accounts([Account|Accounts]) ->
    disconnect_account(Account),
    timer:sleep(?INTERACCOUNT_DELAY_MS),
    disconnect_accounts(Accounts).

-spec disconnect_account(kz_term:ne_binary()) -> 'ok'.
disconnect_account(AccountId) ->
    case ecallmgr_fs_channels:per_minute_channels(AccountId) of
        [] ->
            lager:debug("account ~p doesn't have any per-minute channels",[AccountId]);
        Channels ->
            lager:debug("account ~p has ~p per-minute channels, disconnect them",[AccountId, length(Channels)]),
            disconnect_channels(Channels)
    end.

-spec disconnect_channels(kz_term:proplist()) -> 'ok'.
disconnect_channels([]) -> 'ok';
disconnect_channels([Channel|Channels]) ->
    try_disconnect_channel(Channel),
    disconnect_channels(Channels).

-spec try_disconnect_channel({atom(), kz_term:ne_binary()}) -> 'ok'.
try_disconnect_channel({Node, UUID}) ->
    lager:debug("disconnect channel ~p",[UUID]),
    _ = ecallmgr_util:send_cmd(Node, UUID, "hangup", ""),
    'ok'.
