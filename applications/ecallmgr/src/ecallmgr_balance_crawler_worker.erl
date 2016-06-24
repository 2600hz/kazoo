%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, 2600Hz, INC
%%% @doc
%%% Jonny5 module (worker) for disconnect calls when account
%%% balance drops below zero
%%% @end
%%% @contributors
%%%     Dinkor (Sergey Korobkov)
%%%-------------------------------------------------------------------
-module(ecallmgr_balance_crawler_worker).

-export([start/0]).

-include_lib("ecallmgr/src/ecallmgr.hrl").


-define(SERVER, ?MODULE).
-define(INTERACCOUNT_DELAY_MS, ecallmgr_config:get_integer(<<"balance_crawler_interaccount_delay_ms">>, 10)).
-define(FETCH_TIMEOUT_MS, ecallmgr_config:get_integer(<<"balance_crawler_fetch_timeout_ms">>, 10000)).

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

-spec send_req(ne_binaries()) -> ne_binaries().
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

-spec balance_check_req(ne_binaries()) -> kz_proplist().
balance_check_req(Accounts) ->
    props:filter_undefined(
      [{<<"Accounts">>, Accounts}
       | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
      ]).

-spec balance_check_response(kz_json:object()) -> ne_binaries().
balance_check_response(JObj) ->
    Balances = kz_json:get_json_value(<<"Balances">>, JObj),
    kz_json:foldl(
      fun(Key, Value, Acc) ->
              case Value of
                  'true' -> Acc;
                  'false' -> [ Key | Acc ]
              end
      end
      ,[]
      ,Balances
     ).

-spec disconnect_accounts(ne_binaries()) -> 'ok'.
disconnect_accounts([]) -> 'ok';
disconnect_accounts([Account|Accounts]) ->
    disconnect_account(Account),
    timer:sleep(?INTERACCOUNT_DELAY_MS),
    disconnect_accounts(Accounts).

-spec disconnect_account(ne_binary()) -> 'ok'.
disconnect_account(AccountId) ->
    case ecallmgr_fs_channels:per_minute_channels(AccountId) of
        [] ->
            lager:debug("account ~p doesn't have any per-minute channels",[AccountId]);
        Channels ->
            lager:debug("account ~p has ~p per-minute channels, disconnect them",[AccountId, length(Channels)]),
            disconnect_channels(Channels)
    end.

disconnect_channels([]) -> 'ok';
disconnect_channels([Channel|Channels]) ->
    try_disconnect_channel(Channel),
    disconnect_channels(Channels).

-spec try_disconnect_channel({api_binary(), api_binary()}) -> 'ok'.
try_disconnect_channel({Node, UUID}) ->
    lager:debug("disconnect channel ~p",[UUID]),
    _ = ecallmgr_util:send_cmd(Node, UUID, "hangup", ""),
    'ok'.
