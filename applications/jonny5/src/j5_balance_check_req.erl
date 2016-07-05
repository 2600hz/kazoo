%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, 2600Hz INC
%%% @doc
%%% Handlers for various AMQP payloads
%%% @end
%%% @contributors
%%%     Dinkor (Sergey Korobkov)
%%%-------------------------------------------------------------------
-module(j5_balance_check_req).

-export([handle_req/2]).

-include("jonny5.hrl").

-spec handle_req(kz_json:object(), kz_proplist()) -> any().
handle_req(ReqJObj, _Props) ->
    'true' = kapi_authz:balance_check_req_v(ReqJObj),
    kz_util:put_callid(?APP_NAME),
    ReqAccounts = kz_json:get_list_value(<<"Accounts">>, ReqJObj),
    RespAccounts = kz_json:from_list(lists:foldl(fun account_balance/2, [], ReqAccounts)),
    Resp = build_resp(RespAccounts, ReqJObj),
    ServerId = kz_api:server_id(ReqJObj),
    kapi_authz:publish_balance_check_resp(ServerId, Resp).

-spec account_balance(ne_binary(), ne_binaries()) -> ne_binaries().
account_balance(AccountId, Acc) ->
    Limits = j5_limits:get(AccountId),
    [ {AccountId, j5_per_minute:maybe_credit_available(0, Limits)} | Acc ].

build_resp(RespAccounts, ReqJObj) ->
    props:filter_undefined(
      [{<<"Balances">>, RespAccounts}
      ,{<<"Msg-ID">>, kz_api:msg_id(ReqJObj)}
       | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
      ]).

