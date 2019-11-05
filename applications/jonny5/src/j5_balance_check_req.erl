%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc Handlers for various AMQP payloads
%%% @author Dinkor (Sergey Korobkov)
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(j5_balance_check_req).

-export([handle_req/2]).

-include("jonny5.hrl").

-spec handle_req(kz_json:object(), kz_term:proplist()) -> any().
handle_req(ReqJObj, _Props) ->
    'true' = kapi_authz:balance_check_req_v(ReqJObj),
    kz_log:put_callid(?APP_NAME),
    ReqAccounts = kz_json:get_list_value(<<"Accounts">>, ReqJObj),
    RespAccounts = kz_json:from_list(lists:foldl(fun account_balance/2, [], ReqAccounts)),
    Resp = build_resp(RespAccounts, ReqJObj),
    ServerId = kz_api:server_id(ReqJObj),
    kapi_authz:publish_balance_check_resp(ServerId, Resp).

-spec account_balance(kz_term:ne_binary(), kz_term:ne_binaries()) -> kz_term:ne_binaries().
account_balance(AccountId, Acc) ->
    Limits = j5_limits:get(AccountId),
    [{AccountId, j5_per_minute:maybe_credit_available(0, Limits)} | Acc].

build_resp(RespAccounts, ReqJObj) ->
    props:filter_undefined(
      [{<<"Balances">>, RespAccounts}
      ,{<<"Msg-ID">>, kz_api:msg_id(ReqJObj)}
       | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
      ]).
