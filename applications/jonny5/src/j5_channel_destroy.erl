%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc Handlers for various AMQP payloads
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(j5_channel_destroy).

-export([handle_req/2]).

-include("jonny5.hrl").

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec handle_req(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_req(JObj, _Props) ->
    'true' = kapi_call:event_v(JObj),
    kz_log:put_callid(JObj),
    timer:sleep(1000 + rand:uniform(2000)),
    Request = j5_request:from_jobj(JObj),
    AccountId = j5_request:account_id(Request),
    _ = account_reconcile_cdr(AccountId, Request),
    _ = reseller_reconcile_cdr(AccountId, Request),
    'ok'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec account_reconcile_cdr(kz_term:api_ne_binary(), j5_request:request()) -> 'ok'.
account_reconcile_cdr('undefined', _Request) ->
    lager:debug("no account id to reconcile cdr");
account_reconcile_cdr(AccountId, Request) ->
    lager:debug("reconciling cdr for account ~s", [AccountId]),
    Limits = j5_limits:get(AccountId),
    lager:debug("limits ~s : ~p", [AccountId, Limits]),
    reconcile_cdr(Request, j5_limits:get(AccountId)).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec reseller_reconcile_cdr(kz_term:api_ne_binary(), j5_request:request()) -> 'ok'.
reseller_reconcile_cdr('undefined', _Request) ->
    lager:debug("no account id to reconcile reseller cdr");
reseller_reconcile_cdr(AccountId, Request) ->
    case j5_request:reseller_id(Request) of
        'undefined' ->
            case kz_services_reseller:get_id(AccountId) of
                'undefined' ->
                    lager:debug("account id ~s has no reseller", [AccountId]);
                ResellerId ->
                    reconcile_cdr(Request, j5_limits:get(ResellerId))
            end;
        AccountId -> lager:debug("account id ~s is its reseller", [AccountId]);
        ResellerId -> reconcile_cdr(Request, j5_limits:get(ResellerId))
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec reconcile_cdr(j5_request:request(), j5_limits:limits()) -> 'ok'.
reconcile_cdr(Request, Limits) ->
    _ = j5_allotments:reconcile_cdr(Request, Limits),
    _ = j5_flat_rate:reconcile_cdr(Request, Limits),
    _ = j5_per_minute:reconcile_cdr(Request, Limits),
    'ok'.
