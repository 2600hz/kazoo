%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(j5_hard_limit).

-export([authorize/2]).
-export([reconcile_cdr/2]).

-include("jonny5.hrl").

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec authorize(j5_request:request(), j5_limits:limits()) -> j5_request:request().
authorize(Request, Limits) ->
    case calls_at_limit(Limits)
        orelse resource_consumption_at_limit(Limits)
        orelse inbound_channels_per_did_at_limit(Request, Limits)
    of
        'true' -> j5_request:deny(<<"hard_limit">>, Request, Limits);
        'false' -> Request
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec reconcile_cdr(j5_request:request(), j5_limits:limits()) -> 'ok'.
reconcile_cdr(_, _) -> 'ok'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec calls_at_limit(j5_limits:limits()) -> boolean().
calls_at_limit(Limits) ->
    Limit = j5_limits:calls(Limits),
    Used  = j5_channels:total_calls(j5_limits:account_id(Limits)),
    should_deny(Limit, Used).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec resource_consumption_at_limit(j5_limits:limits()) -> boolean().
resource_consumption_at_limit(Limits) ->
    Limit = j5_limits:resource_consuming_calls(Limits),
    Used  = j5_channels:resource_consuming(j5_limits:account_id(Limits)),
    should_deny(Limit, Used).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec inbound_channels_per_did_at_limit(j5_request:request(), j5_limits:limits()) -> boolean().
inbound_channels_per_did_at_limit(Request, Limits) ->
    AccountId = j5_limits:account_id(Limits),
    ToDID = j5_request:number(Request),
    PerDIDJObj = j5_limits:inbound_channels_per_did_rules(Limits),
    Limit = match_did_limits(ToDID, PerDIDJObj, kz_json:get_keys(PerDIDJObj)),
    Used  = j5_channels:total_inbound_channels_per_did_rules(ToDID, AccountId),
    lager:debug("inbound_channels_per_did_limit AccountId: ~p ToDid: ~p Used: ~p Limit: ~p"
               ,[AccountId ,ToDID ,Used ,Limit]
               ),
    should_deny(Limit, Used).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec should_deny(integer(), integer()) -> boolean().
should_deny(-1, _) -> 'false';
should_deny(0, _) -> 'true';
should_deny(Limit, Used) -> Used > Limit.

-spec match_did_limits(kz_term:ne_binary(), kz_json:object(), kz_json:keys()) -> integer().
match_did_limits(_ToDID, _PerDIDJObj, []) ->
    -1;
match_did_limits(ToDID, PerDIDJObj, [Key|Keys]) ->
    case re:run(ToDID, Key) of
        'nomatch' -> match_did_limits(ToDID, PerDIDJObj, Keys);
        _ ->  kz_json:get_integer_value(Key, PerDIDJObj)
    end.
