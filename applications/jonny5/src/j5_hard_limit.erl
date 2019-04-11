%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
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
        orelse resource_consumption_at_limit(Limits, Request)
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
-spec resource_consumption_at_limit(j5_limits:limits(), j5_request:request()) -> boolean().
resource_consumption_at_limit(Limits, Request) ->
    AccountBilling = j5_request:account_billing(Request),
    Increment = case  AccountBilling =/= 'undefined' 
           andalso AccountBilling =/= <<"limits_disabled">> 
    of
        'true' -> 1;
        'false' -> 0
    end,
    Limit = j5_limits:resource_consuming_calls(Limits),
    Used  = j5_channels:resource_consuming(j5_limits:account_id(Limits)),
    should_deny(Limit, Used + Increment).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec should_deny(integer(), integer()) -> boolean().
should_deny(-1, _) -> 'false';
should_deny(0, _) -> 'true';
should_deny(Limit, Used) -> Used > Limit.
