%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(j5_hard_limit).

-export([authorize/2]).
-export([reauthorize/2]).
-export([reconcile_cdr/2]).

-include("jonny5.hrl").

-spec authorize(j5_request(), j5_limits()) -> j5_request().
authorize(Request, Limits) ->
    Routines = [fun calls_at_limit/2
                ,fun resource_consumption_at_limit/2
               ],
    lists:foldl(fun(F, R) -> F(R, Limits) end, Request, Routines).

-spec reauthorize(j5_request(), j5_limits()) -> j5_request().
reauthorize(Request, _) -> Request.

-spec reconcile_cdr(j5_request(), j5_limits()) -> 'ok'.
reconcile_cdr(_, _) -> 'ok'.

-spec calls_at_limit(j5_request(), j5_limits()) -> j5_request().
calls_at_limit(Request, #limits{calls=-1}) -> Request;
calls_at_limit(Request, #limits{calls=0}=Limits) ->
    %% TODO: send a system_alert
    j5_request:deny(<<"hard_limit">>, Request, Limits);
calls_at_limit(Request, #limits{calls=Resources
                                ,account_id=AccountId}=Limits) ->
    case Resources - j5_channels:total_calls(AccountId) =< 0 of
        'true' -> Request;
        'false' ->
            %% TODO: send a system_alert
            j5_request:deny(<<"hard_limit">>, Request, Limits)
    end.

-spec resource_consumption_at_limit(j5_request(), j5_limits()) -> j5_request().
resource_consumption_at_limit(Request, #limits{resource_consuming_calls=-1}) -> Request;
resource_consumption_at_limit(Request, #limits{resource_consuming_calls=0}=Limits) ->
    %% TODO: send a system_alert
    j5_request:deny(<<"hard_limit">>, Request, Limits);
resource_consumption_at_limit(Request
                              ,#limits{resource_consuming_calls=Resources
                                       ,account_id=AccountId}=Limits) ->
    case Resources - j5_channels:resource_consuming(AccountId) =< 0 of
        'true' -> Request;
        'false' ->
            %% TODO: send a system_alert
            j5_request:deny(<<"hard_limit">>, Request, Limits)
    end.
