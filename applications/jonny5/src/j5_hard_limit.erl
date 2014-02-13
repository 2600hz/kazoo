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

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec authorize(j5_request(), j5_limits()) -> j5_request().
authorize(Request, Limits) ->
    Routines = [fun calls_at_limit/2
                ,fun resource_consumption_at_limit/2
               ],
    lists:foldl(fun(F, R) -> F(R, Limits) end, Request, Routines).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec reauthorize(j5_request(), j5_limits()) -> j5_request().
reauthorize(Request, _) -> Request.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec reconcile_cdr(j5_request(), j5_limits()) -> 'ok'.
reconcile_cdr(_, _) -> 'ok'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec calls_at_limit(j5_request(), j5_limits()) -> j5_request().
calls_at_limit(Request, Limits) ->
    Limit =  j5_limits:calls(Limits),
    Used = j5_channels:total_calls(j5_limits:account_id(Limits)),
    case should_deny(Limit, Used) of
        'true' -> j5_request:deny(<<"hard_limit">>, Request, Limits);
        'false' -> Request
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec resource_consumption_at_limit(j5_request(), j5_limits()) -> j5_request().
resource_consumption_at_limit(Request, Limits) ->
    Limit =  j5_limits:resource_consuming_calls(Limits),
    Used = j5_channels:resource_consuming(j5_limits:account_id(Limits)),
    case should_deny(Limit, Used) of
        'true' -> j5_request:deny(<<"hard_limit">>, Request, Limits);
        'false' -> Request
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec should_deny(integer(), integer()) -> boolean().
should_deny(-1, _) -> 'false';
should_deny(0, _) ->
    %% TODO: send a system_alert
    'true';
should_deny(Limit, Used) ->
    case Limit > Used of
        'true' ->            
            %% TODO: send a system_alert
            'true';
        'false' -> 'false'
    end.

             
    
