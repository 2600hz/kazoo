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
-spec authorize(j5_request:request(), j5_limits:limits()) -> j5_request:request().
authorize(Request, Limits) ->
    case calls_at_limit(Limits) 
        orelse resource_consumption_at_limit(Limits)
    of
        'true' -> j5_request:deny(<<"hard_limit">>, Request, Limits);
        'false' -> Request
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec reauthorize(j5_request:request(), j5_limits:limits()) -> j5_request:request().
reauthorize(Request, _) -> Request.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec reconcile_cdr(j5_request:request(), j5_limits:limits()) -> 'ok'.
reconcile_cdr(_, _) -> 'ok'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec calls_at_limit(j5_limits:limits()) -> j5_request:request().
calls_at_limit(Limits) ->
    Limit =  j5_limits:calls(Limits),
    Used = j5_channels:total_calls(j5_limits:account_id(Limits)),
    should_deny(Limit, Used).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec resource_consumption_at_limit(j5_limits:limits()) -> j5_request:request().
resource_consumption_at_limit(Limits) ->
    Limit =  j5_limits:resource_consuming_calls(Limits),
    Used = j5_channels:resource_consuming(j5_limits:account_id(Limits)),
    should_deny(Limit, Used).

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

             
    
