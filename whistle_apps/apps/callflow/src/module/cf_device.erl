
%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%% @end
%%% Created : 22 Feb 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(cf_device).

-include("../callflow.hrl").

-export([handle/2]).

-import(cf_call_command, [b_bridge/6, wait_for_unbridge/0]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module, attempts to call an endpoint as defined
%% in the Data payload.  Returns continue if fails to connect or
%% stop when successfull.
%% @end
%%--------------------------------------------------------------------
-spec(handle/2 :: (Data :: json_object(), Call :: #cf_call{}) -> no_return()).
handle(Data, #cf_call{cf_pid=CFPid, call_id=CallId}=Call) ->
    put(callid, CallId),
    EndpointId = wh_json:get_value(<<"id">>, Data),
    case cf_endpoint:build(EndpointId, Data, Call) of
        {ok, Endpoints} ->
            Timeout = wh_json:get_value(<<"timeout">>, Data, ?DEFAULT_TIMEOUT),
            IgnoreEarlyMedia = lists:foldr(fun(Endpoint, Acc) ->
                                                   whistle_util:is_true(wh_json:get_value(<<"Ignore-Early-Media">>, Endpoint)) or Acc
                                           end, false, Endpoints),
            case bridge_to_endpoints(Endpoints, Timeout, IgnoreEarlyMedia, Call) of
                {ok, complete} ->
                    CFPid ! { stop };
                _ ->
                    CFPid ! { continue }
            end;
        {error, Reason} ->
            ?LOG("no endpoints to bridge to, ~w", [Reason]),
            CFPid ! { continue }
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempts to bridge to the endpoints created to reach this device
%% @end
%%--------------------------------------------------------------------
-spec(bridge_to_endpoints/4 :: (Endpoints :: json_object(), Timeout :: binary(), IgnoreEarlyMedia :: binary(), Call :: #cf_call{})
                               -> tuple(ok, complete) | tuple(fail, json_object()) | tuple(error, atom())).
bridge_to_endpoints(Endpoints, Timeout, IgnoreEarlyMedia, Call) ->
    case b_bridge(Endpoints, Timeout, <<"internal">>, <<"single">>, IgnoreEarlyMedia, Call) of
        {ok, _} ->
            ?LOG("bridged to endpoint"),
            _ = wait_for_unbridge(),
            ?LOG("bridge completed"),
            {ok, complete};
        {fail, Reason}=Fail ->
            {Cause, Code} = whapps_util:get_call_termination_reason(Reason),
            ?LOG("failed to bridge to endpoint ~s:~s", [Code, Cause]),
            Fail;
        {error, R}=Error ->
            ?LOG("failed to bridge to endpoint ~w", [R]),
            Error
    end.
