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

-import(cf_call_command, [b_bridge/5, wait_for_unbridge/0]).

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
    Id = wh_json:get_value(<<"id">>, Data),
    ?LOG("loading endpoint ~s", [Id]),
    {ok, Endpoint} = cf_endpoint:build(Id, Call),
    Timeout = wh_json:get_value(<<"timeout">>, Data, ?DEFAULT_TIMEOUT),
    IgnoreEarlyMedia = wh_json:get_value(<<"Ignore-Early-Media">>, Endpoint),
    case b_bridge([Endpoint], Timeout, <<"single">>, IgnoreEarlyMedia, Call) of
        {ok, _} ->
            ?LOG("bridged to endpoint"),
            _ = wait_for_unbridge(),
            ?LOG("bridge completed"),
            CFPid ! { stop };
        {fail, Reason} ->
            {Cause, Code} = whapps_util:get_call_termination_reason(Reason),
            ?LOG("failed to bridge to endpoint ~s:~s", [Code, Cause]),
            CFPid ! { continue };
        {error, R} ->
            ?LOG("failed to bridge to endpoint ~w", [R]),
            CFPid ! { continue }
    end.
