
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

-import(cf_call_command, [b_bridge/6]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module, attempts to call an endpoint as defined
%% in the Data payload.  Returns continue if fails to connect or
%% stop when successfull.
%% @end
%%--------------------------------------------------------------------
-spec handle/2 :: (Data, Call) -> {'stop' | 'continue'} when
      Data :: json_object(),
      Call :: #cf_call{}.
handle(Data, #cf_call{cf_pid=CFPid, call_id=CallId}=Call) ->
    put(callid, CallId),
    case cf_endpoint:build(wh_json:get_value(<<"id">>, Data), Call, Data) of
        {ok, Endpoints} ->
            Timeout = wh_json:get_binary_value(<<"timeout">>, Data, ?DEFAULT_TIMEOUT),
            bridge_to_endpoints(Endpoints, Timeout, Call);
        {error, _Reason} ->
            ?LOG("no endpoints to bridge to, ~p", [_Reason]),
            CFPid ! { continue }
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempts to bridge to the endpoints created to reach this device
%% @end
%%--------------------------------------------------------------------
-spec bridge_to_endpoints/3 :: (Endpoints, Timeout, Call) -> {'stop' | 'continue'} when
      Endpoints :: json_objects(),
      Timeout :: binary(),
      Call :: #cf_call{}.
bridge_to_endpoints(Endpoints, Timeout, #cf_call{cf_pid=CFPid}=Call) ->
    IgnoreEarlyMedia = cf_util:ignore_early_media(Endpoints),
    case b_bridge(Endpoints, Timeout, <<"internal">>, <<"simultaneous">>, IgnoreEarlyMedia, Call) of
        {ok, _} ->
            ?LOG("completed successful bridge to the device"),
            CFPid ! { stop };
        {fail, Reason} ->
            {Cause, Code} = whapps_util:get_call_termination_reason(Reason),
            ?LOG("failed to bridge to endpoint ~s:~s", [Code, Cause]),
            CFPid ! { continue };
        {error, R} ->
            ?LOG("failed to bridge to endpoint ~p", [R]),
            CFPid ! { continue }
    end.
