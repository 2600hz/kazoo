
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

-import(cf_call_command, [b_bridge/6, wait_for_callee_release/1]).

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
            Timeout = wh_json:get_binary_value(<<"timeout">>, Data, ?DEFAULT_TIMEOUT),
            case bridge_to_endpoints(Endpoints, Timeout, Call) of
                {fail, _} ->
                    ?LOG("could not reach endpoint"),
                    CFPid ! { continue };
                {transfer, _} ->
                    ?LOG("endpoint was transferred"),
                    CFPid ! { transferred };
                _ ->
                    ?LOG("endpoint was unbridged"),
                    CFPid ! { stop }
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
-spec bridge_to_endpoints/3 :: (Endpoints, Timeout, Call) -> cf_api_bridge_return() when
      Endpoints :: json_objects(),
      Timeout :: binary(),
      Call :: #cf_call{}.
bridge_to_endpoints(Endpoints, Timeout, Call) ->
    IgnoreEarlyMedia = ignore_early_media(Endpoints),
    case b_bridge(Endpoints, Timeout, <<"internal">>, <<"simultaneous">>, IgnoreEarlyMedia, Call) of
        {ok, _} ->
            ?LOG("bridged to endpoint"),
            wait_for_callee_release(Call);
        {fail, Reason}=Fail ->
            {Cause, Code} = whapps_util:get_call_termination_reason(Reason),
            ?LOG("failed to bridge to endpoint ~s:~s", [Code, Cause]),
            Fail;
        {error, R}=Error ->
            ?LOG("failed to bridge to endpoint ~w", [R]),
            Error
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Determine if we should ignore early media
%% @end
%%--------------------------------------------------------------------
-spec ignore_early_media/1 :: (Endpoints) -> binary() when
      Endpoints :: json_objects().
ignore_early_media(Endpoints) ->
    Ignore = lists:foldr(fun(Endpoint, Acc) ->
                                 wh_json:is_true(<<"Ignore-Early-Media">>, Endpoint)
                                     or Acc
                         end, false, Endpoints),
    wh_util:to_binary(Ignore).
