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

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module, attempts to call an endpoint as defined
%% in the Data payload.  Returns continue if fails to connect or
%% stop when successfull.
%% @end
%%--------------------------------------------------------------------
-spec handle/2 :: (json_object(), #cf_call{}) -> 'ok'.
handle(Data, Call) ->
    case bridge_to_endpoints(Data, Call) of
        {ok, _} ->
            ?LOG("completed successful bridge to the device"),
            cf_exe:stop(Call);
        {error, _}=E ->
            ?CF_ALERT(E, "error bridging to device", Call),
            cf_exe:continue(Call);
        {fail, _}=F ->
            ?CF_ALERT(F, Call),
            cf_util:handle_bridge_failure(F, Call)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempts to bridge to the endpoints created to reach this device
%% @end
%%--------------------------------------------------------------------
-spec bridge_to_endpoints/2 :: (json_object(), #cf_call{}) -> cf_api_bridge_return().
bridge_to_endpoints(Data, Call) ->
    EndpointId = wh_json:get_value(<<"id">>, Data),
    Params = wh_json:set_value(<<"source">>, ?MODULE, Data),
    case cf_endpoint:build(EndpointId, Params, Call) of
        {error, _}=E ->
            E;
        {ok, Endpoints} ->
            Timeout = wh_json:get_binary_value(<<"timeout">>, Data, ?DEFAULT_TIMEOUT),
            IgnoreEarlyMedia = cf_util:ignore_early_media(Endpoints),
            cf_call_command:b_bridge(Endpoints, Timeout, <<"simultaneous">>, IgnoreEarlyMedia, Call)
    end.
