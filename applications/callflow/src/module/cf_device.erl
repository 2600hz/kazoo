%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2014, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(cf_device).

-include("callflow.hrl").

-export([handle/2]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module, attempts to call an endpoint as defined
%% in the Data payload.  Returns continue if fails to connect or
%% stop when successfull.
%% @end
%%--------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    case bridge_to_endpoints(Data, Call) of
        {'ok', _} ->
            lager:info("completed successful bridge to the device"),
            cf_exe:stop(Call);
        {'fail', _}=Reason -> maybe_handle_bridge_failure(Reason, Call);
        {'error', _R} ->
            lager:info("error bridging to device: ~s"
                       ,[kz_json:get_value(<<"Error-Message">>, _R)]
                      ),
            cf_exe:continue(Call)
    end.

-spec maybe_handle_bridge_failure(any(), kapps_call:call()) -> 'ok'.
maybe_handle_bridge_failure(Reason, Call) ->
    case cf_util:handle_bridge_failure(Reason, Call) of
        'not_found' -> cf_exe:continue(Call);
        'ok' -> 'ok'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempts to bridge to the endpoints created to reach this device
%% @end
%%--------------------------------------------------------------------
-spec bridge_to_endpoints(kz_json:object(), kapps_call:call()) ->
                                 cf_api_bridge_return().
bridge_to_endpoints(Data, Call) ->
    EndpointId = kz_doc:id(Data),
    Params = kz_json:set_value(<<"source">>, ?MODULE, Data),
    case cf_endpoint:build(EndpointId, Params, Call) of
        {'error', _}=E -> E;
        {'ok', Endpoints} ->
            FailOnSingleReject = kz_json:get_value(<<"fail_on_single_reject">>, Data, 'undefined'),
            Timeout = kz_json:get_integer_value(<<"timeout">>, Data, ?DEFAULT_TIMEOUT_S),
            IgnoreEarlyMedia = cf_util:ignore_early_media(Endpoints),
            Command = [{<<"Application-Name">>, <<"bridge">>}
                ,{<<"Endpoints">>, Endpoints}
                ,{<<"Timeout">>, Timeout}
                ,{<<"Ignore-Early-Media">>, IgnoreEarlyMedia}
                ,{<<"Fail-On-Single-Reject">>, FailOnSingleReject}
                ,{<<"Dial-Endpoint-Method">>, <<"simultaneous">>}
                ,{<<"Ignore-Forward">>, <<"false">>}
            ],
            kapps_call_command:send_command(Command, Call),
            kapps_call_command:b_bridge_wait(Timeout, Call)
    end.
