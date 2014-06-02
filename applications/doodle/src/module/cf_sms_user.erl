%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2014, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(cf_sms_user).

-include("../doodle.hrl").

-export([handle/2
         ,get_endpoints/3
        ]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module, attempts to call an endpoint as defined
%% in the Data payload.  Returns continue if fails to connect or
%% stop when successfull.
%% @end
%%--------------------------------------------------------------------
-spec handle(wh_json:object(), whapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    UserId = wh_json:get_ne_value(<<"id">>, Data),
    Endpoints = get_endpoints(UserId, Data, Call),
    lager:debug("endpoints ~p",[Endpoints]),
    X = doodle_util:send_sms(Call, Endpoints),
    lager:debug("X = ~p",[X]),
    doodle_exe:continue(Call).

-spec maybe_handle_bridge_failure(_, whapps_call:call()) -> 'ok'.
maybe_handle_bridge_failure(Reason, Call) ->
    case cf_util:handle_bridge_failure(Reason, Call) of
        'not_found' -> cf_exe:continue(Call);
        'ok' -> 'ok'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Loop over the provided endpoints for the callflow and build the
%% json object used in the bridge API
%% @end
%%--------------------------------------------------------------------
-spec get_endpoints(api_binary(), wh_json:object(), whapps_call:call()) ->
                           wh_json:objects().
get_endpoints('undefined', _, _) -> [];
get_endpoints(UserId, Data, Call) ->
    Params = wh_json:set_value(<<"source">>, ?MODULE, Data),
    lists:foldr(fun(EndpointId, Acc) ->
                        case cf_endpoint:build(EndpointId, Params, Call) of
                            {'ok', Endpoint} -> Endpoint ++ Acc;
                            {'error', _E} -> Acc
                        end
                end, [], cf_attributes:owned_by(UserId, <<"device">>, Call)).