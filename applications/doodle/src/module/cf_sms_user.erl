%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2014, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Luis Azedo
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
handle(Data, Call1) ->
    UserId = wh_json:get_ne_value(<<"id">>, Data),
    Call = doodle_util:set_callee_id(UserId, Call1),
    Endpoints = get_endpoints(UserId, Data, Call),
    Strategy = wh_json:get_binary_value(<<"sms_strategy">>, Data, <<"single">>),
    case Endpoints =/= []
        andalso whapps_sms_command:b_send_sms(Endpoints, Strategy, Call)
    of
        'false' ->
            lager:notice("user ~s has no endpoints", [UserId]),
            doodle_exe:continue(doodle_util:set_flow_status(<<"pending">>, Call));
        {'ok', JObj} ->
            handle_result(JObj, Call);
        {'error', _R}=Reason ->
            lager:info("error bridging to user: ~p", [_R]),
            maybe_handle_bridge_failure(Reason, Call)
    end.

-spec handle_result(wh_json:object(), whapps_call:call()) -> 'ok'.
handle_result(JObj, Call1) ->
    Status = doodle_util:sms_status(JObj),
    Call = doodle_util:set_flow_status(Status, Call1),
    handle_result_status(Call, Status).

-spec handle_result_status(whapps_call:call(), ne_binary()) -> 'ok'.
handle_result_status(Call, <<"pending">>) ->
    doodle_exe:stop(Call);
handle_result_status(Call, _Status) ->
    lager:info("completed successful message to the user"),
    doodle_exe:continue(Call).

-spec maybe_handle_bridge_failure(_, whapps_call:call()) -> 'ok'.
maybe_handle_bridge_failure(Reason, Call) ->
    case doodle_util:handle_bridge_failure(Reason, Call) of
        'not_found' ->
            doodle_exe:stop(doodle_util:set_flow_status(<<"pending">>, Call));
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
                end
                ,[]
                ,cf_attributes:owned_by(UserId, <<"device">>, Call)
               ).
