%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2014, 2600hz, INC
%%% @doc
%%% Eacesdrop feature code
%%%
%%% data: {
%%%   "group_id":"_group_id_"
%%%   ,"approved_device_id":"_user_id_"
%%%   ,"appoved_user_id":"_device_id_"
%%%   ,"approved_group_id":"_group_id_"
%%% }
%%%
%%% group_id defines list of eavesdrop's targets. If group_id is
%%% undefuned then anybody can be eavesdroped.
%%% One of the approved_device_id, appoved_user_id, approved_group_id
%%% must be defined to access feature code.
%%%
%%% @end
%%% @contributors
%%%   SIPLABS LLC (Maksim Krzhemenevskiy)
%%%-------------------------------------------------------------------

-module(cf_eavesdrop_feature).

-include("../callflow.hrl").

-export([handle/2]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module sends an arbitrary response back to the
%% call originator.
%% @end
%%--------------------------------------------------------------------
-spec handle(wh_json:object(), whapps_call:call()) -> any().
handle(Data, Call) ->
    case maybe_approved_caller(Data, Call) of
        'true' -> 'ok';
        'false' -> no_permission_to_eavesdrop(Call)
    end,
    cf_exe:stop(Call).

-spec maybe_approved_caller(wh_json:object(), whapps_call:call()) -> boolean().
maybe_approved_caller(Data, Call) ->
    case wh_json:get_value(<<"approved_device_id">>, Data) of
        'undefined' ->
            case wh_json:get_value(<<"approved_user_id">>, Data) of
                'undefined' ->
                    case wh_json:get_value(<<"approved_group_id">>, Data) of
                        'undefined' -> 'false';
                        GroupId -> cf_util:caller_belongs_to_group(GroupId, Call)
                    end;
                UserId -> cf_util:caller_belongs_to_user(UserId, Call)
            end;
        DeviceId ->
            % Compare approved device_id with calling one
            DeviceId == whapps_call:authorizing_id(Call)
    end.

-spec no_permission_to_eavesdrop(whapps_call:call()) -> any().
%% TODO: please convert to system_media file (say is not consistent on deployments)
no_permission_to_eavesdrop(Call) ->
    whapps_call_command:answer(Call),
    whapps_call_command:b_say(<<"you have no permission to eavesdrop this call">>, Call).
