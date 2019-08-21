%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc
%%% @author Luis Azedo
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cf_sms_user).

-include("doodle.hrl").

-export([handle/2
        ,get_endpoints/3
        ]).

%%------------------------------------------------------------------------------
%% @doc Entry point for this module, attempts to call an endpoint as defined
%% in the Data payload.  Returns continue if fails to connect or
%% stop when successful.
%% @end
%%------------------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(Data, Call1) ->
    UserId = kz_doc:id(Data),
    Funs = [{fun doodle_util:set_callee_id/2, UserId}
           ,{fun kapps_call:kvs_store/3, <<"target_owner_id">>, UserId}
           ],
    Call = kapps_call:exec(Funs, Call1),
    {Endpoints, Dnd} = get_endpoints(UserId, Data, Call),
    Strategy = kz_json:get_binary_value(<<"sms_strategy">>, Data, <<"single">>),
    case Endpoints =/= []
        andalso kapps_sms_command:b_send_sms(Endpoints, Strategy, Call)
    of
        'false' when Dnd =:= 0 ->
            lager:notice("user ~s has no endpoints", [UserId]),
            doodle_exe:continue(doodle_util:set_flow_error(<<"error">>, <<"user has no endpoints">>, Call));
        'false' when Dnd > 0 ->
            lager:notice("do not disturb user ~s", [UserId]),
            maybe_handle_bridge_failure({'error', 'do_not_disturb'}, Call);
        {'ok', JObj} ->
            handle_result(JObj, Call);
        {'error', _R}=Reason ->
            lager:info("error bridging to user: ~p", [_R]),
            maybe_handle_bridge_failure(Reason, Call)
    end.

-spec handle_result(kz_json:object(), kapps_call:call()) -> 'ok'.
handle_result(JObj, Call1) ->
    Status = doodle_util:sms_status(JObj),
    Call = doodle_util:set_flow_status(Status, Call1),
    handle_result_status(Call, Status).

-spec handle_result_status(kapps_call:call(), kz_term:ne_binary()) -> 'ok'.
handle_result_status(Call, <<"pending">>) ->
    doodle_util:maybe_reschedule_sms(Call);
handle_result_status(Call, _Status) ->
    lager:info("completed successful message to the user"),
    doodle_exe:stop(Call).

-spec maybe_handle_bridge_failure(any(), kapps_call:call()) -> 'ok'.
maybe_handle_bridge_failure({_ , R}=Reason, Call) ->
    case doodle_util:handle_bridge_failure(Reason, Call) of
        'not_found' ->
            doodle_util:maybe_reschedule_sms(
              doodle_util:set_flow_status(<<"pending">>, kz_term:to_binary(R), Call)
             );
        'ok' -> 'ok'
    end.

%%------------------------------------------------------------------------------
%% @doc Loop over the provided endpoints for the callflow and build the
%% json object used in the bridge API
%% Send to endpoint in determined order
%% @end
%%------------------------------------------------------------------------------
-spec get_endpoints(kz_term:api_binary(), kz_json:object(), kapps_call:call()) ->
                           {kz_json:objects(), non_neg_integer()}.
get_endpoints('undefined', _, _) -> {[], 0};
get_endpoints(UserId, Data, Call) ->
    Params = kz_json:set_value(<<"source">>, kz_term:to_binary(?MODULE), Data),
    EndpointIds = kz_attributes:owned_by(UserId, <<"device">>, Call),
    {Endpoints, DndCount} = lists:foldr(fun(EndpointId, {Acc, Dnd}) ->
                                                case kz_endpoint:build(EndpointId, Params, Call) of
                                                    {'ok', Endpoint} -> {Endpoint ++ Acc, Dnd};
                                                    {'error', 'do_not_disturb'} -> {Acc, Dnd+1};
                                                    {'error', _E} -> {Acc, Dnd}
                                                end
                                        end
                                       ,{[], 0}
                                       ,EndpointIds
                                       ),
    SortedEndpoints = sort_endpoints_by_type(Endpoints),
    {SortedEndpoints, DndCount}.

-spec sort_endpoints_by_type(kz_json:objects()) -> kz_json:objects().
sort_endpoints_by_type(Endpoints) ->
    lists:sort(fun(EndpointA, EndpointB) ->
                       EndpointAValue = endpoint_type_sort_value(kz_json:get_value(<<"Endpoint-Type">>, EndpointA)),
                       EndpointBValue = endpoint_type_sort_value(kz_json:get_value(<<"Endpoint-Type">>, EndpointB)),
                       (EndpointAValue < EndpointBValue)
               end,
               Endpoints
              ).

-spec endpoint_type_sort_value(binary()) -> 0..1.
endpoint_type_sort_value(<<"amqp">>) -> 0;
endpoint_type_sort_value(_Type) -> 1.
