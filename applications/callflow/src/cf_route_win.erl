%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc Handler for route wins, bootstraps callflow execution.
%%% @author Karl Anderson
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cf_route_win).

-export([execute_callflow/2]).

-include("callflow.hrl").

-define(JSON(L), kz_json:from_list(L)).

-define(DEFAULT_SERVICES
       ,?JSON([{<<"audio">>, ?JSON([{<<"enabled">>, 'true'}])}
              ,{<<"video">>, ?JSON([{<<"enabled">>, 'true'}])}
              ,{<<"sms">>, ?JSON([{<<"enabled">>, 'true'}])}
              ]
             )
       ).

-spec execute_callflow(kz_json:object(), kapps_call:call()) ->
          {'ok' | 'restricted', kapps_call:call()}.
execute_callflow(JObj, Call) ->
    case should_restrict_call(Call) of
        'true' ->
            lager:debug("endpoint is restricted from making this call, terminate", []),
            _ = kapps_call_command:answer(Call),
            _ = kapps_call_command:prompt(<<"cf-unauthorized_call">>, Call),
            _ = kapps_call_command:queued_hangup(Call),
            {'restricted', Call};
        'false' ->
            lager:info("setting initial information about the call"),
            {'ok', bootstrap_callflow_executer(JObj, Call)}
    end.

-spec should_restrict_call(kapps_call:call()) -> boolean().
should_restrict_call(Call) ->
    should_restrict_call(get_endpoint_id(Call), Call).

-spec should_restrict_call(kz_term:api_ne_binary(), kapps_call:call()) -> boolean().
should_restrict_call('undefined', _Call) -> 'false';
should_restrict_call(EndpointId, Call) ->
    case kz_endpoint:get(EndpointId, Call) of
        {'error', _R} -> 'false';
        {'ok', JObj} -> maybe_service_unavailable(JObj, Call)
    end.

-spec maybe_service_unavailable(kz_json:object(), kapps_call:call()) -> boolean().
maybe_service_unavailable(JObj, Call) ->
    Id = kz_doc:id(JObj),
    Services = get_services(JObj),
    case kz_json:is_true([<<"audio">>,<<"enabled">>], Services, 'true') of
        'true' ->
            maybe_account_service_unavailable(JObj, Call);
        'false' ->
            lager:debug("device ~s does not have audio service enabled", [Id]),
            'true'
    end.

-spec get_services(kz_json:object()) -> kz_json:object().
get_services(JObj) ->
    kz_json:merge(kz_json:get_json_value(<<"services">>, JObj, ?DEFAULT_SERVICES)
                 ,kz_json:get_json_value(<<"pvt_services">>, JObj, kz_json:new())
                 ).

-spec maybe_account_service_unavailable(kz_json:object(), kapps_call:call()) -> boolean().
maybe_account_service_unavailable(JObj, Call) ->
    AccountId = kapps_call:account_id(Call),
    {'ok', Doc} = kzd_accounts:fetch(AccountId),
    Services = get_services(Doc),

    case kz_json:is_true([<<"audio">>,<<"enabled">>], Services, 'true') of
        'true' ->
            maybe_closed_group_restriction(JObj, Call);
        'false' ->
            lager:debug("account ~s does not have audio service enabled", [AccountId]),
            'true'
    end.

-spec maybe_closed_group_restriction(kz_json:object(), kapps_call:call()) ->
          boolean().
maybe_closed_group_restriction(JObj, Call) ->
    case kz_json:get_value([<<"call_restriction">>, <<"closed_groups">>, <<"action">>], JObj) of
        <<"deny">> -> enforce_closed_groups(JObj, Call);
        _Else -> maybe_classification_restriction(JObj, Call)
    end.

-spec maybe_classification_restriction(kz_json:object(), kapps_call:call()) ->
          boolean().
maybe_classification_restriction(JObj, Call) ->
    Request = find_request(Call),
    AccountId = kapps_call:account_id(Call),
    DialPlan = kz_json:get_json_value(<<"dial_plan">>, JObj, kz_json:new()),
    Number = knm_converters:normalize(Request, AccountId, DialPlan),
    Classification = knm_converters:classify(Number),
    lager:debug("classified number ~s as ~s, testing for call restrictions"
               ,[Number, Classification]
               ),
    kz_json:get_value([<<"call_restriction">>, Classification, <<"action">>], JObj) =:= <<"deny">>.

-spec find_request(kapps_call:call()) -> kz_term:ne_binary().
find_request(Call) ->
    case kapps_call:kvs_fetch('cf_capture_group', Call) of
        'undefined' ->
            kapps_call:request_user(Call);
        CaptureGroup ->
            lager:debug("capture group ~s being used instead of request ~s"
                       ,[CaptureGroup, kapps_call:request_user(Call)]
                       ),
            CaptureGroup
    end.

-spec enforce_closed_groups(kz_json:object(), kapps_call:call()) -> boolean().
enforce_closed_groups(JObj, Call) ->
    case get_callee_extension_info(Call) of
        'undefined' ->
            lager:info("dialed number is not an extension, using classification restrictions", []),
            maybe_classification_restriction(JObj, Call);
        {<<"user">>, CalleeId} ->
            lager:info("dialed number is user ~s extension, checking groups", [CalleeId]),
            Groups = kz_attributes:groups(Call),
            CallerGroups = get_caller_groups(Groups, JObj, Call),
            CalleeGroups = get_group_associations(CalleeId, Groups),
            sets:size(sets:intersection(CallerGroups, CalleeGroups)) =:= 0;
        {<<"device">>, CalleeId} ->
            lager:info("dialed number is device ~s extension, checking groups", [CalleeId]),
            Groups = kz_attributes:groups(Call),
            CallerGroups = get_caller_groups(Groups, JObj, Call),
            maybe_device_groups_intersect(CalleeId, CallerGroups, Groups, Call)
    end.

-spec get_caller_groups(kz_json:objects(), kz_json:object(), kapps_call:call()) -> sets:set().
get_caller_groups(Groups, JObj, Call) ->
    Ids = [kapps_call:authorizing_id(Call)
          ,kz_json:get_ne_binary_value(<<"owner_id">>, JObj)
           | kz_json:get_keys([<<"hotdesk">>, <<"users">>], JObj)
          ],
    lists:foldl(fun('undefined', Set) -> Set;
                   (Id, Set) -> get_group_associations(Id, Groups, Set)
                end
               ,sets:new()
               ,Ids
               ).

-spec maybe_device_groups_intersect(kz_term:ne_binary(), sets:set(), kz_json:objects(), kapps_call:call()) -> boolean().
maybe_device_groups_intersect(CalleeId, CallerGroups, Groups, Call) ->
    CalleeGroups = get_group_associations(CalleeId, Groups),
    case sets:size(sets:intersection(CallerGroups, CalleeGroups)) =:= 0 of
        'false' -> 'false';
        'true' ->
            %% In this case the callee-id is a device id, find out if
            %% the owner of the device shares any groups with the caller
            UserIds = kz_attributes:owner_ids(CalleeId, Call),
            UsersGroups = lists:foldl(fun(UserId, Set) ->
                                              get_group_associations(UserId, Groups, Set)
                                      end
                                     ,sets:new()
                                     ,UserIds
                                     ),
            sets:size(sets:intersection(CallerGroups, UsersGroups)) =:= 0
    end.

-spec get_group_associations(kz_term:ne_binary(), kz_json:objects()) -> sets:set().
get_group_associations(Id, Groups) ->
    get_group_associations(Id, Groups, sets:new()).

-spec get_group_associations(kz_term:ne_binary(), kz_json:objects(), sets:set()) -> sets:set().
get_group_associations(Id, Groups, Set) ->
    lists:foldl(fun(Group, S) ->
                        case kz_json:get_value([<<"value">>, Id], Group) of
                            'undefined' -> S;
                            _Else -> sets:add_element(kz_doc:id(Group), S)
                        end
                end, Set, Groups).

-spec get_callee_extension_info(kapps_call:call()) -> {kz_term:ne_binary(), kz_term:ne_binary()} | 'undefined'.
get_callee_extension_info(Call) ->
    Flow = kapps_call:kvs_fetch('cf_flow', Call),
    FirstModule = kz_json:get_ne_binary_value(<<"module">>, Flow),
    FirstId = kz_json:get_ne_binary_value([<<"data">>, <<"id">>], Flow),
    SecondModule = kz_json:get_ne_binary_value([?DEFAULT_CHILD_KEY, <<"module">>], Flow),
    case (FirstModule =:= <<"device">>
              orelse FirstModule =:= <<"user">>
         )
        andalso (SecondModule =:= <<"voicemail">>
                     orelse SecondModule =:= 'undefined'
                )
        andalso FirstId =/= 'undefined'
    of
        'true' -> {FirstModule, FirstId};
        'false' -> 'undefined'
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec bootstrap_callflow_executer(kz_json:object(), kapps_call:call()) -> kapps_call:call().
bootstrap_callflow_executer(_JObj, Call) ->
    Routines = [fun store_owner_id/1
               ,fun set_language/1
               ,fun include_denied_call_restrictions/1
               ,fun maybe_start_recording/1
               ,fun execute_callflow/1
               ,fun maybe_start_metaflow/1
               ],
    kapps_call:exec(Routines, Call).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec store_owner_id(kapps_call:call()) -> kapps_call:call().
store_owner_id(Call) ->
    OwnerId = kz_attributes:owner_id(Call),
    kapps_call:kvs_store('owner_id', OwnerId, Call).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_language(kapps_call:call()) -> kapps_call:call().
set_language(Call) ->
    Default = kz_media_util:prompt_language(kapps_call:account_id(Call)),
    case kz_endpoint:get(Call) of
        {'ok', Endpoint} ->
            Language = kzd_devices:language(Endpoint, Default),
            lager:debug("setting language '~s' for this call", [Language]),
            kapps_call:set_language(kz_term:to_lower_binary(Language), Call);
        {'error', _E} ->
            lager:debug("no source endpoint for this call, setting language to default ~s", [Default]),
            kapps_call:set_language(Default, Call)
    end.

-spec maybe_start_metaflow(kapps_call:call()) -> kapps_call:call().
maybe_start_metaflow(Call) ->
    maybe_start_metaflow(Call, kapps_call:custom_channel_var(<<"Metaflow-App">>, Call)).

-spec maybe_start_metaflow(kapps_call:call(), kz_term:api_binary()) -> kapps_call:call().
maybe_start_metaflow(Call, 'undefined') ->
    maybe_start_endpoint_metaflow(Call, kapps_call:authorizing_id(Call)),
    Call;
maybe_start_metaflow(Call, App) ->
    lager:debug("metaflow app ~s", [App]),
    Call.

-spec maybe_start_endpoint_metaflow(kapps_call:call(), kz_term:api_binary()) -> 'ok'.
maybe_start_endpoint_metaflow(_Call, 'undefined') -> 'ok';
maybe_start_endpoint_metaflow(Call, EndpointId) ->
    lager:debug("looking up endpoint for ~s", [EndpointId]),
    case kz_endpoint:get(EndpointId, Call) of
        {'ok', Endpoint} ->
            lager:debug("trying to send metaflow for a-leg endpoint ~s", [EndpointId]),
            kz_endpoint:maybe_start_metaflow(Call, Endpoint);
        {'error', _E} -> 'ok'
    end.

-spec maybe_start_recording(kapps_call:call()) -> kapps_call:call().
maybe_start_recording(Call) ->
    FromNetwork = kapps_call:inception_type(Call), % onnet or offnet
    ToNetwork = case kapps_call:kvs_fetch('cf_no_match', Call) of
                    'true' -> <<"offnet">>;
                    _ -> <<"onnet">>
                end,
    Routines = [{fun maybe_start_account_recording/3, FromNetwork, ToNetwork}
               ,{fun maybe_start_endpoint_recording/3, FromNetwork, ToNetwork}
               ],
    kapps_call:exec(Routines, Call).

-spec maybe_start_account_recording(kz_term:ne_binary(), kz_term:ne_binary(), kapps_call:call()) -> kapps_call:call().
maybe_start_account_recording(FromNetwork, ToNetwork, Call) ->
    {'ok', Endpoint} = kz_endpoint:get(kapps_call:account_id(Call), Call),

    case kz_account_recording:maybe_record_inbound(FromNetwork, Endpoint, Call) of
        {'true', NewCall} -> NewCall;
        'false' ->
            case kz_account_recording:maybe_record_outbound(ToNetwork, Endpoint, Call) of
                'false' -> Call;
                {'true', NewCall} -> NewCall
            end
    end.

-spec maybe_start_endpoint_recording(kz_term:ne_binary(), kz_term:ne_binary(), kapps_call:call()) ->
          kapps_call:call().
maybe_start_endpoint_recording(<<"onnet">>, ToNetwork, Call) ->
    EndpointId = get_endpoint_id(Call),

    IsCallForward = kapps_call:is_call_forward(Call),
    case maybe_start_onnet_endpoint_recording(EndpointId, ToNetwork, IsCallForward, Call) of
        'false' -> Call;
        {'true', NewCall} -> NewCall
    end;
maybe_start_endpoint_recording(<<"offnet">>, ToNetwork, Call) ->
    EndpointId = get_endpoint_id(Call),

    IsCallForward = kapps_call:is_call_forward(Call),
    case maybe_start_offnet_endpoint_recording(EndpointId, ToNetwork, IsCallForward, Call) of
        'false' -> Call;
        {'true', NewCall} -> NewCall
    end.

-spec maybe_start_onnet_endpoint_recording(kz_term:api_binary(), kz_term:ne_binary(), boolean(), kapps_call:call()) ->
          {'true', kapps_call:call()} | 'false'.
maybe_start_onnet_endpoint_recording('undefined', _ToNetwork, _IsCallForward, _Call) -> 'false';
maybe_start_onnet_endpoint_recording(EndpointId, ToNetwork, 'false', Call) ->
    case kz_endpoint:get(EndpointId, Call) of
        {'ok', Endpoint} ->
            kz_endpoint_recording:maybe_record_outbound(ToNetwork, Endpoint, Call);
        _ -> 'false'
    end;
maybe_start_onnet_endpoint_recording(EndpointId, _ToNetwork, 'true', Call) ->
    maybe_start_call_forwarded_recording(EndpointId, Call, kz_endpoint:get(EndpointId, Call)).

%% @doc if the call isn't call-fowarded, and the endpoint is known, kz_endpoint will setup recording
%% on answer
-spec maybe_start_offnet_endpoint_recording(kz_term:api_binary(), kz_term:ne_binary(), boolean(), kapps_call:call()) ->
          {'true', kapps_call:call()} | 'false'.
maybe_start_offnet_endpoint_recording('undefined', _ToNetwork, _IsCallForward, _Call) -> 'false';
maybe_start_offnet_endpoint_recording(_EndpointId, _ToNetwork, 'false', _Call) -> 'false';
maybe_start_offnet_endpoint_recording(EndpointId, _ToNetwork, 'true', Call) ->
    maybe_start_call_forwarded_recording(EndpointId, Call, kz_endpoint:get(EndpointId, Call)).

-spec maybe_start_call_forwarded_recording(kz_term:ne_binary(), kapps_call:call(), {'ok', kz_json:object()} | {'error', any()}) ->
          {'true', kapps_call:call()} | 'false'.
maybe_start_call_forwarded_recording(_EndpointId, _Call, {'error', _E}) -> 'false';
maybe_start_call_forwarded_recording(_EndpointId, Call, {'ok', Endpoint}) ->
    FromNetwork = kapps_call:custom_channel_var(<<"Call-Forward-From">>, Call),
    case kz_endpoint_recording:maybe_record_inbound(FromNetwork, Endpoint, Call) of
        'false' -> 'false';
        {'true', {ActionKey, ActionApp}} ->
            NewActions = kz_json:set_value(ActionKey, ActionApp, kz_json:new()),
            {'true', kapps_call:kvs_store('outbound_actions', NewActions, Call)}
    end.

-spec get_endpoint_id(kapps_call:call()) ->kz_term:api_ne_binary().
get_endpoint_id(Call) ->
    DefaultEndpointId = kapps_call:authorizing_id(Call),
    kapps_call:kvs_fetch(?RESTRICTED_ENDPOINT_KEY, DefaultEndpointId, Call).

-spec include_denied_call_restrictions(kapps_call:call()) -> kapps_call:call().
include_denied_call_restrictions(Call) ->
    case kz_endpoint:get(get_endpoint_id(Call), Call) of
        {'error', _R} ->
            Call;
        {'ok', JObj} ->
            CallRestriction = kz_json:get_json_value(<<"call_restriction">>, JObj, kz_json:new()),
            Denied = kz_json:filter(fun filter_action/1, CallRestriction),
            kapps_call:kvs_store('denied_call_restrictions', Denied, Call)
    end.

-spec filter_action({any(), kz_json:object()}) -> boolean().
filter_action({_, Action}) ->
    <<"deny">> =:= kz_json:get_ne_binary_value(<<"action">>, Action).

%%------------------------------------------------------------------------------
%% @doc executes the found call flow by starting a new cf_exe process under the
%% cf_exe_sup tree.
%% @end
%%------------------------------------------------------------------------------
-spec execute_callflow(kapps_call:call()) -> kapps_call:call().
execute_callflow(Call) ->
    lager:info("call has been setup, beginning to process the call"),
    kapps_call:kvs_store('cf_exe_pid', self(), Call).
