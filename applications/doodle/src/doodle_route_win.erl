%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc handler for route wins, bootstraps callflow execution
%%% @author Karl Anderson
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(doodle_route_win).

-include("doodle.hrl").

-define(JSON(L), kz_json:from_list(L)).

-define(DEFAULT_SERVICES, ?JSON([{<<"audio">>, ?JSON([{<<"enabled">>, 'true'}])}
                                ,{<<"video">>, ?JSON([{<<"enabled">>, 'true'}])}
                                ,{<<"sms">>, ?JSON([{<<"enabled">>, 'true'}])}
                                ])).

-define(DEFAULT_LANGUAGE, <<"en-US">>).
-define(DEFAULT_UNAVAILABLE_MESSAGE, <<"sms service unavailable">>).
-define(DEFAULT_UNAVAILABLE_MESSAGE_NODE, kz_json:from_list([{?DEFAULT_LANGUAGE, ?DEFAULT_UNAVAILABLE_MESSAGE}])).
-define(RESTRICTED_MSG, <<"endpoint is restricted from making this call">>).
-define(SCHEDULED(Call), kapps_call:custom_channel_var(<<"Scheduled-Delivery">>, 0, Call)).

-export([execute_text_flow/2]).

-spec execute_text_flow(kz_json:object(), kapps_call:call()) -> 'ok' | {'ok', pid()}.
execute_text_flow(JObj, Call) ->
    case should_restrict_call(Call) of
        'true' ->
            lager:debug("endpoint is restricted from sending this text, terminate", []),
            _ = send_service_unavailable(JObj, Call),
            _Call = doodle_util:save_sms(doodle_util:set_flow_error(<<"error">>, ?RESTRICTED_MSG, Call)),
            'ok';
        'false' ->
            maybe_scheduled_delivery(JObj, Call, ?SCHEDULED(Call) , kz_time:now_s())
    end.

-spec maybe_scheduled_delivery(kz_json:object(), kapps_call:call(), integer(), integer()) ->
                                      kapps_call:call() | {'ok', pid()}.
maybe_scheduled_delivery(_JObj, Call, DeliveryAt, Now)
  when DeliveryAt > Now ->
    lager:info("scheduling sms delivery"),
    Schedule = [{<<"rule">>, 1}
               ,{<<"rule_start_time">>, DeliveryAt}
               ,{<<"start_time">>, DeliveryAt}
               ,{<<"attempts">>, 0}
               ,{<<"total_attempts">>, 0}
               ],
    Call1 = kapps_call:kvs_store(<<"flow_schedule">>, kz_json:from_list(Schedule), Call),
    doodle_util:save_sms(doodle_util:set_flow_status(<<"pending">>, Call1));
maybe_scheduled_delivery(JObj, Call, _, _) ->
    lager:info("setting initial information about the text"),
    bootstrap_textflow_executer(JObj, Call).

-spec should_restrict_call(kapps_call:call()) -> boolean().
should_restrict_call(Call) ->
    case kz_endpoint:get(Call) of
        {'error', _R} ->
            lager:debug("error getting kz_endpoint for the sms : ~p", [_R]),
            'false';
        {'ok', JObj} -> maybe_service_unavailable(JObj, Call)
    end.

-spec maybe_service_unavailable(kz_json:object(), kapps_call:call()) -> boolean().
maybe_service_unavailable(JObj, Call) ->
    Id = kz_doc:id(JObj),
    Services = kz_json:merge(
                 kz_json:get_value(<<"services">>, JObj, ?DEFAULT_SERVICES),
                 kz_json:get_value(<<"pvt_services">>, JObj, kz_json:new())),
    case kz_json:is_true([<<"sms">>,<<"enabled">>], Services, 'true') of
        'true' ->
            maybe_account_service_unavailable(JObj, Call);
        'false' ->
            lager:debug("device ~s does not have sms service enabled", [Id]),
            'true'
    end.

-spec maybe_account_service_unavailable(kz_json:object(), kapps_call:call()) -> boolean().
maybe_account_service_unavailable(JObj, Call) ->
    AccountId = kapps_call:account_id(Call),
    {'ok', Doc} = kzd_accounts:fetch(AccountId),
    Services = kz_json:merge(
                 kz_json:get_value(<<"services">>, Doc, ?DEFAULT_SERVICES),
                 kz_json:get_value(<<"pvt_services">>, Doc, kz_json:new())),
    case kz_json:is_true([<<"sms">>,<<"enabled">>], Services, 'true') of
        'true' ->
            maybe_closed_group_restriction(JObj, Call);
        'false' ->
            lager:debug("account ~s does not have sms service enabled", [AccountId]),
            'true'
    end.

-spec maybe_closed_group_restriction(kz_json:object(), kapps_call:call()) -> boolean().
maybe_closed_group_restriction(JObj, Call) ->
    case kz_json:get_value([<<"call_restriction">>, <<"closed_groups">>, <<"action">>], JObj) of
        <<"deny">> -> enforce_closed_groups(JObj, Call);
        _Else -> maybe_classification_restriction(JObj, Call)
    end.

-spec maybe_classification_restriction(kz_json:object(), kapps_call:call()) -> boolean().
maybe_classification_restriction(JObj, Call) ->
    Number = kapps_call:request_user(Call),
    Classification = knm_converters:classify(Number),
    lager:debug("classified number as ~s, testing for call restrictions", [Classification]),
    kz_json:get_value([<<"call_restriction">>, Classification, <<"action">>], JObj) =:= <<"deny">>.

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
          ,kz_json:get_value(<<"owner_id">>, JObj)
           | kz_json:get_keys([<<"hotdesk">>, <<"users">>], JObj)
          ],
    lists:foldl(fun('undefined', Set) -> Set;
                   (Id, Set) ->
                        get_group_associations(Id, Groups, Set)
                end, sets:new(), Ids).

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
                                      end, sets:new(), UserIds),
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
                            _Else ->
                                GroupId = kz_doc:id(Group),
                                sets:add_element(GroupId, S)
                        end
                end, Set, Groups).

-spec get_callee_extension_info(kapps_call:call()) -> {kz_term:ne_binary(), kz_term:ne_binary()} | 'undefined'.
get_callee_extension_info(Call) ->
    Flow = kapps_call:kvs_fetch('cf_flow', Call),
    FirstModule = kz_json:get_value(<<"module">>, Flow),
    FirstId = kz_json:get_value([<<"data">>, <<"id">>], Flow),
    SecondModule = kz_json:get_value([<<"_">>, <<"module">>], Flow),
    case (FirstModule =:= <<"device">>
              orelse
          FirstModule =:= <<"user">>)
        andalso
        (SecondModule =:= <<"voicemail">>
             orelse
         SecondModule =:= 'undefined')
        andalso
        FirstId =/= 'undefined'
    of
        'true' -> {FirstModule, FirstId};
        'false' -> 'undefined'
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec bootstrap_textflow_executer(kz_json:object(), kapps_call:call()) -> {'ok', pid()}.
bootstrap_textflow_executer(_JObj, Call) ->
    Routines = [fun store_owner_id/1
               ,fun update_ccvs/1
                %% all funs above here return kapps_call:call()
               ,fun execute_textflow/1
               ],
    lists:foldl(fun(F, C) -> F(C) end, Call, Routines).

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
-spec update_ccvs(kapps_call:call()) -> kapps_call:call().
update_ccvs(Call) ->
    CallerIdType = case kapps_call:inception(Call) of
                       'undefined' -> <<"internal">>;
                       _Else -> <<"external">>
                   end,
    {CIDNumber, CIDName} = kz_attributes:caller_id(CallerIdType, Call),
    lager:info("bootstrapping with caller id type ~s: \"~s\" ~s"
              ,[CallerIdType, CIDName, CIDNumber]),
    Props = props:filter_undefined(
              [{<<"Caller-ID-Name">>, CIDName}
              ,{<<"Caller-ID-Number">>, CIDNumber}
               | get_incoming_security(Call)
              ]),
    kapps_call:set_custom_channel_vars(Props, Call).

-spec get_incoming_security(kapps_call:call()) -> kz_term:proplist().
get_incoming_security(Call) ->
    case kz_endpoint:get(Call) of
        {'error', _R} -> [];
        {'ok', JObj} ->
            kz_json:to_proplist(
              kz_endpoint:encryption_method_map(kz_json:new(), JObj)
             )
    end.

%%------------------------------------------------------------------------------
%% @doc executes the found call flow by starting a new doodle_exe process under the
%% doodle_exe_sup tree.
%% @end
%%------------------------------------------------------------------------------
-spec execute_textflow(kapps_call:call()) -> {'ok', pid()}.
execute_textflow(Call) ->
    lager:info("message has been setup, beginning to process the message"),
    doodle_exe_sup:new(Call).

-spec send_service_unavailable(kz_json:object(), kapps_call:call()) -> kapps_call:call().
send_service_unavailable(_JObj, Call) ->
    Routines = [fun store_owner_id/1
               ,fun update_ccvs/1
               ,fun set_service_unavailable_message/1
               ,fun set_sms_sender/1
               ,fun send_reply_msg/1
               ],
    kapps_call:exec(Routines, Call).

-spec set_service_unavailable_message(kapps_call:call()) -> kapps_call:call().
set_service_unavailable_message(Call) ->
    {'ok', Endpoint} = kz_endpoint:get(Call),
    Language = kz_json:get_value(<<"language">>, Endpoint, ?DEFAULT_LANGUAGE),
    TextNode = kapps_config:get_json(?CONFIG_CAT, <<"unavailable_message">>, ?DEFAULT_UNAVAILABLE_MESSAGE_NODE),
    Text = kz_json:get_value(Language, TextNode, ?DEFAULT_UNAVAILABLE_MESSAGE),
    kapps_call:kvs_store(<<"Body">>, Text, Call).

-spec send_reply_msg(kapps_call:call()) -> kapps_call:call().
send_reply_msg(Call) ->
    EndpointId = kapps_call:authorizing_id(Call),
    Options = kz_json:set_value(<<"can_call_self">>, 'true', kz_json:new()),
    _ = case kz_endpoint:build(EndpointId, Options, Call) of
            {'error', Msg}=_E ->
                lager:debug("error getting endpoint for reply unavailable service ~s : ~p", [EndpointId, Msg]);
            {'ok', Endpoints} ->
                kapps_sms_command:b_send_sms(Endpoints, Call)
        end,
    Call.

-spec set_sms_sender(kapps_call:call()) -> kapps_call:call().
set_sms_sender(Call) ->
    kapps_call:set_from(<<"sip:sms-service@",(kapps_call:from_realm(Call))/binary>>, Call).
