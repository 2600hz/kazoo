%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz INC
%%% @doc
%%% handler for route wins, bootstraps callflow execution
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(cf_route_win).

-export([execute_callflow/2
        ]).

-include("callflow.hrl").

-define(JSON(L), kz_json:from_list(L)).

-define(DEFAULT_SERVICES
       ,?JSON([{<<"audio">>, ?JSON([{<<"enabled">>, 'true'}])}
              ,{<<"video">>,?JSON([{<<"enabled">>, 'true'}])}
              ,{<<"sms">>,  ?JSON([{<<"enabled">>, 'true'}])}
              ]
             )
       ).

-spec execute_callflow(kz_json:object(), kapps_call:call()) ->
                              'ok' | {'ok', pid()}.
execute_callflow(JObj, Call) ->
    case should_restrict_call(Call) of
        'true' ->
            lager:debug("endpoint is restricted from making this call, terminate", []),
            _ = kapps_call_command:answer(Call),
            _ = kapps_call_command:prompt(<<"cf-unauthorized_call">>, Call),
            _ = kapps_call_command:queued_hangup(Call),
            'ok';
        'false' ->
            lager:info("setting initial information about the call"),
            bootstrap_callflow_executer(JObj, Call)
    end.

-spec should_restrict_call(kapps_call:call()) -> boolean().
should_restrict_call(Call) ->
    DefaultEndpointId = kapps_call:authorizing_id(Call),
    EndpointId = kapps_call:kvs_fetch(?RESTRICTED_ENDPOINT_KEY, DefaultEndpointId, Call),
    should_restrict_call(EndpointId, Call).

-spec should_restrict_call(api_binary(), kapps_call:call()) -> boolean().
should_restrict_call('undefined', _Call) -> 'false';
should_restrict_call(EndpointId, Call) ->
    case kz_endpoint:get(EndpointId, Call) of
        {'error', _R} -> 'false';
        {'ok', JObj} -> maybe_service_unavailable(JObj, Call)
    end.

-spec maybe_service_unavailable(kz_json:object(), kapps_call:call()) -> boolean().
maybe_service_unavailable(JObj, Call) ->
    Id = kz_doc:id(JObj),
    Services = kz_json:merge_recursive(
                 kz_json:get_value(<<"services">>, JObj, ?DEFAULT_SERVICES),
                 kz_json:get_value(<<"pvt_services">>, JObj, kz_json:new())),
    case kz_json:is_true([<<"audio">>,<<"enabled">>], Services, 'true') of
        'true' ->
            maybe_account_service_unavailable(JObj, Call);
        'false' ->
            lager:debug("device ~s does not have audio service enabled", [Id]),
            'true'
    end.

-spec maybe_account_service_unavailable(kz_json:object(), kapps_call:call()) -> boolean().
maybe_account_service_unavailable(JObj, Call) ->
    AccountId = kapps_call:account_id(Call),
    {'ok', Doc} = kz_account:fetch(AccountId),
    Services = kz_json:merge_recursive(
                 kz_json:get_value(<<"services">>, Doc, ?DEFAULT_SERVICES),
                 kz_json:get_value(<<"pvt_services">>, Doc, kz_json:new())),
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
    DialPlan = kz_json:get_value(<<"dial_plan">>, JObj, kz_json:new()),
    Number = knm_converters:normalize(Request, AccountId, DialPlan),
    Classification = knm_converters:classify(Number),
    lager:debug("classified number ~s as ~s, testing for call restrictions"
               ,[Number, Classification]
               ),
    kz_json:get_value([<<"call_restriction">>, Classification, <<"action">>], JObj) =:= <<"deny">>.

-spec find_request(kapps_call:call()) -> ne_binary().
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
          ,kz_json:get_value(<<"owner_id">>, JObj)
           | kz_json:get_keys([<<"hotdesk">>, <<"users">>], JObj)
          ],
    lists:foldl(fun('undefined', Set) -> Set;
                   (Id, Set) ->
                        get_group_associations(Id, Groups, Set)
                end
               ,sets:new()
               ,Ids
               ).

-spec maybe_device_groups_intersect(ne_binary(), sets:set(), kz_json:objects(), kapps_call:call()) -> boolean().
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

-spec get_group_associations(ne_binary(), kz_json:objects()) -> sets:set().
get_group_associations(Id, Groups) ->
    get_group_associations(Id, Groups, sets:new()).

-spec get_group_associations(ne_binary(), kz_json:objects(), sets:set()) -> sets:set().
get_group_associations(Id, Groups, Set) ->
    lists:foldl(fun(Group, S) ->
                        case kz_json:get_value([<<"value">>, Id], Group) of
                            'undefined' -> S;
                            _Else -> sets:add_element(kz_doc:id(Group), S)
                        end
                end, Set, Groups).

-spec get_callee_extension_info(kapps_call:call()) -> {ne_binary(), ne_binary()} | 'undefined'.
get_callee_extension_info(Call) ->
    Flow = kapps_call:kvs_fetch('cf_flow', Call),
    FirstModule = kz_json:get_value(<<"module">>, Flow),
    FirstId = kz_json:get_value([<<"data">>, <<"id">>], Flow),
    SecondModule = kz_json:get_value([<<"_">>, <<"module">>], Flow),
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

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%-----------------------------------------------------------------------------
-spec bootstrap_callflow_executer(kz_json:object(), kapps_call:call()) -> {'ok', pid()}.
bootstrap_callflow_executer(_JObj, Call) ->
    Routines = [fun store_owner_id/1
               ,fun set_language/1
               ,fun update_ccvs/1
               ,fun maybe_start_recording/1
               ,fun execute_callflow/1
               ,fun maybe_start_metaflow/1
               ],
    kapps_call:exec(Routines, Call).

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%-----------------------------------------------------------------------------
-spec store_owner_id(kapps_call:call()) -> kapps_call:call().
store_owner_id(Call) ->
    OwnerId = kz_attributes:owner_id(Call),
    kapps_call:kvs_store('owner_id', OwnerId, Call).

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%-----------------------------------------------------------------------------
-spec set_language(kapps_call:call()) -> kapps_call:call().
set_language(Call) ->
    Default = kz_media_util:prompt_language(kapps_call:account_id(Call)),
    case kz_endpoint:get(Call) of
        {'ok', Endpoint} ->
            Language = kz_device:language(Endpoint, Default),
            lager:debug("setting language '~s' for this call", [Language]),
            kapps_call:set_language(kz_util:to_lower_binary(Language), Call);
        {'error', _E} ->
            lager:debug("no source endpoint for this call, setting language to default ~s", [Default]),
            kapps_call:set_language(Default, Call)
    end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%-----------------------------------------------------------------------------
-spec update_ccvs(kapps_call:call()) -> kapps_call:call().
update_ccvs(Call) ->
    CallerIdType = case kapps_call:inception(Call) of
                       'undefined' -> <<"internal">>;
                       _Else -> <<"external">>
                   end,

    {CIDNumber, CIDName} =
        kz_attributes:caller_id(CallerIdType
                               ,kapps_call:kvs_erase('prepend_cid_name', Call)
                               ),

    lager:info("bootstrapping with caller id type ~s: \"~s\" ~s"
              ,[CallerIdType, CIDName, CIDNumber]
              ),

    Props = props:filter_undefined(
              [{<<"Hold-Media">>, kz_attributes:moh_attributes(<<"media_id">>, Call)}
              ,{<<"Caller-ID-Name">>, CIDName}
              ,{<<"Caller-ID-Number">>, CIDNumber}
               | get_incoming_security(Call)
              ]),
    kapps_call:set_custom_channel_vars(Props, Call).

-spec maybe_start_metaflow(kapps_call:call()) -> kapps_call:call().
-spec maybe_start_metaflow(kapps_call:call(), api_binary()) -> kapps_call:call().
maybe_start_metaflow(Call) ->
    maybe_start_metaflow(Call, kapps_call:custom_channel_var(<<"Metaflow-App">>, Call)).

maybe_start_metaflow(Call, 'undefined') ->
    maybe_start_endpoint_metaflow(Call, kapps_call:authorizing_id(Call)),
    Call;
maybe_start_metaflow(Call, App) ->
    lager:debug("metaflow app ~s", [App]),
    Call.

-spec maybe_start_endpoint_metaflow(kapps_call:call(), api_binary()) -> 'ok'.
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
    case kz_endpoint:get(Call) of
        {'ok', Endpoint} ->
            Data = kz_json:get_value(<<"record_call">>, Endpoint, kz_json:new()),
            kz_endpoint:maybe_start_call_recording(Data, Call);
        {'error', _} -> Call
    end.

-spec get_incoming_security(kapps_call:call()) -> kz_proplist().
get_incoming_security(Call) ->
    case kz_endpoint:get(Call) of
        {'error', _R} -> [];
        {'ok', JObj} ->
            kz_json:to_proplist(
              kz_endpoint:encryption_method_map(kz_json:new(), JObj)
             )
    end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% executes the found call flow by starting a new cf_exe process under the
%% cf_exe_sup tree.
%% @end
%%-----------------------------------------------------------------------------
-spec execute_callflow(kapps_call:call()) -> kapps_call:call().
execute_callflow(Call) ->
    lager:info("call has been setup, beginning to process the call"),
    {'ok', _P} = cf_exe_sup:new(Call),
    Call.
