%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2015, 2600Hz INC
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

-define(JSON(L), wh_json:from_list(L)).

-define(DEFAULT_SERVICES
        ,?JSON([{<<"audio">>, ?JSON([{<<"enabled">>, 'true'}])}
                ,{<<"video">>,?JSON([{<<"enabled">>, 'true'}])}
                ,{<<"sms">>,  ?JSON([{<<"enabled">>, 'true'}])}
               ]
              )
       ).

-spec execute_callflow(wh_json:object(), whapps_call:call()) ->
                                 'ok' | {'ok', pid()}.
execute_callflow(JObj, Call) ->
    case should_restrict_call(Call) of
        'true' ->
            lager:debug("endpoint is restricted from making this call, terminate", []),
            _ = whapps_call_command:answer(Call),
            _ = whapps_call_command:prompt(<<"cf-unauthorized_call">>, Call),
            _ = whapps_call_command:queued_hangup(Call),
            'ok';
        'false' ->
            lager:info("setting initial information about the call"),
            bootstrap_callflow_executer(JObj, Call)
    end.

-spec should_restrict_call(whapps_call:call()) -> boolean().
should_restrict_call(Call) ->
    case cf_endpoint:get(Call) of
        {'error', _R} -> 'false';
        {'ok', JObj} -> maybe_service_unavailable(JObj, Call)
    end.

-spec maybe_service_unavailable(wh_json:object(), whapps_call:call()) -> boolean().
maybe_service_unavailable(JObj, Call) ->
    Id = wh_doc:id(JObj),
    Services = wh_json:merge_recursive(
                 wh_json:get_value(<<"services">>, JObj, ?DEFAULT_SERVICES),
                 wh_json:get_value(<<"pvt_services">>, JObj, wh_json:new())),
    case wh_json:is_true([<<"audio">>,<<"enabled">>], Services, 'true') of
        'true' ->
            maybe_account_service_unavailable(JObj, Call);
        'false' ->
            lager:debug("device ~s does not have audio service enabled", [Id]),
            'true'
    end.

-spec maybe_account_service_unavailable(wh_json:object(), whapps_call:call()) -> boolean().
maybe_account_service_unavailable(JObj, Call) ->
    AccountId = whapps_call:account_id(Call),
    {'ok', Doc} = kz_account:fetch(AccountId),
    Services = wh_json:merge_recursive(
                 wh_json:get_value(<<"services">>, Doc, ?DEFAULT_SERVICES),
                 wh_json:get_value(<<"pvt_services">>, Doc, wh_json:new())),
    case wh_json:is_true([<<"audio">>,<<"enabled">>], Services, 'true') of
        'true' ->
            maybe_closed_group_restriction(JObj, Call);
        'false' ->
            lager:debug("account ~s does not have audio service enabled", [AccountId]),
            'true'
    end.

-spec maybe_closed_group_restriction(wh_json:object(), whapps_call:call()) ->
                                            boolean().
maybe_closed_group_restriction(JObj, Call) ->
    case wh_json:get_value([<<"call_restriction">>, <<"closed_groups">>, <<"action">>], JObj) of
        <<"deny">> -> enforce_closed_groups(JObj, Call);
        _Else -> maybe_classification_restriction(JObj, Call)
    end.

-spec maybe_classification_restriction(wh_json:object(), whapps_call:call()) ->
                                              boolean().
maybe_classification_restriction(JObj, Call) ->
    Request = find_request(Call),
    AccountId = whapps_call:account_id(Call),
    DialPlan = wh_json:get_value(<<"dial_plan">>, JObj, wh_json:new()),
    Number = wnm_util:to_e164(Request, AccountId, DialPlan),
    Classification = wnm_util:classify_number(Number, AccountId),
    lager:debug("classified number ~s as ~s, testing for call restrictions"
                ,[Number, Classification]
               ),
    wh_json:get_value([<<"call_restriction">>, Classification, <<"action">>], JObj) =:= <<"deny">>.

-spec find_request(whapps_call:call()) -> ne_binary().
find_request(Call) ->
    case whapps_call:kvs_fetch('cf_capture_group', Call) of
        'undefined' ->
            whapps_call:request_user(Call);
        CaptureGroup ->
            lager:debug("capture group ~s being used instead of request ~s"
                        ,[CaptureGroup, whapps_call:request_user(Call)]
                       ),
            CaptureGroup
    end.

-spec enforce_closed_groups(wh_json:object(), whapps_call:call()) -> boolean().
enforce_closed_groups(JObj, Call) ->
    case get_callee_extension_info(Call) of
        'undefined' ->
            lager:info("dialed number is not an extension, using classification restrictions", []),
            maybe_classification_restriction(JObj, Call);
        {<<"user">>, CalleeId} ->
            lager:info("dialed number is user ~s extension, checking groups", [CalleeId]),
            Groups = cf_attributes:groups(Call),
            CallerGroups = get_caller_groups(Groups, JObj, Call),
            CalleeGroups = get_group_associations(CalleeId, Groups),
            sets:size(sets:intersection(CallerGroups, CalleeGroups)) =:= 0;
        {<<"device">>, CalleeId} ->
            lager:info("dialed number is device ~s extension, checking groups", [CalleeId]),
            Groups = cf_attributes:groups(Call),
            CallerGroups = get_caller_groups(Groups, JObj, Call),
            maybe_device_groups_intersect(CalleeId, CallerGroups, Groups, Call)
    end.

-spec get_caller_groups(wh_json:objects(), wh_json:object(), whapps_call:call()) -> set().
get_caller_groups(Groups, JObj, Call) ->
    Ids = [whapps_call:authorizing_id(Call)
           ,wh_json:get_value(<<"owner_id">>, JObj)
           | wh_json:get_keys([<<"hotdesk">>, <<"users">>], JObj)
          ],
    lists:foldl(fun('undefined', Set) -> Set;
                   (Id, Set) ->
                        get_group_associations(Id, Groups, Set)
                end
                ,sets:new()
                ,Ids
               ).

-spec maybe_device_groups_intersect(ne_binary(), set(), wh_json:objects(), whapps_call:call()) -> boolean().
maybe_device_groups_intersect(CalleeId, CallerGroups, Groups, Call) ->
    CalleeGroups = get_group_associations(CalleeId, Groups),
    case sets:size(sets:intersection(CallerGroups, CalleeGroups)) =:= 0 of
        'false' -> 'false';
        'true' ->
            %% In this case the callee-id is a device id, find out if
            %% the owner of the device shares any groups with the caller
            UserIds = cf_attributes:owner_ids(CalleeId, Call),
            UsersGroups = lists:foldl(fun(UserId, Set) ->
                                              get_group_associations(UserId, Groups, Set)
                                      end
                                      ,sets:new()
                                      ,UserIds
                                     ),
            sets:size(sets:intersection(CallerGroups, UsersGroups)) =:= 0
    end.

-spec get_group_associations(ne_binary(), wh_json:objects()) -> set().
get_group_associations(Id, Groups) ->
    get_group_associations(Id, Groups, sets:new()).

-spec get_group_associations(ne_binary(), wh_json:objects(), set()) -> set().
get_group_associations(Id, Groups, Set) ->
    lists:foldl(fun(Group, S) ->
                        case wh_json:get_value([<<"value">>, Id], Group) of
                            'undefined' -> S;
                            _Else -> sets:add_element(wh_doc:id(Group), S)
                        end
                end, Set, Groups).

-spec get_callee_extension_info(whapps_call:call()) -> {ne_binary(), ne_binary()} | 'undefined'.
get_callee_extension_info(Call) ->
    Flow = whapps_call:kvs_fetch('cf_flow', Call),
    FirstModule = wh_json:get_value(<<"module">>, Flow),
    FirstId = wh_json:get_value([<<"data">>, <<"id">>], Flow),
    SecondModule = wh_json:get_value([<<"_">>, <<"module">>], Flow),
    case (FirstModule =:= <<"device">> orelse FirstModule =:= <<"user">>)
        andalso
        (SecondModule =:= <<"voicemail">> orelse SecondModule =:= 'undefined')
        andalso
        FirstId =/= 'undefined'
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
-spec bootstrap_callflow_executer(wh_json:object(), whapps_call:call()) -> {'ok', pid()}.
bootstrap_callflow_executer(_JObj, Call) ->
    Routines = [fun store_owner_id/1
                ,fun set_language/1
                ,fun update_ccvs/1
                ,fun maybe_start_recording/1
                %% all funs above here return whapps_call:call()
                ,fun execute_callflow/1
                ,fun maybe_start_metaflow/1
               ],
    lists:foldl(fun(F, C) -> F(C) end, Call, Routines).

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%-----------------------------------------------------------------------------
-spec store_owner_id(whapps_call:call()) -> whapps_call:call().
store_owner_id(Call) ->
    OwnerId = cf_attributes:owner_id(Call),
    whapps_call:kvs_store('owner_id', OwnerId, Call).

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%-----------------------------------------------------------------------------
-spec set_language(whapps_call:call()) -> whapps_call:call().
set_language(Call) ->
    Default = wh_media_util:prompt_language(whapps_call:account_id(Call)),
    case cf_endpoint:get(Call) of
        {'ok', Endpoint} ->
            Language = kz_device:language(Endpoint, Default),
            lager:debug("setting language '~s' for this call", [Language]),
            whapps_call:set_language(wh_util:to_lower_binary(Language), Call);
        {'error', _E} ->
            lager:debug("no source endpoint for this call, setting language to default ~s", [Default]),
            whapps_call:set_language(Default, Call)
    end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%-----------------------------------------------------------------------------
-spec update_ccvs(whapps_call:call()) -> whapps_call:call().
update_ccvs(Call) ->
    CallerIdType = case whapps_call:inception(Call) of
                       'undefined' -> <<"internal">>;
                       _Else -> <<"external">>
                   end,
    {CIDNumber, CIDName} =
        cf_attributes:caller_id(
          CallerIdType
          ,whapps_call:kvs_erase('prepend_cid_name', Call)
         ),
    lager:info("bootstrapping with caller id type ~s: \"~s\" ~s"
               ,[CallerIdType, CIDName, CIDNumber]
              ),
    Props = props:filter_undefined(
              [{<<"Hold-Media">>, cf_attributes:moh_attributes(<<"media_id">>, Call)}
               ,{<<"Caller-ID-Name">>, CIDName}
               ,{<<"Caller-ID-Number">>, CIDNumber}
               | get_incoming_security(Call)
              ]),
    whapps_call:set_custom_channel_vars(Props, Call).

-spec maybe_start_metaflow(whapps_call:call()) -> whapps_call:call().
-spec maybe_start_metaflow(whapps_call:call(), api_binary()) -> whapps_call:call().
maybe_start_metaflow(Call) ->
    maybe_start_metaflow(Call, whapps_call:custom_channel_var(<<"Metaflow-App">>, Call)).

maybe_start_metaflow(Call, 'undefined') ->
    maybe_start_endpoint_metaflow(Call, whapps_call:authorizing_id(Call)),
    Call;
maybe_start_metaflow(Call, App) ->
    lager:debug("metaflow app ~s", [App]),
    Call.

-spec maybe_start_endpoint_metaflow(whapps_call:call(), api_binary()) -> 'ok'.
maybe_start_endpoint_metaflow(Call, 'undefined') ->
    Account = whapps_call:account_id(Call),
    HackedCall = whapps_call:set_authorizing_id(Account, Call),
    maybe_start_endpoint_metaflow(HackedCall, Account);
maybe_start_endpoint_metaflow(Call, EndpointId) ->
    lager:debug("looking up endpoint for ~s", [EndpointId]),
    case cf_endpoint:get(EndpointId, Call) of
        {'ok', Endpoint} ->
            lager:debug("trying to send metaflow for a-leg endpoint ~s", [EndpointId]),
            cf_util:maybe_start_metaflow(Call, Endpoint);
        {'error', _E} -> 'ok'
    end.

-spec maybe_start_recording(whapps_call:call()) -> whapps_call:call().
maybe_start_recording(Call) ->
    maybe_start_endpoint_recording(Call, cf_endpoint:get(Call)).

-spec maybe_start_endpoint_recording(whapps_call:call(), cf_api_std_return()) -> 'ok'.
maybe_start_endpoint_recording(Call, {'ok', Endpoint}) ->
    Data = wh_json:get_value(<<"record_call">>, Endpoint, wh_json:new()),
    cf_util:maybe_start_call_recording(Data, Call);
maybe_start_endpoint_recording(Call, _) -> Call.

-spec get_incoming_security(whapps_call:call()) -> wh_proplist().
 get_incoming_security(Call) ->
    case cf_endpoint:get(Call) of
        {'error', _R} -> [];
        {'ok', JObj} ->
            wh_json:to_proplist(
              cf_util:encryption_method_map(wh_json:new(), JObj)
             )
    end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% executes the found call flow by starting a new cf_exe process under the
%% cf_exe_sup tree.
%% @end
%%-----------------------------------------------------------------------------
-spec execute_callflow(whapps_call:call()) -> whapps_call:call().
execute_callflow(Call) ->
    lager:info("call has been setup, beginning to process the call"),
    {'ok', _P} = cf_exe_sup:new(Call),
    Call.
