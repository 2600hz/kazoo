%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc Callflow resource.
%%%
%%% <h4>Data options:</h4>
%%% <dl>
%%%   <dt>`to_did'</dt>
%%%   <dd>Statically dial DID</dd>
%%%
%%%   <dt>`media'</dt>
%%%   <dd>"Media ID</dd>
%%%
%%%   <dt>`ringback'</dt>
%%%   <dd>Ringback ID</dd>
%%%
%%%   <dt>`format_from_did'</dt>
%%%   <dd>`boolean()'</dd>
%%%
%%%   <dt>`timeout'</dt>
%%%   <dd>`integer()'</dd>
%%%
%%%   <dt>`do_not_normalize'</dt>
%%%   <dd>`boolean()'</dd>
%%%
%%%   <dt>`bypass_e164'</dt>
%%%   <dd>`boolean()'</dd>
%%%
%%%   <dt>`from_uri_realm'</dt>
%%%   <dd>Realm</dd>
%%%
%%%   <dt>`caller_id_type'</dt>
%%%   <dd>Can use custom caller id properties on endpoints, e.g. `external'.</dd>
%%%
%%%   <dt>`use_local_resources'</dt>
%%%   <dd>`boolean()'</dd>
%%%
%%%   <dt>`hunt_account_id'</dt>
%%%   <dd>Use this account's local carriers instead of current account.</dd>
%%%
%%%   <dt>`emit_account_id'</dt>
%%%   <dd>`boolean()', puts account ID in SIP header `X-Account-ID'</dd>
%%%
%%%   <dt>`custom_sip_headers'</dt>
%%%   <dd>`{"header":"value",...}'</dd>
%%%
%%%   <dt>`ignore_early_media'</dt>
%%%   <dd>`boolean()'</dd>
%%%
%%%   <dt>`outbound_flags'</dt>
%%%   <dd>`["flag_1","flag_2"]', used to match flags on carrier docs</dd>
%%% </dl>
%%%
%%% @author Karl Anderson
%%% @author Sponsored by Raffel Internet B.V. Implemented by Voyager Internet Ltd.
%%% @end
%%%-----------------------------------------------------------------------------
-module(cf_resources).
-behaviour(gen_cf_action).

-export([handle/2]).

-include("callflow.hrl").
-include_lib("kazoo_amqp/include/kapi_offnet_resource.hrl").

-define(DEFAULT_EVENT_WAIT, 10000).
-define(RES_CONFIG_CAT, <<?CF_CONFIG_CAT/binary, ".resources">>).

%%------------------------------------------------------------------------------
%% @doc Entry point for this module
%% @end
%%------------------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    'ok' = kapi_offnet_resource:publish_req(build_offnet_request(Data, Call)),
    case wait_for_stepswitch(Call) of
        {<<"SUCCESS">>, _} ->
            lager:info("completed successful offnet request"),
            cf_exe:stop(Call);
        {<<"TRANSFER">>, _} ->
            lager:info("completed successful offnet request"),
            cf_exe:transfer(Call);
        {<<"NORMAL_CLEARING">>, <<"sip:200">>} ->
            lager:info("completed successful offnet request"),
            cf_exe:stop(Call);
        {<<"NORMAL_CLEARING">>, 'undefined'} ->
            lager:info("completed successful offnet request"),
            cf_exe:stop(Call);
        {Cause, Code} -> handle_bridge_failure(Cause, Code, Call)
    end.

-spec handle_bridge_failure(kz_term:api_binary(), kz_term:api_binary(), kapps_call:call()) -> 'ok'.
handle_bridge_failure(Cause, Code, Call) ->
    lager:info("offnet request error, attempting to find failure branch for ~s:~s", [Code, Cause]),
    case cf_util:handle_bridge_failure(Cause, Code, Call) of
        'ok' -> lager:debug("found bridge failure child");
        'not_found' ->
            cf_util:send_default_response(Cause, Call),
            cf_exe:stop(Call, Cause)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec build_offnet_request(kz_json:object(), kapps_call:call()) -> kz_term:proplist().
build_offnet_request(Data, Call) ->
    {ECIDNum, ECIDName} = kz_attributes:caller_id(<<"emergency">>, Call),
    {AssertedNumber, AssertedName, AssertedRealm} =
        get_asserted_identity(Data, Call),
    {CIDNumber, CIDName} = get_caller_id(Data, Call),
    props:filter_undefined(
      [{?KEY_ACCOUNT_ID, kapps_call:account_id(Call)}
      ,{?KEY_ACCOUNT_REALM, kapps_call:account_realm(Call)}
      ,{?KEY_APPLICATION_NAME, ?APPLICATION_BRIDGE}
      ,{?KEY_BYPASS_E164, get_bypass_e164(Data)}
      ,{?KEY_B_LEG_EVENTS, [<<"DTMF">>]}
      ,{?KEY_CALL_ID, cf_exe:callid(Call)}
      ,{?KEY_CCVS, get_channel_vars(Call)}
      ,{?KEY_CAVS, kapps_call:custom_application_vars(Call)}
      ,{?KEY_REQUESTOR_CCVS, kapps_call:custom_channel_vars(Call)}
      ,{?KEY_CONTROL_QUEUE, cf_exe:control_queue(Call)}
      ,{?KEY_CSHS, get_sip_headers(Data, Call)}
      ,{?KEY_REQUESTOR_CSHS, kapps_call:custom_sip_headers(Call)}
      ,{?KEY_E_CALLER_ID_NAME, ECIDName}
      ,{?KEY_E_CALLER_ID_NUMBER, ECIDNum}
      ,{?KEY_FLAGS, get_flags(Data, Call)}
      ,{?KEY_FORMAT_FROM_URI, kz_json:is_true(<<"format_from_uri">>, Data)}
      ,{?KEY_FROM_URI_REALM, get_from_uri_realm(Data, Call)}
      ,{?KEY_HUNT_ACCOUNT_ID, get_hunt_account_id(Data, Call)}
      ,{?KEY_IGNORE_EARLY_MEDIA, get_ignore_early_media(Data)}
      ,{?KEY_INCEPTION, get_inception(Call)}
      ,{?KEY_MEDIA, kz_json:get_first_defined([<<"media">>, <<"Media">>], Data)}
      ,{?KEY_MSG_ID, kz_binary:rand_hex(6)}
      ,{?KEY_OUTBOUND_CALLER_ID_NAME, CIDName}
      ,{?KEY_OUTBOUND_CALLER_ID_NUMBER, CIDNumber}
      ,{?KEY_PRESENCE_ID, maybe_presence_id(Call)}
      ,{?KEY_ASSERTED_IDENTITY_NAME, AssertedName}
      ,{?KEY_ASSERTED_IDENTITY_NUMBER, AssertedNumber}
      ,{?KEY_ASSERTED_IDENTITY_REALM, AssertedRealm}
      ,{?KEY_RESOURCE_TYPE, ?RESOURCE_TYPE_AUDIO}
      ,{?KEY_RINGBACK, kz_json:get_ne_binary_value(<<"ringback">>, Data)}
      ,{?KEY_T38_ENABLED, get_t38_enabled(Call)}
      ,{?KEY_TIMEOUT, kz_json:get_integer_value(<<"timeout">>, Data)}
      ,{?KEY_TO_DID, get_to_did(Data, Call)}
      ,{?KEY_DENIED_CALL_RESTRICTIONS, kapps_call:kvs_fetch('denied_call_restrictions', Call)}
      ,{?KEY_OUTBOUND_ACTIONS, kapps_call:kvs_fetch('outbound_actions', Call)}
       | kz_api:default_headers(cf_exe:queue_name(Call), ?APP_NAME, ?APP_VERSION)
      ]).

-spec get_channel_vars(kapps_call:call()) -> kz_json:object().
get_channel_vars(Call) ->
    GetterFuns = [fun add_privacy_flags/2
                 ,fun maybe_require_ignore_early_media/2
                 ,fun maybe_require_single_fail/2
                 ,fun maybe_set_bridge_generate_comfort_noise/2
                 ,fun maybe_call_forward/2
                 ],
    CCVs = lists:foldl(fun(F, Acc) -> F(Call, Acc) end
                      ,[]
                      ,GetterFuns
                      ),
    kz_json:from_list(CCVs).

-spec maybe_presence_id(kapps_call:call()) -> kz_term:api_ne_binary().
maybe_presence_id(Call) ->
    case kapps_call:is_call_forward(Call) of
        'true' -> kz_attributes:presence_id(Call);
        'false' -> 'undefined'
    end.

-spec add_privacy_flags(kapps_call:call(), kz_term:proplist()) -> kz_term:proplist().
add_privacy_flags(Call, Acc) ->
    CCVs = kapps_call:custom_channel_vars(Call),
    props:set_values(get_privacy_prefs(Call), kz_privacy:flags(CCVs)) ++ Acc.

-spec maybe_require_ignore_early_media(kapps_call:call(), kz_term:proplist()) -> kz_term:proplist().
maybe_require_ignore_early_media(Call, Acc) ->
    [{<<"Require-Ignore-Early-Media">>, kapps_call:custom_channel_var(<<"Require-Ignore-Early-Media">>, Call)} | Acc].

-spec maybe_require_single_fail(kapps_call:call(), kz_term:proplist()) -> kz_term:proplist().
maybe_require_single_fail(Call, Acc) ->
    [{<<"Require-Fail-On-Single-Reject">>, kapps_call:custom_channel_var(<<"Require-Fail-On-Single-Reject">>, Call)} | Acc].

-spec maybe_call_forward(kapps_call:call(), kz_term:proplist()) -> kz_term:proplist().
maybe_call_forward(Call, Acc) ->
    case kapps_call:is_call_forward(Call) of
        'true' -> [{<<"Authorizing-ID">>, kapps_call:authorizing_id(Call)}
                  ,{<<"Authorizing-Type">>, kapps_call:authorizing_type(Call)}
                  ,{<<"Application-Other-Leg-UUID">>, kapps_call:custom_channel_var(<<"Call-Forward-For-UUID">>, Call)}
                   | Acc
                  ];
        'false' -> Acc
    end.

-spec get_bypass_e164(kz_json:object()) -> boolean().
get_bypass_e164(Data) ->
    kz_json:is_true(<<"do_not_normalize">>, Data)
        orelse kz_json:is_true(<<"bypass_e164">>, Data).

-spec get_from_uri_realm(kz_json:object(), kapps_call:call()) -> kz_term:api_binary().
get_from_uri_realm(Data, Call) ->
    case kz_json:get_ne_binary_value(<<"from_uri_realm">>, Data) of
        'undefined' -> maybe_get_call_from_realm(Call);
        Realm -> Realm
    end.

-spec maybe_get_call_from_realm(kapps_call:call()) -> kz_term:api_ne_binary().
maybe_get_call_from_realm(Call) ->
    case kapps_call:from_realm(Call) of
        <<"norealm">> ->
            kzd_accounts:fetch_realm(kapps_call:account_id(Call));
        Realm -> Realm
    end.

-spec maybe_set_bridge_generate_comfort_noise(kapps_call:call(), kz_term:proplist()) -> kz_term:proplist().
maybe_set_bridge_generate_comfort_noise(Call, Acc) ->
    case get_endpoint(Call) of
        {'ok', Endpoint} ->
            maybe_has_comfort_noise_option_enabled(Endpoint, Acc);
        {'error', _E} ->
            lager:debug("error acquiring originating endpoint information"),
            Acc
    end.

-spec maybe_has_comfort_noise_option_enabled(kz_json:object(), kz_term:proplist()) -> kz_term:proplist().
maybe_has_comfort_noise_option_enabled(Endpoint, Acc) ->
    case kz_json:is_true([<<"media">>, <<"bridge_generate_comfort_noise">>], Endpoint) of
        'true' -> [{<<"Bridge-Generate-Comfort-Noise">>, 'true'} | Acc];
        'false' -> Acc
    end.

-spec get_caller_id(kz_json:object(), kapps_call:call()) ->
                           {kz_term:api_binary(), kz_term:api_binary()}.
get_caller_id(Data, Call) ->
    Type = kz_json:get_value(<<"caller_id_type">>, Data, <<"external">>),
    kz_attributes:caller_id(Type, Call).

-spec get_asserted_identity(kz_json:object(), kapps_call:call()) ->
                                   {kz_term:api_binary(), kz_term:api_binary(), kz_term:api_binary()}.
get_asserted_identity(_Data, Call) ->
    case get_endpoint(Call) of
        {'error', _E} -> {'undefined', 'undefined', 'undefined'};
        {'ok', Endpoint} ->
            CallerId = kzd_devices:caller_id(Endpoint),
            DefaultRealm = kapps_call:account_realm(Call),
            DefaultName = get_asserted_default_name(CallerId, Call),
            DefaultNumber = kzd_caller_id:external_number(CallerId),
            {kzd_caller_id:asserted_number(CallerId, DefaultNumber)
            ,kzd_caller_id:asserted_name(CallerId, DefaultName)
            ,kzd_caller_id:asserted_realm(CallerId, DefaultRealm)
            }
    end.

-spec get_asserted_default_name(kz_json:object(), kapps_call:call()) -> kz_term:api_binary().
get_asserted_default_name(CallerId, Call) ->
    case kzd_caller_id:external_name(CallerId) of
        'undefined' ->
            AccountId = kapps_call:account_id(Call),
            kzd_accounts:fetch_name(AccountId);
        Name -> Name
    end.

-spec get_hunt_account_id(kz_json:object(), kapps_call:call()) -> kz_term:api_binary().
get_hunt_account_id(Data, Call) ->
    case kz_json:is_true(<<"use_local_resources">>, Data, 'true') of
        'false' -> 'undefined';
        'true' ->
            AccountId = kapps_call:account_id(Call),
            kz_json:get_ne_binary_value(<<"hunt_account_id">>, Data, AccountId)
    end.

-spec get_to_did(kz_json:object(), kapps_call:call()) -> kz_term:ne_binary().
get_to_did(Data, Call) ->
    case kz_json:get_ne_binary_value(<<"to_did">>, Data) of
        'undefined' -> get_request_did(Data, Call);
        ToDID -> ToDID
    end.

-spec get_request_did(kz_json:object(), kapps_call:call()) -> kz_term:ne_binary().
get_request_did(Data, Call) ->
    case kz_json:is_true(<<"do_not_normalize">>, Data) of
        'true' -> get_original_request_user(Call);
        'false' ->
            case get_endpoint(Call) of
                {'ok', Endpoint} -> maybe_apply_dialplan(Endpoint, Data, Call);
                _Else -> maybe_bypass_e164(Data, Call)
            end
    end.

-spec maybe_apply_dialplan(kz_json:object(), kz_json:object(), kapps_call:call()) -> kz_term:ne_binary().
maybe_apply_dialplan(Endpoint, Data, Call) ->
    case kz_json:get_value(<<"dial_plan">>, Endpoint, []) of
        [] -> maybe_bypass_e164(Data, Call);
        DialPlan ->
            Request = kapps_call:request(Call),
            [RequestUser, _] = binary:split(Request, <<"@">>),
            cf_util:apply_dialplan(RequestUser, DialPlan)
    end.

-spec maybe_bypass_e164(kz_json:object(), kapps_call:call()) -> kz_term:ne_binary().
maybe_bypass_e164(Data, Call) ->
    case kz_json:is_true(<<"bypass_e164">>, Data) of
        'true' -> get_original_request_user(Call);
        'false' -> kapps_call:request_user(Call)
    end.

-spec get_original_request_user(kapps_call:call()) -> kz_term:ne_binary().
get_original_request_user(Call) ->
    Request = kapps_call:request(Call),
    [RequestUser, _] = binary:split(Request, <<"@">>),
    RequestUser.

-spec get_sip_headers(kz_json:object(), kapps_call:call()) -> kz_term:api_object().
get_sip_headers(Data, Call) ->
    Routines = [fun(J) -> maybe_emit_account_id(J, Data, Call) end
               ,fun(J) -> maybe_include_diversions(J, Call) end
               ],
    AuthEndCSH = case kz_endpoint:get(Call) of
                     {'ok', AuthorizingEndpoint} ->
                         kzd_devices:custom_sip_headers_outbound(AuthorizingEndpoint, kz_json:new());
                     _ -> kz_json:new()
                 end,
    CSH = kz_json:get_json_value(<<"custom_sip_headers">>, Data),
    Headers = maybe_merge(AuthEndCSH, CSH),

    JObj = lists:foldl(fun(F, J) -> F(J) end, Headers, Routines),
    case kz_term:is_empty(JObj) of
        'true' -> 'undefined';
        'false' -> JObj
    end.

-spec maybe_merge(kz_term:api_object(), kz_term:api_object()) -> kz_json:object().
maybe_merge(JObj1, 'undefined') -> JObj1;
maybe_merge('undefined', JObj2) -> JObj2;
maybe_merge(JObj1, JObj2) -> kz_json:merge_jobjs(JObj1, JObj2).

-spec maybe_include_diversions(kz_json:object(), kapps_call:call()) ->
                                      kz_json:object().
maybe_include_diversions(JObj, Call) ->
    case kapps_call:custom_sip_header(<<"Diversions">>, Call) of
        'undefined' -> JObj;
        Diversions ->
            kz_json:set_value(<<"Diversions">>, Diversions, JObj)
    end.

-spec maybe_emit_account_id(kz_json:object(), kz_json:object(), kapps_call:call()) ->
                                   kz_json:object().
maybe_emit_account_id(JObj, Data, Call) ->
    Default = kapps_config:get_is_true(?RES_CONFIG_CAT, <<"default_emit_account_id">>, 'false'),
    case kz_json:is_true(<<"emit_account_id">>, Data, Default) of
        'false' -> JObj;
        'true' ->
            kz_json:set_value(<<"X-Account-ID">>, kapps_call:account_id(Call), JObj)
    end.

-spec get_ignore_early_media(kz_json:object()) -> kz_term:api_binary().
get_ignore_early_media(Data) ->
    kz_term:to_binary(kz_json:is_true(<<"ignore_early_media">>, Data, 'false')).

-spec get_t38_enabled(kapps_call:call()) -> kz_term:api_boolean().
get_t38_enabled(Call) ->
    case kz_endpoint:get(Call) of
        {'ok', JObj} -> kz_json:is_true([<<"media">>, <<"fax_option">>], JObj);
        {'error', _} -> 'undefined'
    end.

-spec get_flags(kz_json:object(), kapps_call:call()) -> kz_term:api_binaries().
get_flags(Data, Call) ->
    Flags = kz_attributes:get_flags(?APP_NAME, Call),
    Routines = [fun get_flow_flags/3
               ,fun get_flow_dynamic_flags/3
               ],
    lists:foldl(fun(F, A) -> F(Data, Call, A) end, Flags, Routines).

-spec get_flow_flags(kz_json:object(), kapps_call:call(), kz_term:ne_binaries()) ->
                            kz_term:ne_binaries().
get_flow_flags(Data, _Call, Flags) ->
    case kz_json:get_list_value(<<"outbound_flags">>, Data, []) of
        [] -> Flags;
        FlowFlags -> FlowFlags ++ Flags
    end.

-spec get_flow_dynamic_flags(kz_json:object(), kapps_call:call(), kz_term:ne_binaries()) ->
                                    kz_term:ne_binaries().
get_flow_dynamic_flags(Data, Call, Flags) ->
    case kz_json:get_list_value(<<"dynamic_flags">>, Data) of
        'undefined' -> Flags;
        DynamicFlags -> kz_attributes:process_dynamic_flags(DynamicFlags, Flags, Call)
    end.

-spec get_inception(kapps_call:call()) -> kz_term:api_binary().
get_inception(Call) ->
    kz_json:get_value(<<"Inception">>, kapps_call:custom_channel_vars(Call)).


-spec get_endpoint(kapps_call:call()) -> kz_endpoint:std_return().
get_endpoint(Call) ->
    AuthId = kapps_call:authorizing_id(Call),
    EndpointId = kapps_call:kvs_fetch(?RESTRICTED_ENDPOINT_KEY, AuthId, Call),
    kz_endpoint:get(EndpointId, kapps_call:account_db(Call)).

%%------------------------------------------------------------------------------
%% @doc Consume Erlang messages and return on offnet response
%% @end
%%------------------------------------------------------------------------------
-spec wait_for_stepswitch(kapps_call:call()) -> {kz_term:ne_binary(), kz_term:api_binary()}.
wait_for_stepswitch(Call) ->
    case kapps_call_command:receive_event(?DEFAULT_EVENT_WAIT, 'true') of
        {'ok', JObj} ->
            case kz_util:get_event_type(JObj) of
                {<<"resource">>, <<"offnet_resp">>} ->
                    {kz_call_event:response_message(JObj)
                    ,kz_call_event:response_code(JObj)
                    };
                {<<"call_event">>, <<"CHANNEL_DESTROY">>} ->
                    handle_channel_destroy(JObj);
                {_Cat, _Evt} ->
                    wait_for_stepswitch(Call)
            end;
        _ -> wait_for_stepswitch(Call)
    end.

handle_channel_destroy(JObj) ->
    handle_channel_destroy(kz_json:get_value(<<"Channel-Name">>, JObj), JObj).

handle_channel_destroy(<<"loopback", _/binary>>, _JObj) ->
    {<<"TRANSFER">>, 'ok'};
handle_channel_destroy(_, JObj) ->
    {kz_call_event:hangup_cause(JObj), kz_call_event:hangup_code(JObj)}.

-spec get_privacy_prefs(kapps_call:call()) -> kz_term:proplist().
get_privacy_prefs(Call) ->
    case use_endpoint_prefs(Call) of
        'true' -> check_inception(Call);
        'false'  -> []
    end.

-spec use_endpoint_prefs(kapps_call:call()) -> boolean().
use_endpoint_prefs(Call) ->
    %% only overwrite the ccvs if privacy has not been set by cf_privacy
    %% or if the call has been configured to overwrite cf_privacy settings
    not kz_privacy:has_flags(kapps_call:custom_channel_vars(Call))
        orelse kapps_call:kvs_fetch(<<"use_endpoint_privacy">>, 'false', Call).

-spec check_inception(kapps_call:call()) -> kz_term:proplist().
check_inception(Call) ->
    lager:debug("checking inception of call"),
    case kapps_call:inception(Call) of
        'undefined' -> get_privacy_prefs_from_endpoint(Call);
        _Else -> []
    end.

-spec get_privacy_prefs_from_endpoint(kapps_call:call()) -> kz_term:proplist().
get_privacy_prefs_from_endpoint(Call) ->
    get_privacy_prefs_from_endpoint(Call, get_endpoint(Call)).

-spec get_privacy_prefs_from_endpoint(kapps_call:call(), {'ok', kz_json:object()} | {'error', any()}) -> kz_term:proplist().
get_privacy_prefs_from_endpoint(Call, {'ok', Endpoint}) ->
    lager:debug("call is outbound, checking caller_id_outbound_privacy value"),
    case kz_json:get_value([<<"caller_id_options">>, <<"outbound_privacy">>], Endpoint) of
        'undefined' -> [];
        %% can't call kapps_call_command:privacy/2 with Mode = <<"none">>
        <<"none">>=NoneMode ->
            cf_util:ccvs_by_privacy_mode(NoneMode);
        Mode ->
            kapps_call_command:privacy(Mode, Call),
            cf_util:ccvs_by_privacy_mode(Mode)
    end;
get_privacy_prefs_from_endpoint(_Call, {'error', _E}) ->
    lager:debug("authorizing endpoint has no privacy settings"),
    [].
