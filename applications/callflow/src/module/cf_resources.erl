%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz INC
%%% @doc
%%% "data":{
%%%   "to_did":"+14155550987" // statically dial DID
%%%   ,"media":"media_id"
%%%   ,"ringback":"ringback_id"
%%%   ,"format_from_did":boolean()
%%%   ,"timeout":integer()
%%%   ,"do_not_normalize":boolean()
%%%   ,"bypass_e164":boolean()
%%%   ,"from_uri_realm":"realm.com"
%%%   ,"caller_id_type":"external" // can use custom caller id properties on endpoints
%%%   ,"use_local_resources":boolean()
%%%   ,"hunt_account_id":"account_3" // use account_3's local carriers instead of current account
%%%   ,"emit_account_id":boolean() // puts account id in SIP header X-Account-ID
%%%   ,"custom_sip_headers:{"header":"value",...}
%%%   ,"ignore_early_media":boolean()
%%%   ,"outbound_flags":["flag_1","flag_2"] // used to match flags on carrier docs
%%% }
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(cf_resources).
-behaviour(gen_cf_action).

-export([handle/2]).

-include("callflow.hrl").
-include_lib("kazoo_amqp/include/kapi_offnet_resource.hrl").

-define(DEFAULT_EVENT_WAIT, 10000).
-define(RES_CONFIG_CAT, <<?CF_CONFIG_CAT/binary, ".resources">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module
%% @end
%%--------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    UpdatedCall = update_ccvs(Call),
    'ok' = kapi_offnet_resource:publish_req(build_offnet_request(Data, UpdatedCall)),
    case wait_for_stepswitch(UpdatedCall) of
        {<<"SUCCESS">>, _} ->
            lager:info("completed successful offnet request"),
            cf_exe:stop(UpdatedCall);
        {<<"TRANSFER">>, _} ->
            lager:info("completed successful offnet request"),
            cf_exe:transfer(UpdatedCall);
        {<<"NORMAL_CLEARING">>, <<"sip:200">>} ->
            lager:info("completed successful offnet request"),
            cf_exe:stop(UpdatedCall);
        {<<"NORMAL_CLEARING">>, 'undefined'} ->
            lager:info("completed successful offnet request"),
            cf_exe:stop(UpdatedCall);
        {Cause, Code} -> handle_bridge_failure(Cause, Code, UpdatedCall)
    end.

-spec handle_bridge_failure(api_binary(), api_binary(), kapps_call:call()) -> 'ok'.
handle_bridge_failure(Cause, Code, Call) ->
    lager:info("offnet request error, attempting to find failure branch for ~s:~s", [Code, Cause]),
    case cf_util:handle_bridge_failure(Cause, Code, Call) of
        'ok' -> lager:debug("found bridge failure child");
        'not_found' ->
            cf_util:send_default_response(Cause, Call),
            cf_exe:stop(Call)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec build_offnet_request(kz_json:object(), kapps_call:call()) -> kz_proplist().
build_offnet_request(Data, Call) ->
    {ECIDNum, ECIDName} = kz_attributes:caller_id(<<"emergency">>, Call),
    {CIDNumber, CIDName} = get_caller_id(Data, Call),
    props:filter_undefined(
      [{?KEY_ACCOUNT_ID, kapps_call:account_id(Call)}
      ,{?KEY_ACCOUNT_REALM, kapps_call:from_realm(Call)}
      ,{?KEY_APPLICATION_NAME, ?APPLICATION_BRIDGE}
      ,{?KEY_BYPASS_E164, get_bypass_e164(Data)}
      ,{?KEY_B_LEG_EVENTS, [<<"DTMF">>]}
      ,{?KEY_CALL_ID, cf_exe:callid(Call)}
      ,{?KEY_CCVS, get_channel_vars(Call)}
      ,{?KEY_CONTROL_QUEUE, cf_exe:control_queue(Call)}
      ,{?KEY_CSHS, get_sip_headers(Data, Call)}
      ,{?KEY_E_CALLER_ID_NAME, ECIDName}
      ,{?KEY_E_CALLER_ID_NUMBER, ECIDNum}
      ,{?KEY_FLAGS, get_flags(Data, Call)}
      ,{?KEY_FORMAT_FROM_URI, kz_json:is_true(<<"format_from_uri">>, Data)}
      ,{?KEY_FROM_URI_REALM, get_from_uri_realm(Data, Call)}
      ,{?KEY_HUNT_ACCOUNT_ID, get_hunt_account_id(Data, Call)}
      ,{?KEY_IGNORE_EARLY_MEDIA, get_ignore_early_media(Data)}
      ,{?KEY_INCEPTION, get_inception(Call)}
      ,{?KEY_MEDIA, kz_json:get_first_defined([<<"media">>, <<"Media">>], Data)}
      ,{?KEY_MSG_ID, kz_util:rand_hex_binary(6)}
      ,{?KEY_OUTBOUND_CALLER_ID_NAME, CIDName}
      ,{?KEY_OUTBOUND_CALLER_ID_NUMBER, CIDNumber}
      ,{?KEY_PRESENCE_ID, kz_attributes:presence_id(Call)}
      ,{?KEY_RESOURCE_TYPE, ?RESOURCE_TYPE_AUDIO}
      ,{?KEY_RINGBACK, kz_json:get_value(<<"ringback">>, Data)}
      ,{?KEY_T38_ENABLED, get_t38_enabled(Call)}
      ,{?KEY_TIMEOUT, kz_json:get_value(<<"timeout">>, Data)}
      ,{?KEY_TO_DID, get_to_did(Data, Call)}
       | kz_api:default_headers(cf_exe:queue_name(Call), ?APP_NAME, ?APP_VERSION)
      ]).

-spec get_channel_vars(kapps_call:call()) -> kz_json:object().
get_channel_vars(Call) ->
    AuthId = kapps_call:authorizing_id(Call),
    EndpointId = kapps_call:kvs_fetch(?RESTRICTED_ENDPOINT_KEY, AuthId, Call),
    kz_json:from_list(
      props:filter_undefined(get_channel_vars(EndpointId, Call))
     ).

-spec get_channel_vars(api_binary(), kapps_call:call()) -> kz_proplist().
get_channel_vars('undefined', Call) -> [maybe_require_ignore_early_media(Call)];
get_channel_vars(EndpointId, Call) ->
    case kz_endpoint:get(EndpointId, kapps_call:account_db(Call)) of
        {'ok', Endpoint} ->
            [{<<"Authorizing-ID">>, EndpointId}
            ,{<<"Owner-ID">>, kz_json:get_value(<<"owner_id">>, Endpoint)}
            ,maybe_require_ignore_early_media(Call)
            ];
        {'error', _} -> [maybe_require_ignore_early_media(Call)]
    end.

-spec maybe_require_ignore_early_media(kapps_call:call()) -> {ne_binary(), api_binary()}.
maybe_require_ignore_early_media(Call) ->
    {<<"Require-Ignore-Early-Media">>, kapps_call:custom_channel_var(<<"Require-Ignore-Early-Media">>, Call)}.

-spec get_bypass_e164(kz_json:object()) -> boolean().
get_bypass_e164(Data) ->
    kz_json:is_true(<<"do_not_normalize">>, Data)
        orelse kz_json:is_true(<<"bypass_e164">>, Data).

-spec get_from_uri_realm(kz_json:object(), kapps_call:call()) -> api_binary().
get_from_uri_realm(Data, Call) ->
    case kz_json:get_ne_value(<<"from_uri_realm">>, Data) of
        'undefined' -> maybe_get_call_from_realm(Call);
        Realm -> Realm
    end.

-spec maybe_get_call_from_realm(kapps_call:call()) -> api_binary().
maybe_get_call_from_realm(Call) ->
    case kapps_call:from_realm(Call) of
        <<"norealm">> -> get_account_realm(Call);
        Realm -> Realm
    end.

-spec update_ccvs(kapps_call:call()) -> kapps_call:call().
update_ccvs(Call) ->
    Props = props:filter_undefined(
              [{<<"Bridge-Generate-Comfort-Noise">>, maybe_set_bridge_generate_comfort_noise(Call)}]
             ),
    kapps_call:set_custom_channel_vars(Props, Call).

-spec maybe_set_bridge_generate_comfort_noise(kapps_call:call()) -> api_binary().
maybe_set_bridge_generate_comfort_noise(Call) ->
    case kz_endpoint:get(Call) of
        {'ok', Endpoint} ->
            maybe_has_comfort_noise_option_enabled(Endpoint);
        {'error', _E} ->
            lager:debug("error acquiring originating endpoint information"),
            'undefined'
    end.

-spec maybe_has_comfort_noise_option_enabled(kz_json:object()) -> api_binary().
maybe_has_comfort_noise_option_enabled(Endpoint) ->
    kz_json:get_ne_binary_value([<<"media">>, <<"bridge_generate_comfort_noise">>], Endpoint).

-spec get_account_realm(kapps_call:call()) -> api_binary().
get_account_realm(Call) ->
    case kz_account:fetch(kapps_call:account_id(Call)) of
        {'ok', JObj} -> kz_account:realm(JObj);
        {'error', _} -> 'undefined'
    end.

-spec get_caller_id(kz_json:object(), kapps_call:call()) -> {api_binary(), api_binary()}.
get_caller_id(Data, Call) ->
    Type = kz_json:get_value(<<"caller_id_type">>, Data, <<"external">>),
    kz_attributes:caller_id(Type, Call).

-spec get_hunt_account_id(kz_json:object(), kapps_call:call()) -> api_binary().
get_hunt_account_id(Data, Call) ->
    case kz_json:is_true(<<"use_local_resources">>, Data, 'true') of
        'false' -> 'undefined';
        'true' ->
            AccountId = kapps_call:account_id(Call),
            kz_json:get_value(<<"hunt_account_id">>, Data, AccountId)
    end.

-spec get_to_did(kz_json:object(), kapps_call:call()) -> ne_binary().
get_to_did(Data, Call) ->
    case kz_json:get_value(<<"to_did">>, Data) of
        'undefined' -> get_request_did(Data, Call);
        ToDID -> ToDID
    end.

-spec get_request_did(kz_json:object(), kapps_call:call()) -> ne_binary().
get_request_did(Data, Call) ->
    case kz_json:is_true(<<"do_not_normalize">>, Data) of
        'true' -> get_original_request_user(Call);
        'false' ->
            case kz_endpoint:get(Call) of
                {'error', _ } -> maybe_bypass_e164(Data, Call);
                {'ok', Endpoint} ->
                    maybe_apply_dialplan(Endpoint, Data, Call)
            end
    end.

-spec maybe_apply_dialplan(kz_json:object(), kz_json:object(), kapps_call:call()) -> ne_binary().
maybe_apply_dialplan(Endpoint, Data, Call) ->
    case kz_json:get_value(<<"dial_plan">>, Endpoint, []) of
        [] -> maybe_bypass_e164(Data, Call);
        DialPlan ->
            Request = kapps_call:request(Call),
            [RequestUser, _] = binary:split(Request, <<"@">>),
            cf_util:apply_dialplan(RequestUser, DialPlan)
    end.

-spec maybe_bypass_e164(kz_json:object(), kapps_call:call()) -> ne_binary().
maybe_bypass_e164(Data, Call) ->
    case kz_json:is_true(<<"bypass_e164">>, Data) of
        'true' -> get_original_request_user(Call);
        'false' -> kapps_call:request_user(Call)
    end.

-spec get_original_request_user(kapps_call:call()) -> ne_binary().
get_original_request_user(Call) ->
    Request = kapps_call:request(Call),
    [RequestUser, _] = binary:split(Request, <<"@">>),
    RequestUser.

-spec get_sip_headers(kz_json:object(), kapps_call:call()) -> api_object().
get_sip_headers(Data, Call) ->
    Routines = [fun(J) -> maybe_emit_account_id(J, Data, Call) end
               ,fun(J) -> maybe_include_diversions(J, Call) end
               ],
    AuthEndCSH = case kz_endpoint:get(Call) of
                     {'ok', AuthorizingEndpoint} ->
                         kz_device:custom_sip_headers_outbound(AuthorizingEndpoint, kz_json:new());
                     _ -> kz_json:new()
                 end,
    CSH = kz_json:get_value(<<"custom_sip_headers">>, Data, kz_json:new()),
    Headers = kz_json:merge_jobjs(AuthEndCSH, CSH),

    JObj = lists:foldl(fun(F, J) -> F(J) end, Headers, Routines),
    case kz_util:is_empty(JObj) of
        'true' -> 'undefined';
        'false' -> JObj
    end.

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

-spec get_ignore_early_media(kz_json:object()) -> api_binary().
get_ignore_early_media(Data) ->
    kz_util:to_binary(kz_json:is_true(<<"ignore_early_media">>, Data, 'false')).

-spec get_t38_enabled(kapps_call:call()) -> api_boolean().
get_t38_enabled(Call) ->
    case kz_endpoint:get(Call) of
        {'ok', JObj} -> kz_json:is_true([<<"media">>, <<"fax_option">>], JObj);
        {'error', _} -> 'undefined'
    end.

-spec get_flags(kz_json:object(), kapps_call:call()) -> api_binaries().
get_flags(Data, Call) ->
    Routines = [fun maybe_get_endpoint_flags/3
               ,fun get_flow_flags/3
               ,fun get_account_flags/3
               ,fun get_flow_dynamic_flags/3
               ,fun maybe_get_endpoint_dynamic_flags/3
               ,fun get_account_dynamic_flags/3
               ],
    lists:foldl(fun(F, A) -> F(Data, Call, A) end, [], Routines).

-spec maybe_get_endpoint_flags(kz_json:object(), kapps_call:call(), ne_binaries()) ->
                                      ne_binaries().
maybe_get_endpoint_flags(_Data, Call, Flags) ->
    case kz_endpoint:get(Call) of
        {'error', _} -> Flags;
        {'ok', Endpoint} ->
            get_endpoint_flags(Flags, Endpoint)
    end.

-spec get_endpoint_flags(ne_binaries(), kz_json:object()) ->
                                ne_binaries().
get_endpoint_flags(Flags, Endpoint) ->
    case kz_json:get_value(<<"outbound_flags">>, Endpoint) of
        'undefined' -> Flags;
        EndpointFlags -> EndpointFlags ++ Flags
    end.

-spec get_flow_flags(kz_json:object(), kapps_call:call(), ne_binaries()) ->
                            ne_binaries().
get_flow_flags(Data, _Call, Flags) ->
    case kz_json:get_value(<<"outbound_flags">>, Data) of
        'undefined' -> Flags;
        FlowFlags -> FlowFlags ++ Flags
    end.

-spec get_account_flags(kz_json:object(), kapps_call:call(), ne_binaries()) ->
                               ne_binaries().
get_account_flags(_Data, Call, Flags) ->
    AccountId = kapps_call:account_id(Call),
    case kz_account:fetch(AccountId) of
        {'ok', AccountJObj} ->
            AccountFlags = kz_json:get_value(<<"outbound_flags">>, AccountJObj, []),
            AccountFlags ++ Flags;
        {'error', _E} ->
            lager:error("not applying account outbound flags for ~s: ~p"
                       ,[AccountId, _E]
                       ),
            Flags
    end.

-spec get_flow_dynamic_flags(kz_json:object(), kapps_call:call(), ne_binaries()) ->
                                    ne_binaries().
get_flow_dynamic_flags(Data, Call, Flags) ->
    case kz_json:get_value(<<"dynamic_flags">>, Data) of
        'undefined' -> Flags;
        DynamicFlags -> process_dynamic_flags(DynamicFlags, Flags, Call)
    end.

-spec maybe_get_endpoint_dynamic_flags(kz_json:object(), kapps_call:call(), ne_binaries()) ->
                                              ne_binaries().
maybe_get_endpoint_dynamic_flags(_Data, Call, Flags) ->
    case kz_endpoint:get(Call) of
        {'error', _} -> Flags;
        {'ok', Endpoint} ->
            get_endpoint_dynamic_flags(Call, Flags, Endpoint)
    end.

-spec get_endpoint_dynamic_flags(kapps_call:call(), ne_binaries(), kz_json:object()) ->
                                        ne_binaries().
get_endpoint_dynamic_flags(Call, Flags, Endpoint) ->
    case kz_json:get_value(<<"dynamic_flags">>, Endpoint) of
        'undefined' -> Flags;
        DynamicFlags ->
            process_dynamic_flags(DynamicFlags, Flags, Call)
    end.

-spec get_account_dynamic_flags(kz_json:object(), kapps_call:call(), ne_binaries()) ->
                                       ne_binaries().
get_account_dynamic_flags(_, Call, Flags) ->
    DynamicFlags = kapps_account_config:get(kapps_call:account_id(Call)
                                           ,<<"callflow">>
                                           ,<<"dynamic_flags">>
                                           ,[]
                                           ),
    process_dynamic_flags(DynamicFlags, Flags, Call).

-spec process_dynamic_flags(ne_binaries(), ne_binaries(), kapps_call:call()) ->
                                   ne_binaries().
process_dynamic_flags([], Flags, _) -> Flags;
process_dynamic_flags([<<"zone">>|DynamicFlags], Flags, Call) ->
    Zone = kz_util:to_binary(kz_nodes:local_zone()),
    lager:debug("adding dynamic flag ~s", [Zone]),
    process_dynamic_flags(DynamicFlags, [Zone|Flags], Call);
process_dynamic_flags([DynamicFlag|DynamicFlags], Flags, Call) ->
    case is_flag_exported(DynamicFlag) of
        'false' -> process_dynamic_flags(DynamicFlags, Flags, Call);
        'true' ->
            Fun = kz_util:to_atom(DynamicFlag),
            process_dynamic_flags(DynamicFlags, [kapps_call:Fun(Call)|Flags], Call)
    end.

-spec is_flag_exported(ne_binary()) -> boolean().
is_flag_exported(Flag) ->
    is_flag_exported(Flag, kapps_call:module_info('exports')).

is_flag_exported(_, []) -> 'false';
is_flag_exported(Flag, [{F, 1}|Funs]) ->
    kz_util:to_binary(F) =:= Flag
        orelse is_flag_exported(Flag, Funs);
is_flag_exported(Flag, [_|Funs]) -> is_flag_exported(Flag, Funs).

-spec get_inception(kapps_call:call()) -> api_binary().
get_inception(Call) ->
    kz_json:get_value(<<"Inception">>, kapps_call:custom_channel_vars(Call)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Consume Erlang messages and return on offnet response
%% @end
%%--------------------------------------------------------------------
-spec wait_for_stepswitch(kapps_call:call()) -> {ne_binary(), api_binary()}.
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
