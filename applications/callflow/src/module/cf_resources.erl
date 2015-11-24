%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2015, 2600Hz INC
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

-include("../callflow.hrl").
-include_lib("whistle/include/wapi_offnet_resource.hrl").

-export([handle/2]).

-define(DEFAULT_EVENT_WAIT, 10000).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module
%% @end
%%--------------------------------------------------------------------
-spec handle(wh_json:object(), whapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    UpdatedCall = update_ccvs(Call),
    'ok' = wapi_offnet_resource:publish_req(build_offnet_request(Data, UpdatedCall)),
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
        {Cause, Code} -> handle_bridge_failure(Cause, Code, UpdatedCall)
    end.

-spec handle_bridge_failure(api_binary(), api_binary(), whapps_call:call()) -> 'ok'.
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
-spec build_offnet_request(wh_json:object(), whapps_call:call()) -> wh_proplist().
build_offnet_request(Data, Call) ->
    {ECIDNum, ECIDName} = cf_attributes:caller_id(<<"emergency">>, Call),
    {CIDNumber, CIDName} = get_caller_id(Data, Call),
    props:filter_undefined(
      [{?KEY_ACCOUNT_ID, whapps_call:account_id(Call)}
       ,{?KEY_ACCOUNT_REALM, whapps_call:from_realm(Call)}
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
       ,{?KEY_FORMAT_FROM_URI, wh_json:is_true(<<"format_from_uri">>, Data)}
       ,{?KEY_FROM_URI_REALM, get_from_uri_realm(Data, Call)}
       ,{?KEY_HUNT_ACCOUNT_ID, get_hunt_account_id(Data, Call)}
       ,{?KEY_IGNORE_EARLY_MEDIA, get_ignore_early_media(Data)}
       ,{?KEY_INCEPTION, get_inception(Call)}
       ,{?KEY_MEDIA, wh_json:get_first_defined([<<"media">>, <<"Media">>], Data)}
       ,{?KEY_MSG_ID, wh_util:rand_hex_binary(6)}
       ,{?KEY_OUTBOUND_CALLER_ID_NAME, CIDName}
       ,{?KEY_OUTBOUND_CALLER_ID_NUMBER, CIDNumber}
       ,{?KEY_PRESENCE_ID, cf_attributes:presence_id(Call)}
       ,{?KEY_RESOURCE_TYPE, ?RESOURCE_TYPE_AUDIO}
       ,{?KEY_RINGBACK, wh_json:get_value(<<"ringback">>, Data)}
       ,{?KEY_T38_ENABLED, get_t38_enabled(Call)}
       ,{?KEY_TIMEOUT, wh_json:get_value(<<"timeout">>, Data)}
       ,{?KEY_TO_DID, get_to_did(Data, Call)}
       | wh_api:default_headers(cf_exe:queue_name(Call), ?APP_NAME, ?APP_VERSION)
      ]).

-spec get_channel_vars(whapps_call:call()) -> wh_json:object().
get_channel_vars(Call) ->
    wh_json:from_list(
      [{<<"Authorizing-ID">>, whapps_call:authorizing_id(Call)}]
     ).

-spec get_bypass_e164(wh_json:object()) -> boolean().
get_bypass_e164(Data) ->
    wh_json:is_true(<<"do_not_normalize">>, Data)
        orelse wh_json:is_true(<<"bypass_e164">>, Data).

-spec get_from_uri_realm(wh_json:object(), whapps_call:call()) -> api_binary().
get_from_uri_realm(Data, Call) ->
    case wh_json:get_ne_value(<<"from_uri_realm">>, Data) of
        'undefined' -> maybe_get_call_from_realm(Call);
        Realm -> Realm
    end.

-spec maybe_get_call_from_realm(whapps_call:call()) -> api_binary().
maybe_get_call_from_realm(Call) ->
    case whapps_call:from_realm(Call) of
        <<"norealm">> -> get_account_realm(Call);
        Realm -> Realm
    end.

-spec update_ccvs(whapps_call:call()) -> whapps_call:call().
update_ccvs(Call) ->
    Props = props:filter_undefined(
              [{<<"Bridge-Generate-Comfort-Noise">>, maybe_set_bridge_generate_comfort_noise(Call)}]
             ),
    whapps_call:set_custom_channel_vars(Props, Call).

-spec maybe_set_bridge_generate_comfort_noise(whapps_call:call()) -> api_binary().
maybe_set_bridge_generate_comfort_noise(Call) ->
    case cf_endpoint:get(Call) of
        {'ok', Endpoint} ->
            maybe_has_comfort_noise_option_enabled(Endpoint);
        {'error', _E} ->
            lager:debug("error acquiring originating endpoint information"),
            'undefined'
    end.

-spec maybe_has_comfort_noise_option_enabled(wh_json:object()) -> api_binary().
maybe_has_comfort_noise_option_enabled(Endpoint) ->
    wh_json:get_ne_binary_value([<<"media">>, <<"bridge_generate_comfort_noise">>], Endpoint).

-spec get_account_realm(whapps_call:call()) -> api_binary().
get_account_realm(Call) ->
    case kz_account:fetch(whapps_call:account_id(Call)) of
        {'ok', JObj} -> kz_account:realm(JObj);
        {'error', _} -> 'undefined'
    end.

-spec get_caller_id(wh_json:object(), whapps_call:call()) -> {api_binary(), api_binary()}.
get_caller_id(Data, Call) ->
    Type = wh_json:get_value(<<"caller_id_type">>, Data, <<"external">>),
    cf_attributes:caller_id(Type, Call).

-spec get_hunt_account_id(wh_json:object(), whapps_call:call()) -> api_binary().
get_hunt_account_id(Data, Call) ->
    case wh_json:is_true(<<"use_local_resources">>, Data, 'true') of
        'false' -> 'undefined';
        'true' ->
            AccountId = whapps_call:account_id(Call),
            wh_json:get_value(<<"hunt_account_id">>, Data, AccountId)
    end.

-spec get_to_did(wh_json:object(), whapps_call:call()) -> ne_binary().
get_to_did(Data, Call) ->
    case wh_json:get_value(<<"to_did">>, Data) of
        'undefined' -> get_request_did(Data, Call);
        ToDID -> ToDID
    end.

-spec get_request_did(wh_json:object(), whapps_call:call()) -> ne_binary().
get_request_did(Data, Call) ->
    case wh_json:is_true(<<"do_not_normalize">>, Data) of
        'true' -> get_original_request_user(Call);
        'false' ->
            case cf_endpoint:get(Call) of
                {'error', _ } -> maybe_bypass_e164(Data, Call);
                {'ok', Endpoint} ->
                    maybe_apply_dialplan(Endpoint, Data, Call)
            end
    end.

-spec maybe_apply_dialplan(wh_json:object(), wh_json:object(), whapps_call:call()) -> ne_binary().
maybe_apply_dialplan(Endpoint, Data, Call) ->
    case wh_json:get_value(<<"dial_plan">>, Endpoint, []) of
        [] -> maybe_bypass_e164(Data, Call);
        DialPlan ->
            Request = whapps_call:request(Call),
            [RequestUser, _] = binary:split(Request, <<"@">>),
            cf_util:apply_dialplan(RequestUser, DialPlan)
    end.

-spec maybe_bypass_e164(wh_json:object(), whapps_call:call()) -> ne_binary().
maybe_bypass_e164(Data, Call) ->
    case wh_json:is_true(<<"bypass_e164">>, Data) of
        'true' -> get_original_request_user(Call);
        'false' -> whapps_call:request_user(Call)
    end.

-spec get_original_request_user(whapps_call:call()) -> ne_binary().
get_original_request_user(Call) ->
    Request = whapps_call:request(Call),
    [RequestUser, _] = binary:split(Request, <<"@">>),
    RequestUser.

-spec get_sip_headers(wh_json:object(), whapps_call:call()) -> api_object().
get_sip_headers(Data, Call) ->
    Routines = [fun(J) ->
                        case wh_json:is_true(<<"emit_account_id">>, Data) of
                            'false' -> J;
                            'true' ->
                                wh_json:set_value(<<"X-Account-ID">>, whapps_call:account_id(Call), J)
                        end
                end
               ],
    CustomHeaders = wh_json:get_value(<<"custom_sip_headers">>, Data, wh_json:new()),

    Diversions = whapps_call:custom_sip_header(<<"Diversions">>, Call),

    Headers = wh_json:set_value(<<"Diversions">>, Diversions, CustomHeaders),

    JObj = lists:foldl(fun(F, J) -> F(J) end, Headers, Routines),
    case wh_util:is_empty(JObj) of
        'true' -> 'undefined';
        'false' -> JObj
    end.

-spec get_ignore_early_media(wh_json:object()) -> api_binary().
get_ignore_early_media(Data) ->
    wh_util:to_binary(wh_json:is_true(<<"ignore_early_media">>, Data, 'false')).

-spec get_t38_enabled(whapps_call:call()) -> api_boolean().
get_t38_enabled(Call) ->
    case cf_endpoint:get(Call) of
        {'ok', JObj} -> wh_json:is_true([<<"media">>, <<"fax_option">>], JObj);
        {'error', _} -> 'undefined'
    end.

-spec get_flags(wh_json:object(), whapps_call:call()) -> api_binaries().
get_flags(Data, Call) ->
    Routines = [fun maybe_get_endpoint_flags/3
                ,fun get_flow_flags/3
                ,fun get_account_flags/3
                ,fun get_flow_dynamic_flags/3
                ,fun maybe_get_endpoint_dynamic_flags/3
                ,fun get_account_dynamic_flags/3
               ],
    lists:foldl(fun(F, A) -> F(Data, Call, A) end, [], Routines).

-spec maybe_get_endpoint_flags(wh_json:object(), whapps_call:call(), ne_binaries()) ->
                                      ne_binaries().
maybe_get_endpoint_flags(_Data, Call, Flags) ->
    case cf_endpoint:get(Call) of
        {'error', _} -> Flags;
        {'ok', Endpoint} ->
            get_endpoint_flags(Flags, Endpoint)
    end.

-spec get_endpoint_flags(ne_binaries(), wh_json:object()) ->
                                ne_binaries().
get_endpoint_flags(Flags, Endpoint) ->
    case wh_json:get_value(<<"outbound_flags">>, Endpoint) of
        'undefined' -> Flags;
        EndpointFlags -> EndpointFlags ++ Flags
    end.

-spec get_flow_flags(wh_json:object(), whapps_call:call(), ne_binaries()) ->
                            ne_binaries().
get_flow_flags(Data, _Call, Flags) ->
    case wh_json:get_value(<<"outbound_flags">>, Data) of
        'undefined' -> Flags;
        FlowFlags -> FlowFlags ++ Flags
    end.

-spec get_account_flags(wh_json:object(), whapps_call:call(), ne_binaries()) ->
                               ne_binaries().
get_account_flags(_Data, Call, Flags) ->
    AccountId = whapps_call:account_id(Call),
    case kz_account:fetch(AccountId) of
        {'ok', AccountJObj} ->
            AccountFlags = wh_json:get_value(<<"outbound_flags">>, AccountJObj, []),
            AccountFlags ++ Flags;
        {'error', _E} ->
            lager:error("not applying account outbound flags for ~s: ~p"
                        ,[AccountId, _E]
                       ),
            Flags
    end.

-spec get_flow_dynamic_flags(wh_json:object(), whapps_call:call(), ne_binaries()) ->
                                    ne_binaries().
get_flow_dynamic_flags(Data, Call, Flags) ->
    case wh_json:get_value(<<"dynamic_flags">>, Data) of
        'undefined' -> Flags;
        DynamicFlags -> process_dynamic_flags(DynamicFlags, Flags, Call)
    end.

-spec maybe_get_endpoint_dynamic_flags(wh_json:object(), whapps_call:call(), ne_binaries()) ->
                                        ne_binaries().
maybe_get_endpoint_dynamic_flags(_Data, Call, Flags) ->
    case cf_endpoint:get(Call) of
        {'error', _} -> Flags;
        {'ok', Endpoint} ->
            get_endpoint_dynamic_flags(Call, Flags, Endpoint)
    end.

-spec get_endpoint_dynamic_flags(whapps_call:call(), ne_binaries(), wh_json:object()) ->
                                        ne_binaries().
get_endpoint_dynamic_flags(Call, Flags, Endpoint) ->
    case wh_json:get_value(<<"dynamic_flags">>, Endpoint) of
        'undefined' -> Flags;
        DynamicFlags ->
            process_dynamic_flags(DynamicFlags, Flags, Call)
    end.

-spec get_account_dynamic_flags(wh_json:object(), whapps_call:call(), ne_binaries()) ->
                                       ne_binaries().
get_account_dynamic_flags(_, Call, Flags) ->
    DynamicFlags = whapps_account_config:get(whapps_call:account_id(Call)
                                             ,<<"callflow">>
                                             ,<<"dynamic_flags">>
                                             ,[]
                                            ),
    process_dynamic_flags(DynamicFlags, Flags, Call).

-spec process_dynamic_flags(ne_binaries(), ne_binaries(), whapps_call:call()) ->
                                   ne_binaries().
process_dynamic_flags([], Flags, _) -> Flags;
process_dynamic_flags([DynamicFlag|DynamicFlags], Flags, Call) ->
    case is_flag_exported(DynamicFlag) of
        'false' -> process_dynamic_flags(DynamicFlags, Flags, Call);
        'true' ->
            Fun = wh_util:to_atom(DynamicFlag),
            process_dynamic_flags(DynamicFlags, [whapps_call:Fun(Call)|Flags], Call)
    end.

-spec is_flag_exported(ne_binary()) -> boolean().
is_flag_exported(Flag) ->
    is_flag_exported(Flag, whapps_call:module_info('exports')).

is_flag_exported(_, []) -> 'false';
is_flag_exported(Flag, [{F, 1}|Funs]) ->
    case wh_util:to_binary(F) =:= Flag of
        'true' -> 'true';
        'false' -> is_flag_exported(Flag, Funs)
    end;
is_flag_exported(Flag, [_|Funs]) -> is_flag_exported(Flag, Funs).

-spec get_inception(whapps_call:call()) -> api_binary().
get_inception(Call) ->
    wh_json:get_value(<<"Inception">>, whapps_call:custom_channel_vars(Call)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Consume Erlang messages and return on offnet response
%% @end
%%--------------------------------------------------------------------
-spec wait_for_stepswitch(whapps_call:call()) -> {ne_binary(), api_binary()}.
wait_for_stepswitch(Call) ->
    case whapps_call_command:receive_event(?DEFAULT_EVENT_WAIT, 'true') of
        {'ok', JObj} ->
            case wh_util:get_event_type(JObj) of
                {<<"resource">>, <<"offnet_resp">>} ->
                    {kz_call_event:response_message(JObj)
                     ,kz_call_event:response_code(JObj)
                    };
                {<<"call_event">>, <<"CHANNEL_BRIDGE">>} ->
                    maybe_start_offnet_metaflow(Call, kz_call_event:other_leg_call_id(JObj)),
                    wait_for_stepswitch(Call);
%%                 {<<"call_event">>, <<"CHANNEL_DESTROY">>} ->
%%                     handle_channel_destroy(JObj);
                _ -> wait_for_stepswitch(Call)
            end;
        _ -> wait_for_stepswitch(Call)
    end.

%% handle_channel_destroy(JObj) ->
%%     handle_channel_destroy(wh_json:get_value(<<"Channel-Name">>, JObj), JObj).
%%
%% handle_channel_destroy(<<"loopback", _/binary>>, _JObj) ->
%%     {<<"TRANSFER">>, 'ok'};
%% handle_channel_destroy(_, JObj) ->
%%     {kz_call_event:hangup_cause(JObj), kz_call_event:hangup_code(JObj)}.

-spec maybe_start_offnet_metaflow(whapps_call:call(), ne_binary()) -> 'ok'.
maybe_start_offnet_metaflow(Call, BridgedTo) ->
    HackedCall = hack_call(Call, BridgedTo),
    case cf_endpoint:get(HackedCall) of
        {'ok', EP} -> cf_util:maybe_start_metaflow(HackedCall, EP);
        _Else -> lager:debug("can't get endpoint for ~s", whapps_call:authorizing_id(HackedCall))
    end.

-spec hack_call(whapps_call:call(), ne_binary()) -> whapps_call:call().
hack_call(Call, BridgedTo) ->
    {AuthorizingType, AuthorizingId} = hack_authz(Call),
    Funs = [{fun whapps_call:set_call_id/2, BridgedTo}
            ,{fun whapps_call:set_other_leg_call_id/2, whapps_call:call_id(Call)}
            ,{fun whapps_call:set_authorizing_type/2, AuthorizingType}
            ,{fun whapps_call:set_authorizing_id/2, AuthorizingId}
           ],
    whapps_call:exec(Funs, Call).

-spec hack_authz(whapps_call:call()) -> {ne_binary(), ne_binary()}.
-spec hack_authz(whapps_call:call(), api_binary()) -> {ne_binary(), ne_binary()}.

hack_authz(Call) ->
    hack_authz(Call, whapps_call:authorizing_id(Call)).

hack_authz(Call, 'undefined') ->
    {<<"account">>, whapps_call:account_id(Call)};
hack_authz(Call, _) ->
    {whapps_call:authorizing_type(Call), whapps_call:authorizing_id(Call)}.
