%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2015, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   Luis Azedo
%%%-------------------------------------------------------------------
-module(cf_sms_resources).

-include("doodle.hrl").

-export([handle/2]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module
%% @end
%%--------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(Data, Call1) ->
    AccountId = kapps_call:account_id(Call1),
    Call = case kapps_call:custom_channel_var(<<"API-Call">>, 'false', Call1)
               andalso kapps_account_config:get_global(AccountId, ?CONFIG_CAT, <<"api_preserve_caller_id">>, 'true')
           of
               'true' -> doodle_util:set_caller_id(kapps_call:from_user(Call1), Call1);
               'false' -> doodle_util:set_caller_id(Data, Call1)
           end,
    case kapps_util:amqp_pool_request(
           build_offnet_request(Data, Call)
           ,fun kapi_offnet_resource:publish_req/1
           ,fun kapi_offnet_resource:resp_v/1
           ,30 * ?MILLISECONDS_IN_SECOND
          )
    of
        {'ok', Res} ->
            handle_result(Res, Call);
        {'error', E} ->
            lager:debug("error executing offnet action : ~p", [E]),
            doodle_util:maybe_reschedule_sms(doodle_util:set_flow_error(E, Call))
    end.

-spec handle_result(kz_json:object(), kapps_call:call()) -> 'ok'.
handle_result(JObj, Call) ->
    Message = kz_json:get_value(<<"Response-Message">>, JObj),
    Code = kz_json:get_value(<<"Response-Code">>, JObj),
    Response = kz_json:get_value(<<"Resource-Response">>, JObj),
    handle_result(Message, Code, Response, JObj, Call).

-spec handle_result(binary(), binary()
                    ,kz_json:object(), kz_json:object()
                    ,kapps_call:call()
                   ) -> 'ok'.
handle_result(_Message, <<"sip:200">>, Response, _JObj, Call1) ->
    Status = doodle_util:sms_status(Response),
    Call = doodle_util:set_flow_status(Status, Call1),
    handle_result_status(Call, Status);
handle_result(Message, Code, _Response, _JObj, Call) ->
    handle_bridge_failure(Message, Code, Call).

-spec handle_result_status(kapps_call:call(), ne_binary()) -> 'ok'.
handle_result_status(Call, <<"pending">>) ->
    doodle_util:maybe_reschedule_sms(Call);
handle_result_status(Call, _Status) ->
    lager:info("completed successful message to the device"),
    doodle_exe:continue(Call).

-spec handle_bridge_failure(api_binary(), api_binary(), kapps_call:call()) -> 'ok'.
handle_bridge_failure(Cause, Code, Call) ->
    lager:info("offnet request error, attempting to find failure branch for ~s:~s", [Code, Cause]),
    case doodle_util:handle_bridge_failure(Cause, Code, Call) of
        'ok' ->
            lager:debug("found bridge failure child");
        'not_found' ->
            doodle_util:maybe_reschedule_sms(Code, Cause, Call)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec build_offnet_request(kz_json:object(), kapps_call:call()) -> kz_proplist().
build_offnet_request(Data, Call) ->
    props:filter_undefined(
      [{<<"Resource-Type">>, <<"sms">>}
       ,{<<"Application-Name">>, <<"sms">>}
       ,{<<"Outbound-Caller-ID-Name">>, kapps_call:caller_id_name(Call)}
       ,{<<"Outbound-Caller-ID-Number">>, kapps_call:caller_id_number(Call)}
       ,{<<"Msg-ID">>, kz_util:rand_hex_binary(16)}
       ,{<<"Call-ID">>, doodle_exe:callid(Call)}
       ,{<<"Presence-ID">>, kz_attributes:presence_id(Call)}
       ,{<<"Account-ID">>, kapps_call:account_id(Call)}
       ,{<<"Account-Realm">>, kapps_call:from_realm(Call)}
       ,{<<"Timeout">>, kz_json:get_value(<<"timeout">>, Data)}
       ,{<<"Format-From-URI">>, kz_json:is_true(<<"format_from_uri">>, Data)}
       ,{<<"Hunt-Account-ID">>, get_hunt_account_id(Data, Call)}
       ,{<<"Flags">>, get_flags(Data, Call)}
       ,{<<"Custom-SIP-Headers">>, get_sip_headers(Data, Call)}
       ,{<<"Custom-Channel-Vars">>, kapps_call:custom_channel_vars(Call)}
       ,{<<"To-DID">>, get_to_did(Data, Call)}
       ,{<<"From-URI-Realm">>, get_from_uri_realm(Data, Call)}
       ,{<<"Bypass-E164">>, get_bypass_e164(Data)}
       ,{<<"Inception">>, get_inception(Call)}
       ,{<<"Message-ID">>, kapps_call:kvs_fetch(<<"Message-ID">>, Call)}
       ,{<<"Body">>, kapps_call:kvs_fetch(<<"Body">>, Call)}
       | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
      ]).

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

-spec get_account_realm(kapps_call:call()) -> api_binary().
get_account_realm(Call) ->
    case kz_account:fetch(kapps_call:account_id(Call)) of
        {'ok', JObj} -> kz_json:get_value(<<"realm">>, JObj);
        {'error', _} -> 'undefined'
    end.

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
    case kz_json:is_true(<<"do_not_normalize">>, Data) of
        'false' -> get_to_did(Data, Call, kapps_call:request_user(Call));
        'true' ->
            Request = kapps_call:request(Call),
            [RequestUser, _] = binary:split(Request, <<"@">>),
            RequestUser
    end.

-spec get_to_did(kz_json:object(), kapps_call:call(), ne_binary()) -> ne_binary().
get_to_did(_Data, Call, Number) ->
    case kz_endpoint:get(Call) of
        {'ok', Endpoint} ->
            case kz_json:get_value(<<"dial_plan">>, Endpoint, []) of
                [] -> Number;
                DialPlan -> cf_util:apply_dialplan(Number, DialPlan)
            end;
        {'error', _ } -> Number
    end.

-spec get_sip_headers(kz_json:object(), kapps_call:call()) -> api_object().
get_sip_headers(Data, Call) ->
    Routines = [fun(J) ->
                        case kz_json:is_true(<<"emit_account_id">>, Data) of
                            'false' -> J;
                            'true' ->
                                kz_json:set_value(<<"X-Account-ID">>, kapps_call:account_id(Call), J)
                        end
                end
               ],
    CustomHeaders = kz_json:get_value(<<"custom_sip_headers">>, Data, kz_json:new()),
    JObj = lists:foldl(fun(F, J) -> F(J) end, CustomHeaders, Routines),
    case kz_util:is_empty(JObj) of
        'true' -> 'undefined';
        'false' -> JObj
    end.

-spec get_flags(kz_json:object(), kapps_call:call()) -> 'undefined' | ne_binaries().
get_flags(Data, Call) ->
    Routines = [fun get_endpoint_flags/3
                ,fun get_flow_flags/3
                ,fun get_flow_dynamic_flags/3
                ,fun get_endpoint_dynamic_flags/3
                ,fun get_account_dynamic_flags/3
                ,fun get_resource_flags/3
               ],
    lists:foldl(fun(F, A) -> F(Data, Call, A) end, [], Routines).

-spec get_resource_flags(kz_json:object(), kapps_call:call(), ne_binaries()) -> ne_binaries().
get_resource_flags(JObj, Call, Flags) ->
    get_resource_type_flags(kapps_call:resource_type(Call), JObj, Call, Flags).

-spec get_resource_type_flags(ne_binary(), kz_json:object(), kapps_call:call(), ne_binaries()) -> ne_binaries().
get_resource_type_flags(<<"sms">>, _JObj, _Call, Flags) -> [<<"sms">> | Flags];
get_resource_type_flags(_Other, _JObj, _Call, Flags) -> Flags.

-spec get_endpoint_flags(kz_json:object(), kapps_call:call(), ne_binaries()) -> ne_binaries().
get_endpoint_flags(_, Call, Flags) ->
    case kz_endpoint:get(Call) of
        {'error', _} -> Flags;
        {'ok', JObj} ->
            case kz_json:get_value(<<"outbound_flags">>, JObj) of
                'undefined' -> Flags;
                EndpointFlags -> EndpointFlags ++ Flags
            end
    end.

-spec get_flow_flags(kz_json:object(), kapps_call:call(), ne_binaries()) -> ne_binaries().
get_flow_flags(Data, _, Flags) ->
    case kz_json:get_value(<<"outbound_flags">>, Data) of
        'undefined' -> Flags;
        FlowFlags -> FlowFlags ++ Flags
    end.

-spec get_flow_dynamic_flags(kz_json:object(), kapps_call:call(), ne_binaries()) -> ne_binaries().
get_flow_dynamic_flags(Data, Call, Flags) ->
    case kz_json:get_value(<<"dynamic_flags">>, Data) of
        'undefined' -> Flags;
        DynamicFlags -> process_dynamic_flags(DynamicFlags, Flags, Call)
    end.

-spec get_endpoint_dynamic_flags(kz_json:object(), kapps_call:call(), ne_binaries()) -> ne_binaries().
get_endpoint_dynamic_flags(_, Call, Flags) ->
    case kz_endpoint:get(Call) of
        {'error', _} -> Flags;
        {'ok', JObj} ->
            case kz_json:get_value(<<"dynamic_flags">>, JObj) of
                'undefined' -> Flags;
                 DynamicFlags ->
                    process_dynamic_flags(DynamicFlags, Flags, Call)
            end
    end.

-spec get_account_dynamic_flags(kz_json:object(), kapps_call:call(), ne_binaries()) -> ne_binaries().
get_account_dynamic_flags(_, Call, Flags) ->
    DynamicFlags = kapps_account_config:get(kapps_call:account_id(Call)
                                             ,<<"callflow">>
                                             ,<<"dynamic_flags">>
                                             ,[]
                                            ),
    process_dynamic_flags(DynamicFlags, Flags, Call).

-spec process_dynamic_flags(ne_binaries(), ne_binaries(), kapps_call:call()) -> ne_binaries().
process_dynamic_flags([], Flags, _) -> Flags;
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
    case kz_util:to_binary(F) =:= Flag of
        'true' -> 'true';
        'false' -> is_flag_exported(Flag, Funs)
    end;
is_flag_exported(Flag, [_|Funs]) -> is_flag_exported(Flag, Funs).

-spec get_inception(kapps_call:call()) -> api_binary().
get_inception(Call) ->
    kz_json:get_value(<<"Inception">>, kapps_call:custom_channel_vars(Call)).
