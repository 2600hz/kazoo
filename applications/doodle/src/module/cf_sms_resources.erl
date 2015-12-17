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

-include("../doodle.hrl").

-export([handle/2]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module
%% @end
%%--------------------------------------------------------------------
-spec handle(wh_json:object(), whapps_call:call()) -> 'ok'.
handle(Data, Call1) ->
    AccountId = whapps_call:account_id(Call1),
    Call = case whapps_call:custom_channel_var(<<"API-Call">>, 'false', Call1)
               andalso whapps_account_config:get_global(AccountId, ?CONFIG_CAT, <<"api_preserve_caller_id">>, 'true')
           of
               'true' -> doodle_util:set_caller_id(whapps_call:from_user(Call1), Call1);
               'false' -> doodle_util:set_caller_id(Data, Call1)
           end,
    case whapps_util:amqp_pool_request(
           build_offnet_request(Data, Call)
           ,fun wapi_offnet_resource:publish_req/1
           ,fun wapi_offnet_resource:resp_v/1
           ,30 * ?MILLISECONDS_IN_SECOND
          )
    of
        {'ok', Res} ->
            handle_result(Res, Call);
        {'error', E} ->
            lager:debug("error executing offnet action : ~p", [E]),
            doodle_util:maybe_reschedule_sms(doodle_util:set_flow_error(E, Call))
    end.

-spec handle_result(wh_json:object(), whapps_call:call()) -> 'ok'.
handle_result(JObj, Call) ->
    Message = wh_json:get_value(<<"Response-Message">>, JObj),
    Code = wh_json:get_value(<<"Response-Code">>, JObj),
    Response = wh_json:get_value(<<"Resource-Response">>, JObj),
    handle_result(Message, Code, Response, JObj, Call).

-spec handle_result(binary(), binary()
                    ,wh_json:object(), wh_json:object()
                    ,whapps_call:call()
                   ) -> 'ok'.
handle_result(_Message, <<"sip:200">>, Response, _JObj, Call1) ->
    Status = doodle_util:sms_status(Response),
    Call = doodle_util:set_flow_status(Status, Call1),
    handle_result_status(Call, Status);
handle_result(Message, Code, _Response, _JObj, Call) ->
    handle_bridge_failure(Message, Code, Call).

-spec handle_result_status(whapps_call:call(), ne_binary()) -> 'ok'.
handle_result_status(Call, <<"pending">>) ->
    doodle_util:maybe_reschedule_sms(Call);
handle_result_status(Call, _Status) ->
    lager:info("completed successful message to the device"),
    doodle_exe:continue(Call).

-spec handle_bridge_failure(api_binary(), api_binary(), whapps_call:call()) -> 'ok'.
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
-spec build_offnet_request(wh_json:object(), whapps_call:call()) -> wh_proplist().
build_offnet_request(Data, Call) ->
    props:filter_undefined(
      [{<<"Resource-Type">>, <<"sms">>}
       ,{<<"Application-Name">>, <<"sms">>}
       ,{<<"Outbound-Caller-ID-Name">>, whapps_call:caller_id_name(Call)}
       ,{<<"Outbound-Caller-ID-Number">>, whapps_call:caller_id_number(Call)}
       ,{<<"Msg-ID">>, wh_util:rand_hex_binary(16)}
       ,{<<"Call-ID">>, doodle_exe:callid(Call)}
       ,{<<"Presence-ID">>, cf_attributes:presence_id(Call)}
       ,{<<"Account-ID">>, whapps_call:account_id(Call)}
       ,{<<"Account-Realm">>, whapps_call:from_realm(Call)}
       ,{<<"Timeout">>, wh_json:get_value(<<"timeout">>, Data)}
       ,{<<"Format-From-URI">>, wh_json:is_true(<<"format_from_uri">>, Data)}
       ,{<<"Hunt-Account-ID">>, get_hunt_account_id(Data, Call)}
       ,{<<"Flags">>, get_flags(Data, Call)}
       ,{<<"Custom-SIP-Headers">>, get_sip_headers(Data, Call)}
       ,{<<"Custom-Channel-Vars">>, whapps_call:custom_channel_vars(Call)}
       ,{<<"To-DID">>, get_to_did(Data, Call)}
       ,{<<"From-URI-Realm">>, get_from_uri_realm(Data, Call)}
       ,{<<"Bypass-E164">>, get_bypass_e164(Data)}
       ,{<<"Inception">>, get_inception(Call)}
       ,{<<"Message-ID">>, whapps_call:kvs_fetch(<<"Message-ID">>, Call)}
       ,{<<"Body">>, whapps_call:kvs_fetch(<<"Body">>, Call)}
       | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
      ]).

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

-spec get_account_realm(whapps_call:call()) -> api_binary().
get_account_realm(Call) ->
    case kz_account:fetch(whapps_call:account_id(Call)) of
        {'ok', JObj} -> wh_json:get_value(<<"realm">>, JObj);
        {'error', _} -> 'undefined'
    end.

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
    case wh_json:is_true(<<"do_not_normalize">>, Data) of
        'false' -> get_to_did(Data, Call, whapps_call:request_user(Call));
        'true' ->
            Request = whapps_call:request(Call),
            [RequestUser, _] = binary:split(Request, <<"@">>),
            RequestUser
    end.

-spec get_to_did(wh_json:object(), whapps_call:call(), ne_binary()) -> ne_binary().
get_to_did(_Data, Call, Number) ->
    case cf_endpoint:get(Call) of
        {'ok', Endpoint} ->
            case wh_json:get_value(<<"dial_plan">>, Endpoint, []) of
                [] -> Number;
                DialPlan -> cf_util:apply_dialplan(Number, DialPlan)
            end;
        {'error', _ } -> Number
    end.

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
    JObj = lists:foldl(fun(F, J) -> F(J) end, CustomHeaders, Routines),
    case wh_util:is_empty(JObj) of
        'true' -> 'undefined';
        'false' -> JObj
    end.

-spec get_flags(wh_json:object(), whapps_call:call()) -> 'undefined' | ne_binaries().
get_flags(Data, Call) ->
    Routines = [fun get_endpoint_flags/3
                ,fun get_flow_flags/3
                ,fun get_flow_dynamic_flags/3
                ,fun get_endpoint_dynamic_flags/3
                ,fun get_account_dynamic_flags/3
                ,fun get_resource_flags/3
               ],
    lists:foldl(fun(F, A) -> F(Data, Call, A) end, [], Routines).

-spec get_resource_flags(wh_json:object(), whapps_call:call(), ne_binaries()) -> ne_binaries().
get_resource_flags(JObj, Call, Flags) ->
    get_resource_type_flags(whapps_call:resource_type(Call), JObj, Call, Flags).

-spec get_resource_type_flags(ne_binary(), wh_json:object(), whapps_call:call(), ne_binaries()) -> ne_binaries().
get_resource_type_flags(<<"sms">>, _JObj, _Call, Flags) -> [<<"sms">> | Flags];
get_resource_type_flags(_Other, _JObj, _Call, Flags) -> Flags.

-spec get_endpoint_flags(wh_json:object(), whapps_call:call(), ne_binaries()) -> ne_binaries().
get_endpoint_flags(_, Call, Flags) ->
    case cf_endpoint:get(Call) of
        {'error', _} -> Flags;
        {'ok', JObj} ->
            case wh_json:get_value(<<"outbound_flags">>, JObj) of
                'undefined' -> Flags;
                EndpointFlags -> EndpointFlags ++ Flags
            end
    end.

-spec get_flow_flags(wh_json:object(), whapps_call:call(), ne_binaries()) -> ne_binaries().
get_flow_flags(Data, _, Flags) ->
    case wh_json:get_value(<<"outbound_flags">>, Data) of
        'undefined' -> Flags;
        FlowFlags -> FlowFlags ++ Flags
    end.

-spec get_flow_dynamic_flags(wh_json:object(), whapps_call:call(), ne_binaries()) -> ne_binaries().
get_flow_dynamic_flags(Data, Call, Flags) ->
    case wh_json:get_value(<<"dynamic_flags">>, Data) of
        'undefined' -> Flags;
        DynamicFlags -> process_dynamic_flags(DynamicFlags, Flags, Call)
    end.

-spec get_endpoint_dynamic_flags(wh_json:object(), whapps_call:call(), ne_binaries()) -> ne_binaries().
get_endpoint_dynamic_flags(_, Call, Flags) ->
    case cf_endpoint:get(Call) of
        {'error', _} -> Flags;
        {'ok', JObj} ->
            case wh_json:get_value(<<"dynamic_flags">>, JObj) of
                'undefined' -> Flags;
                 DynamicFlags ->
                    process_dynamic_flags(DynamicFlags, Flags, Call)
            end
    end.

-spec get_account_dynamic_flags(wh_json:object(), whapps_call:call(), ne_binaries()) -> ne_binaries().
get_account_dynamic_flags(_, Call, Flags) ->
    DynamicFlags = whapps_account_config:get(whapps_call:account_id(Call)
                                             ,<<"callflow">>
                                             ,<<"dynamic_flags">>
                                             ,[]
                                            ),
    process_dynamic_flags(DynamicFlags, Flags, Call).

-spec process_dynamic_flags(ne_binaries(), ne_binaries(), whapps_call:call()) -> ne_binaries().
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
