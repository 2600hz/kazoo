%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc
%%% @author Karl Anderson
%%% @author Luis Azedo
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cf_sms_resources).

-include("doodle.hrl").

-export([handle/2]).

%%------------------------------------------------------------------------------
%% @doc Entry point for this module
%% @end
%%------------------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(Data, Call1) ->
    AccountId = kapps_call:account_id(Call1),
    Call = case kapps_call:custom_channel_var(<<"API-Call">>, 'false', Call1)
               andalso kapps_account_config:get_global(AccountId, ?CONFIG_CAT, <<"api_preserve_caller_id">>, 'true')
           of
               'true' -> doodle_util:set_caller_id(kapps_call:from_user(Call1), Call1);
               'false' -> doodle_util:set_caller_id(Data, Call1)
           end,
    case kz_amqp_worker:call(build_offnet_request(Data, Call)
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

-spec handle_result_status(kapps_call:call(), kz_term:ne_binary()) -> 'ok'.
handle_result_status(Call, <<"pending">>) ->
    doodle_util:maybe_reschedule_sms(Call);
handle_result_status(Call, _Status) ->
    lager:info("completed successful message to the device"),
    doodle_exe:stop(Call).

-spec handle_bridge_failure(kz_term:api_binary(), kz_term:api_binary(), kapps_call:call()) -> 'ok'.
handle_bridge_failure(Cause, Code, Call) ->
    lager:info("offnet request error, attempting to find failure branch for ~s:~s", [Code, Cause]),
    case doodle_util:handle_bridge_failure(Cause, Code, Call) of
        'ok' ->
            lager:debug("found bridge failure child"),
            doodle_exe:stop(Call);
        'not_found' ->
            doodle_util:maybe_reschedule_sms(Code, Cause, Call)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec build_offnet_request(kz_json:object(), kapps_call:call()) -> kz_term:proplist().
build_offnet_request(Data, Call) ->
    props:filter_undefined(
      [{<<"Resource-Type">>, <<"sms">>}
      ,{<<"Application-Name">>, <<"sms">>}
      ,{<<"Outbound-Caller-ID-Name">>, kapps_call:caller_id_name(Call)}
      ,{<<"Outbound-Caller-ID-Number">>, kapps_call:caller_id_number(Call)}
      ,{<<"Msg-ID">>, kz_binary:rand_hex(16)}
      ,{<<"Call-ID">>, doodle_exe:callid(Call)}
      ,{<<"Control-Queue">>, doodle_exe:control_queue(Call)}
      ,{<<"Presence-ID">>, kz_attributes:presence_id(Call)}
      ,{<<"Account-ID">>, kapps_call:account_id(Call)}
      ,{<<"Account-Realm">>, kapps_call:from_realm(Call)}
      ,{<<"Timeout">>, kz_json:get_value(<<"timeout">>, Data)}
      ,{<<"Format-From-URI">>, kz_json:is_true(<<"format_from_uri">>, Data)}
      ,{<<"Hunt-Account-ID">>, get_hunt_account_id(Data, Call)}
      ,{<<"Flags">>, get_flags(Data, Call)}
      ,{<<"Custom-SIP-Headers">>, get_sip_headers(Data, Call)}
      ,{<<"Custom-Channel-Vars">>, kapps_call:custom_channel_vars(Call)}
      ,{<<"Custom-Application-Vars">>, kapps_call:custom_application_vars(Call)}
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

-spec get_from_uri_realm(kz_json:object(), kapps_call:call()) -> kz_term:api_ne_binary().
get_from_uri_realm(Data, Call) ->
    case kz_json:get_ne_value(<<"from_uri_realm">>, Data) of
        'undefined' -> maybe_get_call_from_realm(Call);
        Realm -> Realm
    end.

-spec maybe_get_call_from_realm(kapps_call:call()) -> kz_term:api_ne_binary().
maybe_get_call_from_realm(Call) ->
    case kapps_call:from_realm(Call) of
        <<"norealm">> -> kzd_accounts:fetch_realm(kapps_call:account_id(Call));
        Realm -> Realm
    end.

-spec get_hunt_account_id(kz_json:object(), kapps_call:call()) -> kz_term:api_ne_binary().
get_hunt_account_id(Data, Call) ->
    case kz_json:is_true(<<"use_local_resources">>, Data, 'true') of
        'false' -> 'undefined';
        'true' ->
            AccountId = kapps_call:account_id(Call),
            kz_json:get_value(<<"hunt_account_id">>, Data, AccountId)
    end.

-spec get_to_did(kz_json:object(), kapps_call:call()) -> kz_term:ne_binary().
get_to_did(Data, Call) ->
    case kz_json:is_true(<<"do_not_normalize">>, Data) of
        'false' -> get_to_did(Data, Call, kapps_call:request_user(Call));
        'true' ->
            Request = kapps_call:request(Call),
            [RequestUser, _] = binary:split(Request, <<"@">>),
            RequestUser
    end.

-spec get_to_did(kz_json:object(), kapps_call:call(), kz_term:ne_binary()) -> kz_term:ne_binary().
get_to_did(_Data, Call, Number) ->
    case kz_endpoint:get(Call) of
        {'ok', Endpoint} ->
            case kz_json:get_value(<<"dial_plan">>, Endpoint, []) of
                [] -> Number;
                DialPlan -> cf_util:apply_dialplan(Number, DialPlan)
            end;
        {'error', _ } -> Number
    end.

-spec get_sip_headers(kz_json:object(), kapps_call:call()) -> kz_term:api_object().
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
    case kz_term:is_empty(JObj) of
        'true' -> 'undefined';
        'false' -> JObj
    end.

-spec get_flags(kz_json:object(), kapps_call:call()) -> kz_term:ne_binaries() | undefined.
get_flags(Data, Call) ->
    Flags = kz_attributes:get_flags(?APP_NAME, Call),
    Routines = [fun get_flow_flags/3
               ,fun get_flow_dynamic_flags/3
               ,fun get_resource_flags/3
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

-spec get_resource_flags(kz_json:object(), kapps_call:call(), kz_term:ne_binaries()) -> kz_term:ne_binaries().
get_resource_flags(JObj, Call, Flags) ->
    get_resource_type_flags(kapps_call:resource_type(Call), JObj, Call, Flags).

-spec get_resource_type_flags(kz_term:ne_binary(), kz_json:object(), kapps_call:call(), kz_term:ne_binaries()) -> kz_term:ne_binaries().
get_resource_type_flags(<<"sms">>, _JObj, _Call, Flags) -> [<<"sms">> | Flags];
get_resource_type_flags(_Other, _JObj, _Call, Flags) -> Flags.

-spec get_inception(kapps_call:call()) -> kz_term:api_ne_binary().
get_inception(Call) ->
    kz_json:get_value(<<"Inception">>, kapps_call:custom_channel_vars(Call)).
