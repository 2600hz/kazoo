%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2017, 2600Hz
%%% @doc
%%% Handle offnet requests, including rating them
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%   Ben Wann
%%%-------------------------------------------------------------------
-module(stepswitch_outbound).

-export([handle_req/2]).

-include("stepswitch.hrl").
-include_lib("kazoo_amqp/include/kapi_offnet_resource.hrl").
-include_lib("kazoo/include/kz_api_literals.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%% process an offnet resource request (outbound)
%% route
%% @end
%%--------------------------------------------------------------------
-spec handle_req(kz_json:object(), kz_proplist()) -> any().
handle_req(OffnetJObj, _Props) ->
    'true' = kapi_offnet_resource:req_v(OffnetJObj),
    OffnetReq = kapi_offnet_resource:jobj_to_req(OffnetJObj),
    _ = kapi_offnet_resource:put_callid(OffnetReq),
    case kapi_offnet_resource:resource_type(OffnetReq) of
        ?RESOURCE_TYPE_AUDIO -> handle_audio_req(OffnetReq);
        ?RESOURCE_TYPE_ORIGINATE -> handle_originate_req(OffnetReq);
        ?RESOURCE_TYPE_SMS -> handle_sms_req(OffnetReq)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_audio_req(kapi_offnet_resource:req()) -> any().
-spec handle_audio_req(ne_binary(), kapi_offnet_resource:req()) -> any().
handle_audio_req(OffnetReq) ->
    Number = stepswitch_util:get_outbound_destination(OffnetReq),
    lager:debug("received outbound audio resource request for ~s: ~p", [Number, OffnetReq]),
    handle_audio_req(Number, OffnetReq).

handle_audio_req(Number, OffnetReq) ->
    case knm_number:lookup_account(Number) of
        {'ok', _AccountId, Props} -> maybe_force_outbound(Props, OffnetReq);
        _ -> maybe_bridge(Number, OffnetReq)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_originate_req(kapi_offnet_resource:req()) -> any().
handle_originate_req(OffnetReq) ->
    Number = stepswitch_util:get_outbound_destination(OffnetReq),
    lager:debug("received outbound audio resource request for ~s from account ~s"
               ,[Number, kapi_offnet_resource:account_id(OffnetReq)]
               ),
    handle_originate_req(Number, maybe_add_call_id(kz_json:get_value(<<"Outbound-Call-ID">>, OffnetReq) , OffnetReq)).

-spec handle_originate_req(ne_binary(), kz_json:object()) -> any().
handle_originate_req(Number, JObj) ->
    case knm_number:lookup_account(Number) of
        {'ok', _AccountId, Props} ->
            maybe_force_originate_outbound(Props, JObj);
        _ -> maybe_originate(Number, JObj)
    end.

-spec maybe_add_call_id(api_binary(), kz_json:object()) -> kz_json:object().
maybe_add_call_id('undefined', JObj) ->
    kz_json:set_value(<<"Outbound-Call-ID">>, kz_binary:rand_hex(8), JObj);
maybe_add_call_id(_, JObj) -> JObj.

-spec maybe_force_originate_outbound(knm_number_options:extra_options(), kz_json:object()) -> any().
maybe_force_originate_outbound(Props, JObj) ->
    case knm_number_options:should_force_outbound(Props)
        orelse kz_json:is_true(<<"Force-Outbound">>, JObj, 'false')
        orelse kapi_offnet_resource:hunt_account_id(JObj) /= 'undefined'
    of
        'false' -> local_originate(Props, JObj);
        'true' -> maybe_originate(knm_number_options:number(Props), JObj)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_sms_req(kapi_offnet_resource:req()) -> any().
handle_sms_req(OffnetReq) ->
    Number = stepswitch_util:get_outbound_destination(OffnetReq),
    lager:debug("received outbound sms resource request for ~s", [Number]),
    case knm_number:lookup_account(Number) of
        {'ok', _AccountId, Props} ->
            maybe_force_outbound_sms(Props, OffnetReq);
        _ -> maybe_sms(Number, OffnetReq)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec maybe_force_outbound(knm_number_options:extra_options(), kapi_offnet_resource:req()) -> any().
maybe_force_outbound(Props, OffnetReq) ->
    case knm_number_options:should_force_outbound(Props)
        orelse kapi_offnet_resource:force_outbound(OffnetReq, 'false')
        orelse kapi_offnet_resource:hunt_account_id(OffnetReq) /= 'undefined'
    of
        'false' -> local_extension(Props, OffnetReq);
        'true' -> maybe_bridge(knm_number_options:number(Props), OffnetReq)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec maybe_force_outbound_sms(knm_number_options:extra_options(), kapi_offnet_resource:req()) -> any().
maybe_force_outbound_sms(Props, OffnetReq) ->
    case knm_number_options:should_force_outbound(Props)
        orelse kapi_offnet_resource:force_outbound(OffnetReq, 'false')
        orelse kapi_offnet_resource:hunt_account_id(OffnetReq) /= 'undefined'
    of
        'false' -> local_sms(Props, OffnetReq);
        'true' -> maybe_sms(knm_number_options:number(Props), OffnetReq)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec maybe_bridge(ne_binary(), kapi_offnet_resource:req()) -> any().
maybe_bridge(Number, OffnetReq) ->
    RouteBy = stepswitch_util:route_by(),
    case RouteBy:endpoints(Number, OffnetReq) of
        [] -> maybe_correct_shortdial(Number, OffnetReq);
        Endpoints -> stepswitch_request_sup:bridge(Endpoints, OffnetReq)
    end.

-spec maybe_correct_shortdial(ne_binary(), kapi_offnet_resource:req()) -> any().
maybe_correct_shortdial(Number, OffnetReq) ->
    case stepswitch_util:correct_shortdial(Number, OffnetReq) of
        'undefined' ->
            lager:debug("no endpoints found for '~s', and no shortdial correction available", [Number]),
            publish_no_resources(OffnetReq);
        Number ->
            lager:debug("shortdial correction invalid, no resources"),
            publish_no_resources(OffnetReq);
        CorrectedNumber ->
            lager:debug("corrected shortdial from '~s' to '~s', trying routing again", [Number, CorrectedNumber]),
            handle_audio_req(CorrectedNumber, OffnetReq)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec maybe_sms(ne_binary(), kapi_offnet_resource:req()) -> any().
maybe_sms(Number, OffnetReq) ->
    RouteBy = stepswitch_util:route_by(),
    case RouteBy:endpoints(Number, OffnetReq) of
        [] -> publish_no_resources(OffnetReq);
        Endpoints -> stepswitch_request_sup:sms(Endpoints, OffnetReq)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec local_extension(knm_number_options:extra_options(), kapi_offnet_resource:req()) ->
                             sup_startchild_ret().
local_extension(Props, OffnetReq) ->
    stepswitch_request_sup:local_extension(Props, OffnetReq).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec local_sms(knm_number_options:extra_options(), kapi_offnet_resource:req()) -> 'ok'.
local_sms(Props, OffnetReq) ->
    stepswitch_local_sms:local_message_handling(Props, OffnetReq).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec maybe_originate(ne_binary(), kapi_offnet_resource:req()) -> any().
maybe_originate(Number, OffnetReq) ->
    RouteBy = stepswitch_util:route_by(),
    case RouteBy:endpoints(Number, OffnetReq) of
        [] -> publish_no_resources(OffnetReq);
        Endpoints -> stepswitch_request_sup:originate(Endpoints, OffnetReq)
    end.

-spec local_originate(knm_number_options:extra_options(), kz_json:object()) -> any().
local_originate(Props, JObj) ->
    Endpoints = [create_loopback_endpoint(Props, JObj)],
    J = kz_json:set_values([{<<"Simplify-Loopback">>, <<"false">>}
                           ,{<<"Loopback-Bowout">>, <<"false">>}
                           ], JObj),
    lager:debug("originate local request"),
    stepswitch_request_sup:originate(Endpoints, J).

-spec local_originate_caller_id(kz_json:object()) -> {api_binary(), api_binary()}.
local_originate_caller_id(JObj) ->
    {kz_json:get_first_defined([<<"Outbound-Caller-ID-Number">>
                               ,<<"Emergency-Caller-ID-Number">>
                               ], JObj)
    ,kz_json:get_first_defined([<<"Outbound-Caller-ID-Name">>
                               ,<<"Emergency-Caller-ID-Name">>
                               ], JObj)
    }.

-spec get_account_realm(ne_binary()) -> ne_binary().
get_account_realm(AccountId) ->
    case kz_account:fetch_realm(AccountId) of
        undefined -> AccountId;
        Realm -> Realm
    end.

-spec create_loopback_endpoint(knm_number_options:extra_options(), kz_json:object()) -> kz_json:object().
create_loopback_endpoint(Props, JObj) ->
    {CIDNum, CIDName} = local_originate_caller_id(JObj),
    lager:debug("set outbound caller id to ~s '~s'", [CIDNum, CIDName]),
    Number = knm_number_options:number(Props),
    AccountId = knm_number_options:account_id(Props),
    Realm = get_account_realm(AccountId),
    CCVs = kz_json:from_list(
             [{<<?CHANNEL_LOOPBACK_HEADER_PREFIX, "Inception">>, <<Number/binary, "@", Realm/binary>>}
             ,{<<?CHANNEL_LOOPBACK_HEADER_PREFIX, "Account-ID">>, AccountId}
             ,{<<?CHANNEL_LOOPBACK_HEADER_PREFIX, "Retain-CID">>, "true"}
             ,{<<?CHANNEL_LOOPBACK_HEADER_PREFIX, "Resource-Type">>, <<"onnet-origination">>}
             ,{<<"Resource-ID">>, AccountId}
             ,{<<"Loopback-Request-URI">>, <<Number/binary, "@", Realm/binary>>}
             ,{<<"Resource-Type">>, <<"onnet-termination">>}
             ]),
    kz_json:from_list(
      [{<<"Invite-Format">>, <<"loopback">>}
      ,{<<"Route">>, Number}
      ,{<<"To-DID">>, Number}
      ,{<<"To-Realm">>, Realm}
      ,{<<"Custom-Channel-Vars">>, CCVs}
      ,{<<"Outbound-Caller-ID-Name">>, CIDName}
      ,{<<"Outbound-Caller-ID-Number">>, CIDNum}
      ,{<<"Caller-ID-Name">>, CIDName}
      ,{<<"Caller-ID-Number">>, CIDNum}
      ,{<<"Ignore-Early-Media">>, 'true'}
      ,{<<"Ignore-Early-Media">>, 'true'}
      ,{<<"Enable-T38-Fax">>, 'false'}
      ,{<<"Enable-T38-Fax-Request">>, 'false'}
      ]).
%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec publish_no_resources(kapi_offnet_resource:req()) -> 'ok'.
publish_no_resources(OffnetReq) ->
    case kapi_offnet_resource:server_id(OffnetReq) of
        'undefined' -> 'ok';
        ResponseQ ->
            kapi_offnet_resource:publish_resp(ResponseQ, no_resources(OffnetReq))
    end.

-spec no_resources(kapi_offnet_resource:req()) -> kz_proplist().
no_resources(OffnetReq) ->
    ToDID = kapi_offnet_resource:to_did(OffnetReq),
    lager:info("no available resources for ~s", [ToDID]),
    props:filter_undefined(
      [{?KEY_TO_DID, ToDID}
      ,{<<"Response-Message">>, <<"NO_ROUTE_DESTINATION">>}
      ,{<<"Response-Code">>, <<"sip:404">>}
      ,{<<"Error-Message">>, <<"no available resources">>}
      ,{?KEY_CALL_ID, kapi_offnet_resource:call_id(OffnetReq)}
      ,{?KEY_MSG_ID, kapi_offnet_resource:msg_id(OffnetReq)}
       | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
      ]).
