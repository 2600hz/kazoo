%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2015, 2600Hz
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
-include_lib("whistle_number_manager/include/wh_number_manager.hrl").
-include_lib("whistle/include/wapi_offnet_resource.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%% process an offnet resource request (outbound)
%% route
%% @end
%%--------------------------------------------------------------------
-spec handle_req(wh_json:object(), wh_proplist()) -> any().
handle_req(OffnetJObj, _Props) ->
    'true' = wapi_offnet_resource:req_v(OffnetJObj),
    OffnetReq = wapi_offnet_resource:jobj_to_req(OffnetJObj),
    _ = wapi_offnet_resource:put_callid(OffnetReq),
    case wapi_offnet_resource:resource_type(OffnetReq) of
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
-spec handle_audio_req(wapi_offnet_resource:req()) -> any().
-spec handle_audio_req(ne_binary(), wapi_offnet_resource:req()) -> any().
handle_audio_req(OffnetReq) ->
    Number = stepswitch_util:get_outbound_destination(OffnetReq),
    lager:debug("received outbound audio resource request for ~s: ~p", [Number, OffnetReq]),
    handle_audio_req(Number, OffnetReq).

handle_audio_req(Number, OffnetReq) ->
    case stepswitch_util:lookup_number(Number) of
        {'ok', AccountId, Props} ->
            maybe_force_outbound(wh_number_properties:set_account_id(Props, AccountId), OffnetReq);
        _ -> maybe_bridge(Number, OffnetReq)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_originate_req(wapi_offnet_resource:req()) -> any().
handle_originate_req(OffnetReq) ->
    Number = stepswitch_util:get_outbound_destination(OffnetReq),
    lager:debug("received outbound audio resource request for ~s from account ~s"
                ,[Number, wapi_offnet_resource:account_id(OffnetReq)]
               ),
    handle_originate_req(Number, maybe_add_call_id(wh_json:get_value(<<"Outbound-Call-ID">>, OffnetReq) , OffnetReq)).

-spec handle_originate_req(ne_binary(), wh_json:object()) -> any().
handle_originate_req(Number, JObj) ->
    case stepswitch_util:lookup_number(Number) of
        {'ok', AccountId, Props} ->
            maybe_force_originate_outbound(wh_number_properties:set_account_id(Props, AccountId), JObj);
        _ -> maybe_originate(Number, JObj)
    end.

-spec maybe_add_call_id(api_binary(), wh_json:object()) -> wh_json:object().
maybe_add_call_id('undefined', JObj) ->
    wh_json:set_value(<<"Outbound-Call-ID">>, wh_util:rand_hex_binary(8), JObj);
maybe_add_call_id(_, JObj) -> JObj.

-spec maybe_force_originate_outbound(wh_proplist(), wh_json:object()) -> any().
maybe_force_originate_outbound(Props, JObj) ->
    case wh_number_properties:should_force_outbound(Props)
        orelse wh_json:is_true(<<"Force-Outbound">>, JObj, 'false')
    of
        'false' -> local_originate(Props, JObj);
        'true' ->
            Number = wh_number_properties:number(Props),
            maybe_originate(Number, JObj)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_sms_req(wapi_offnet_resource:req()) -> any().
handle_sms_req(OffnetReq) ->
    Number = stepswitch_util:get_outbound_destination(OffnetReq),
    lager:debug("received outbound sms resource request for ~s", [Number]),
    case stepswitch_util:lookup_number(Number) of
        {'ok', AccountId, Props} ->
            maybe_force_outbound_sms(wh_number_properties:set_account_id(Props, AccountId), OffnetReq);
        _ -> maybe_sms(Number, OffnetReq)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec maybe_force_outbound(wh_proplist(), wapi_offnet_resource:req()) -> any().
maybe_force_outbound(Props, OffnetReq) ->
    case wh_number_properties:should_force_outbound(Props)
        orelse wapi_offnet_resource:force_outbound(OffnetReq, 'false')
    of
        'false' -> local_extension(Props, OffnetReq);
        'true' ->
            Number = wh_number_properties:number(Props),
            maybe_bridge(Number, OffnetReq)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec maybe_force_outbound_sms(wh_proplist(), wapi_offnet_resource:req()) -> any().
maybe_force_outbound_sms(Props, OffnetReq) ->
    case props:get_is_true('force_outbound', Props)
        orelse wapi_offnet_resource:force_outbound(OffnetReq, 'false')
    of
        'false' -> local_sms(Props, OffnetReq);
        'true' ->
            Number = props:get_value('number', Props),
            maybe_sms(Number, OffnetReq)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec maybe_bridge(ne_binary(), wapi_offnet_resource:req()) -> any().
maybe_bridge(Number, OffnetReq) ->
    case stepswitch_resources:endpoints(Number, OffnetReq) of
        [] -> maybe_correct_shortdial(Number, OffnetReq);
        Endpoints -> stepswitch_request_sup:bridge(Endpoints, OffnetReq)
    end.

-spec maybe_correct_shortdial(ne_binary(), wapi_offnet_resource:req()) -> any().
maybe_correct_shortdial(Number, OffnetReq) ->
    case stepswitch_util:correct_shortdial(Number, OffnetReq) of
        'undefined' ->
            lager:debug("no endpoints found for '~s', and no shortdial correction available", [Number]),
            publish_no_resources(OffnetReq);
        Number ->
            lager:debug("shortdial correction invalid, no resources", []),
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
-spec maybe_sms(ne_binary(), wapi_offnet_resource:req()) -> any().
maybe_sms(Number, OffnetReq) ->
    case stepswitch_resources:endpoints(Number, OffnetReq) of
        [] -> publish_no_resources(OffnetReq);
        Endpoints -> stepswitch_request_sup:sms(Endpoints, OffnetReq)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec local_extension(wh_proplist(), wapi_offnet_resource:req()) -> any().
local_extension(Props, OffnetReq) -> stepswitch_request_sup:local_extension(Props, OffnetReq).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec local_sms(wh_proplist(), wapi_offnet_resource:req()) -> 'ok'.
local_sms(Props, OffnetReq) -> stepswitch_local_sms:local_message_handling(Props, OffnetReq).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec maybe_originate(ne_binary(), wapi_offnet_resource:req()) -> any().
maybe_originate(Number, OffnetReq) ->
    case stepswitch_resources:endpoints(Number, OffnetReq) of
        [] -> publish_no_resources(OffnetReq);
        Endpoints -> stepswitch_request_sup:originate(Endpoints, OffnetReq)
    end.

-spec local_originate(wh_proplist(), wh_json:object()) -> any().
local_originate(Props, JObj) ->
    Endpoints = [create_loopback_endpoint(Props, JObj)],
    J = wh_json:set_value(<<"Simplify-Loopback">>, <<"false">>, JObj),
    stepswitch_request_sup:originate(Endpoints, J).

-spec local_originate_caller_id(wh_json:object()) -> {api_binary(), api_binary()}.
local_originate_caller_id(JObj) ->
    {wh_json:get_first_defined([<<"Outbound-Caller-ID-Number">>
                                ,<<"Emergency-Caller-ID-Number">>
                               ], JObj)
     ,wh_json:get_first_defined([<<"Outbound-Caller-ID-Name">>
                                 ,<<"Emergency-Caller-ID-Name">>
                                ], JObj)
    }.

-spec get_account_realm(ne_binary()) -> ne_binary().
get_account_realm(AccountId) ->
    case kz_account:fetch(AccountId) of
        {'ok', JObj} -> kz_account:realm(JObj, AccountId);
        _ -> AccountId
    end.

-spec create_loopback_endpoint(wh_proplist(), wh_json:object()) -> any().
create_loopback_endpoint(Props, JObj) ->
    {CIDNum, CIDName} = local_originate_caller_id(JObj),
    lager:debug("set outbound caller id to ~s '~s'", [CIDNum, CIDName]),
    Number = props:get_value('number', Props),
    AccountId = props:get_value('account_id', Props),
    Realm = get_account_realm(AccountId),
    CCVs = props:filter_undefined(
                   [{<<?CHANNEL_LOOPBACK_HEADER_PREFIX, "Inception">>, <<Number/binary, "@", Realm/binary>>}
                    ,{<<?CHANNEL_LOOPBACK_HEADER_PREFIX, "Account-ID">>, AccountId}
                    ,{<<?CHANNEL_LOOPBACK_HEADER_PREFIX, "Retain-CID">>, "true"}
                    ,{<<"Resource-ID">>, AccountId}
                   ]),
    Endpoint = wh_json:from_list(
                 props:filter_undefined(
                   [{<<"Invite-Format">>, <<"loopback">>}
                    ,{<<"Route">>,  Number}
                    ,{<<"To-DID">>, Number}
                    ,{<<"To-Realm">>, Realm}
                    ,{<<"Custom-Channel-Vars">>, wh_json:from_list(CCVs)}
                    ,{<<"Outbound-Caller-ID-Name">>, CIDName}
                    ,{<<"Outbound-Caller-ID-Number">>, CIDNum}
                    ,{<<"Caller-ID-Name">>, CIDName}
                    ,{<<"Caller-ID-Number">>, CIDNum}
                    ,{<<"Ignore-Early-Media">>, 'true'}
                   ])),
    Endpoint.
%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec publish_no_resources(wapi_offnet_resource:req()) -> 'ok'.
publish_no_resources(OffnetReq) ->
    case wapi_offnet_resource:server_id(OffnetReq) of
        'undefined' -> 'ok';
        ResponseQ ->
            wapi_offnet_resource:publish_resp(ResponseQ, no_resources(OffnetReq))
    end.

-spec no_resources(wapi_offnet_resource:req()) -> wh_proplist().
no_resources(OffnetReq) ->
    ToDID = wapi_offnet_resource:to_did(OffnetReq),
    lager:info("no available resources for ~s", [ToDID]),
    props:filter_undefined(
      [{?KEY_TO_DID, ToDID}
       ,{<<"Response-Message">>, <<"NO_ROUTE_DESTINATION">>}
       ,{<<"Response-Code">>, <<"sip:404">>}
       ,{<<"Error-Message">>, <<"no available resources">>}
       ,{?KEY_CALL_ID, wapi_offnet_resource:call_id(OffnetReq)}
       ,{?KEY_MSG_ID, wapi_offnet_resource:msg_id(OffnetReq)}
       | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
      ]).
