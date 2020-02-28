%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc Handle offnet requests, including rating them
%%% @author Karl Anderson
%%% @author James Aimonetti
%%% @author Ben Wann
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(stepswitch_outbound).

-export([handle_req/2]).

-include("stepswitch.hrl").
-include_lib("kazoo_amqp/include/kapi_offnet_resource.hrl").
-include_lib("kazoo_amqp/include/kz_api_literals.hrl").

%%------------------------------------------------------------------------------
%% @doc process an offnet resource request (outbound)
%% route
%% @end
%%------------------------------------------------------------------------------
-spec handle_req(kz_json:object(), kz_term:proplist()) -> any().
handle_req(OffnetJObj, _Props) ->
    'true' = kapi_offnet_resource:req_v(OffnetJObj),
    OffnetReq = kapi_offnet_resource:jobj_to_req(OffnetJObj),
    _ = kapi_offnet_resource:put_callid(OffnetReq),
    case kapi_offnet_resource:resource_type(OffnetReq) of
        ?RESOURCE_TYPE_AUDIO -> handle_audio_req(OffnetReq);
        ?RESOURCE_TYPE_ORIGINATE -> handle_originate_req(OffnetReq)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec handle_audio_req(kapi_offnet_resource:req()) -> any().
handle_audio_req(OffnetReq) ->
    Number = stepswitch_util:get_outbound_destination(OffnetReq),
    lager:debug("received outbound audio resource request for ~s: ~p", [Number, OffnetReq]),
    handle_audio_req(Number, OffnetReq).

-spec handle_audio_req(kz_term:ne_binary(), kapi_offnet_resource:req()) -> any().
handle_audio_req(Number, OffnetReq) ->
    case knm_numbers:lookup_account(Number) of
        {'ok', _AccountId, Props} -> maybe_force_outbound(Props, OffnetReq);
        _ -> maybe_bridge(Number, OffnetReq)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec handle_originate_req(kapi_offnet_resource:req()) -> any().
handle_originate_req(OffnetReq) ->
    Number = stepswitch_util:get_outbound_destination(OffnetReq),
    lager:debug("received outbound audio resource request for ~s from account ~s"
               ,[Number, kapi_offnet_resource:account_id(OffnetReq)]
               ),
    handle_originate_req(Number, maybe_add_call_id(kz_json:get_value(<<"Outbound-Call-ID">>, OffnetReq) , OffnetReq)).

-spec handle_originate_req(kz_term:ne_binary(), kapi_offnet_resource:req()) -> any().
handle_originate_req(Number, OffnetReq) ->
    case knm_numbers:lookup_account(Number) of
        {'ok', _AccountId, Props} ->
            maybe_force_originate_outbound(Props, OffnetReq);
        _ -> maybe_originate(Number, OffnetReq)
    end.

-spec maybe_add_call_id(kz_term:api_binary(), kapi_offnet_resource:req()) -> kz_json:object().
maybe_add_call_id('undefined', OffnetReq) ->
    kz_json:set_value(<<"Outbound-Call-ID">>, kz_binary:rand_hex(8), OffnetReq);
maybe_add_call_id(_, OffnetReq) -> OffnetReq.

-spec maybe_force_originate_outbound(knm_options:extra_options(), kapi_offnet_resource:req()) -> any().
maybe_force_originate_outbound(Props, OffnetReq) ->
    case knm_options:should_force_outbound(Props)
        orelse kz_json:is_true(<<"Force-Outbound">>, OffnetReq, 'false')
        orelse kz_term:is_ne_binary(kapi_offnet_resource:hunt_account_id(OffnetReq))
    of
        'false' -> local_originate(Props, OffnetReq);
        'true' -> maybe_originate(knm_options:number(Props), OffnetReq)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_force_outbound(knm_options:extra_options(), kapi_offnet_resource:req()) -> any().
maybe_force_outbound(Props, OffnetReq) ->
    case knm_options:should_force_outbound(Props)
        orelse kapi_offnet_resource:force_outbound(OffnetReq, 'false')
        orelse kapi_offnet_resource:hunt_account_id(OffnetReq) /= 'undefined'
    of
        'false' -> local_extension(Props, OffnetReq);
        'true' -> maybe_bridge(knm_options:number(Props), OffnetReq)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_bridge(kz_term:ne_binary(), kapi_offnet_resource:req()) -> any().
maybe_bridge(Number, OffnetReq) ->
    RouteBy = stepswitch_util:route_by(),
    case RouteBy:endpoints(Number, OffnetReq) of
        [] -> maybe_correct_shortdial(Number, OffnetReq);
        Endpoints -> stepswitch_request_sup:bridge(Endpoints, OffnetReq)
    end.

-spec maybe_correct_shortdial(kz_term:ne_binary(), kapi_offnet_resource:req()) -> any().
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

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec local_extension(knm_options:extra_options(), kapi_offnet_resource:req()) ->
          kz_types:sup_startchild_ret().
local_extension(Props, OffnetReq) ->
    stepswitch_request_sup:local_extension(Props, OffnetReq).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_originate(kz_term:ne_binary(), kapi_offnet_resource:req()) -> any().
maybe_originate(Number, OffnetReq) ->
    RouteBy = stepswitch_util:route_by(),
    case RouteBy:endpoints(Number, OffnetReq) of
        [] -> publish_no_resources(OffnetReq);
        Endpoints -> stepswitch_request_sup:originate(Endpoints, OffnetReq)
    end.

-spec local_originate(knm_options:extra_options(), kapi_offnet_resource:req()) -> any().
local_originate(Props, OffnetReq) ->
    Endpoints = [create_loopback_endpoint(Props, OffnetReq)],
    J = kz_json:set_values([{<<"Simplify-Loopback">>, <<"false">>}
                           ,{<<"Loopback-Bowout">>, <<"false">>}
                           ], OffnetReq),
    lager:debug("originate local request"),
    stepswitch_request_sup:originate(Endpoints, J).

-spec local_originate_caller_id(kapi_offnet_resource:req()) -> {kz_term:api_binary(), kz_term:api_binary()}.
local_originate_caller_id(OffnetReq) ->
    {kz_json:get_first_defined([<<"Outbound-Caller-ID-Number">>
                               ,<<"Emergency-Caller-ID-Number">>
                               ], OffnetReq)
    ,kz_json:get_first_defined([<<"Outbound-Caller-ID-Name">>
                               ,<<"Emergency-Caller-ID-Name">>
                               ], OffnetReq)
    }.

-spec get_account_realm(kz_term:ne_binary()) -> kz_term:ne_binary().
get_account_realm(AccountId) ->
    case kzd_accounts:fetch_realm(AccountId) of
        'undefined' -> AccountId;
        Realm -> Realm
    end.

-spec create_loopback_endpoint(knm_options:extra_options(), kapi_offnet_resource:req()) ->
          kz_json:object().
create_loopback_endpoint(Props, OffnetReq) ->
    {CIDNum, CIDName} = local_originate_caller_id(OffnetReq),
    lager:debug("set outbound caller id to ~s '~s'", [CIDNum, CIDName]),
    Number = knm_options:number(Props),
    TargetAccountId = knm_options:account_id(Props),
    TargetResellerId = kz_services_reseller:get_id(TargetAccountId),
    TargetRealm = get_account_realm(TargetAccountId),
    OriginalAccountId = kapi_offnet_resource:account_id(OffnetReq),
    OriginalResellerId = kz_services_reseller:get_id(OriginalAccountId),
    FromRealm = get_account_realm(OriginalAccountId),
    CCVs = kz_json:from_list(
             [{<<"Account-ID">>, OriginalAccountId}
             ,{<<"Reseller-ID">>, OriginalResellerId}
             ,{<<"Realm">>, FromRealm}
             ,{<<"Resource-ID">>, TargetAccountId}
             ,{<<"Resource-Type">>, <<"onnet-termination">>}
             ,{<<"From-URI">>, <<CIDNum/binary, "@", FromRealm/binary>>}
             ,{<<"Request-URI">>, <<Number/binary, "@", FromRealm/binary>>}
             ,{<<"To-URI">>, <<Number/binary, "@", FromRealm/binary>>}

             ,{<<?CHANNEL_LOOPBACK_HEADER_PREFIX, "Inception">>, <<Number/binary, "@", TargetRealm/binary>>}
             ,{<<?CHANNEL_LOOPBACK_HEADER_PREFIX, "Account-ID">>, TargetAccountId}
             ,{<<?CHANNEL_LOOPBACK_HEADER_PREFIX, "Reseller-ID">>, TargetResellerId}
             ,{<<?CHANNEL_LOOPBACK_HEADER_PREFIX, "Retain-CID">>, "true"}
             ,{<<?CHANNEL_LOOPBACK_HEADER_PREFIX, "Resource-Type">>, <<"onnet-origination">>}
             ,{<<?CHANNEL_LOOPBACK_HEADER_PREFIX, "From-URI">>, <<CIDNum/binary, "@", TargetRealm/binary>>}
             ,{<<?CHANNEL_LOOPBACK_HEADER_PREFIX, "Request-URI">>, <<Number/binary, "@", TargetRealm/binary>>}
             ,{<<?CHANNEL_LOOPBACK_HEADER_PREFIX, "To-URI">>, <<Number/binary, "@", TargetRealm/binary>>}
             ,{<<?CHANNEL_LOOPBACK_HEADER_PREFIX, "SIP-Invite-Domain">>, TargetRealm}
             ]),
    CAVs = kapi_offnet_resource:custom_application_vars(OffnetReq),
    kz_json:from_list(
      [{<<"Caller-ID-Name">>, CIDName}
      ,{<<"Caller-ID-Number">>, CIDNum}
      ,{<<"Custom-Application-Vars">>, CAVs}
      ,{<<"Custom-Channel-Vars">>, CCVs}
      ,{<<"Enable-T38-Fax">>, 'false'}
      ,{<<"Enable-T38-Fax-Request">>, 'false'}
      ,{<<"Ignore-Early-Media">>, 'true'}
      ,{<<"Ignore-Early-Media">>, 'true'}
      ,{<<"Invite-Format">>, <<"loopback">>}
      ,{<<"Outbound-Caller-ID-Name">>, CIDName}
      ,{<<"Outbound-Caller-ID-Number">>, CIDNum}
      ,{<<"Route">>, Number}
      ,{<<"To-DID">>, Number}
      ,{<<"To-Realm">>, TargetRealm}
      ]).
%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec publish_no_resources(kapi_offnet_resource:req()) -> 'ok'.
publish_no_resources(OffnetReq) ->
    case kapi_offnet_resource:server_id(OffnetReq) of
        'undefined' -> 'ok';
        ResponseQ ->
            kapi_offnet_resource:publish_resp(ResponseQ, no_resources(OffnetReq))
    end.

-spec no_resources(kapi_offnet_resource:req()) -> kz_term:proplist().
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
