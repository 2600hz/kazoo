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

%%--------------------------------------------------------------------
%% @public
%% @doc
%% process an offnet resource request (outbound)
%% route
%% @end
%%--------------------------------------------------------------------
-spec handle_req(wapi_offnet_resource:req(), wh_proplist()) -> any().
handle_req(OffnetReq, _Props) ->
    'true' = wapi_offnet_resource:req_v(OffnetReq),
    _ = wapi_offnet_resource:put_callid(OffnetReq),
    case wapi_offnet_resource:resource_type(OffnetReq) of
        <<"audio">> -> handle_audio_req(OffnetReq);
        <<"originate">> -> handle_originate_req(OffnetReq);
        <<"sms">> -> handle_sms_req(OffnetReq)
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
    maybe_originate(Number
                    ,wapi_offnet_resource:set_outbound_call_id(OffnetReq, wh_util:rand_hex_binary(8))
                   ).

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
      [{<<"To-DID">>, ToDID}
       ,{<<"Response-Message">>, <<"NO_ROUTE_DESTINATION">>}
       ,{<<"Response-Code">>, <<"sip:404">>}
       ,{<<"Error-Message">>, <<"no available resources">>}
       ,{<<"Call-ID">>, wapi_offnet_resource:call_id(OffnetReq)}
       ,{<<"Msg-ID">>, wapi_offnet_resource:msg_id(OffnetReq)}
       | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
      ]).
