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
-spec handle_req(wh_json:object(), wh_proplist()) -> any().
handle_req(OffnetJObj, _Props) ->
    'true' = wapi_offnet_resource:req_v(OffnetJObj),
    _ = wh_util:put_callid(OffnetJObj),
    case wapi_offnet_resource:resource_type(OffnetJObj) of
        <<"audio">> -> handle_audio_req(OffnetJObj);
        <<"originate">> -> handle_originate_req(OffnetJObj);
        <<"sms">> -> handle_sms_req(OffnetJObj)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_audio_req(wh_json:object()) -> any().
-spec handle_audio_req(ne_binary(), wh_json:object()) -> any().
handle_audio_req(OffnetJObj) ->
    Number = stepswitch_util:get_outbound_destination(OffnetJObj),
    lager:debug("received outbound audio resource request for ~s: ~p", [Number, OffnetJObj]),
    handle_audio_req(Number, OffnetJObj).

handle_audio_req(Number, OffnetJObj) ->
    case stepswitch_util:lookup_number(Number) of
        {'ok', AccountId, Props} ->
            maybe_force_outbound(wh_number_properties:set_account_id(Props, AccountId), OffnetJObj);
        _ -> maybe_bridge(Number, OffnetJObj)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_originate_req(wh_json:object()) -> any().
handle_originate_req(OffnetJObj) ->
    Number = stepswitch_util:get_outbound_destination(OffnetJObj),
    lager:debug("received outbound audio resource request for ~s from account ~s"
                ,[Number, wapi_offnet_resourece:account_id(OffnetJObj)]
               ),
    maybe_originate(Number
                    ,wh_json:insert_value(<<"Outbound-Call-ID">>, wh_util:rand_hex_binary(8), OffnetJObj)
                   ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_sms_req(wh_json:object()) -> any().
handle_sms_req(OffnetJObj) ->
    Number = stepswitch_util:get_outbound_destination(OffnetJObj),
    lager:debug("received outbound sms resource request for ~s", [Number]),
    case stepswitch_util:lookup_number(Number) of
        {'ok', AccountId, Props} ->
            maybe_force_outbound_sms(wh_number_properties:set_account_id(Props, AccountId), OffnetJObj);
        _ -> maybe_sms(Number, OffnetJObj)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec maybe_force_outbound(wh_proplist(), wh_json:object()) -> any().
maybe_force_outbound(Props, OffnetJObj) ->
    case wh_number_properties:should_force_outbound(Props)
        orelse wapi_offnet_resource:force_outbound(OffnetJObj, 'false')
    of
        'false' -> local_extension(Props, OffnetJObj);
        'true' ->
            Number = wh_number_properties:number(Props),
            maybe_bridge(Number, OffnetJObj)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec maybe_force_outbound_sms(wh_proplist(), wh_json:object()) -> any().
maybe_force_outbound_sms(Props, OffnetJObj) ->
    case props:get_is_true('force_outbound', Props)
        orelse wapi_offnet_resource:force_outbound(OffnetJObj, 'false')
    of
        'false' -> local_sms(Props, OffnetJObj);
        'true' ->
            Number = props:get_value('number', Props),
            maybe_sms(Number, OffnetJObj)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec maybe_bridge(ne_binary(), wh_json:object()) -> any().
maybe_bridge(Number, OffnetJObj) ->
    case stepswitch_resources:endpoints(Number, OffnetJObj) of
        [] -> maybe_correct_shortdial(Number, OffnetJObj);
        Endpoints -> stepswitch_request_sup:bridge(Endpoints, OffnetJObj)
    end.

-spec maybe_correct_shortdial(ne_binary(), wh_json:object()) -> any().
maybe_correct_shortdial(Number, OffnetJObj) ->
    case stepswitch_util:correct_shortdial(Number, OffnetJObj) of
        'undefined' ->
            lager:debug("no endpoints found for '~s', and no shortdial correction available", [Number]),
            publish_no_resources(OffnetJObj);
        Number ->
            lager:debug("shortdial correction invalid, no resources", []),
            publish_no_resources(OffnetJObj);
        CorrectedNumber ->
            lager:debug("corrected shortdial from '~s' to '~s', trying routing again", [Number, CorrectedNumber]),
            handle_audio_req(CorrectedNumber, OffnetJObj)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec maybe_sms(ne_binary(), wh_json:object()) -> any().
maybe_sms(Number, OffnetJObj) ->
    case stepswitch_resources:endpoints(Number, OffnetJObj) of
        [] -> publish_no_resources(OffnetJObj);
        Endpoints -> stepswitch_request_sup:sms(Endpoints, OffnetJObj)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec local_extension(wh_proplist(), wh_json:object()) -> any().
local_extension(Props, OffnetJObj) -> stepswitch_request_sup:local_extension(Props, OffnetJObj).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec local_sms(wh_proplist(), wh_json:object()) -> any().
local_sms(Props, OffnetJObj) -> stepswitch_local_sms:local_message_handling(Props, OffnetJObj).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec maybe_originate(ne_binary(), wh_json:object()) -> any().
maybe_originate(Number, OffnetJObj) ->
    case stepswitch_resources:endpoints(Number, OffnetJObj) of
        [] -> publish_no_resources(OffnetJObj);
        Endpoints -> stepswitch_request_sup:originate(Endpoints, OffnetJObj)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec publish_no_resources(wh_json:object()) -> 'ok'.
publish_no_resources(OffnetJObj) ->
    case wh_api:server_id(OffnetJObj) of
        'undefined' -> 'ok';
        ResponseQ ->
            wapi_offnet_resource:publish_resp(ResponseQ, no_resources(OffnetJObj))
    end.

-spec no_resources(wh_json:object()) -> wh_proplist().
no_resources(OffnetJObj) ->
    ToDID = wapi_offnet_resource:to_did(OffnetJObj),
    lager:info("no available resources for ~s", [ToDID]),
    props:filter_undefined(
      [{<<"To-DID">>, ToDID}
       ,{<<"Response-Message">>, <<"NO_ROUTE_DESTINATION">>}
       ,{<<"Response-Code">>, <<"sip:404">>}
       ,{<<"Error-Message">>, <<"no available resources">>}
       ,{<<"Call-ID">>, wapi_offnet_resource:call_id(OffnetJObj)}
       ,{<<"Msg-ID">>, wh_api:msg_id(OffnetJObj)}
       | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
      ]).
