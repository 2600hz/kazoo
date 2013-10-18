%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2013, 2600Hz
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
%% process a Whistle offnet resource request (outbound) for a audio
%% route
%% @end
%%--------------------------------------------------------------------
-spec handle_req(wh_json:object(), wh_proplist()) -> any().
handle_req(JObj, _Props) ->
    'true' = wapi_offnet_resource:req_v(JObj),
    _ = whapps_util:put_callid(JObj),
    case wh_json:get_value(<<"Resource-Type">>, JObj) of
        <<"audio">> -> handle_audio_req(JObj);
        <<"originate">> -> handle_originate_req(JObj)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec handle_audio_req(wh_json:object()) -> any().
handle_audio_req(JObj) ->
    Number = stepswitch_util:get_outbound_destination(JObj),
    lager:debug("received outbound audio resource request for ~s", [Number]),
    case stepswitch_util:lookup_number(Number) of
        {'ok', AccountId, Props} ->
            maybe_force_outbound([{'account_id', AccountId}
                                  | Props
                                 ], JObj);
        _ -> maybe_bridge(Number, JObj)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec handle_originate_req(wh_json:object()) -> any().
handle_originate_req(JObj) ->
    Number = stepswitch_util:get_outbound_destination(JObj),
    lager:debug("received outbound audio resource request for ~s from account ~s"
                ,[Number, wh_json:get_value(<<"Account-ID">>, JObj)]),
    case wh_json:get_value(<<"Outbound-Call-ID">>, JObj) of
        'undefined' -> 
            J = wh_json:set_value(<<"Outbound-Call-ID">>, wh_util:rand_hex_binary(8), JObj),
            maybe_originate(Number, J);
        _Else -> maybe_originate(Number, JObj)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec maybe_force_outbound(wh_proplist(), wh_json:object()) -> any().
maybe_force_outbound(Props, JObj) ->
    case props:get_is_true('force_outbound', Props) orelse
        wh_json:is_true(<<"Force-Outbound">>, JObj, 'false')
    of
        'false' -> local_extension(Props, JObj);
        'true' -> 
            Number = props:get_value('number', Props),
            maybe_bridge(Number, JObj)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec maybe_bridge(ne_binary(), wh_json:object()) -> any().
maybe_bridge(Number, JObj) ->
    case stepswitch_resources:endpoints(Number, JObj) of
        [] -> publish_no_resources(JObj);
        Endpoints -> stepswitch_request_sup:bridge(Endpoints, JObj)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec local_extension(wh_proplist(), wh_json:object()) -> any().
local_extension(Props, JObj) -> stepswitch_request_sup:local_extension(Props, JObj).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec maybe_originate(ne_binary(), wh_json:object()) -> any().
maybe_originate(Number, JObj) ->
    case stepswitch_resources:endpoints(Number, JObj) of
        [] -> publish_no_resources(JObj);
        Endpoints -> stepswitch_request_sup:originate(Endpoints, JObj)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec publish_no_resources(wh_json:object()) -> 'ok'.
publish_no_resources(JObj) ->
    case wh_json:get_ne_value(<<"Server-ID">>, JObj) of
        'undefined' -> 'ok';
        ResponseQ ->
            wapi_offnet_resource:publish_resp(ResponseQ, no_resources(JObj))
    end.

-spec no_resources(wh_json:object()) -> wh_proplist().
no_resources(JObj) ->
    ToDID = wh_json:get_value(<<"To-DID">>, JObj),
    lager:info("no available resources for ~s", [ToDID]),
    props:filter_undefined([{<<"To-DID">>, ToDID}
                            ,{<<"Response-Message">>, <<"NO_ROUTE_DESTINATION">>}
                            ,{<<"Response-Code">>, <<"sip:404">>}
                            ,{<<"Error-Message">>, <<"no available resources">>}
                            ,{<<"Call-ID">>, wh_json:get_value(<<"Call-ID">>, JObj)}
                            ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj, <<>>)}
                            | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                           ]).
