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

-export([init/0]).
-export([handle_req/2]).

-include("stepswitch.hrl").
-include_lib("whistle_number_manager/include/wh_number_manager.hrl").

-type bridge_resp() :: {'error', wh_json:object()} |
                       {'error', 'timeout'} |
                       {'ok', wh_json:object()} |
                       {'fail', wh_json:object()}.

-type execute_ext_resp() ::  {'ok', wh_json:object()} |
                             {'ok', 'execute_extension'} |
                             {'fail', wh_json:object()}.

-type originate_resp() :: {'error', wh_json:object()} |
                          {'ok', wh_json:object()} |
                          {'ready', wh_json:object()} |
                          {'fail', wh_json:object()}.

init() -> 'ok'.

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
    lager:debug("received outbound request"),
    case wh_json:get_value(<<"Resource-Type">>, JObj) of
        <<"audio">> -> handle_audio_req(JObj);
        <<"originate">> -> handle_originate_req(JObj)
    end.

handle_audio_req(JObj) ->
    Number = stepswitch_util:get_outbound_destination(JObj),
    case stepswitch_util:lookup_number(Number) of
        {'ok', Props} -> maybe_force_outbound(Props, JObj);
        _ -> maybe_bridge(Number, JObj)
    end.

%%    {Number, _} = whapps_util:get_destination(JObj, ?APP_NAME, <<"outbound_user_field">>),
%%    lager:debug("bridge request to ~s from account ~s", [Number, wh_json:get_value(<<"Account-ID">>, JObj)]),
%%    CtrlQ = wh_json:get_value(<<"Control-Queue">>, JObj),
%%    Result = attempt_to_fulfill_bridge_req(Number, CtrlQ, JObj, Props),
%%    wapi_offnet_resource:publish_resp(wh_json:get_value(<<"Server-ID">>, JObj), response(Result, JObj)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
handle_originate_req(JObj) ->
    'ok'.
%%    {Number, _} = whapps_util:get_destination(JObj, ?APP_NAME, <<"outbound_user_field">>),
%%    lager:debug("originate request to ~s from account ~s", [Number, wh_json:get_value(<<"Account-ID">>, JObj)]),
%%    Result = attempt_to_fulfill_originate_req(Number, JObj, Props),
%%    wapi_offnet_resource:publish_resp(wh_json:get_value(<<"Server-ID">>, JObj), response(Result, JObj)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------    
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
maybe_bridge(Number, JObj) ->
    case stepswitch_resources:endpoints(Number, JObj) of
        [] -> {'error', 'no_resources'};
        Endpoints -> 
            R = stepswitch_bridge:build_bridge(Endpoints, JObj),
            io:format("Bridge: ~p~n", [R])
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
local_extension(Props, JObj) ->
    'ok'.











