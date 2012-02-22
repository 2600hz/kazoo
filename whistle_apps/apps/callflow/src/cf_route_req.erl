%%%-------------------------------------------------------------------
%%% @author Karl anderson <karl@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% handler for route requests, responds if callflows match
%%% @end
%%% Created : 30 Nov 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(cf_route_req).

-include("callflow.hrl").

-export([handle_req/2]).

-spec handle_req/2 :: (wh_json:json_object(), proplist()) -> ok.
handle_req(JObj, Options) ->
    Call = whapps_call:from_route_req(JObj),
    case is_binary(whapps_call:account_id(Call)) andalso callflow_should_respond(Call) of
        true ->
            ?LOG_START("received route request"),
            ControllerQ = props:get_value(queue, Options),
            fulfill_call_request(JObj, whapps_call:set_controller_queue(ControllerQ, Call));
        {_, _} ->
            ok
    end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% determine if callflows should respond to a route request
%% @end
%%-----------------------------------------------------------------------------
-spec callflow_should_respond/1 :: (whapps_call:call()) -> boolean().
callflow_should_respond(Call) ->
    case whapps_call:authorizing_type(Call) of
        <<"device">> -> true;
        <<"callforward">> -> true;
        <<"clicktocall">> -> true;
        undefined -> true;
        _Else -> false
    end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% attempt to fulfill authorized call requests
%% @end
%%-----------------------------------------------------------------------------
-spec fulfill_call_request/2 :: (wh_json:json_object(), whapps_call:call()) -> 'ok'.
fulfill_call_request(JObj, Call) ->
    case cf_util:lookup_callflow(Call) of
        {ok, Flow, NoMatch} ->
            FlowId = wh_json:get_value(<<"_id">>, Flow),
            CaptureGroup = wh_json:get_ne_value(<<"capture_group">>, Flow),
            ?LOG("callflow ~s in ~s satisfies request", [FlowId, whapps_call:account_id(Call)]),
            Call2=whapps_call:kvs_store_proplist([{cf_flow_id, FlowId}
                                                  ,{cf_flow, wh_json:get_value(<<"flow">>, Flow)}
                                                  ,{cf_capture_group, CaptureGroup}
                                                  ,{cf_no_match, NoMatch}
                                                 ], Call),
            whapps_call:cache(Call2),
            send_route_response(JObj, whapps_call:controller_queue(Call2));
        {error, R} ->
            ?LOG_END("unable to find callflow ~p", [R]),
            ok
    end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% send a route response for a route request that can be fulfilled by this
%% process
%% @end
%%-----------------------------------------------------------------------------
-spec send_route_response/2 :: (wh_json:json_object(), ne_binary()) -> 'ok'.
send_route_response(JObj, Q) ->
    Resp = [{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
            ,{<<"Routes">>, []}
            ,{<<"Method">>, <<"park">>}
            | wh_api:default_headers(Q, ?APP_NAME, ?APP_VERSION)],
    wapi_route:publish_resp(wh_json:get_value(<<"Server-ID">>, JObj), Resp),
    ?LOG_END("sent route response to park the call").
