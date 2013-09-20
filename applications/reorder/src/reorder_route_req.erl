%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2012, VoIP INC
%%% @doc
%%% handler for route requests, responds if reorder match
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(reorder_route_req).

-include("reorder.hrl").

-export([handle_req/2]).

-spec handle_req/2 :: (wh_json:object(), wh_proplist()) -> 'ok'.
handle_req(JObj, Props) ->
    Call = whapps_call:from_route_req(JObj),
    case whapps_call:authorizing_id(Call) =:= 'undefined' of
        'true' ->
            lager:info("received a request asking if callflows can route this call"),
            ControllerQ = props:get_value('queue', Props),
            send_route_response(JObj, ControllerQ);
        'false' -> 'ok'
    end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% send a route response for a route request that can be fulfilled by this
%% process
%% @end
%%-----------------------------------------------------------------------------
-spec send_route_response/2 :: (wh_json:object(), ne_binary()) -> 'ok'.
send_route_response(JObj, Q) ->
    ErrorCode = whapps_config:get_binary(<<"reorder">>, <<"error-code">>, <<"604">>),
    ErrorMsg = whapps_config:get_binary(<<"reorder">>, <<"error-message">>, <<"PEBCAK">>),
    Resp = [{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
            ,{<<"Method">>, <<"error">>}
            ,{<<"Route-Error-Code">>, ErrorCode}
            ,{<<"Route-Error-Message">>, ErrorMsg}
            ,{<<"Defer-Response">>, <<"true">>}
            | wh_api:default_headers(Q, ?APP_NAME, ?APP_VERSION)
           ],
    wapi_route:publish_resp(wh_json:get_value(<<"Server-ID">>, JObj), Resp).
