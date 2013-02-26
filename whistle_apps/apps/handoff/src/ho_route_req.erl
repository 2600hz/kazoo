%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2012, VoIP INC
%%% @doc
%%% handler for route requests, responds if handoff match
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(ho_route_req).

-include("handoff.hrl").

-export([handle_req/2]).

-spec handle_req/2 :: (wh_json:object(), wh_proplist()) -> 'ok'.
handle_req(JObj, Props) ->
    Call = whapps_call:from_route_req(JObj),
    case is_binary(whapps_call:account_id(Call))
        andalso handoff_should_respond(Call)
    of
        true ->
            lager:info("received a request asking if callflows can route this call"),
            ControllerQ = props:get_value(queue, Props),
            send_route_response(JObj, ControllerQ);
        false ->
            ok
    end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% determine if handoff should respond to a route request
%% @end
%%-----------------------------------------------------------------------------
-spec handoff_should_respond/1 :: (whapps_call:call()) -> boolean().
handoff_should_respond(Call) ->
    whapps_call:custom_channel_var(<<"Referred-By">>, Call) =:= undefined
        andalso (whapps_call:authorizing_type(Call) =:= undefined
                orelse whapps_call:authorizing_type(Call) =:= <<"resource">>).

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% send a route response for a route request that can be fulfilled by this
%% process
%% @end
%%-----------------------------------------------------------------------------
-spec send_route_response/2 :: (wh_json:object(), ne_binary()) -> 'ok'.
send_route_response(JObj, Q) ->
    ErrorCode = whapps_config:get_binary(<<"handoff">>, <<"error-code">>, <<"604">>),
    ErrorMsg = whapps_config:get_binary(<<"handoff">>, <<"error-message">>, <<"Misconfigured configured number">>),
    Resp = [{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
            ,{<<"Method">>, <<"error">>}
            ,{<<"Route-Error-Code">>, ErrorCode}
            ,{<<"Route-Error-Message">>, ErrorMsg}
            ,{<<"Defer-Response">>, <<"true">>}
            | wh_api:default_headers(Q, ?APP_NAME, ?APP_VERSION)
           ],
    wapi_route:publish_resp(wh_json:get_value(<<"Server-ID">>, JObj), Resp).
