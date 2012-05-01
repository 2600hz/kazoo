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
            lager:debug("received route request"),
            AllowNoMatch = whapps_call:authorizing_type(Call) =/= undefined
                orelse whapps_call:custom_channel_var(<<"Referred-By">>, Call) =/= undefined,
            case cf_util:lookup_callflow(Call) of
                %% if NoMatch is false then allow the callflow or if it is true and we are able allowed
                %% to use it for this call
                {ok, Flow, NoMatch} when (not NoMatch) orelse AllowNoMatch ->
                    lager:debug("callflow ~s in ~s satisfies request", [wh_json:get_value(<<"_id">>, Flow)
                                                                        ,whapps_call:account_id(Call)
                                                                       ]),                    
                    ControllerQ = props:get_value(queue, Options),
                    Updaters = [fun(C) ->
                                        Props = [{cf_flow_id, wh_json:get_value(<<"_id">>, Flow)}
                                                 ,{cf_flow, wh_json:get_value(<<"flow">>, Flow)}
                                                 ,{cf_capture_group, wh_json:get_ne_value(<<"capture_group">>, Flow)}
                                                 ,{cf_no_match, NoMatch}
                                                ], 
                                        whapps_call:kvs_store_proplist(Props, C)
                                end
                                ,fun(C) -> whapps_call:set_controller_queue(ControllerQ, C) end
                                ,fun(C) -> whapps_call:set_application_name(?APP_NAME, C) end
                                ,fun(C) -> whapps_call:set_application_version(?APP_VERSION, C) end
                               ],
                    whapps_call:cache(lists:foldr(fun(F, C) -> F(C) end, Call, Updaters)),
                    send_route_response(JObj, ControllerQ);
                {ok, _, _} ->
                    lager:debug("only available callflow is a nomatch for a unauthorized call", []),
                    ok;
                {error, R} ->
                    lager:debug("unable to find callflow ~p", [R]),
                    ok
            end;
        false ->
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
    lager:debug("sent route response to park the call").
