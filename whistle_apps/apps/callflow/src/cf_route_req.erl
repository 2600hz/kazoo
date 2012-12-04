%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2012, VoIP INC
%%% @doc
%%% handler for route requests, responds if callflows match
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(cf_route_req).

-include("callflow.hrl").

-export([handle_req/2]).

-spec handle_req/2 :: (wh_json:object(), wh_proplist()) -> 'ok'.
handle_req(JObj, Options) ->
    Call = whapps_call:from_route_req(JObj),
    case is_binary(whapps_call:account_id(Call)) andalso callflow_should_respond(Call) of
        true ->
            lager:debug("received route request"),
            ControllerQ = props:get_value(queue, Options),
            AllowNoMatch = whapps_call:authorizing_type(Call) =/= undefined
                orelse whapps_call:custom_channel_var(<<"Referred-By">>, Call) =/= undefined,
            case cf_util:lookup_callflow(Call) of
                %% if NoMatch is false then allow the callflow or if it is true and we are able allowed
                %% to use it for this call
                {ok, Flow, NoMatch} when (not NoMatch) orelse AllowNoMatch ->
                    lager:debug("callflow ~s in ~s satisfies request", [wh_json:get_value(<<"_id">>, Flow)
                                                                        ,whapps_call:account_id(Call)
                                                                       ]),
                    cache_call(Flow, NoMatch, ControllerQ, Call),
                    send_route_response(JObj, ControllerQ, <<"false">>, Call);
                {ok, _, true} ->
                    lager:debug("only available callflow is a nomatch for a unauthorized call", []),
                    maybe_send_defered_route_response(JObj, ControllerQ, Call);
                {error, R} ->
                    lager:debug("unable to find callflow ~p", [R]),
                    maybe_send_defered_route_response(JObj, ControllerQ, Call)
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
        <<"user">> -> true;
        <<"device">> -> true;
        <<"callforward">> -> true;
        <<"clicktocall">> -> true;
        <<"resource">> -> true;
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
-spec send_route_response/4 :: (wh_json:object(), ne_binary(), ne_binary(), whapps_call:call()) -> 'ok'.
send_route_response(JObj, Q, Defer, Call) ->
    Resp = [{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
            ,{<<"Routes">>, []}
            ,{<<"Method">>, <<"park">>}
            ,{<<"Defer-Response">>, Defer}
            ,{<<"Pre-Park">>, pre_park_action(Call)}
            | wh_api:default_headers(Q, ?APP_NAME, ?APP_VERSION)
           ],
    wapi_route:publish_resp(wh_json:get_value(<<"Server-ID">>, JObj), Resp),
    lager:debug("sent route response to park the call").

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%-----------------------------------------------------------------------------
-spec pre_park_action/1 :: (whapps_call:call()) -> ne_binary().
pre_park_action(Call) ->
    case whapps_config:get_is_true(<<"callflow">>, <<"ring_ready_offnet">>, true)
        andalso whapps_call:inception(Call) =:= <<"off-net">>
        andalso whapps_call:authorizing_type(Call) =:= undefined
    of
        false -> <<"none">>;
        true -> <<"ring_ready">>
    end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%%
%% process
%% @end
%%-----------------------------------------------------------------------------
-spec maybe_send_defered_route_response/3 :: (wh_json:object(), ne_binary(), whapps_call:call()) -> 'ok'.
maybe_send_defered_route_response(JObj, ControllerQ, Call) ->
    AccountId = whapps_call:account_id(Call),
    case cf_util:lookup_callflow(<<"0">>, AccountId) of
        {ok, Flow, false} ->
            lager:debug("default callflow ~s in ~s could satisfy request", [wh_json:get_value(<<"_id">>, Flow)
                                                                            ,whapps_call:account_id(Call)
                                                                           ]),
            cache_call(Flow, false, ControllerQ, Call),
            send_route_response(JObj, ControllerQ, <<"true">>, Call);
        _Else ->
            ok
    end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%%
%% process
%% @end
%%-----------------------------------------------------------------------------
-spec cache_call/4 :: (wh_json:object(), boolean(), ne_binary(), whapps_call:call()) -> 'ok'.
cache_call(Flow, NoMatch, ControllerQ, Call) ->
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
    whapps_call:cache(lists:foldr(fun(F, C) -> F(C) end, Call, Updaters)).
