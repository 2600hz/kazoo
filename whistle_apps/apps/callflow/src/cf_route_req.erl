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

-spec handle_req(wh_json:object(), wh_proplist()) -> 'ok'.
handle_req(JObj, Options) ->
    Call = whapps_call:from_route_req(JObj),
    case is_binary(whapps_call:account_id(Call)) andalso callflow_should_respond(Call) of
        true ->
            lager:info("received a request asking if callflows can route this call"),
            ControllerQ = props:get_value(queue, Options),
            AllowNoMatch = allow_no_match(Call),
            case cf_util:lookup_callflow(Call) of
                %% if NoMatch is false then allow the callflow or if it is true and we are able allowed
                %% to use it for this call
                {ok, Flow, NoMatch} when (not NoMatch) orelse AllowNoMatch ->
                    lager:info("callflow ~s in ~s satisfies request", [wh_json:get_value(<<"_id">>, Flow)
                                                                        ,whapps_call:account_id(Call)
                                                                       ]),
                    cache_call(Flow, NoMatch, ControllerQ, Call),
                    send_route_response(Flow, JObj, ControllerQ, Call);
                {ok, _, true} ->
                    lager:info("only available callflow is a nomatch for a unauthorized call", []);
                {error, R} ->
                    lager:info("unable to find callflow ~p", [R])
            end;
        false ->
            ok
    end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% Should this call be able to use outbound resources, the exact opposite
%% exists in the handoff module.  When updating this one make sure to sync
%% the change with that module
%% @end
%%-----------------------------------------------------------------------------
-spec allow_no_match(whapps_call:call()) -> boolean().
allow_no_match(Call) ->
    whapps_call:custom_channel_var(<<"Referred-By">>, Call) =/= undefined
        orelse (whapps_call:authorizing_type(Call) =/= undefined
                andalso whapps_call:authorizing_type(Call) =/= <<"resource">>).

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% determine if callflows should respond to a route request
%% @end
%%-----------------------------------------------------------------------------
-spec callflow_should_respond(whapps_call:call()) -> boolean().
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
-spec send_route_response(wh_json:object(), wh_json:object(), ne_binary(), whapps_call:call()) -> 'ok'.
send_route_response(Flow, JObj, Q, Call) ->
    Resp = props:filter_undefined([{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
                                   ,{<<"Routes">>, []}
                                   ,{<<"Method">>, <<"park">>}
                                   ,{<<"Transfer-Media">>, get_transfer_media(Flow, JObj)}
                                   ,{<<"Ringback-Media">>, get_ringback_media(Flow, JObj)}
                                   ,{<<"Pre-Park">>, pre_park_action(Call)}
                                   | wh_api:default_headers(Q, ?APP_NAME, ?APP_VERSION)
                                  ]),
    ServerId = wh_json:get_value(<<"Server-ID">>, JObj),
    Publisher = fun(P) -> wapi_route:publish_resp(ServerId, P) end,
    whapps_util:amqp_pool_send(Resp, Publisher),
    lager:info("callflows knows how to route the call! sent park response").

-spec get_transfer_media(wh_json:object(), wh_json:object()) -> api_binary().
get_transfer_media(Flow, JObj) ->
    case wh_json:get_value([<<"ringback">>, <<"transfer">>], Flow) of
        'undefined' ->
            wh_json:get_value(<<"Transfer-Media">>, JObj);
        MediaId -> MediaId
    end.

-spec get_ringback_media(wh_json:object(), wh_json:object()) -> api_binary().
get_ringback_media(Flow, JObj) ->
    case wh_json:get_value([<<"ringback">>, <<"early">>], Flow) of
        'undefined' ->
            wh_json:get_value(<<"Ringback-Media">>, JObj);
        MediaId -> MediaId
    end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%-----------------------------------------------------------------------------
-spec pre_park_action(whapps_call:call()) -> ne_binary().
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
-spec cache_call(wh_json:object(), boolean(), ne_binary(), whapps_call:call()) -> 'ok'.
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
