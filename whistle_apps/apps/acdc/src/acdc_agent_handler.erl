%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% Handlers for various call events, acdc events, etc
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(acdc_agent_handler).

%% Listener callbacks
-export([handle_status_update/2
         ,handle_sync_req/2
         ,handle_sync_resp/2
         ,handle_call_event/2
         ,handle_member_message/2
        ]).

-include("acdc.hrl").

-spec handle_status_update/2 :: (wh_json:json_object(), wh_proplist()) -> 'ok'.
handle_status_update(_JObj, _Props) ->
    ok.

-spec handle_sync_req/2 :: (wh_json:json_object(), wh_proplist()) -> 'ok'.
handle_sync_req(JObj, Props) ->
    Defaults = wh_api:default_headers(props:get_value(queue, Props)
                                      ,?APP_NAME
                                      ,?APP_VERSION
                                     ),

    case props:get_value(status, Props) of
        init -> lager:debug("in init ourselves, ignoring sync request");
        ready -> sync_resp(JObj, ready, props:get_value(my_id, Props), Defaults);
        waiting -> sync_resp(JObj, waiting, props:get_value(my_id, Props), Defaults);
        ringing -> sync_resp(JObj, ringing, props:get_value(my_id, Props), Defaults);
        answered -> sync_resp(JObj, answered, props:get_value(my_id, Props)
                              ,[{<<"Call-ID">>, props:get_value(callid, Props)}
                                | Defaults
                               ]
                             );
        wrapup -> sync_resp(JObj, wrapup, props:get_value(my_id, Props)
                            ,[{<<"Time-Left">>, props:get_value(time_left, Props)}
                              | Defaults
                             ]
                           );
        paused -> sync_resp(JObj, paused, props:get_value(my_id, Props)
                            ,[{<<"Time-Left">>, props:get_value(time_left, Props)}
                              | Defaults
                             ]
                           )
    end.

-spec handle_sync_resp/2 :: (wh_json:json_object(), wh_proplist()) -> 'ok'.
handle_sync_resp(JObj, Props) ->
    case props:get_value(status, Props) of
        init -> gen_listener:cast(props:get_value(server, Props), {recv_sync_resp, JObj});
        _S -> lager:debug("ignoring sync_resp, in status ~s", [_S])
    end.

-spec handle_call_event/2 :: (wh_json:json_object(), wh_proplist()) -> 'ok'.
-spec handle_call_event/3 :: (wh_json:json_object(), wh_proplist(), acdc_agent:agent_status()) -> 'ok'.
-spec handle_call_event/5 :: (wh_json:json_object(), wh_proplist(), acdc_agent:agent_status(), ne_binary(), ne_binary()) -> 'ok'.
handle_call_event(JObj, Props) ->
    case props:get_value(callid, Props) =:= wh_json:get_value(<<"Call-ID">>, JObj) of
        true ->
            handle_call_event(JObj, Props, props:get_value(status, Props));
        false ->
            lager:debug("call event for ~s when we're monitoring ~s"
                        ,[wh_json:get_value(<<"Call-ID">>, JObj),
                          props:get_value(callid, Props)
                         ])
    end.

handle_call_event(JObj, Props, ringing) ->
    case wh_util:get_event_type(JObj) of
        {<<"call_event">>, <<"CHANNEL_BRIDGE">>} ->
            CallId = wh_json:get_value(<<"Other-Leg-Unique-ID">>, JObj),
            gen_listener:cast(props:get_value(server, Props)
                              ,{call_bridged, CallId}
                             );
        {Cat, Name} ->
            handle_call_event(JObj, Props, ringing, Cat, Name)
    end;
handle_call_event(JObj, Props, Status) ->
    {Cat, Name} = wh_util:get_event_type(JObj),
    handle_call_event(JObj, Props, Status, Cat, Name).

%% Generic call event handler
handle_call_event(JObj, Props, _, <<"call_event">>, <<"CHANNEL_DESTROY">>) ->
    gen_listener:cast(props:get_value(server, Props)
                      ,{call_hungup, wh_json:get_value(<<"Call-ID">>, JObj)}
                     );
handle_call_event(JObj, Props, ringing, <<"error">>, _) ->
    case application_name(JObj) of
        <<"bridge">> ->
            gen_listener:cast(props:get_value(server, Props)
                              ,{bridge_failed, JObj}
                             );
        _ -> ok
    end;
handle_call_event(_JObj, _, _Status, _, _) ->
    lager:debug("ignore evt in status ~s: ~p", [_Status, _JObj]).

-spec handle_member_message/2 :: (wh_json:json_object(), wh_proplist()) -> 'ok'.
-spec handle_member_message/3 :: (wh_json:json_object(), wh_proplist(), ne_binary()) -> 'ok'.
handle_member_message(JObj, Props) ->
    handle_member_message(JObj, Props, wh_json:get_value(<<"Event-Name">>, JObj)).

handle_member_message(JObj, Props, <<"connect_req">>) ->
    acdc_agent_fsm:member_connect_req(props:get_value(fsm_pid, Props), JObj);
handle_member_message(JObj, Props, <<"connect_win">>) ->
    handle_member_connect_win(JObj, Props, props:get_value(status, Props));
handle_member_message(JObj, Props, <<"connect_monitor">>) ->
    handle_member_connect_monitor(JObj, Props, props:get_value(status, Props));
handle_member_message(JObj, Props, <<"connect_ignore">>) ->
    handle_member_connect_ignore(JObj, Props, props:get_value(status, Props));
handle_member_message(_, _, EvtName) ->
    lager:debug("not handling member event ~s", [EvtName]).

-spec handle_member_connect_win/3 :: (wh_json:json_object(), wh_proplist(), acdc_agent:agent_status()) -> 'ok'.
handle_member_connect_win(JObj, Props, waiting) ->
    lager:debug("recv win for call"),
    gen_listener:cast(props:get_value(server, Props)
                      ,{member_connect_win, JObj}
                     );
handle_member_connect_win(JObj, Props, _Status) ->
    lager:debug("our status changed from waiting to ~s, turning away win", [_Status]),
    send_member_connect_retry(JObj, Props).

%% handle how a member_connect_req payload is handled
-spec handle_member_connect_req/3 :: (wh_json:json_object(), wh_proplist(), acdc_agent:agent_status()) -> 'ok'.
handle_member_connect_req(JObj, Props, ready) ->
    gen_listener:cast(props:get_value(server, Props)
                      ,{member_connect_req, JObj, props:get_value(queue, Props)}
                     );
handle_member_connect_req(_, _Props, _S) ->
    lager:debug("ignoring member connect, agent ~p in status ~s"
                ,[props:get_value(server, _Props), _S]
               ).

-spec sync_resp/4 :: (wh_json:json_object(), acdc_agent:agent_status(), ne_binary(), wh_proplist()) -> 'ok'.
sync_resp(JObj, Status, MyId, Fields) ->
    Resp = props:filter_undefined(
             [{<<"Account-ID">>, wh_json:get_value(<<"Account-ID">>, JObj)}
              ,{<<"Agent-ID">>, wh_json:get_value(<<"Agent-ID">>, JObj)}
              ,{<<"Status">>, wh_util:to_binary(Status)}
              ,{<<"Process-ID">>, MyId}
              | Fields
             ]),
    wapi_agent:publish_sync_resp(wh_json:get_value(<<"Server-ID">>, JObj), Resp).

-spec send_member_connect_retry/2 :: (wh_json:json_object(), wh_proplist()) -> 'ok'.
send_member_connect_retry(JObj, Props) ->
    Resp = [{<<"Call-ID">>, wh_json:get_value(<<"Call-ID">>, JObj)}
            | wh_api:default_headers(props:get_value(queue, Props)
                                     ,?APP_NAME
                                     ,?APP_VERSION
                                    )
           ],
    wapi_acdc_queue:publish_member_connect_retry(wh_json:get_value(<<"Server-ID">>, JObj)
                                                 ,props:filter_undefined(Resp)
                                                ).

-spec application_name/1 :: (wh_json:json_object()) -> binary().
application_name(JObj) ->
    wh_json:get_value(<<"Application-Name">>
                      ,JObj
                      ,wh_json:get_value([<<"Request">>, <<"Application-Name">>]
                                         ,JObj
                                         ,<<>>)
                     ).

-spec handle_member_connect_monitor/3 :: (wh_json:json_object(), wh_proplist(), acdc_agent:agent_status()) -> 'ok'.
handle_member_connect_monitor(JObj, Props, waiting) ->
    gen_listener:cast(props:get_value(server, Props)
                      ,{member_connect_monitor, JObj}
                     );
handle_member_connect_monitor(_, Props, _Status) ->
    lager:debug("ignoring member monitor, agent ~p in status ~s"
                ,[props:get_value(server, Props), _Status]
               ).

-spec handle_member_connect_ignore/3 :: (wh_json:json_object(), wh_proplist(), acdc_agent:agent_status()) -> 'ok'.
handle_member_connect_ignore(JObj, Props, waiting) ->
    gen_listener:cast(props:get_value(server, Props)
                      ,{member_connect_ignore, JObj}
                     );
handle_member_connect_ignore(_, Props, _Status) ->
    lager:debug("ignoring member ignore, agent ~p in status ~s"
                ,[props:get_value(server, Props), _Status]
               ).
