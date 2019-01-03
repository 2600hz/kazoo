%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(ecallmgr_events).

-export[event/4].

-include("ecallmgr.hrl").

-spec event(kz_term:ne_binary(), kz_term:api_binary(), kzd_freeswitch:data(), atom()) -> any().
event(<<"CHANNEL_CREATE">> = EventName, UUID, EventProps, Node) ->
    Props = ecallmgr_fs_loopback:filter(Node, UUID, EventProps, 'true'),
    process_event(EventName, UUID, Props, Node),
    maybe_send_event(EventName, UUID, Props, Node);
event(<<"sofia::transferor">> = EventName, UUID, Props, Node) ->
    _ = case props:get_value(<<"variable_refer_uuid">>, Props) of
            'undefined' -> 'ok';
            ReferUUID ->
                lager:debug("found refer uuid ~s for interaction caching", [ReferUUID]),
                {'ok', Channel} = ecallmgr_fs_channel:fetch(UUID),
                CDR = kz_json:get_value(<<"interaction_id">>, Channel),
                kz_cache:store_local(?ECALLMGR_INTERACTION_CACHE, ReferUUID, CDR),
                lager:debug("caching interaction id ~s for callid ~s", [CDR, ReferUUID]),
                ecallmgr_fs_command:set(Node, ReferUUID, [{<<?CALL_INTERACTION_ID>>, CDR}])
        end,
    maybe_send_event(EventName, UUID, Props, Node),
    process_event(EventName, UUID, Props, Node);
event(<<"sofia::intercepted">> = EventName, UUID, Props, Node) ->
    InterceptedBy = props:get_value(<<"intercepted_by">>, Props),
    _ = case ecallmgr_fs_channel:fetch(UUID, 'record') of
            {'ok', #channel{interaction_id=InterAction
                           ,direction=Direction
                           }
            } ->
                lager:debug("sofia::intercepted: channel ~s Intercepted by ~s", [UUID, InterceptedBy]),
                Vars = [{<<"Application-Logical-Direction">>, Direction}
                       ,{<<?CALL_INTERACTION_ID>>, InterAction}
                       ],
                ecallmgr_fs_command:set(Node, InterceptedBy, Vars);
            _ -> 'ok'
        end,
    ChannelUUID = props:get_value(<<"Channel-Call-UUID">>, Props),
    Updates = props:filter_undefined(
                [{<<"Caller-Callee-ID-Name">>, props:get_value(<<"Caller-Callee-ID-Name">>, Props)}
                ,{<<"Caller-Callee-ID-Number">>, props:get_value(<<"Caller-Callee-ID-Number">>, Props)}
                ]),
    _ = ecallmgr_fs_command:set(Node, ChannelUUID, Updates),
    maybe_send_event(EventName, UUID, Props, Node),
    process_event(EventName, UUID, Props, Node);
event(<<"CHANNEL_HOLD">> = EventName, UUID, Props, Node) ->
    gproc:send({'p', 'l', ?FS_EVENT_REG_MSG(Node, EventName)}, {'event', [UUID | Props]});
event(<<"CHANNEL_UNHOLD">> = EventName, UUID, Props, Node) ->
    gproc:send({'p', 'l', ?FS_EVENT_REG_MSG(Node, EventName)}, {'event', [UUID | Props]});
event(<<"RECORD_START">> = EventName, UUID, Props, Node) ->
    gproc:send({'p', 'l', ?FS_EVENT_REG_MSG(Node, EventName)}, {'event', [UUID | Props]});
event(<<"RECORD_STOP">> = EventName, UUID, Props, Node) ->
    gproc:send({'p', 'l', ?FS_EVENT_REG_MSG(Node, EventName)}, {'event', [UUID | Props]});
event(EventName, UUID, EventProps, Node) ->
    maybe_send_event(EventName, UUID, EventProps, Node),
    process_event(EventName, UUID, EventProps, Node).

-spec process_event(kz_term:ne_binary(), kz_term:api_binary(), kzd_freeswitch:data(), atom()) -> any().
process_event(<<"CHANNEL_CREATE">>, UUID, Props, Node) ->
    _ = ecallmgr_fs_channel:new(Props, Node),
    lager:debug("channel added to cache"),
    _ = ecallmgr_fs_channel:maybe_update_interaction_id(Props, Node),
    maybe_start_event_listener(Node, UUID);
process_event(?CHANNEL_MOVE_RELEASED_EVENT_BIN, _, Props, Node) ->
    UUID = props:get_value(<<"old_node_channel_uuid">>, Props),
    gproc:send({'p', 'l', ?CHANNEL_MOVE_REG(Node, UUID)}
              ,?CHANNEL_MOVE_RELEASED_MSG(Node, UUID, Props)
              );
process_event(?CHANNEL_MOVE_COMPLETE_EVENT_BIN, _, Props, Node) ->
    UUID = props:get_value(<<"old_node_channel_uuid">>, Props),
    gproc:send({'p', 'l', ?CHANNEL_MOVE_REG(Node, UUID)}
              ,?CHANNEL_MOVE_COMPLETE_MSG(Node, UUID, Props)
              );
process_event(<<"sofia::register">>, _UUID, Props, Node) ->
    gproc:send({'p', 'l', ?REGISTER_SUCCESS_REG}, ?REGISTER_SUCCESS_MSG(Node, Props));
process_event(<<"loopback::bowout">>, _UUID, Props, Node) ->
    ResigningUUID = props:get_value(?RESIGNING_UUID, Props),
    kz_util:put_callid(ResigningUUID),
    lager:debug("bowout detected on ~s, transferring to ~s"
               ,[ResigningUUID, props:get_value(?ACQUIRED_UUID, Props)]
               ),
    gproc:send({'p', 'l', ?LOOPBACK_BOWOUT_REG(ResigningUUID)}, ?LOOPBACK_BOWOUT_MSG(Node, Props));
process_event(_, _, _, _) -> 'ok'.

-spec maybe_send_event(kz_term:ne_binary(), kz_term:api_binary(), kzd_freeswitch:data(), atom()) -> any().
maybe_send_event(<<"HEARTBEAT">>, _UUID, _Props, _Node) -> 'ok';
maybe_send_event(<<"CHANNEL_BRIDGE">>=EventName, UUID, Props, Node) ->
    BridgeID = props:get_value(<<"variable_bridge_uuid">>, Props),
    DialPlan = props:get_value(<<"Caller-Dialplan">>, Props),
    Direction = props:get_value(?GET_CCV(<<"Application-Logical-Direction">>), Props),
    App = props:get_value(<<"variable_current_application">>, Props),
    Destination = props:get_value(<<"Caller-Destination-Number">>, Props),

    _ = case {BridgeID, Direction, DialPlan, App, Destination} of
            {'undefined', _, _, _, _} -> 'ok';
            {BridgeID, <<"outbound">>, <<"inline">>, <<"intercept">>, 'undefined'} ->
                ALeg = props:get_value(<<"Bridge-A-Unique-ID">>, Props),
                BLeg = props:get_value(<<"Bridge-B-Unique-ID">>, Props),
                lager:debug("channel bridge intercept: UUID: ~s, A : ~s, B : ~s", [UUID, ALeg, BLeg]),
                case ecallmgr_fs_channel:channel_data(Node, BLeg) of
                    {'ok', CData} ->
                        Data = props:filter_undefined(
                                 [{<<"Event-Subclass">>, props:get_value(<<"Event-Subclass">>, Props)}
                                 ,{<<"Event-Name">>, props:get_value(<<"Event-Name">>, Props)}
                                 ]) ++ CData,
                        gproc:send({'p', 'l', ?FS_EVENT_REG_MSG(Node, EventName)}, {'event', [BLeg | Data]}),
                        gproc:send({'p', 'l', ?FS_CALL_EVENT_REG_MSG(Node, BLeg)}, {'event', [BLeg | Data]});
                    _ ->
                        lager:debug("channel bridge intercept: failed to get channel data for ~s", [BLeg])
                end;
            _Else -> 'ok'
        end,
    gproc:send({'p', 'l', ?FS_EVENT_REG_MSG(Node, EventName)}, {'event', [UUID | Props]}),
    maybe_send_call_event(UUID, EventName, Props, Node);
maybe_send_event(<<"loopback::bowout">> = EventName, _UUID, Props, Node) ->
    ResigningUUID = props:get_value(?RESIGNING_UUID, Props),
    kz_util:put_callid(ResigningUUID),

    send_event(EventName, ResigningUUID, Props, Node);
maybe_send_event(<<"CHANNEL_DESTROY">> = EventName, UUID, Props, Node) ->
    case ecallmgr_fs_channel:node(UUID) of
        {'ok', OtherNode} when Node =/= OtherNode  ->
            lager:info("dropping channel destroy from ~s (expected node: ~s)", [Node, OtherNode]);
        _Else ->
            gproc:send({'p', 'l', ?FS_EVENT_REG_MSG(Node, EventName)}, {'event', [UUID | Props]}),
            maybe_send_call_event(UUID, EventName, Props, Node)
    end;
maybe_send_event(EventName, UUID, Props, Node) ->
    case kz_term:is_true(props:get_value(<<"variable_channel_is_moving">>, Props)) of
        'true' -> 'ok';
        'false' ->
            send_event(EventName, UUID, Props, Node)
    end.

send_event(EventName, UUID, Props, Node) ->
    gproc:send({'p', 'l', ?FS_EVENT_REG_MSG(Node, EventName)}, {'event', [UUID | Props]}),
    maybe_send_call_event(UUID, EventName, Props, Node).

-spec maybe_send_call_event(kz_term:api_binary(), kz_term:ne_binary(), kz_term:proplist(), atom()) -> any().
maybe_send_call_event('undefined', _, _, _) -> 'ok';
maybe_send_call_event(CallId, EventName, Props, Node) ->
    gproc:send({'p', 'l', ?FS_CALL_EVENT_MSG(Node, EventName, CallId)}, {'event', EventName, [CallId | Props]}),
    gproc:send({'p', 'l', ?FS_CALL_EVENT_REG_MSG(Node, CallId)}, {'event', [CallId | Props]}).

-spec maybe_start_event_listener(atom(), kz_term:ne_binary()) -> 'ok' | kz_types:sup_startchild_ret().
maybe_start_event_listener(Node, UUID) ->
    case kz_cache:fetch_local(?ECALLMGR_UTIL_CACHE, {UUID, 'start_listener'}) of
        {'ok', 'true'} -> ecallmgr_call_sup:start_event_process(Node, UUID);
        _E -> 'ok'
    end.
