%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc Receive call events from FreeSWITCH, publish to the call's event queue
%%% @author James Aimonetti <james@2600hz.org>
%%% @author Karl Anderson <karl@2600hz.org>
%%% @end
%%%-----------------------------------------------------------------------------
-module(ecallmgr_call_events).
-behaviour(gen_server).

-include("ecallmgr.hrl").

-define(SERVER, ?MODULE).

-define(EVENT_CAT, <<"call_event">>).
-define(MAX_FAILED_NODE_CHECKS, 10).
-define(NODE_CHECK_PERIOD, ?MILLISECONDS_IN_SECOND).

-define(DEFAULT_DEBUG_CHANNEL, 'false' ).
-define(DEBUG_CHANNEL, kapps_config:get_boolean(?APP_NAME, <<"debug_channel">>, ?DEFAULT_DEBUG_CHANNEL) ).

-export([start_link/2]).
-export([graceful_shutdown/2]).
-export([shutdown/2]).
-export([to_json/1]).
-export([swap_call_legs/1
        ,listen_for_other_leg/3
        ]).
-export([create_event/1
        ,create_event/2
        ,create_event/3
        ]).
-export([publish_event/1]).
-export([process_channel_event/1]).
-export([transfer/3]).
-export([get_application_name/1]).
-export([get_event_name/1]).
-export([callid/1
        ,node/1
        ,update_node/2
        ]).
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3
        ]).

-record(state, {node :: atom()
               ,call_id :: kz_term:api_binary()
               ,other_leg :: kz_term:api_binary()
               ,other_leg_events = [] :: kz_term:ne_binaries()
               ,is_node_up = 'true' :: boolean()
               ,failed_node_checks = 0 :: non_neg_integer()
               ,node_down_tref :: kz_term:api_reference()
               ,sanity_check_tref :: kz_term:api_reference()
               ,ref = kz_binary:rand_hex(12) :: kz_term:ne_binary()
               ,passive = 'false' :: boolean()
               }).
-type state() :: #state{}.

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the server.
%% @end
%%------------------------------------------------------------------------------
-spec start_link(atom(), kz_term:ne_binary()) -> kz_types:startlink_ret().
start_link(Node, CallId) ->
    gen_server:start_link(?SERVER, [Node, CallId], []).

-spec graceful_shutdown(atom(), kz_term:ne_binary()) -> 'ok'.
graceful_shutdown(Node, UUID) ->
    _ = [gen_server:cast(Pid, {'graceful_shutdown', UUID})
         || Pid <- gproc:lookup_pids({'p', 'l', ?FS_CALL_EVENT_REG_MSG(Node, UUID)})
        ],
    'ok'.

-spec shutdown(atom(), kz_term:ne_binary()) -> 'ok'.
shutdown(Node, UUID) ->
    _ = [gen_server:cast(Pid, 'shutdown')
         || Pid <- gproc:lookup_pids({'p', 'l', ?FS_CALL_EVENT_REG_MSG(Node, UUID)})
        ],
    'ok'.

-spec listen_for_other_leg(atom(), kz_term:ne_binary(), kz_term:api_binaries()) -> 'ok'.
listen_for_other_leg(_Node, _UUID, 'undefined') -> 'ok';
listen_for_other_leg(_Node, _UUID, []) -> 'ok';
listen_for_other_leg(Node, UUID, [_|_] = Events) ->
    _ = [gen_server:cast(Pid, {'b_leg_events', Events})
         || Pid <- gproc:lookup_pids({'p', 'l', ?FS_CALL_EVENT_REG_MSG(Node, UUID)})
        ],
    lager:debug("sent msg to ~s to bind for b leg events ~p", [UUID, Events]).

-spec callid(pid()) -> kz_term:ne_binary().
callid(Srv) -> gen_server:call(Srv, 'callid', ?MILLISECONDS_IN_SECOND).

-spec node(pid()) -> kz_term:ne_binary().
node(Srv) -> gen_server:call(Srv, 'node', ?MILLISECONDS_IN_SECOND).

-spec update_node(pid(), atom()) -> 'ok'.
update_node(Srv, Node) -> gen_server:cast(Srv, {'update_node', Node}).

-spec transfer(pid(), atom(), kz_term:proplist()) -> 'ok'.
transfer(Srv, TransferType, Props) -> gen_server:cast(Srv, {TransferType, Props}).

-spec to_json(kz_term:proplist()) -> kz_json:object().
to_json(Props) ->
    kz_json:from_list(create_event(Props)).

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server.
%% @end
%%------------------------------------------------------------------------------
-spec init([atom() | kz_term:ne_binary(),...]) -> {'ok', state()}.
init([Node, CallId]) when is_atom(Node)
                          andalso is_binary(CallId) ->
    case register_event_process(Node, CallId) of
        'ok' -> init(Node, CallId);
        {'error', _R} ->
            lager:debug("failed to register for ~s:~s: ~p", [Node, CallId, _R]),
            {'stop', 'normal'}
    end.

init(Node, CallId) ->
    kz_util:put_callid(CallId),
    register_for_events(Node, CallId),
    gen_server:cast(self(), 'init'),
    lager:debug("started call event publisher"),
    {'ok', #state{node=Node
                 ,call_id=CallId
                 ,ref=kz_binary:rand_hex(12)
                 }}.

-spec register_event_process(atom(), kz_term:ne_binary()) -> 'ok' | {'error', any()}.
register_event_process(Node, CallId) ->
    try gproc:reg(?FS_CALL_EVENTS_PROCESS_REG(Node, CallId)) of
        'true' -> 'ok'
    catch
        _E:R -> {'error', R}
    end.

-spec unregister_event_process(atom(), kz_term:ne_binary()) -> 'ok' | {'error', any()}.
unregister_event_process(Node, CallId) ->
    try gproc:unreg(?FS_CALL_EVENTS_PROCESS_REG(Node, CallId)) of
        'true' -> 'ok'
    catch
        _E:R -> {'error', R}
    end.

%%------------------------------------------------------------------------------
%% @doc Handling call messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call('node', _From, #state{node=Node}=State) ->
    {'reply', Node, State};
handle_call('callid', _From, #state{call_id=CallId}=State) ->
    {'reply', CallId, State};
handle_call(_Request, _From, State) ->
    {'reply', {'error', 'not_implemented'}, State}.

%%------------------------------------------------------------------------------
%% @doc Handling cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast('init', #state{node=Node}=State) ->
    erlang:monitor_node(Node, 'true'),
    TRef = erlang:send_after(?SANITY_CHECK_PERIOD, self(), 'sanity_check'),
    _ = usurp_other_publishers(State),
    {'noreply', State#state{sanity_check_tref=TRef}};
handle_cast({'update_node', Node}, #state{node=Node}=State) ->
    {'noreply', State};
handle_cast({'update_node', Node}, #state{node=OldNode
                                         ,call_id=CallId
                                         }=State) ->
    lager:debug("node has changed from ~s to ~s", [OldNode, Node]),
    erlang:monitor_node(OldNode, 'false'),
    unregister_for_events(OldNode, CallId),
    {'noreply', State#state{node=Node}, 0};
handle_cast({'channel_redirected', Props}, State) ->
    lager:debug("our channel has been redirected, shutting down immediately"),
    process_channel_event(Props),
    {'stop', {'shutdown', 'redirect'}, State};
handle_cast({'graceful_shutdown', CallId}, #state{node=Node
                                                 ,call_id=CallId
                                                 }=State) ->
    lager:debug("call event listener on node ~s received graceful shutdown request", [Node]),
    erlang:send_after(5 * ?MILLISECONDS_IN_SECOND, self(), 'shutdown'),
    {'noreply', State};
handle_cast({'graceful_shutdown', _CallId}, #state{}=State) ->
    lager:debug("ignoring graceful shutdown for ~s", [_CallId]),
    {'noreply', State};
handle_cast('shutdown', #state{node=Node}=State) ->
    lager:debug("call event listener on node ~s received shutdown request", [Node]),
    {'stop', 'normal', State};
handle_cast({'transferer', _Props}, State) ->
    lager:debug("call control has been transferred"),
    {'stop', 'normal', State};
handle_cast({'b_leg_events', NewEvents}, #state{other_leg_events=Evts
                                               ,other_leg='undefined'
                                               }=State) ->
    Events = lists:usort(Evts ++ NewEvents),
    lager:debug("will start event listener for b-leg if encountered"),
    {'noreply', State#state{other_leg_events=Events}};
handle_cast({'b_leg_events', NewEvents}, #state{other_leg_events=Evts
                                               ,other_leg=OtherLeg
                                               ,node=Node
                                               }=State) ->
    Events = lists:usort(Evts ++ NewEvents),
    lager:debug("tracking other leg events for ~s: ~p", [OtherLeg, Events]),
    _Started = (Events =/= [])
        andalso ecallmgr_call_sup:start_event_process(Node, OtherLeg),
    lager:debug("started event process: ~p", [_Started]),

    {'noreply', State#state{other_leg_events=Events}};

handle_cast({'other_leg', OtherLeg}
           ,#state{other_leg_events=Events
                  ,node=Node
                  }=State) ->
    lager:debug("tracking other leg events for ~s: ~p", [OtherLeg, Events]),
    _Started = (Events =/= [])
        andalso ecallmgr_call_sup:start_event_process(Node, OtherLeg),
    lager:debug("started event process: ~p", [_Started]),

    {'noreply', State#state{other_leg=OtherLeg}};
handle_cast(_Msg, State) ->
    lager:debug("unhandled cast: ~p", [_Msg]),
    {'noreply', State}.

-spec register_for_events(atom(), kz_term:ne_binary()) -> 'true'.
register_for_events(Node, CallId) ->
    update_events(Node, CallId, fun gproc:reg/1).

-spec unregister_for_events(atom(), kz_term:ne_binary()) -> 'true'.
unregister_for_events(Node, CallId) ->
    update_events(Node, CallId, fun gproc:unreg/1).

-spec update_events(atom(), kz_term:ne_binary(), function()) -> 'true'.
update_events(Node, CallId, Fun) ->
    Regs = ['call_events_processes'
           ,?FS_CALL_EVENT_REG_MSG(Node, CallId)
           ,?FS_EVENT_REG_MSG(Node, ?CHANNEL_MOVE_RELEASED_EVENT_BIN)
           ,?LOOPBACK_BOWOUT_REG(CallId)
           ],
    _ = [update_event(Fun, Reg) || Reg <- Regs],
    'true'.

-spec update_event(fun(), tuple() | atom()) -> 'true'.
update_event(Fun, Reg) ->
    catch Fun({'p', 'l', Reg}).

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info({'event', [CallId | _]}, #state{call_id=CallId
                                           ,passive='true'
                                           }=State) ->
    {'noreply', State};
handle_info({'event', [CallId | Props]}, #state{node=Node
                                               ,call_id=CallId
                                               }=State) ->
    case {props:get_first_defined([<<"Event-Subclass">>, <<"Event-Name">>], Props)
         ,props:get_value(<<"Application">>, Props)
         }
    of
        {_, <<"redirect">>} ->
            gen_server:cast(self(), {'channel_redirected', Props}),
            {'noreply', State};
        {<<"CHANNEL_DESTROY">>, _} ->
            maybe_process_channel_destroy(Node, CallId, Props),
            {'noreply', State};
        {<<"CHANNEL_ANSWER">>, _} ->
            {'noreply', State};
        {?CHANNEL_MOVE_RELEASED_EVENT_BIN, _} ->
            lager:debug("channel move released call on our node", []),
            {'stop', 'normal', State};
        {<<"sofia::transferee">>, _} ->
            process_channel_event(Props),
            %% NOTE: if we are the transferee upstream apps need
            %%   to unbind from the C leg call events and bind to
            %%   our A leg events.  Give them time by buffering
            %%   into our mailbox for 1 second...
            lager:debug("buffering call events for 1 second post transfer"),
            timer:sleep(?MILLISECONDS_IN_SECOND),
            {'noreply', State};
        {<<"RECORD_STOP">>, _} -> {'noreply', State};
        {<<"RECORD_START">>, _} -> {'noreply', State};
        {_A, _B} ->
            process_channel_event(Props),
            {'noreply', State}
    end;
handle_info({'event', [CallId | Props]}, #state{other_leg=CallId
                                               ,other_leg_events=Events
                                               }=State) ->
    Event = props:get_first_defined([<<"Event-Subclass">>
                                    ,<<"Event-Name">>
                                    ], Props),

    case lists:member(Event, Events) of
        'true' ->
            process_channel_event(Props),
            {'noreply', State};
        'false' ->
            lager:debug("ignoring b-leg event ~s (not in ~p)", [Event, Events]),
            {'noreply', State}
    end;
handle_info({'event', [_WrongCallId | _Props]}, #state{call_id=_CallId
                                                      ,other_leg=_OtherLeg
                                                      }=State) ->
    lager:debug("recv event for unknown ~s: ~s", [_WrongCallId, props:get_value(<<"Event-Name">>, _Props)]),
    lager:debug("will process events for call ~s and other leg ~s", [_CallId, _OtherLeg]),
    {'noreply', State};
handle_info({'nodedown', _}, #state{node=Node
                                   ,is_node_up='true'
                                   }=State) ->
    lager:debug("lost connection to node ~s, waiting for reconnection", [Node]),
    erlang:monitor_node(Node, 'false'),
    TRef = erlang:send_after(?NODE_CHECK_PERIOD, self(), {'check_node_status'}),
    {'noreply', State#state{node_down_tref=TRef, is_node_up='false'}, 'hibernate'};
handle_info({'nodedown', _}, #state{is_node_up='false'}=State) ->
    {'noreply', State};
handle_info({'check_node_status'}, #state{is_node_up='false'
                                         ,failed_node_checks=FNC
                                         }=State) when (FNC+1) > ?MAX_FAILED_NODE_CHECKS ->
    lager:debug("node still not up after ~p checks, giving up", [FNC]),
    {'stop', 'normal', State};
handle_info({'check_node_status'}, #state{node=Node
                                         ,call_id=CallId
                                         ,is_node_up='false'
                                         ,failed_node_checks=FNC
                                         }=State) ->
    case ecallmgr_util:is_node_up(Node, CallId) of
        'true' ->
            lager:debug("reconnected to node ~s and call is active", [Node]),
            {'noreply', State#state{node_down_tref='undefined'
                                   ,is_node_up='true'
                                   ,failed_node_checks=0
                                   }
            ,'hibernate'};
        'false' ->
            lager:debug("node ~s still not up, waiting ~pms to test again", [Node, ?NODE_CHECK_PERIOD]),
            TRef = erlang:send_after(?NODE_CHECK_PERIOD, self(), {'check_node_status'}),
            {'noreply', State#state{node_down_tref=TRef
                                   ,failed_node_checks=FNC+1
                                   }
            ,'hibernate'}
    end;
handle_info('timeout', #state{failed_node_checks=FNC}=State) when (FNC+1) > ?MAX_FAILED_NODE_CHECKS ->
    lager:debug("unable to establish initial connectivity to the media node, laterz"),
    {'stop', 'normal', State};
handle_info('timeout', #state{node=Node
                             ,call_id=CallId
                             ,failed_node_checks=FNC
                             }=State) ->
    erlang:monitor_node(Node, 'true'),
    %% TODO: die if there is already a event producer on the AMQP queue... ping/pong?
    case freeswitch:api(Node, 'uuid_exists', CallId) of
        {'error', 'timeout'} ->
            lager:warning("timeout trying to find call on node ~s, trying again", [Node]),
            {'noreply', State#state{failed_node_checks=FNC+1}, ?MILLISECONDS_IN_SECOND};
        {'error', Reason} ->
            lager:warning("unable to find call on node ~s: ~p", [Node, Reason]),
            {'stop', 'normal', State};
        {'ok', <<"true">>} ->
            lager:debug("processing call events from ~s", [Node]),
            'true' = gproc:reg({'p', 'l', ?FS_CALL_EVENT_REG_MSG(Node, CallId)}),
            'true' = gproc:reg({'p', 'l', ?FS_EVENT_REG_MSG(Node, ?CHANNEL_MOVE_RELEASED_EVENT_BIN)}),
            _ = usurp_other_publishers(State),
            {'noreply', State#state{failed_node_checks=0}};
        {'ok', Reason} ->
            lager:warning("unable to find call on node ~s: ~p", [Node, Reason]),
            {'stop', 'normal', State}
    end;
handle_info('sanity_check', #state{call_id=CallId}=State) ->
    case ecallmgr_fs_channel:exists(CallId) of
        'true' ->
            lager:debug("listener passed sanity check, call is still up"),
            TRef = erlang:send_after(?SANITY_CHECK_PERIOD, self(), 'sanity_check'),
            {'noreply', State#state{sanity_check_tref=TRef}};
        'false' ->
            lager:debug("call no longer exists, shutting down immediately"),
            {'stop', 'normal', State#state{sanity_check_tref='undefined'}}
    end;
handle_info('shutdown', State) ->
    {'stop', 'normal', State};
handle_info(?LOOPBACK_BOWOUT_MSG(Node, Props), #state{call_id=ResigningUUID
                                                     ,node=Node
                                                     }=State) ->
    NewUUID = handle_bowout(Node, Props, ResigningUUID),
    {'noreply', State#state{call_id=NewUUID}};
handle_info({'usurp_publisher', CallId, RefId, _JObj}, #state{ref=RefId
                                                             ,call_id=CallId
                                                             } = State) ->
    {'noreply', State};
handle_info({'usurp_publisher', CallId, _RefId, _JObj}, #state{call_id=CallId} = State) ->
    {'noreply', State#state{passive='true'}};
handle_info(_Info, State) ->
    lager:debug("unhandled message: ~p", [_Info]),
    {'noreply', State}.

-spec handle_bowout(atom(), kz_term:proplist(), kz_term:ne_binary()) -> kz_term:ne_binary().
handle_bowout(Node, Props, ResigningUUID) ->
    case {props:get_value(?RESIGNING_UUID, Props)
         ,props:get_value(?ACQUIRED_UUID, Props)
         }
    of
        {ResigningUUID, ResigningUUID} ->
            lager:debug("call id after bowout remains the same"),
            ResigningUUID;
        {ResigningUUID, AcquiringUUID} when AcquiringUUID =/= 'undefined' ->
            lager:debug("loopback bowout detected, replacing ~s with ~s"
                       ,[ResigningUUID, AcquiringUUID]
                       ),
            _ = register_event_process(Node, AcquiringUUID),
            register_for_events(Node, AcquiringUUID),
            unregister_for_events(Node, ResigningUUID),
            _ = unregister_event_process(Node, ResigningUUID),

            kz_util:put_callid(AcquiringUUID),
            AcquiringUUID;
        {_UUID, _AcquiringUUID} ->
            lager:debug("failed to update after bowout, r: ~s a: ~s", [_UUID, _AcquiringUUID]),
            ResigningUUID
    end.

%%------------------------------------------------------------------------------
%% @doc This function is called by a `gen_server' when it is about to
%% terminate. It should be the opposite of `Module:init/1' and do any
%% necessary cleaning up. When it returns, the `gen_server' terminates
%% with Reason. The return value is ignored.
%%
%% @end
%%------------------------------------------------------------------------------
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, #state{node_down_tref=NDTRef
                         ,sanity_check_tref=SCTRef
                         }) ->
    catch (erlang:cancel_timer(SCTRef)),
    catch (erlang:cancel_timer(NDTRef)),
    lager:debug("goodbye and thanks for all the fish: ~p", [_Reason]).

%%------------------------------------------------------------------------------
%% @doc Convert process state when code is changed.
%% @end
%%------------------------------------------------------------------------------
-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_process_channel_destroy(atom(), kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
maybe_process_channel_destroy(Node, CallId, Props) ->
    kz_util:spawn(fun maybe_manual_bowout/2, [Node, Props]),
    case ecallmgr_fs_channel:node(CallId) of
        {'ok', Node} -> gen_server:cast(self(), {'graceful_shutdown', CallId});
        {'error', _} -> gen_server:cast(self(), {'graceful_shutdown', CallId});
        {'ok', _NewNode} ->
            lager:debug("channel is on ~s, not ~s: publishing channel move"
                       ,[CallId, _NewNode, Node]
                       ),
            Event = create_event(<<"CHANNEL_MOVED">>, <<"call_pickup">>, Props),
            publish_event(Event)
    end.

-spec maybe_manual_bowout(atom(), kz_term:proplist()) -> 'ok'.
maybe_manual_bowout(Node, Props) ->
    App = props:get_value(<<"variable_last_app">>, Props),
    Role = props:get_value(<<"variable_last_bridge_role">>, Props),
    BridgeTo = props:get_value(<<"variable_last_bridge_to">>, Props),
    maybe_manual_bowout(Node, App, Role, BridgeTo).

-spec maybe_manual_bowout(atom(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
maybe_manual_bowout(Node, <<"att_xfer">>, <<"originator">>, UUID) ->
    case ecallmgr_fs_channel:fetch(UUID,  'record') of
        {'ok', #channel{loopback_other_leg=OtherLeg, is_loopback='true'}} ->
            _ = freeswitch:api(Node, 'uuid_setvar', <<UUID/binary, " ", "loopback_bowout true">>),
            _ = freeswitch:api(Node, 'uuid_setvar', <<OtherLeg/binary, " ", "loopback_bowout true">>),
            lager:info("performed manual loopback bowout on ~s and ~s", [UUID, OtherLeg]);
        _ -> 'ok'
    end;
maybe_manual_bowout(_Node, _App, _Role, _UUID) -> 'ok'.

-spec process_channel_event(kz_term:proplist()) -> 'ok'.
process_channel_event(Props) ->
    kz_util:put_callid(get_call_id(Props)),
    EventName = get_event_name(Props),
    ApplicationName = get_application_name(Props),
    Masqueraded = is_masquerade(Props),
    case should_publish(EventName, ApplicationName, Masqueraded) of
        'false' -> 'ok';
        'true' ->
            Event = create_event(EventName, ApplicationName, Props),
            publish_event(Event)
    end.

-spec create_event(kz_term:proplist()) -> kz_term:proplist().
create_event(Props) ->
    create_event(get_event_name(Props), Props).

-spec create_event(kz_term:ne_binary(), kz_term:proplist()) -> kz_term:proplist().
create_event(EventName, Props) ->
    create_event(EventName, get_application_name(Props), Props).

-spec create_event(kz_term:ne_binary(), kz_term:api_binary(), kz_term:proplist()) -> kz_term:proplist().
create_event(EventName, ApplicationName, Props) ->
    props:filter_undefined(
      [{<<"Event-Name">>, EventName}
       | specific_call_event_props(EventName, ApplicationName, Props)
       ++ generic_call_event_props(Props)
       ++ specific_call_channel_vars_props(EventName, Props)
      ]).

-spec specific_call_channel_vars_props(kz_term:ne_binary(), kz_term:proplist()) ->
                                              kz_term:proplist().
specific_call_channel_vars_props(<<"CHANNEL_DESTROY">>, Props) ->
    UUID = get_call_id(Props),
    ChanVars = kz_json:from_list(ecallmgr_util:custom_channel_vars(Props)),
    AppVars = kz_json:from_list(ecallmgr_util:custom_application_vars(Props)),

    lager:debug("checking interaction cache for ~s", [UUID]),
    case kz_cache:peek_local(?ECALLMGR_INTERACTION_CACHE, UUID) of
        {'ok', 'undefined'} ->
            lager:debug("interaction cache for ~s in null", [UUID]),
            [{<<"Custom-Channel-Vars">>, ChanVars}
            ,{<<"Custom-Application-Vars">>, AppVars}
            ];
        {'ok', CDR} ->
            NewVars = kz_json:set_value(<<?CALL_INTERACTION_ID>>, CDR, ChanVars),
            lager:debug("found interaction cache ~s for ~s", [CDR, UUID]),
            [{<<"Custom-Channel-Vars">>, NewVars}
            ,{<<"Custom-Application-Vars">>, AppVars}
            ];
        _ ->
            lager:debug("interaction cache for ~s not found", [UUID]),
            [{<<"Custom-Channel-Vars">>, ChanVars}
            ,{<<"Custom-Application-Vars">>, AppVars}
            ]
    end;
specific_call_channel_vars_props(_EventName, Props) ->
    [{<<"Custom-Channel-Vars">>, kz_json:from_list(ecallmgr_util:custom_channel_vars(Props))}
    ,{<<"Custom-Application-Vars">>, kz_json:from_list(ecallmgr_util:custom_application_vars(Props))}
    ].

-spec generic_call_event_props(kz_term:proplist()) -> kz_term:proplist().
generic_call_event_props(Props) ->
    Timestamp = kz_time:now_us(),
    FSTimestamp = props:get_integer_value(<<"Event-Date-Timestamp">>, Props, Timestamp),
    NormalizedFSTimestamp = kz_time:unix_seconds_to_gregorian_seconds(FSTimestamp div ?MICROSECONDS_IN_SECOND),

    [{<<"Call-Direction">>, kzd_freeswitch:call_direction(Props)}
    ,{<<"Call-ID">>, get_call_id(Props)}
    ,{<<"Caller-ID-Name">>, kzd_freeswitch:caller_id_name(Props)}
    ,{<<"Caller-ID-Number">>, kzd_freeswitch:caller_id_number(Props)}
    ,{<<"Channel-Call-State">>, props:get_value(<<"Channel-Call-State">>, Props)}
    ,{<<"Channel-Created-Time">>, props:get_integer_value(<<"Caller-Channel-Created-Time">>, Props)}
    ,{<<"Channel-Is-Loopback">>, get_is_loopback(props:get_value(<<"variable_is_loopback">>, Props))}
    ,{<<"Channel-Loopback-Bowout">>, props:get_is_true(<<"variable_loopback_bowout">>, Props)}
    ,{<<"Channel-Loopback-Bowout-Execute">>, props:get_is_true(<<"variable_loopback_bowout_on_execute">>, Props)}
    ,{<<"Channel-Loopback-Leg">>, kzd_freeswitch:loopback_leg_name(Props)}
    ,{<<"Channel-Loopback-Other-Leg-ID">>, kzd_freeswitch:loopback_other_leg(Props)}
    ,{<<"Channel-Moving">>, get_channel_moving(Props)}
    ,{<<"Channel-Name">>, props:get_value(<<"Channel-Name">>, Props)}
    ,{<<"Channel-State">>, get_channel_state(Props)}
    ,{<<"Custom-SIP-Headers">>, kz_json:from_list(ecallmgr_util:custom_sip_headers(Props))}
    ,{<<"Disposition">>, get_disposition(Props)}
    ,{<<"From-Tag">>, props:get_value(<<"variable_sip_from_tag">>, Props)}
    ,{<<"Group-ID">>, kzd_freeswitch:ccv(Props, <<"media_group_id">>)}
    ,{<<"Hangup-Cause">>, get_hangup_cause(Props)}
    ,{<<"Hangup-Code">>, get_hangup_code(Props)}
    ,{<<"Media-Server">>, kzd_freeswitch:hostname(Props)}
    ,{<<"Msg-ID">>, kz_term:to_binary(FSTimestamp)}
    ,{<<"Origination-Call-ID">>, kzd_freeswitch:origination_call_id(Props)}
    ,{<<"Other-Leg-Call-ID">>, get_other_leg(Props)}
    ,{<<"Other-Leg-Caller-ID-Name">>, props:get_value(<<"Other-Leg-Caller-ID-Name">>, Props)}
    ,{<<"Other-Leg-Caller-ID-Number">>, props:get_value(<<"Other-Leg-Caller-ID-Number">>, Props)}
    ,{<<"Other-Leg-Destination-Number">>, props:get_value(<<"Other-Leg-Destination-Number">>, Props)}
    ,{<<"Other-Leg-Direction">>, props:get_value(<<"Other-Leg-Direction">>, Props)}
    ,{<<"Presence-ID">>, kzd_freeswitch:presence_id(Props)}
    ,{<<"Raw-Application-Data">>, props:get_value(<<"Application-Data">>, Props)}
    ,{<<"Raw-Application-Name">>, get_raw_application_name(Props)}
    ,{<<"Replaced-By">>, props:get_first_defined([<<"att_xfer_replaced_by">>, ?ACQUIRED_UUID], Props)}
    ,{<<"Switch-Hostname">>, kzd_freeswitch:hostname(Props)}
    ,{<<"Switch-Nodename">>, kzd_freeswitch:switch_nodename(Props)}
    ,{<<"Switch-URI">>, kzd_freeswitch:switch_uri(Props)}
    ,{<<"Switch-URL">>, kzd_freeswitch:switch_url(Props)}
    ,{<<"Timestamp">>, NormalizedFSTimestamp}
    ,{<<"To-Tag">>, props:get_value(<<"variable_sip_to_tag">>, Props)}
    ,{<<"Transfer-History">>, get_transfer_history(Props)}
     | callee_call_event_props(Props)
     ++ kz_api:default_headers(?APP_NAME, ?APP_VERSION)
    ].

-spec publish_event(kz_term:proplist()) -> 'ok'.
publish_event(Props) ->
    %% call_control publishes channel create/destroy on the control
    %% events queue by calling create_event then this directly.
    EventName = kz_term:to_lower_binary(props:get_value(<<"Event-Name">>, Props, <<>>)),
    ApplicationName = kz_term:to_lower_binary(props:get_value(<<"Application-Name">>, Props, <<>>)),
    case {ApplicationName, EventName} of
        {_, <<"dtmf">>} ->
            lager:debug("publishing received DTMF digit ~s"
                       ,[props:get_value(<<"DTMF-Digit">>, Props)]
                       );
        {<<>>, <<"channel_bridge">>} ->
            OtherLeg = get_other_leg(Props),
            gen_server:cast(self(), {'other_leg', OtherLeg}),
            lager:debug("publishing channel_bridge to other leg ~s", [OtherLeg]);
        {<<>>, _Event} ->
            lager:debug("publishing call event ~s", [_Event]);
        {ApplicationName, <<"channel_execute_complete">>} ->
            ApplicationResponse = kz_term:to_lower_binary(props:get_value(<<"Application-Response">>, Props, <<>>)),
            ApplicationData = props:get_value(<<"Raw-Application-Data">>, Props, <<>>),
            lager:debug("publishing call event ~s '~s(~s)' result: ~s", [EventName, ApplicationName, ApplicationData, ApplicationResponse]);
        {ApplicationName, _} ->
            ApplicationData = props:get_value(<<"Raw-Application-Data">>, Props, <<>>),
            lager:debug("publishing call event ~s '~s(~s)'", [EventName, ApplicationName, ApplicationData])
    end,
    kz_amqp_worker:cast(Props, fun kapi_call:publish_event/1).

-spec is_masquerade(kz_term:proplist()) -> boolean().
is_masquerade(Props) ->
    case props:get_value(<<"Event-Subclass">>, Props) of
        %% If this is a event created by kazoo, then use
        %% the flag it as masqueraded
        <<"kazoo::", _/binary>> -> 'true';
        %% otherwise process as the genuine article
        _Else -> 'false'
    end.

%% return a proplist of k/v pairs specific to the event
-spec specific_call_event_props(binary(), kz_term:api_binary(), kz_term:proplist()) -> kz_term:proplist().
specific_call_event_props(<<"CHANNEL_EXECUTE">>, <<"conference">>, Props) ->
    conference_specific(Props);
specific_call_event_props(<<"CHANNEL_EXECUTE_COMPLETE">>, <<"conference">>, Props) ->
    case props:get_value(<<"variable_current_application_data">>, Props) of
        <<"page_", _/binary>> -> page_specific(Props);
        _Else -> conference_specific(Props)
    end;
specific_call_event_props(<<"CHANNEL_EXECUTE_COMPLETE">>, <<"playback">> = Application, Props) ->
    %% if the playback was terminated as a result of DTMF, include it
    [{<<"DTMF-Digit">>, props:get_value(<<"variable_playback_terminator_used">>, Props)}
    ,{<<"Application-Name">>, props:get_value(Application, ?FS_APPLICATION_NAMES)}
    ,{<<"Application-Response">>, props:get_value(<<"Application-Response">>, Props)}
    ];
specific_call_event_props(<<"CHANNEL_EXECUTE_COMPLETE">>, <<"noop">>, Props) ->
    [{<<"Application-Name">>, <<"noop">>}
    ,{<<"Application-Response">>, props:get_value(<<"kazoo_application_response">>, Props)}
    ];
specific_call_event_props(<<"CHANNEL_EXECUTE_COMPLETE">>, <<"bridge">>, Props) ->
    [{<<"Application-Name">>, <<"bridge">>}
    ,{<<"Application-Response">>, props:get_value(<<"variable_originate_disposition">>, Props, <<"FAIL">>)}
    ];
specific_call_event_props(<<"CHANNEL_EXECUTE_COMPLETE">>, <<"record">>, Props) ->
    [{<<"Application-Name">>, <<"bridge">>}
    ,{<<"Application-Response">>, props:get_value(<<"variable_originate_disposition">>, Props, <<"FAIL">>)}
    ,{<<"Length">>, props:get_value(<<"variable_record_ms">>, Props)}
    ];
specific_call_event_props(<<"CHANNEL_EXECUTE_COMPLETE">>, <<"set">>, Props) ->
    [{<<"Application-Name">>, props:get_value(<<"set">>, ?FS_APPLICATION_NAMES)}
    ,{<<"Application-Response">>, props:get_value(<<"Application-Response">>, Props)}
    ];
specific_call_event_props(<<"CHANNEL_CREATE">>, _, Props) ->
    [{<<"Request">>, ecallmgr_util:get_sip_request(Props)}
    ,{<<"To">>, ecallmgr_util:get_sip_to(Props)}
    ,{<<"To-Uri">>, props:get_value(<<"variable_sip_to_uri">>, Props)}
    ,{<<"From">>, ecallmgr_util:get_sip_from(Props)}
    ,{<<"From-Uri">>, props:get_value(<<"variable_sip_from_uri">>, Props)}
    ];
specific_call_event_props(<<"CHANNEL_ANSWER">>, _, Props) ->
    [{<<"Request">>, ecallmgr_util:get_sip_request(Props)}
    ,{<<"To">>, ecallmgr_util:get_sip_to(Props)}
    ,{<<"To-Uri">>, props:get_value(<<"variable_sip_to_uri">>, Props)}
    ,{<<"From">>, ecallmgr_util:get_sip_from(Props)}
    ,{<<"From-Uri">>, props:get_value(<<"variable_sip_from_uri">>, Props)}
    ];
specific_call_event_props(<<"CHANNEL_DESTROY">>, _, Props) ->
    [{<<"Request">>, ecallmgr_util:get_sip_request(Props)}
    ,{<<"To">>, ecallmgr_util:get_sip_to(Props)}
    ,{<<"To-Uri">>, props:get_value(<<"variable_sip_to_uri">>, Props)}
    ,{<<"From">>, ecallmgr_util:get_sip_from(Props)}
    ,{<<"From-Uri">>, props:get_value(<<"variable_sip_from_uri">>, Props)}
    ,{<<"Remote-SDP">>, props:get_value(<<"variable_switch_r_sdp">>, Props)}
    ,{<<"Local-SDP">>, props:get_value(<<"variable_rtp_local_sdp_str">>, Props)}
    ,{<<"Duration-Seconds">>, props:get_integer_value(<<"variable_duration">>, Props)}
    ,{<<"Billing-Seconds">>, get_billing_seconds(Props)}
    ,{<<"Ringing-Seconds">>, get_ringing_seconds(Props)}
    ,{<<"User-Agent">>, props:get_value(<<"variable_sip_user_agent">>, Props)}
    ,{<<"Fax-Info">>, maybe_fax_specific(Props)}
     | debug_channel_props(Props)
    ];
specific_call_event_props(<<"RECORD_START">>, _, Props) ->
    [{<<"Application-Name">>, <<"record">>}
    ,{<<"Application-Response">>, props:get_first_defined([<<"Record-File-Path">>
                                                          ,<<"kazoo_application_response">>
                                                          ], Props)
     }
    ];
specific_call_event_props(<<"RECORD_STOP">>, _, Props) ->
    [{<<"Application-Name">>, <<"record">>}
    ,{<<"Application-Response">>, props:get_first_defined([<<"Record-File-Path">>
                                                          ,<<"kazoo_application_response">>
                                                          ], Props)
     }
    ,{<<"Terminator">>, props:get_value(<<"variable_playback_terminator_used">>, Props)}
    ,{<<"Length">>, props:get_value(<<"variable_record_ms">>, Props, 0)}
    ,{<<"Silence-Terminated">>, silence_terminated(Props)}
    ,{<<"Request">>, ecallmgr_util:get_sip_request(Props)}
    ,{<<"To">>, ecallmgr_util:get_sip_to(Props)}
    ,{<<"From">>, ecallmgr_util:get_sip_from(Props)}
    ];
specific_call_event_props(<<"DETECTED_TONE">>, _, Props) ->
    [{<<"Detected-Tone">>, props:get_value(<<"Detected-Tone">>, Props)}];
specific_call_event_props(<<"DTMF">>, _, Props) ->
    [{<<"DTMF-Digit">>, props:get_value(<<"DTMF-Digit">>, Props)}
    ,{<<"DTMF-Duration">>, props:get_value(<<"DTMF-Duration">>, Props)}
    ];
specific_call_event_props(_, <<"play_and_get_digits">>, Props) ->
    [{<<"Application-Name">>, <<"play_and_collect_digits">>}
    ,{<<"Application-Response">>, props:get_value(<<"variable_collected_digits">>, Props, <<"">>)}
    ];
specific_call_event_props(<<"FAX_DETECTED">>, _, _Props) ->
    [{<<"Application-Name">>, <<"fax_detection">>}];
specific_call_event_props(<<"CHANNEL_FAX_STATUS">>, <<"rxfax", Event/binary>>, Prop) ->
    [{<<"Application-Name">>, <<"receive_fax">>}
    ,{<<"Application-Event">>, Event}
    ,{<<"Application-Data">>, kz_json:from_list(fax_specific(Prop))}
    ];
specific_call_event_props(<<"CHANNEL_FAX_STATUS">>, <<"txfax", Event/binary>>, Prop) ->
    [{<<"Application-Name">>, <<"send_fax">>}
    ,{<<"Application-Event">>, Event}
    ,{<<"Application-Data">>, kz_json:from_list(fax_specific(Prop))}
    ];
specific_call_event_props(<<"CHANNEL_INTERCEPTED">>, _, Props) ->
    [{<<"Intercepted-By">>, props:get_value(<<"intercepted_by">>, Props)}];
specific_call_event_props(<<"CHANNEL_TRANSFEROR">>, _, Props) ->
    {Type, To} = transfer_to(Props),
    [{<<"Transfer-Type">>, Type}
    ,{<<"Transfer-To">>, To}
    ];
specific_call_event_props(_Evt, Application, Props) ->
    [{<<"Application-Name">>, props:get_value(Application, ?FS_APPLICATION_NAMES)}
    ,{<<"Application-Response">>, props:get_value(<<"Application-Response">>, Props)}
    ].

-spec transfer_to(kz_term:proplist() | kz_term:api_binary()) -> {kz_term:api_binary(), kz_term:api_binary()}.
transfer_to(Props)
  when is_list(Props) ->
    transfer_to(props:get_value(<<"variable_transfer_to">>, Props));
transfer_to(<<"att:", TransferTo/binary>>) -> {<<"attended">>, TransferTo};
transfer_to(<<"blind:", TransferTo/binary>>) -> {<<"blind">>, TransferTo};
transfer_to(_) -> {'undefined', 'undefined'}.

-spec page_specific(kz_term:proplist()) -> kz_term:proplist().
page_specific(Props) ->
    [{<<"Application-Name">>, <<"page">>}
    ,{<<"Application-Response">>, props:get_value(<<"Application-Response">>, Props)}
    ].

-spec conference_specific(kz_term:proplist()) -> kz_term:proplist().
conference_specific(Props) ->
    Default = [{<<"Application-Name">>, <<"conference">>}
              ,{<<"Application-Response">>, props:get_value(<<"Application-Response">>, Props)}
              ],
    case props:get_value(<<"Application-Data">>, Props) of
        'undefined' -> Default;
        ConfData ->
            case binary:split(ConfData, <<"@">>) of
                [ConfName, ConfConfig] ->
                    [{<<"Conference-Name">>, ConfName}
                    ,{<<"Conference-Config">>, ConfConfig}
                     | Default
                    ];
                _ -> Default
            end
    end.

-spec maybe_fax_specific(kz_term:proplist()) -> kz_term:api_object().
maybe_fax_specific(Props) ->
    case fax_specific(Props) of
        [] -> 'undefined';
        FaxProps -> kz_json:from_list(FaxProps)
    end.

-spec fax_specific(kz_term:proplist()) -> kz_term:proplist().
fax_specific(Props) ->
    props:filter_undefined(
      [{<<"Fax-Success">>, get_fax_success(Props)}
      ,{<<"Fax-ECM-Used">>, get_fax_ecm_used(Props)}
      ,{<<"Fax-T38-Used">>, get_fax_t38_used(Props)}
      ,{<<"Fax-Result-Text">>, props:get_value(<<"variable_fax_result_text">>, Props)}
      ,{<<"Fax-Result-Code">>, props:get_value(<<"variable_fax_result_code">>, Props)}
      ,{<<"Fax-Transferred-Pages">>, props:get_value(<<"variable_fax_document_transferred_pages">>, Props)}
      ,{<<"Fax-Total-Pages">>, props:get_value(<<"variable_fax_document_total_pages">>, Props)}
      ,{<<"Fax-Bad-Rows">>, props:get_value(<<"variable_fax_bad_rows">>, Props)}
      ,{<<"Fax-Transfer-Rate">>, props:get_value(<<"variable_fax_transfer_rate">>, Props)}
      ,{<<"Fax-Local-Station-ID">>, props:get_value(<<"variable_fax_local_station_id">>, Props)}
      ,{<<"Fax-Remote-Station-ID">>, props:get_value(<<"variable_fax_remote_station_id">>, Props)}
      ,{<<"Fax-Remote-Country">>, props:get_value(<<"variable_fax_remote_country">>, Props)}
      ,{<<"Fax-Remote-Vendor">>, props:get_value(<<"variable_fax_remote_vendor">>, Props)}
      ,{<<"Fax-Remote-Model">>, props:get_value(<<"variable_fax_remote_model">>, Props)}
      ,{<<"Fax-Image-Resolution">>, props:get_value(<<"variable_fax_image_resolution">>, Props)}
      ,{<<"Fax-File-Image-Resolution">>, props:get_value(<<"variable_fax_file_image_resolution">>, Props)}
      ,{<<"Fax-Image-Size">>, props:get_value(<<"variable_fax_image_size">>, Props)}
      ,{<<"Fax-Image-Pixel-Size">>, props:get_value(<<"variable_fax_image_pixel_size">>, Props)}
      ,{<<"Fax-File-Image-Pixel-Size">>, props:get_value(<<"variable_fax_file_image_pixel_size">>, Props)}
      ,{<<"Fax-Longest-Bad-Row-Run">>, props:get_value(<<"variable_fax_longest_bad_row_run">>, Props)}
      ,{<<"Fax-Encoding">>, props:get_value(<<"variable_fax_encoding">>, Props)}
      ,{<<"Fax-Encoding-Name">>, props:get_value(<<"variable_fax_encoding_name">>, Props)}
      ,{<<"Fax-Timezone">>, props:get_value(<<"variable_fax_timezone">>, Props)}
      ,{<<"Fax-Identity-Number">>, props:get_value(<<"variable_fax_ident">>, Props)}
      ,{<<"Fax-Identity-Name">>, props:get_value(<<"variable_fax_header">>, Props)}
      ,{<<"Fax-Doc-ID">>, props:get_value(<<"variable_fax_doc_id">>, Props)}
      ,{<<"Fax-Doc-DB">>, props:get_value(<<"variable_fax_doc_database">>, Props)}
      ]).

-spec should_publish(kz_term:ne_binary(), kz_term:ne_binary(), boolean()) -> boolean().
should_publish(<<"CHANNEL_EXECUTE_COMPLETE">>, <<"bridge">>, 'false') ->
    lager:debug("suppressing bridge execute complete in favour the kazoo masquerade of this event"),
    'false';
should_publish(<<"CHANNEL_EXECUTE_COMPLETE">>, <<"set">>, _) ->
    'false';
should_publish(<<"CHANNEL_EXECUTE_COMPLETE">>, <<"export">>, _) ->
    'false';
should_publish(<<"CHANNEL_EXECUTE_COMPLETE">>, <<"intercept">>, 'false') ->
    lager:debug("suppressing intercept execute complete in favour the kazoo masquerade of this event"),
    'false';
should_publish(<<"CHANNEL_EXECUTE_COMPLETE">>, <<"execute_extension">>, 'false') ->
    lager:debug("suppressing execute_extension execute complete in favour the kazoo masquerade of this event"),
    'false';
should_publish(<<"CHANNEL_EXECUTE", _/binary>>, <<"park">>, _) ->
    'false';
should_publish(<<"CHANNEL_EXECUTE", _/binary>>, Application, _) ->
    props:get_value(Application, ?FS_APPLICATION_NAMES) =/= 'undefined';
should_publish(_, <<"transfer">>, _) ->
    'true';
should_publish(<<"CHANNEL_FAX_STATUS">>, _, _) ->
    'true';
should_publish(<<"FAX_DETECTED">>, _, _) ->
    'true';
should_publish(<<"DETECTED_TONE">>, _, _) ->
    'true';
should_publish(EventName, _A, _) ->
    lists:member(EventName, ?CALL_EVENTS).

-spec silence_terminated(kz_term:api_integer() | kz_term:proplist()) -> kz_term:api_boolean().
silence_terminated('undefined') -> 'undefined';
silence_terminated(Hits) when is_integer(Hits) -> Hits =:= 0;
silence_terminated(Prop) when is_list(Prop) ->
    case props:get_value(<<"variable_silence_hits_exhausted">>, Prop) of
        'undefined' -> silence_terminated(props:get_integer_value(<<"variable_record_silence_hits">>, Prop));
        Ex -> kz_term:is_true(Ex)
    end.

-spec is_channel_moving(kz_term:proplist()) -> boolean().
is_channel_moving(Props) ->
    props:get_is_true(<<"variable_channel_is_moving">>, Props, 'false').

-spec get_channel_moving(kz_term:proplist()) -> kz_term:api_boolean().
get_channel_moving(Props) ->
    case is_channel_moving(Props) of
        'false' -> 'undefined';
        'true' -> 'true'
    end.

-spec get_channel_state(kz_term:proplist()) -> kz_term:api_binary().
get_channel_state(Props) ->
    case props:get_value(<<"Channel-State">>, Props) of
        'undefined' -> 'undefined';
        <<"CS_", ChannelState/binary>> -> ChannelState;
        Other -> Other
    end.

-spec get_call_id(kz_term:proplist()) -> kz_term:api_binary().
get_call_id(Props) ->
    kzd_freeswitch:call_id(Props).

-spec get_other_leg(kz_term:proplist()) -> kz_term:api_binary().
get_other_leg(Props) ->
    ecallmgr_fs_channel:get_other_leg(get_call_id(Props), Props).

-spec get_event_name(kz_term:proplist()) -> kz_term:api_binary().
get_event_name(Props) ->
    case kzd_freeswitch:application_name(Props) of
        <<"sofia::transferee">> -> <<"CHANNEL_TRANSFEREE">>;
        <<"sofia::transferor">> -> <<"CHANNEL_TRANSFEROR">>;
        <<"sofia::replaced">> -> <<"CHANNEL_REPLACED">>;
        <<"sofia::intercepted">> -> <<"CHANNEL_INTERCEPTED">>;
        <<"spandsp::txfax", _/binary>> -> <<"CHANNEL_FAX_STATUS">>;
        <<"spandsp::rxfax", _/binary>> -> <<"CHANNEL_FAX_STATUS">>;
        <<"loopback::bowout">> -> <<"CHANNEL_REPLACED">>;
        _AppName -> get_fs_event_name(Props)
    end.

-spec get_fs_event_name(kz_term:proplist()) -> kz_term:api_binary().
get_fs_event_name(Props) ->
    case kzd_freeswitch:event_name(Props) of
        <<"DETECTED_TONE">> ->
            case props:get_value(<<"Detected-Fax-Tone">>, Props) of
                'undefined' -> <<"DETECTED_TONE">>;
                _FaxDetected -> <<"FAX_DETECTED">>
            end;
        Event -> Event
    end.

-spec get_application_name(kz_term:proplist()) -> kz_term:api_binary().
get_application_name(Props) ->
    case kzd_freeswitch:application_name(Props) of
        <<"sofia::transferee">> -> <<"transfer">>;
        <<"sofia::transferor">> -> <<"transfer">>;
        <<"sofia::replaced">> -> <<"transfer">>;
        <<"spandsp::rxfax", Event/binary >> -> <<"rxfax",Event/binary>>;
        <<"spandsp::txfax", Event/binary >> -> <<"txfax", Event/binary>>;
        AppName -> AppName
    end.

-spec get_raw_application_name(kz_term:proplist()) -> kz_term:api_binary().
get_raw_application_name(Props) ->
    kzd_freeswitch:raw_application_name(Props).

-spec get_fax_success(kz_term:proplist()) -> kz_term:api_boolean().
get_fax_success(Props) ->
    case props:get_value(<<"variable_fax_success">>, Props) of
        'undefined' -> 'undefined';
        Else -> Else =/= <<"0">>
    end.

-spec get_fax_t38_used(kz_term:proplist()) -> kz_term:api_boolean().
get_fax_t38_used(Props) ->
    case props:get_value(<<"variable_has_t38">>, Props) of
        'undefined' -> 'undefined';
        Else -> kz_term:is_true(Else)
    end.

-spec get_fax_ecm_used(kz_term:proplist()) -> kz_term:api_boolean().
get_fax_ecm_used(Props) ->
    case props:get_value(<<"variable_fax_ecm_used">>, Props) of
        'undefined' -> 'undefined';
        Else -> Else =/= <<"off">>
    end.

-spec get_serialized_history(kz_term:proplist()) -> kz_term:binaries().
get_serialized_history(Props) ->
    case kzd_freeswitch:transfer_history(Props) of
        'undefined' -> [];
        History when is_binary(History) ->
            ecallmgr_util:unserialize_fs_array(History);
        History when is_list(History) ->
            History
    end.

-spec get_transfer_history(kz_term:proplist()) -> kz_term:api_object().
get_transfer_history(Props) ->
    SerializedHistory = get_serialized_history(Props),
    case [HistJObj
          || Trnsf <- SerializedHistory,
             (HistJObj = create_trnsf_history_object(binary:split(Trnsf, <<":">>, ['global']))) =/= 'undefined'
         ]
    of
        [] -> 'undefined';
        History -> kz_json:from_list(History)
    end.

-spec create_trnsf_history_object(list()) -> {kz_term:ne_binary(), kz_json:object()} | 'undefined'.
create_trnsf_history_object([Epoch, CallId, <<"att_xfer">>, Props]) ->
    [Transferee, Transferer] = binary:split(Props, <<"/">>),
    Trans = [{<<"Call-ID">>, CallId}
            ,{<<"Type">>, <<"attended">>}
            ,{<<"Transferee">>, Transferee}
            ,{<<"Transferer">>, Transferer}
            ],
    {Epoch, kz_json:from_list(Trans)};
create_trnsf_history_object([Epoch, CallId, <<"bl_xfer">> | Props]) ->
    %% This looks confusing but FS uses the same delimiter to in the array
    %% as it does for inline dialplan actions (like those created during partial attended)
    %% so we have to put it together to take it apart... I KNOW! ARRRG
    Dialplan = lists:last(binary:split(kz_binary:join(Props, <<":">>), <<",">>)),
    [Exten | _] = binary:split(Dialplan, <<"/">>, ['global']),
    Trans = [{<<"Call-ID">>, CallId}
            ,{<<"Type">>, <<"blind">>}
            ,{<<"Extension">>, Exten}
            ],
    {Epoch, kz_json:from_list(Trans)};
create_trnsf_history_object([Epoch, CallId, <<"uuid_br">> , OtherLeg]) ->
    Trans = [{<<"Call-ID">>, CallId}
            ,{<<"Type">>, <<"bridge">>}
            ,{<<"Other-Leg">>, OtherLeg}
            ],
    {Epoch, kz_json:from_list(Trans)};
create_trnsf_history_object(_Params) ->
    lager:debug("unhandled transfer type : ~p", [_Params]),
    'undefined'.

-spec get_hangup_cause(kz_term:proplist()) -> kz_term:api_binary().
get_hangup_cause(Props) ->
    kzd_freeswitch:hangup_cause(Props).

-spec get_disposition(kz_term:proplist()) -> kz_term:api_binary().
get_disposition(Props) ->
    kzd_freeswitch:disposition(Props).

-spec get_hangup_code(kz_term:proplist()) -> kz_term:api_binary().
get_hangup_code(Props) ->
    kzd_freeswitch:hangup_code(Props).

-spec get_billing_seconds(kz_term:proplist()) -> integer().
get_billing_seconds(Props) ->
    case props:get_integer_value(<<"variable_billmsec">>, Props) of
        'undefined' -> props:get_integer_value(<<"variable_billsec">>, Props, 0);
        Billmsec -> kz_term:ceiling(Billmsec / 1000)
    end.

-spec get_ringing_seconds(kz_term:proplist()) -> integer().
get_ringing_seconds(Props) ->
    DurationS = props:get_integer_value(<<"variable_duration">>, Props, 0),
    BillingS = get_billing_seconds(Props),
    ProgressS = props:get_integer_value(<<"variable_progresssec">>, Props, 0),

    DurationS - BillingS - ProgressS.

-spec swap_call_legs(kz_term:proplist() | kz_json:object()) -> kz_term:proplist().
swap_call_legs(Props) when is_list(Props) -> swap_call_legs(Props, []);
swap_call_legs(JObj) -> swap_call_legs(kz_json:to_proplist(JObj)).

-spec swap_call_legs(kz_term:proplist(), kz_term:proplist()) -> kz_term:proplist().
swap_call_legs([], Swap) -> Swap;
swap_call_legs([{<<"Unique-ID">>, Value}|T], Swap) ->
    swap_call_legs(T, [{<<"Other-Leg-Call-ID">>, Value}|Swap]);
swap_call_legs([{<<"Other-Leg-Call-ID">>, Value}|T], Swap) ->
    swap_call_legs(T, [{<<"Call-ID">>, Value}|Swap]);
swap_call_legs([{<<"Caller-", Key/binary>>, Value}|T], Swap) ->
    swap_call_legs(T, [{<<"Other-Leg-", Key/binary>>, Value}|Swap]);
swap_call_legs([{<<"Other-Leg-", Key/binary>>, Value}|T], Swap) ->
    swap_call_legs(T, [{<<"Caller-", Key/binary>>, Value}|Swap]);
swap_call_legs([Prop|T], Swap) ->
    swap_call_legs(T, [Prop|Swap]).

-spec usurp_other_publishers(state()) -> 'ok'.
usurp_other_publishers(#state{node=Node
                             ,ref=Ref
                             ,call_id=CallId}) ->
    Usurp = [{<<"Call-ID">>, CallId}
            ,{<<"Media-Node">>, Node}
            ,{<<"Reference">>, Ref}
             | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
            ],
    PublisherFun = fun(P) -> kapi_call:publish_usurp_publisher(CallId, P) end,
    kz_amqp_worker:cast(Usurp, PublisherFun),
    ecallmgr_usurp_monitor:register('usurp_publisher', CallId, Ref).

-spec get_is_loopback(kz_term:api_binary()) -> atom().
get_is_loopback('undefined') -> 'undefined';
get_is_loopback(_) -> 'true'.

-spec callee_call_event_props(kz_term:proplist()) -> kz_term:proplist().
callee_call_event_props(Props) ->
    UUID = get_call_id(Props),
    case kz_cache:peek_local(?ECALLMGR_INTERACTION_CACHE, {'channel', UUID}) of
        {'ok', #channel{callee_name = Name,
                        callee_number = Num}} when Num =/= 'undefined' ->
            [{<<"Callee-ID-Number">>, Num}
            ,{<<"Callee-ID-Name">>, Name}
            ];
        _ ->
            [{<<"Callee-ID-Number">>, kzd_freeswitch:callee_id_number(Props)}
            ,{<<"Callee-ID-Name">>, kzd_freeswitch:callee_id_name(Props)}
            ]
    end.

-spec debug_channel_props(kz_term:proplist()) -> kz_term:proplist().
debug_channel_props(Props) ->
    debug_channel_props(Props, ?DEBUG_CHANNEL).

-spec debug_channel_props(kz_term:proplist(), boolean()) -> kz_term:proplist().
debug_channel_props(_Props, 'false') -> [];
debug_channel_props(Props, 'true') ->
    [{<<"Channel-Debug">>
     ,kz_json:from_list(lists:sort(fun sort_debug/2, Props))
     }
    ].

-spec sort_debug({any(), any()}, {any(), any()}) -> boolean().
sort_debug({A,_}, {B,_}) -> A =< B.
