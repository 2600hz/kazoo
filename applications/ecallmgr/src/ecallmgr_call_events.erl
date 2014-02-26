%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2014, 2600Hz
%%% @doc
%%% Receive call events from freeSWITCH, publish to the call's event queue
%%% @end
%%%
%%% @contributors
%%%   James Aimonetti <james@2600hz.org>
%%%   Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(ecallmgr_call_events).

-behaviour(gen_listener).

-include("ecallmgr.hrl").

-define(EVENT_CAT, <<"call_event">>).
-define(MAX_FAILED_NODE_CHECKS, 10).
-define(NODE_CHECK_PERIOD, 1000).

-export([start_link/2]).
-export([graceful_shutdown/2]).
-export([shutdown/2]).
-export([to_json/1]).
-export([swap_call_legs/1]).
-export([create_event/1
         ,create_event/2
         ,create_event/3
        ]).
-export([publish_event/1]).
-export([process_channel_event/1]).
-export([transfer/3]).
-export([handle_publisher_usurp/2]).
-export([get_application_name/1]).
-export([queue_name/1
         ,callid/1
         ,node/1
         ,update_node/2
        ]).
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,handle_event/2
         ,terminate/2
         ,code_change/3
        ]).

-define(RESPONDERS, [{{?MODULE, 'handle_publisher_usurp'}
                      ,[{<<"call_event">>, <<"usurp_publisher">>}]
                     }]).
-define(QUEUE_NAME, <<>>).
-define(QUEUE_OPTIONS, []).
-define(CONSUME_OPTIONS, [{'no_local', 'true'}]).

-define(SERVER, ?MODULE).

-record(state, {
          node :: atom()
          ,callid :: api_binary()
          ,is_node_up = 'true' :: boolean()
          ,failed_node_checks = 0 :: non_neg_integer()
          ,node_down_tref :: reference()
          ,sanity_check_tref :: reference()
          ,ref = wh_util:rand_hex_binary(12) :: ne_binary()
          ,passive = 'false' :: boolean()
         }).
-type state() :: #state{}.

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {'ok', Pid} | ignore | {'error', Error}
%% @end
%%--------------------------------------------------------------------
-spec start_link(atom(), ne_binary()) -> startlink_ret().
start_link(Node, CallId) ->
    Bindings = [{'call', [{'callid', CallId}
                          ,{'restrict_to', ['publisher_usurp']}
                         ]}
               ],
    gen_listener:start_link(?MODULE, [{'bindings', Bindings}
                                      ,{'responders', ?RESPONDERS}
                                      ,{'queue_name', ?QUEUE_NAME}
                                      ,{'queue_options', ?QUEUE_OPTIONS}
                                      ,{'consume_options', ?CONSUME_OPTIONS}
                                     ], [Node, CallId]).

-spec graceful_shutdown(atom(), ne_binary()) -> 'ok'.
graceful_shutdown(Node, UUID) ->
    _ = [gen_listener:cast(Pid, {'graceful_shutdown'})
         || Pid <- gproc:lookup_pids({'p', 'l', ?FS_CALL_EVENT_REG_MSG(Node, UUID)})
        ],
    'ok'.

-spec shutdown(atom(), ne_binary()) -> 'ok'.
shutdown(Node, UUID) ->
    _ = [gen_listener:cast(Pid, 'shutdown')
         || Pid <- gproc:lookup_pids({'p', 'l', ?FS_CALL_EVENT_REG_MSG(Node, UUID)})
        ],
    'ok'.

-spec callid(pid()) -> ne_binary().
callid(Srv) -> gen_listener:call(Srv, 'callid', 1000).

-spec node(pid()) -> ne_binary().
node(Srv) -> gen_listener:call(Srv, 'node', 1000).

update_node(Srv, Node) -> gen_listener:cast(Srv, {'update_node', Node}).

-spec transfer(pid(), atom(), wh_proplist()) -> 'ok'.
transfer(Srv, TransferType, Props) -> gen_listener:cast(Srv, {TransferType, Props}).

-spec queue_name(pid()) -> ne_binary().
queue_name(Srv) -> gen_listener:queue_name(Srv).

-spec to_json(wh_proplist()) -> wh_json:object().
to_json(Props) ->
    wh_json:from_list(create_event(Props)).

-spec handle_publisher_usurp(wh_json:object(), wh_proplist()) -> 'ok'.
handle_publisher_usurp(JObj, Props) ->
    CallId = props:get_value('call_id', Props),
    Ref = props:get_value('reference', Props),
    Node = wh_util:to_binary(props:get_value('node', Props)),

    lager:debug("recieved publisher usurp for ~s on ~s (if ~s != ~s)"
                ,[wh_json:get_value(<<"Call-ID">>, JObj)
                  ,wh_json:get_value(<<"Media-Node">>, JObj)
                  ,Ref
                  ,wh_json:get_value(<<"Reference">>, JObj)
                 ]),

    case CallId =:= wh_json:get_value(<<"Call-ID">>, JObj)
        andalso Node =:= wh_json:get_value(<<"Media-Node">>, JObj)
        andalso Ref =/= wh_json:get_value(<<"Reference">>, JObj)
    of
        'false' -> 'ok';
        'true' ->
            put('callid', CallId),
            gen_listener:cast(props:get_value('server', Props), {'passive'})
    end.

%%%===================================================================
%%% gen_listener callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {'ok', State} |
%%                     {'ok', State, Timeout} |
%%                     ignore |
%%                     {'stop', Reason}
%% @end
%%--------------------------------------------------------------------
-spec init([atom() | ne_binary(),...]) -> {'ok', state()}.
init([Node, CallId]) when is_atom(Node) andalso is_binary(CallId) ->
    put('callid', CallId),
    gen_listener:cast(self(), 'init'),
    {'ok', #state{node=Node
                  ,callid=CallId
                  ,ref=wh_util:rand_hex_binary(12)
                 }}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {'reply', Reply, State} |
%%                                   {'reply', Reply, State, Timeout} |
%%                                   {'noreply', State} |
%%                                   {'noreply', State, Timeout} |
%%                                   {'stop', Reason, Reply, State} |
%%                                   {'stop', Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call('node', _From, #state{node=Node}=State) ->
    {'reply', Node, State};
handle_call('callid', _From, #state{callid=CallId}=State) ->
    {'reply', CallId, State};
handle_call(_Request, _From, State) ->
    {'reply', {'error', 'not_implemented'}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {'noreply', State} |
%%                                  {'noreply', State, Timeout} |
%%                                  {'stop', Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast('init', #state{node=Node
                           ,callid=CallId
                          }=State) ->
    erlang:monitor_node(Node, 'true'),
    TRef = erlang:send_after(?SANITY_CHECK_PERIOD, self(), 'sanity_check'),
    'true' = gproc:reg({'p', 'l', 'call_events_processes'}),
    'true' = gproc:reg({'p', 'l', {'call_events_process', Node, CallId}}),
    'true' = gproc:reg({'p', 'l', ?FS_CALL_EVENT_REG_MSG(Node, CallId)}),
    'true' = gproc:reg({'p', 'l', ?FS_EVENT_REG_MSG(Node, ?CHANNEL_MOVE_RELEASED_EVENT_BIN)}),
    _ = usurp_other_publishers(State),
    {'noreply', State#state{sanity_check_tref=TRef}};
handle_cast({'update_node', Node}, #state{node=Node}=State) ->
    {'noreply', State};
handle_cast({'update_node', Node}, #state{node=OldNode
                                          ,callid=CallId
                                         }=State) ->
    lager:debug("node has changed from ~s to ~s", [OldNode, Node]),
    erlang:monitor_node(OldNode, 'false'),
    _ = gproc:unreg({'p', 'l', {'call_events_process', OldNode, CallId}}),
    _ = gproc:unreg({'p', 'l', ?FS_CALL_EVENT_REG_MSG(OldNode, CallId)}),
    _ = gproc:unreg({'p', 'l', ?FS_EVENT_REG_MSG(OldNode, ?CHANNEL_MOVE_RELEASED_EVENT_BIN)}),
    {'noreply', State#state{node=Node}, 0};
handle_cast({'passive'}, State) ->
    lager:debug("publisher has been usurp'd by newer process on another ecallmgr, moving to passive mode"),
    {'noreply', State#state{passive='true'}};
handle_cast({'channel_redirected', Props}, State) ->
    lager:debug("our channel has been redirected, shutting down immediately"),
    process_channel_event(Props),
    {'stop', {'shutdown', 'redirect'}, State};
handle_cast({'graceful_shutdown'}, #state{node=Node}=State) ->
    lager:debug("call event listener on node ~s received graceful shutdown request", [Node]),
    erlang:send_after(5000, self(), 'shutdown'),
    {'noreply', State};
handle_cast('shutdown', #state{node=Node}=State) ->
    lager:debug("call event listener on node ~s received shutdown request", [Node]),
    {'stop', 'normal', State};
handle_cast({'transferer', _}, State) ->
    lager:debug("call control has been transfered."),
    {'stop', 'normal', State};
handle_cast({'gen_listener', {'created_queue', _Q}}, State) ->
    {'noreply', State};
handle_cast({'gen_listener',{'is_consuming', _IsConsuming}}, State) ->
    {'noreply', State};
handle_cast(_Msg, State) ->
    lager:debug("unhandled cast: ~p", [_Msg]),
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {'noreply', State} |
%%                                   {'noreply', State, Timeout} |
%%                                   {'stop', Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({'event', [CallId | _]}, #state{callid=CallId
                                            ,passive='true'
                                           }=State) ->
    {'noreply', State};
handle_info({'event', [CallId | Props]}, #state{node=Node
                                                ,callid=CallId
                                               }=State) ->
    case {props:get_value(<<"Event-Subclass">>, Props, props:get_value(<<"Event-Name">>, Props))
          ,props:get_value(<<"Application">>, Props)}
    of
        {_, <<"redirect">>} ->
            gen_listener:cast(self(), {'channel_redirected', Props}),
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
            timer:sleep(1000),
            {'noreply', State};
        {_, _} ->
            process_channel_event(Props),
            {'noreply', State}
    end;
handle_info({'nodedown', _}, #state{node=Node
                                    ,is_node_up='true'
                                   }=State) ->
    lager:debug("lost connection to node ~s, waiting for reconnection", [Node]),
    erlang:monitor_node(Node, 'false'),
    TRef = erlang:send_after(?NODE_CHECK_PERIOD, self(), {'check_node_status'}),
    {'noreply', State#state{node_down_tref=TRef, is_node_up='false'}, 'hibernate'};
handle_info({'check_node_status'}, #state{is_node_up='false'
                                          ,failed_node_checks=FNC
                                         }=State) when (FNC+1) > ?MAX_FAILED_NODE_CHECKS ->
    lager:debug("node still not up after ~p checks, giving up", [FNC]),
    {'stop', 'normal', State};
handle_info({'check_node_status'}, #state{node=Node
                                          ,callid=CallId
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
                              ,callid=CallId
                              ,failed_node_checks=FNC
                             }=State) ->
    erlang:monitor_node(Node, 'true'),
    %% TODO: die if there is already a event producer on the AMPQ queue... ping/pong?
    case freeswitch:api(Node, 'uuid_exists', CallId) of
        {'error', 'timeout'} ->
            lager:warning("timeout trying to find call on node ~s, trying again", [Node]),
            {'noreply', State#state{failed_node_checks=FNC+1}, 1000};
        {'error', Reason} ->
            lager:warning("unable to find call on node ~s: ~p", [Node, Reason]),
            {'stop', 'normal', State};
        {'ok', <<"true">>} ->
            lager:debug("processing call events from ~s", [Node]),
            'true' = gproc:reg({'p', 'l', {'call_events_process', Node, CallId}}),
            'true' = gproc:reg({'p', 'l', ?FS_CALL_EVENT_REG_MSG(Node, CallId)}),
            'true' = gproc:reg({'p', 'l', ?FS_EVENT_REG_MSG(Node, ?CHANNEL_MOVE_RELEASED_EVENT_BIN)}),
            _ = usurp_other_publishers(State),
            {'noreply', State#state{failed_node_checks=0}};
        {'ok', Reason} ->
            lager:warning("unable to find call on node ~s: ~p", [Node, Reason]),
            {'stop', 'normal', State}
    end;
handle_info('sanity_check', #state{callid=CallId}=State) ->
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
handle_info(_Info, State) ->
    lager:debug("unhandled message: ~p", [_Info]),
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Allows listener to pass options to handlers
%%
%% @spec handle_event(JObj, State) -> {'reply', Options}
%% @end
%%--------------------------------------------------------------------
handle_event(_JObj, #state{ref=Ref
                           ,callid=CallId
                           ,node=Node
                          }) ->
    {'reply', [{'reference', Ref}
               ,{'call_id', CallId}
               ,{'node', Node}
              ]}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_listener when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_listener terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, #state{node_down_tref=NDTRef
                          ,sanity_check_tref=SCTRef
                         }) ->
    catch (erlang:cancel_timer(SCTRef)),
    catch (erlang:cancel_timer(NDTRef)),
    lager:debug("goodbye and thanks for all the fish: ~p", [_Reason]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {'ok', NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec maybe_process_channel_destroy(atom(), ne_binary(), wh_proplist()) -> 'ok'.
maybe_process_channel_destroy(Node, CallId, Props) ->
    case ecallmgr_fs_channel:node(CallId) of
        {'ok', Node} -> gen_server:cast(self(), {'graceful_shutdown'});
        {'error', _} -> gen_server:cast(self(), {'graceful_shutdown'});
        {'ok', _NewNode} ->
            lager:debug("channel is on ~s, not ~s: publishing channel move"
                        ,[CallId, _NewNode, Node]),
            Event = create_event(<<"CHANNEL_MOVED">>, <<"call_pickup">>, Props),
            publish_event(Event)
    end.

-spec process_channel_event(wh_proplist()) -> 'ok'.
process_channel_event(Props) ->
    put('callid', get_call_id(Props)),
    EventName = get_event_name(Props),
    ApplicationName = get_application_name(Props),
    Masqueraded = is_masquerade(Props),
    case should_publish(EventName, ApplicationName, Masqueraded) of
        'false' ->
            Action = props:get_value(<<"Action">>, Props),
            lager:debug("not publishing ~s(~s): ~s"
                               ,[EventName, ApplicationName, Action]);
        'true' ->
            Event = create_event(EventName, ApplicationName, Props),
            publish_event(Event)
    end.

-spec create_event(wh_proplist()) -> wh_proplist().
create_event(Props) ->
    create_event(get_event_name(Props), Props).

-spec create_event(ne_binary(), wh_proplist()) -> wh_proplist().
create_event(EventName, Props) ->
    create_event(EventName, get_application_name(Props), Props).

-spec create_event(ne_binary(), api_binary(), wh_proplist()) -> wh_proplist().
create_event(EventName, ApplicationName, Props) ->
    props:filter_undefined(
      [{<<"Event-Name">>, EventName}
       |specific_call_event_props(EventName, ApplicationName, Props)
       ++ generic_call_event_props(Props)
      ]).

-spec generic_call_event_props(wh_proplist()) -> wh_proplist().
generic_call_event_props(Props) ->
    {Mega,Sec,Micro} = os:timestamp(),
    Timestamp = wh_util:to_binary(((Mega * 1000000 + Sec) * 1000000 + Micro)),
    FSTimestamp = props:get_integer_value(<<"Event-Date-Timestamp">>, Props, Timestamp),
    NormalizedFSTimestamp = wh_util:unix_seconds_to_gregorian_seconds(FSTimestamp div 1000000),
    [{<<"Timestamp">>, NormalizedFSTimestamp}
     ,{<<"Msg-ID">>, wh_util:to_binary(FSTimestamp)}
     ,{<<"Call-ID">>, get_call_id(Props)}
     ,{<<"Transfer-History">>, get_transfer_history(Props)}
     ,{<<"Hangup-Cause">>, get_hangup_cause(Props)}
     ,{<<"Hangup-Code">>, get_hangup_code(Props)}
     ,{<<"Disposition">>, get_disposition(Props)}
     ,{<<"Raw-Application-Name">>, get_raw_application_name(Props)}
     ,{<<"Channel-Moving">>, get_channel_moving(Props)}
     ,{<<"Call-Direction">>, props:get_value(<<"Call-Direction">>, Props)}
     ,{<<"Caller-ID-Number">>, props:get_first_defined([<<"variable_effective_caller_id_number">>
                                                        ,<<"Caller-Caller-ID-Number">>
                                                       ], Props)}
     ,{<<"Caller-ID-Name">>, props:get_first_defined([<<"variable_effective_caller_id_name">>
                                                      ,<<"Caller-Caller-ID-Name">>
                                                     ], Props)}
     ,{<<"Callee-ID-Number">>, props:get_first_defined([<<"variable_effective_callee_id_number">>
                                                        ,<<"Caller-Callee-ID-Number">>
                                                       ], Props)}
     ,{<<"Callee-ID-Name">>, props:get_first_defined([<<"variable_effective_callee_id_name">>
                                                      ,<<"Caller-Callee-ID-Name">>
                                                     ], Props)}
     ,{<<"Other-Leg-Direction">>, props:get_value(<<"Other-Leg-Direction">>, Props)}
     ,{<<"Other-Leg-Caller-ID-Name">>, props:get_value(<<"Other-Leg-Caller-ID-Name">>, Props)}
     ,{<<"Other-Leg-Caller-ID-Number">>, props:get_value(<<"Other-Leg-Caller-ID-Number">>, Props)}
     ,{<<"Other-Leg-Destination-Number">>, props:get_value(<<"Other-Leg-Destination-Number">>, Props)}
     ,{<<"Other-Leg-Call-ID">>, props:get_first_defined([<<"Other-Leg-Unique-ID">>
                                                         ,<<"variable_holding_uuid">>
                                                        ], Props)}
     ,{<<"Presence-ID">>, props:get_value(<<"variable_presence_id">>, Props)}
     ,{<<"Raw-Application-Data">>, props:get_value(<<"Application-Data">>, Props)}
     ,{<<"Media-Server">>, props:get_value(<<"FreeSWITCH-Hostname">>, Props)}
     ,{<<"Replaced-By">>, props:get_value(<<"att_xfer_replaced_by">>, Props)}
     ,{<<"Custom-Channel-Vars">>, wh_json:from_list(ecallmgr_util:custom_channel_vars(Props))}
     | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
    ].

-spec publish_event(wh_proplist()) -> 'ok'.
publish_event(Props) ->
    %% call_control publishes channel create/destroy on the control
    %% events queue by calling create_event then this directly.
    EventName = wh_util:to_lower_binary(props:get_value(<<"Event-Name">>, Props, <<>>)),
    ApplicationName = wh_util:to_lower_binary(props:get_value(<<"Application-Name">>, Props, <<>>)),
    case {ApplicationName, EventName} of
        {_, <<"dtmf">>} ->
            Pressed = props:get_value(<<"DTMF-Digit">>, Props),
            lager:debug("publishing received DTMF digit ~s", [Pressed]);
        {<<>>, _} ->
            lager:debug("publishing call event ~s", [wh_util:to_lower_binary(EventName)]);
        {ApplicationName, <<"channel_execute_complete">>} ->
            ApplicationResponse = wh_util:to_lower_binary(props:get_value(<<"Application-Response">>, Props, <<>>)),
            ApplicationData = props:get_value(<<"Raw-Application-Data">>, Props, <<>>),
            lager:debug("publishing call event ~s '~s(~s)' result: ~s", [EventName, ApplicationName, ApplicationData, ApplicationResponse]);
        {ApplicationName, _} ->
            ApplicationData = props:get_value(<<"Raw-Application-Data">>, Props, <<>>),
            lager:debug("publishing call event ~s '~s(~s)'", [EventName, ApplicationName, ApplicationData])
    end,
    wapi_call:publish_event(Props).

-spec is_masquerade(wh_proplist()) -> boolean().
is_masquerade(Props) ->
    case props:get_value(<<"Event-Subclass">>, Props) of
        %% If this is a event created by whistle, then use
        %% the flag it as masqueraded
        <<"whistle::", _/binary>> -> 'true';
        %% otherwise process as the genuine article
        _Else -> 'false'
    end.

%% return a proplist of k/v pairs specific to the event
-spec specific_call_event_props(binary(), api_binary(), wh_proplist()) -> wh_proplist().
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
     ,{<<"Group-ID">>, props:get_value(<<"variable_media_group_id">>, Props)}
    ];
specific_call_event_props(<<"CHANNEL_EXECUTE_COMPLETE">>, <<"noop">>, Props) ->
    [{<<"Application-Name">>, <<"noop">>}
     ,{<<"Application-Response">>, props:get_value(<<"whistle_application_response">>, Props)}
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
    [{<<"Fax-Success">>, get_fax_success(Props)}
     ,{<<"Fax-ECM-Used">>, get_fax_ecm_used(Props)}
     ,{<<"Request">>, ecallmgr_util:get_sip_request(Props)}
     ,{<<"To">>, ecallmgr_util:get_sip_to(Props)}
     ,{<<"To-Uri">>, props:get_value(<<"variable_sip_to_uri">>, Props)}
     ,{<<"From">>, ecallmgr_util:get_sip_from(Props)}
     ,{<<"From-Uri">>, props:get_value(<<"variable_sip_from_uri">>, Props)}
     ,{<<"Remote-SDP">>, props:get_value(<<"variable_switch_r_sdp">>, Props)}
     ,{<<"Local-SDP">>, props:get_value(<<"variable_sip_local_sdp_str">>, Props)}
     ,{<<"Duration-Seconds">>, props:get_value(<<"variable_duration">>, Props)}
     ,{<<"Billing-Seconds">>, props:get_value(<<"variable_billsec">>, Props)}
     ,{<<"Ringing-Seconds">>, props:get_value(<<"variable_progresssec">>, Props)}
     ,{<<"User-Agent">>, props:get_value(<<"variable_sip_user_agent">>, Props)}
     ,{<<"Fax-Result-Code">>, props:get_value(<<"variable_fax_result_code">>, Props)}
     ,{<<"Fax-Result-Text">>, props:get_value(<<"variable_fax_result_text">>, Props)}
     ,{<<"Fax-Transferred-Pages">>, props:get_value(<<"variable_fax_document_transferred_pages">>, Props)}
     ,{<<"Fax-Total-Pages">>, props:get_value(<<"variable_fax_document_total_pages">>, Props)}
     ,{<<"Fax-Bad-Rows">>, props:get_value(<<"variable_fax_bad_rows">>, Props)}
     ,{<<"Fax-Transfer-Rate">>, props:get_value(<<"variable_fax_transfer_rate">>, Props)}
    ];
specific_call_event_props(<<"RECORD_STOP">>, _, Props) ->
    [{<<"Application-Name">>, <<"record">>}
     ,{<<"Application-Response">>, props:get_first_defined([<<"Record-File-Path">>
                                                            ,<<"whistle_application_response">>
                                                           ], Props)}
     ,{<<"Terminator">>, props:get_value(<<"variable_playback_terminator_used">>, Props)}
     ,{<<"Length">>, props:get_value(<<"variable_record_ms">>, Props)}
     ,{<<"Silence-Terminated">>, silence_terminated(Props)}
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
specific_call_event_props(<<"CHANNEL_FAX_STATUS">>, <<"rxfax", Event/binary>>, Prop) ->
    [{<<"Application-Name">>, <<"receive_fax">>}
    ,{<<"Application-Event">>, Event}
    ,{<<"Application-Data">>, wh_json:from_list(fax_specific(Prop))}
    ];
specific_call_event_props(<<"CHANNEL_FAX_STATUS">>, <<"txfax", Event/binary>>, Prop) ->
    [{<<"Application-Name">>, <<"send_fax">>}
    ,{<<"Application-Event">>, Event}
    ,{<<"Application-Data">>, wh_json:from_list(fax_specific(Prop))}
    ];
specific_call_event_props(_Evt, Application, Props) ->
    [{<<"Application-Name">>, props:get_value(Application, ?FS_APPLICATION_NAMES)}
     ,{<<"Application-Response">>, props:get_value(<<"Application-Response">>, Props)}
    ].

-spec page_specific(wh_proplist()) -> wh_proplist().
page_specific(Props) ->
    [{<<"Application-Name">>, <<"page">>}
     ,{<<"Application-Response">>, props:get_value(<<"Application-Response">>, Props)}
    ].

-spec conference_specific(wh_proplist()) -> wh_proplist().
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

-spec fax_specific(wh_proplist()) -> wh_proplist().
fax_specific(Props) ->
    props:filter_undefined(      
      [{<<"Fax-Success">>, get_fax_success(Props)}
       ,{<<"Fax-ECM-Used">>, get_fax_ecm_used(Props)}
       ,{<<"Fax-Result-Text">>, props:get_value(<<"variable_fax_result_text">>, Props)}
       ,{<<"Fax-Transferred-Pages">>, props:get_value(<<"variable_fax_document_transferred_pages">>, Props)}
       ,{<<"Fax-Total-Pages">>, props:get_value(<<"variable_fax_document_total_pages">>, Props)}
       ,{<<"Fax-Bad-Rows">>, props:get_value(<<"variable_fax_bad_rows">>, Props)}
       ,{<<"Fax-Transfer-Rate">>, props:get_value(<<"variable_fax_transfer_rate">>, Props)}
       ,{<<"Fax-Local-Station-ID">>, props:get_value(<<"variable_fax_local_statio_id">>, Props)}
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
       ]).

-spec should_publish(ne_binary(), ne_binary(), boolean()) -> boolean().
should_publish(<<"CHANNEL_EXECUTE_COMPLETE">>, <<"bridge">>, 'false') ->
    lager:debug("suppressing bridge execute complete in favour the whistle masquerade of this event"),
    'false';
should_publish(<<"CHANNEL_EXECUTE_COMPLETE">>, <<"intercept">>, 'false') ->
    lager:debug("suppressing intercept execute complete in favour the whistle masquerade of this event"),
    'false';
should_publish(<<"CHANNEL_EXECUTE_COMPLETE">>, <<"execute_extension">>, 'false') ->
    lager:debug("suppressing execute_extension execute complete in favour the whistle masquerade of this event"),
    'false';
should_publish(<<"CHANNEL_EXECUTE", _/binary>>, <<"park">>, _) ->
    'false';
should_publish(<<"CHANNEL_EXECUTE", _/binary>>, Application, _) ->
    props:get_value(Application, ?FS_APPLICATION_NAMES) =/= 'undefined';
should_publish(_, <<"transfer">>, _) ->
    'true';
should_publish(<<"CHANNEL_FAX_STATUS">>, _, _) ->
    'true';
should_publish(EventName, _A, _) ->
    lists:member(EventName, ?CALL_EVENTS).

-spec silence_terminated(api_integer() | wh_proplist()) -> api_boolean().
silence_terminated('undefined') -> 'undefined';
silence_terminated(Hits) when is_integer(Hits) -> Hits =:= 0;
silence_terminated(Prop) when is_list(Prop) ->
    case props:get_value(<<"variable_silence_hits_exhausted">>, Prop) of
        'undefined' -> silence_terminated(props:get_integer_value(<<"variable_record_silence_hits">>, Prop));
        Ex -> wh_util:is_true(Ex)
    end.

-spec is_channel_moving(wh_proplist()) -> boolean().
is_channel_moving(Props) ->
    props:get_is_true(<<"variable_channel_is_moving">>, Props, 'false').

-spec get_channel_moving(wh_proplist()) -> 'undefined' | boolean().
get_channel_moving(Props) ->
    case is_channel_moving(Props) of
        'false' -> 'undefined';
        'true' -> 'true'
    end.

-spec get_call_id(wh_proplist()) -> api_binary().
get_call_id(Props) ->
    props:get_first_defined([<<"Caller-Unique-ID">>
                             ,<<"Unique-ID">>
                             ,<<"variable_uuid">>
                             ,<<"Channel-Call-UUID">>
                             ,<<"variable_sip_call_id">>
                            ], Props).

-spec get_event_name(wh_proplist()) -> api_binary().
get_event_name(Props) ->
    case props:get_first_defined([<<"whistle_application_name">>
                                  ,<<"Application">>
                                  ,<<"Event-Subclass">>
                                 ], Props)
    of
        <<"sofia::transferee">> -> <<"CHANNEL_TRANSFEREE">>;
        <<"sofia::transferor">> -> <<"CHANNEL_TRANSFEROR">>;
        <<"sofia::replaced">> -> <<"CHANNEL_REPLACED">>;
        <<"spandsp::txfax", _/binary>> -> <<"CHANNEL_FAX_STATUS">>;
        <<"spandsp::rxfax", _/binary>> -> <<"CHANNEL_FAX_STATUS">>;
        _Else ->
            props:get_first_defined([<<"whistle_event_name">>
                                     ,<<"Event-Name">>
                                    ], Props)
    end.

-spec get_application_name(wh_proplist()) -> api_binary().
get_application_name(Props) ->
    case props:get_first_defined([<<"whistle_application_name">>
                                  ,<<"Application">>
                                  ,<<"Event-Subclass">>
                                 ], Props)
    of
        <<"sofia::transferee">> -> <<"transfer">>;
        <<"sofia::transferor">> -> <<"transfer">>;
        <<"sofia::replaced">> -> <<"transfer">>;
        <<"spandsp::rxfax", Event/binary >> -> <<"rxfax",Event/binary>>;
        <<"spandsp::txfax", Event/binary >> -> <<"txfax", Event/binary>>;
        Else -> Else
    end.

-spec get_raw_application_name(wh_proplist()) -> api_binary().
get_raw_application_name(Props) ->
    props:get_first_defined([<<"Application">>
                             ,<<"whistle_application_name">>
                             ,<<"Event-Subclass">>
                            ], Props).

-spec get_fax_success(wh_proplist()) -> 'undefined' | boolean().
get_fax_success(Props) ->
    case props:get_value(<<"variable_fax_success">>, Props) of
        'undefined' -> 'undefined';
        Else -> Else =/= <<"0">>
    end.

-spec get_fax_ecm_used(wh_proplist()) -> 'undefined' | boolean().
get_fax_ecm_used(Props) ->
    case props:get_value(<<"variable_fax_ecm_used">>, Props) of
        'undefined' -> 'undefined';
        Else -> Else =/= <<"off">>
    end.

-spec get_transfer_history(wh_proplist()) -> wh_json:object().
get_transfer_history(Props) ->
    SerializedHistory = props:get_value(<<"variable_transfer_history">>, Props),
    case [HistJObj
          || Trnsf <- ecallmgr_util:unserialize_fs_array(SerializedHistory),
             (HistJObj = create_trnsf_history_object(binary:split(Trnsf, <<":">>, ['global']))) =/= 'undefined']
    of
        [] -> 'undefined';
        History -> wh_json:from_list(History)
    end.

-spec create_trnsf_history_object(list()) -> {ne_binary(), wh_json:object()} | 'undefined'.
create_trnsf_history_object([Epoch, CallId, <<"att_xfer">>, Props]) ->
    [Transferee, Transferer] = binary:split(Props, <<"/">>),
    Trans = [{<<"Call-ID">>, CallId}
             ,{<<"Type">>, <<"attended">>}
             ,{<<"Transferee">>, Transferee}
             ,{<<"Transferer">>, Transferer}
            ],
    {Epoch, wh_json:from_list(Trans)};
create_trnsf_history_object([Epoch, CallId, <<"bl_xfer">> | Props]) ->
    %% This looks confusing but FS uses the same delimiter to in the array
    %% as it does for inline dialplan actions (like those created during partial attended)
    %% so we have to put it together to take it apart... I KNOW! ARRRG
    Dialplan = lists:last(binary:split(wh_util:join_binary(Props, <<":">>), <<",">>)),
    [Exten | _] = binary:split(Dialplan, <<"/">>, ['global']),
    Trans = [{<<"Call-ID">>, CallId}
             ,{<<"Type">>, <<"blind">>}
             ,{<<"Extension">>, Exten}
            ],
    {Epoch, wh_json:from_list(Trans)};
create_trnsf_history_object(_) ->
    'undefined'.

-spec get_hangup_cause(wh_proplist()) -> api_binary().
get_hangup_cause(Props) ->
    case props:get_value(<<"variable_current_application">>, Props) of
        <<"bridge">> ->
            props:get_first_defined([<<"variable_bridge_hangup_cause">>
                                     ,<<"variable_hangup_cause">>
                                     ,<<"Hangup-Cause">>], Props);
        _Else ->
            props:get_first_defined([<<"variable_hangup_cause">>
                                     ,<<"variable_bridge_hangup_cause">>
                                     ,<<"Hangup-Cause">>], Props)
    end.

-spec get_disposition(wh_proplist()) -> api_binary().
get_disposition(Props) ->
    props:get_first_defined([<<"variable_originate_disposition">>
                             ,<<"variable_endpoint_disposition">>
                            ], Props).

-spec get_hangup_code(wh_proplist()) -> api_binary().
get_hangup_code(Props) ->
    props:get_first_defined([<<"variable_proto_specific_hangup_cause">>
                             ,<<"variable_last_bridge_proto_specific_hangup_cause">>
                            ], Props).

-spec swap_call_legs(wh_proplist() | wh_json:object()) -> wh_proplist().
-spec swap_call_legs(wh_proplist(), wh_proplist()) -> wh_proplist().

swap_call_legs(Props) when is_list(Props) -> swap_call_legs(Props, []);
swap_call_legs(JObj) -> swap_call_legs(wh_json:to_proplist(JObj)).

swap_call_legs([], Swap) -> Swap;
swap_call_legs([{<<"Caller-", Key/binary>>, Value}|T], Swap) ->
    swap_call_legs(T, [{<<"Other-Leg-", Key/binary>>, Value}|Swap]);
swap_call_legs([{<<"Other-Leg-", Key/binary>>, Value}|T], Swap) ->
    swap_call_legs(T, [{<<"Caller-", Key/binary>>, Value}|Swap]);
swap_call_legs([Prop|T], Swap) ->
    swap_call_legs(T, [Prop|Swap]).

-spec usurp_other_publishers(state()) -> 'ok'.
usurp_other_publishers(#state{node=Node
                              ,ref=Ref
                              ,callid=CallId}) ->
    Usurp = [{<<"Call-ID">>, CallId}
             ,{<<"Media-Node">>, Node}
             ,{<<"Reference">>, Ref}
             | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
            ],
    wapi_call:publish_usurp_publisher(CallId, Usurp).
