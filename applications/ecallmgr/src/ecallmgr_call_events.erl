%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2013, 2600Hz
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
-export([create_event/3]).
-export([create_event_props/3]).
-export([publish_event/1]).
-export([transfer/3]).
-export([get_fs_var/4]).
-export([handle_publisher_usurp/2]).
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
    Masqueraded = is_masquerade(Props),
    EventName = get_event_name(Props, Masqueraded),
    ApplicationName = get_event_application(Props, Masqueraded),
    wh_json:from_list(create_event(EventName, ApplicationName, Props)).

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
    {'ok', #state{node=Node, callid=CallId}}.

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
    Ref = wh_util:rand_hex_binary(12),
    Usurp = [{<<"Call-ID">>, CallId}
             ,{<<"Media-Node">>, Node}
             ,{<<"Reference">>, Ref}
             | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
            ],
    wapi_call:publish_usurp_publisher(CallId, Usurp),
    {'noreply', State#state{sanity_check_tref=TRef
                            ,ref = Ref
                           }};
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
                              ,ref=Ref
                             }=State) ->
    erlang:monitor_node(Node, 'true'),
    %% TODO: die if there is already a event producer on the AMPQ queue... ping/pong?
    case freeswitch:api(Node, 'uuid_exists', CallId) of
        'timeout' ->
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
            Usurp = [{<<"Call-ID">>, CallId}
                     ,{<<"Media-Node">>, Node}
                     ,{<<"Reference">>, Ref}
                     | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                    ],
            wapi_call:publish_usurp_publisher(CallId, Usurp),
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
        {'ok', Node} -> process_channel_destroy(Node, CallId, Props);
        {'error', _} -> process_channel_destroy(Node, CallId, Props);
        {'ok', _NewNode} ->
            lager:debug("surpressing destroy; ~s is on ~s, not ~s: publishing channel move instead", [CallId, _NewNode, Node]),
            publish_event(create_event(<<"CHANNEL_MOVED">>, <<"call_pickup">>, Props))
    end.

-spec process_channel_destroy(atom(), ne_binary(), wh_proplist()) -> 'ok'.
process_channel_destroy(_, _, Props) ->
    EventName = props:get_value(<<"Event-Name">>, Props),
    ApplicationName = props:get_value(<<"Application">>, Props),
    publish_event(create_event(EventName, ApplicationName, Props)),
    gen_server:cast(self(), {'graceful_shutdown'}).

-spec process_channel_event(wh_proplist()) -> 'ok'.
process_channel_event(Props) ->
    CallId = props:get_value(<<"Caller-Unique-ID">>, Props,
                             props:get_value(<<"Unique-ID">>, Props)),
    put('callid', CallId),
    Masqueraded = is_masquerade(Props),
    EventName = get_event_name(Props, Masqueraded),
    ApplicationName = get_event_application(Props, Masqueraded),

    case should_publish(EventName, ApplicationName, Masqueraded) of
        'false' -> lager:debug("not publishing ~s(~s): ~s", [EventName, ApplicationName, props:get_value(<<"Action">>, Props)]);
        'true' ->
            %% TODO: the adding of the node to the props is for event_specific conference
            %% clause until we can break the conference stuff into its own module
            publish_event(create_event(EventName, ApplicationName, Props))
    end.

-spec create_event(ne_binary(), api_binary(), wh_proplist()) -> wh_proplist().
create_event(_, <<"sofia::transferee">>, Props) ->
    create_event(<<"CHANNEL_TRANSFEREE">>, <<"transfer">>, Props);
create_event(_, <<"sofia::transferor">>, Props) ->
    create_event(<<"CHANNEL_TRANSFEROR">>, <<"transfer">>, Props);
create_event(_, <<"sofia::replaced">>, Props) ->
    create_event(<<"CHANNEL_REPLACED">>, <<"transfer">>, Props);
create_event(EventName, ApplicationName, Props) ->
    wh_api:default_headers(?EVENT_CAT, EventName, ?APP_NAME, ?APP_VERSION)
        ++ create_event_props(EventName, ApplicationName, Props).

-spec create_event_props(binary(), api_binary(), wh_proplist()) -> wh_proplist().
create_event_props(EventName, ApplicationName, Props) ->
    CCVs = ecallmgr_util:custom_channel_vars(Props),

    {Mega,Sec,Micro} = os:timestamp(),
    Timestamp = wh_util:to_binary(((Mega * 1000000 + Sec) * 1000000 + Micro)),

    %% Unix-epoch, in microseconds
    FSTimestamp = props:get_integer_value(<<"Event-Date-Timestamp">>, Props, Timestamp),
    NormalizedFSTimestamp = wh_util:unix_seconds_to_gregorian_seconds(FSTimestamp div 1000000),

    props:filter_undefined(
      [{<<"Msg-ID">>, wh_util:to_binary(FSTimestamp)}
       ,{<<"Timestamp">>, NormalizedFSTimestamp}
       ,{<<"Call-ID">>, find_fs_callid(Props)}
       ,{<<"Call-Direction">>, props:get_value(<<"Call-Direction">>, Props)}
       ,{<<"Channel-Call-State">>, props:get_value(<<"Channel-Call-State">>, Props)}
       ,{<<"Channel-State">>, get_channel_state(Props)}
       ,{<<"Transfer-History">>, get_transfer_history(Props)}
       ,{<<"Hangup-Cause">>, get_hangup_cause(Props)}
       ,{<<"Hangup-Code">>, get_hangup_code(Props)}
       ,{<<"Disposition">>, get_disposition(Props)}
       ,{<<"Other-Leg-Direction">>, props:get_value(<<"Other-Leg-Direction">>, Props)}
       ,{<<"Other-Leg-Caller-ID-Name">>, props:get_value(<<"Other-Leg-Caller-ID-Name">>, Props)}
       ,{<<"Other-Leg-Caller-ID-Number">>, props:get_value(<<"Other-Leg-Caller-ID-Number">>, Props)}
       ,{<<"Other-Leg-Destination-Number">>, props:get_value(<<"Other-Leg-Destination-Number">>, Props)}
       ,{<<"Other-Leg-Unique-ID">>, props:get_value(<<"Other-Leg-Unique-ID">>, Props,
                                                    props:get_value(<<"variable_holding_uuid">>, Props))}

       ,{<<"Fax-Success">>, props:get_value(<<"variable_fax_success">>, Props) =/= <<"0">>}
       ,{<<"Fax-Result-Code">>, props:get_value(<<"variable_fax_result_code">>, Props)}
       ,{<<"Fax-Result-Text">>, props:get_value(<<"variable_fax_result_text">>, Props)}
       ,{<<"Fax-ECM-Used">>, props:get_value(<<"variable_fax_ecm_used">>, Props) =/= <<"off">>}
       ,{<<"Fax-Transferred-Pages">>, props:get_value(<<"variable_fax_document_transferred_pages">>, Props)}
       ,{<<"Fax-Total-Pages">>, props:get_value(<<"variable_fax_document_total_pages">>, Props)}
       ,{<<"Fax-Bad-Rows">>, props:get_value(<<"variable_fax_bad_rows">>, Props)}
       ,{<<"Fax-Transfer-Rate">>, props:get_value(<<"variable_fax_transfer_rate">>, Props)}

       ,{<<"Custom-Channel-Vars">>, wh_json:from_list(CCVs)}
       %% this sucks, its leaky but I dont see a better way around it since we need the raw application
       %% name in call_control... (see note in call_control on start_link for why we need to use AMQP
       %% to communicate to it)
       ,{<<"Presence-ID">>, props:get_value(<<"variable_presence_id">>, Props)}
       ,{<<"Raw-Application-Name">>, props:get_value(<<"Application">>, Props, ApplicationName)}
       ,{<<"Raw-Application-Data">>, props:get_value(<<"Application-Data">>, Props)}
       ,{<<"Media-Server">>, props:get_value(<<"FreeSWITCH-Hostname">>, Props)}
       ,{<<"Channel-Moving">>, wh_util:to_binary(is_channel_moving(Props))}
       ,{<<"Replaced-By">>, props:get_value(<<"att_xfer_replaced_by">>, Props)}
       | event_specific(EventName, ApplicationName, Props)
      ]).

find_fs_callid(Props) ->
    find_fs_callid(Props, [<<"Caller-Unique-ID">>
                           ,<<"Unique-ID">>
                           ,<<"variable_uuid">>
                           ,<<"Channel-Call-UUID">>
                           ,<<"variable_sip_call_id">>
                          ]).
find_fs_callid(Props, [H|T]) ->
    case props:get_value(H, Props) of
        'undefined' -> find_fs_callid(Props, T);
        V -> V
    end;
find_fs_callid(_, []) -> 'undefined'.

-spec is_channel_moving(wh_proplist()) -> boolean().
is_channel_moving(Props) ->
    wh_util:is_true(props:get_value(<<"variable_channel_is_moving">>, Props)).

-spec publish_event(wh_proplist()) -> 'ok'.
publish_event(Props) ->
    %% call_control publishes channel create/destroy on the control
    %% events queue by calling create_event then this directly.
    EventName = wh_util:to_lower_binary(props:get_value(<<"Event-Name">>, Props, <<>>)),
    ApplicationName = wh_util:to_lower_binary(props:get_value(<<"Application-Name">>, Props, <<>>)),
    CallId = props:get_value(<<"Call-ID">>, Props),

    put('callid', CallId),

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
    wapi_call:publish_event(CallId, Props).

-spec is_masquerade(wh_proplist()) -> boolean().
is_masquerade(Props) ->
    case props:get_value(<<"Event-Subclass">>, Props) of
        %% If this is a event created by whistle, then use
        %% the flag it as masqueraded
        <<"whistle::", _/binary>> -> 'true';
        %% otherwise process as the genuine article
        _Else -> 'false'
    end.

-spec get_event_name(wh_proplist(), boolean()) -> api_binary().
get_event_name(Props, 'true') -> props:get_value(<<"whistle_event_name">>, Props);
get_event_name(Props, 'false') -> props:get_value(<<"Event-Name">>, Props).

-spec get_event_application(wh_proplist(), boolean()) -> api_binary().
get_event_application(Props, 'true') ->
    %% when the event is masqueraded override the actual application
    %% with what whistle wants the event to be
    props:get_value(<<"whistle_application_name">>, Props);
get_event_application(Props, 'false') ->
    props:get_value(<<"Application">>, Props
                    ,props:get_value(<<"Event-Subclass">>, Props)).

%% return a proplist of k/v pairs specific to the event
-spec event_specific(binary(), api_binary(), wh_proplist()) -> wh_proplist().
event_specific(<<"CHANNEL_EXECUTE_COMPLETE">>, <<"playback">> = Application, Prop) ->
    %% if the playback was terminated as a result of DTMF, include it
    [{<<"DTMF-Digit">>, props:get_value(<<"variable_playback_terminator_used">>, Prop)}
     ,{<<"Application-Name">>, props:get_value(Application, ?FS_APPLICATION_NAMES)}
     ,{<<"Application-Response">>, props:get_value(<<"Application-Response">>, Prop)}
     ,{<<"Group-ID">>, props:get_value(<<"variable_media_group_id">>, Prop)}
    ];

event_specific(<<"CHANNEL_EXECUTE">>, <<"conference">>, Prop) ->
    conference_specific(Prop);
event_specific(<<"CHANNEL_EXECUTE_COMPLETE">>, <<"conference">>, Prop) ->
    case props:get_value(<<"variable_current_application_data">>, Prop) of
        <<"page_", _/binary>> -> page_specific(Prop);
        _Else -> conference_specific(Prop)
    end;

event_specific(<<"CHANNEL_EXECUTE_COMPLETE">>, <<"noop">>, Prop) ->
    [{<<"Application-Name">>, <<"noop">>}
     ,{<<"Application-Response">>, props:get_value(<<"whistle_application_response">>, Prop)}
    ];
event_specific(<<"CHANNEL_EXECUTE_COMPLETE">>, <<"bridge">>, Prop) ->
    [{<<"Application-Name">>, <<"bridge">>}
     ,{<<"Application-Response">>, props:get_value(<<"variable_originate_disposition">>, Prop, <<"FAIL">>)}
    ];
event_specific(<<"CHANNEL_EXECUTE_COMPLETE">>, <<"record">>, Prop) ->
    [{<<"Application-Name">>, <<"bridge">>}
     ,{<<"Application-Response">>, props:get_value(<<"variable_originate_disposition">>, Prop, <<"FAIL">>)}
     ,{<<"Length">>, props:get_value(<<"variable_record_ms">>, Prop)}
    ];
event_specific(<<"CHANNEL_EXECUTE_COMPLETE">>, <<"set">>, Prop) ->
    [{<<"Application-Name">>, props:get_value(<<"set">>, ?FS_APPLICATION_NAMES)}
     ,{<<"Application-Response">>, props:get_value(<<"Application-Response">>, Prop)}
    ];

event_specific(<<"CHANNEL_CREATE">>, _, Prop) ->
    [{<<"To">>, ecallmgr_util:get_sip_to(Prop)}
     ,{<<"From">>, ecallmgr_util:get_sip_from(Prop)}
     ,{<<"Request">>, ecallmgr_util:get_sip_request(Prop)}
    ];
event_specific(<<"CHANNEL_ANSWER">>, _, Prop) ->
    [{<<"To">>, ecallmgr_util:get_sip_to(Prop)}
     ,{<<"From">>, ecallmgr_util:get_sip_from(Prop)}
     ,{<<"Request">>, ecallmgr_util:get_sip_request(Prop)}
    ];
event_specific(<<"CHANNEL_DESTROY">>, _, Prop) ->
    [{<<"To">>, ecallmgr_util:get_sip_to(Prop)}
     ,{<<"From">>, ecallmgr_util:get_sip_from(Prop)}
     ,{<<"Request">>, ecallmgr_util:get_sip_request(Prop)}
    ];

event_specific(<<"RECORD_STOP">>, _, Prop) ->
    [{<<"Application-Name">>, <<"record">>}
     ,{<<"Application-Response">>, props:get_value(<<"Record-File-Path">>, Prop, props:get_value(<<"whistle_application_response">>, Prop))}
     ,{<<"Terminator">>, props:get_value(<<"variable_playback_terminator_used">>, Prop)}
     ,{<<"Length">>, props:get_value(<<"variable_record_ms">>, Prop)}
     ,{<<"Silence-Terminated">>, silence_terminated(Prop)}
    ];
event_specific(<<"DETECTED_TONE">>, _, Prop) ->
    [{<<"Detected-Tone">>, props:get_value(<<"Detected-Tone">>, Prop)}];
event_specific(<<"DTMF">>, _, Prop) ->
    Pressed = props:get_value(<<"DTMF-Digit">>, Prop),
    Duration = props:get_value(<<"DTMF-Duration">>, Prop),
    [{<<"DTMF-Digit">>, Pressed}
     ,{<<"DTMF-Duration">>, Duration}
    ];
event_specific(_, <<"play_and_get_digits">>, Prop) ->
    [{<<"Application-Name">>, <<"play_and_collect_digits">>}
     ,{<<"Application-Response">>, props:get_value(<<"variable_collected_digits">>, Prop, <<"">>)}
    ];
event_specific(_Evt, Application, Prop) ->
    [{<<"Application-Name">>, props:get_value(Application, ?FS_APPLICATION_NAMES)}
     ,{<<"Application-Response">>, props:get_value(<<"Application-Response">>, Prop)}
    ].

page_specific(Prop) ->
    [{<<"Application-Name">>, <<"page">>}
     ,{<<"Application-Response">>, props:get_value(<<"Application-Response">>, Prop)}
    ].
conference_specific(Prop) ->
    Default = [{<<"Application-Name">>, <<"conference">>}
               ,{<<"Application-Response">>, props:get_value(<<"Application-Response">>, Prop)}
              ],
    case props:get_value(<<"Application-Data">>, Prop) of
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

-spec silence_terminated(api_integer() | wh_proplist()) -> api_boolean().
silence_terminated('undefined') -> 'undefined';
silence_terminated(Hits) when is_integer(Hits) -> Hits =:= 0;
silence_terminated(Prop) when is_list(Prop) ->
    case props:get_value(<<"variable_silence_hits_exhausted">>, Prop) of
        'undefined' -> silence_terminated(props:get_integer_value(<<"variable_record_silence_hits">>, Prop));
        Ex -> wh_util:is_true(Ex)
    end.

-spec get_fs_var(atom(), ne_binary(), ne_binary(), binary()) -> binary().
get_fs_var(Node, CallId, Var, Default) ->
    case freeswitch:api(Node, 'uuid_getvar', wh_util:to_list(<<CallId/binary, " ", Var/binary>>)) of
        {'ok', <<"_undef_">>} -> Default;
        {'ok', <<"_none_">>} -> Default;
        {'ok', Value} -> Value;
        _ -> Default
    end.

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
should_publish(<<"CHANNEL_EXECUTE", _/binary>>, Application, _) ->
    props:get_value(Application, ?FS_APPLICATION_NAMES) =/= 'undefined';
should_publish(_, <<"sofia::transferee">>, _) ->
    'true';
should_publish(_, <<"sofia::transferor">>, _) ->
    'true';
should_publish(_, <<"sofia::replaced">>, _) ->
    'true';
should_publish(EventName, _A, _) ->
    lists:member(EventName, ?CALL_EVENTS).

-spec get_transfer_history(wh_proplist()) -> wh_json:object().
get_transfer_history(Props) ->
    SerializedHistory = props:get_value(<<"variable_transfer_history">>, Props),
    Hist = [HistJObj
            || Trnsf <- ecallmgr_util:unserialize_fs_array(SerializedHistory),
               (HistJObj = create_trnsf_history_object(binary:split(Trnsf, <<":">>, ['global']))) =/= 'undefined'],
    wh_json:from_list(Hist).

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

-spec get_channel_state(wh_proplist()) -> ne_binary().
get_channel_state(Prop) ->
    case props:get_value(props:get_value(<<"Channel-State">>, Prop), ?FS_CHANNEL_STATES) of
        'undefined' -> <<"unknown">>;
        ChannelState -> ChannelState
    end.

-spec get_hangup_cause(wh_proplist()) -> api_binary().
get_hangup_cause(Props) ->
    Causes = case props:get_value(<<"variable_current_application">>, Props) of
                 <<"bridge">> ->
                     [<<"variable_bridge_hangup_cause">>, <<"variable_hangup_cause">>, <<"Hangup-Cause">>];
                 _ ->
                     [<<"variable_hangup_cause">>, <<"variable_bridge_hangup_cause">>, <<"Hangup-Cause">>]
             end,
    find_event_value(Causes, Props).

-spec get_disposition(wh_proplist()) -> api_binary().
get_disposition(Props) ->
    find_event_value([<<"variable_originate_disposition">>
                      ,<<"variable_endpoint_disposition">>
                     ], Props).

-spec get_hangup_code(wh_proplist()) -> api_binary().
get_hangup_code(Props) ->
    find_event_value([<<"variable_proto_specific_hangup_cause">>
                      ,<<"variable_last_bridge_proto_specific_hangup_cause">>
                     ], Props).

-spec find_event_value(ne_binaries(), wh_proplist()) -> api_binary().
find_event_value(Keys, Props) ->
    find_event_value(Keys, Props, 'undefined').

-spec find_event_value(ne_binaries(), wh_proplist(), term()) -> term().
find_event_value([], _, Default) -> Default;
find_event_value([H|T], Props, Default) ->
    Value = props:get_value(H, Props),
    case wh_util:is_empty(Value) of
        'true' -> find_event_value(T, Props, Default);
        'false' -> Value
    end.

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
