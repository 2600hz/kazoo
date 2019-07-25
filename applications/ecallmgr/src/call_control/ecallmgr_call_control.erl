%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc Created when a call hits a fetch_handler in ecallmgr_route.
%%% A Control Queue is created by the lookup_route function in the
%%% fetch_handler. On initialization, besides adding itself as the
%%% consumer for the AMQP messages, Call Control creates an empty queue
%%% object (not to be confused with AMQP queues), sets the current
%%% application running on the switch to the empty binary, and records
%%% the timestamp of when the initialization finishes. The process then
%%% enters its loop to wait.
%%%
%%% When receiving an AMQP message, after decoding the JSON into a proplist,
%%% we check if the application is "queue" or not; if it is "queue", we
%%% extract the default headers out, iterate through the Commands portion,
%%% and append the default headers to the application-specific portions, and
%%% insert these commands into the CmdQ. We then check whether the old CmdQ is
%%% empty AND the new CmdQ is not, and that the current App is the empty
%%% binary. If so, we de-queue the next command, execute it, and loop; otherwise
%%% we loop with the CmdQ.
%%% If just a single application is sent in the message, we check the CmdQ's
%%% size and the current App's status; if both are empty, we fire the command
%%% immediately; otherwise we add the command to the CmdQ and loop.
%%%
%%% When receiving an {execute_complete, CALLID, EvtName} tuple from
%%% the corresponding ecallmgr_call_events process tracking the call,
%%% we convert the CurrApp name from Kazoo parlance to FS, matching
%%% it against what application name we got from FS via the events
%%% process. If CurrApp is empty, we just loop since the completed
%%% execution probably wasn't related to our stuff (perhaps FS internal);
%%% if the converted Kazoo name matches the passed FS name, we know
%%% the CurrApp cmd has finished and can execute the next command in the
%%% queue. If there are no commands in the queue, set CurrApp to 'undefined' and
%%% loop; otherwise take the next command, execute it, and look with it as
%%% the CurrApp. If EvtName and the converted Kazoo name don't match,
%%% something else executed that might have been related to the main
%%% application's execute (think set commands, like playback terminators);
%%% we can note the event happened, and continue looping as we were.
%%%
%%%
%%% @author James Aimonetti <james@2600hz.org>
%%% @author Karl Anderson <karl@2600hz.org>
%%% @end
%%%-----------------------------------------------------------------------------
-module(ecallmgr_call_control).
-behaviour(gen_server).

%% API
-export([start_link/1, stop/1]).
-export([callid/1]).
-export([node/1]).
-export([hostname/1]).
-export([queue_name/1]).
-export([other_legs/1
        ,update_node/2
        ,control_procs/1
        ]).
-export([fs_nodeup/2]).
-export([fs_nodedown/2]).

%% gen_server callbacks
-export([handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3
        ,init/1
        ,init_control/2
        ]).

-include("ecallmgr.hrl").

-define(SERVER, ?MODULE).

-define(KEEP_ALIVE, 2 * ?MILLISECONDS_IN_SECOND).

-type insert_at_options() :: 'now' | 'head' | 'tail' | 'flush'.

-record(state, {node :: atom()
               ,call_id :: kz_term:api_ne_binary()
               ,command_q = queue:new() :: queue:queue()
               ,current_app :: kz_term:api_ne_binary()
               ,current_cmd :: kz_term:api_object()
               ,start_time = os:timestamp() :: kz_time:now()
               ,is_call_up = 'true' :: boolean()
               ,is_node_up = 'true' :: boolean()
               ,keep_alive_ref :: kz_term:api_reference()
               ,other_legs = [] :: kz_term:ne_binaries()
               ,last_removed_leg :: kz_term:api_ne_binary()
               ,sanity_check_tref :: kz_term:api_reference()
               ,msg_id :: kz_term:api_ne_binary()
               ,fetch_id :: kz_term:api_ne_binary()
               ,controller_q :: kz_term:api_ne_binary()
               ,controller_p :: kz_term:api_pid()
               ,control_q :: kz_term:api_ne_binary()
               ,initial_ccvs :: kz_term:api_object()
               ,node_down_tref :: kz_term:api_reference()
               ,current_cmd_uuid :: kz_term:api_binary()
               ,event_uuids = [] :: kz_term:ne_binaries()
               ,control_ctx = #{} :: map()
               }).
-type state() :: #state{}.

-define(RESPONDERS, []).
-define(QUEUE_NAME, <<>>).
-define(QUEUE_OPTIONS, []).
-define(CONSUME_OPTIONS, []).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the server
%% @end
%%------------------------------------------------------------------------------
-spec start_link(map()) -> kz_types:startlink_ret().
start_link(Map) ->
    proc_lib:start_link(?MODULE, 'init_control', [self(), Map]).

-spec stop(pid()) -> 'ok'.
stop(Srv) ->
    gen_server:cast(Srv, 'stop').

-spec callid(pid()) -> kz_term:ne_binary().
callid(Srv) ->
    gen_server:call(Srv, 'callid', ?MILLISECONDS_IN_SECOND).

-spec node(pid()) -> kz_term:ne_binary().
node(Srv) ->
    gen_server:call(Srv, 'node', ?MILLISECONDS_IN_SECOND).

-spec hostname(pid()) -> binary().
hostname(Srv) ->
    Node = ?MODULE:node(Srv),
    case binary:split(kz_term:to_binary(Node), <<"@">>) of
        [_, Hostname] -> Hostname;
        Other -> Other
    end.

-spec queue_name(kz_term:api_pid()) -> kz_term:api_ne_binary().
queue_name('undefined') -> 'undefined';
queue_name(Srv) when is_pid(Srv) -> gen_server:call(Srv, 'queue_name');
queue_name(_) -> 'undefined'.

-spec other_legs(pid()) -> kz_term:ne_binaries().
other_legs(Srv) ->
    gen_server:call(Srv, 'other_legs', ?MILLISECONDS_IN_SECOND).

-spec update_node(atom(), kz_term:ne_binary() | kz_term:pids()) -> 'ok'.
update_node(Node, CallId) when is_binary(CallId) ->
    update_node(Node, gproc:lookup_pids({'p', 'l', {'call_control', CallId}}));
update_node(Node, Pids) when is_list(Pids) ->
    _ = [gen_server:cast(Srv, {'update_node', Node}) || Srv <- Pids],
    'ok'.

-spec control_procs(kz_term:ne_binary()) -> kz_term:pids().
control_procs(CallId) ->
    gproc:lookup_pids({'p', 'l', {'call_control', CallId}}).

-spec fs_nodeup(pid(), atom()) -> 'ok'.
fs_nodeup(Srv, Node) ->
    gen_server:cast(Srv, {'fs_nodeup', Node}).

-spec fs_nodedown(pid(), atom()) -> 'ok'.
fs_nodedown(Srv, Node) ->
    gen_server:cast(Srv, {'fs_nodedown', Node}).

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server.
%% @end
%%------------------------------------------------------------------------------
-spec init(list()) -> {'ok', state()}.
init(_) -> {'ok', #state{}}.

-spec init_control(pid() , map()) -> 'ok'.
init_control(Pid, #{node := Node
                   ,call_id := CallId
                   ,callback := Fun
                   ,fetch_id := FetchId
                   ,control_q := ControlQ
                   ,init_fun := InitFun
                   ,exit_fun := ExitFun
                                                %                   ,channel := Channel
                   }=Payload) ->
    proc_lib:init_ack(Pid, {'ok', self()}),
    InitFun(Payload),
                                                %    kz_amqp_channel:consumer_channel(Channel),
    try Fun(Payload) of
        {'ok', #{controller_q := ControllerQ
                ,initial_ccvs := CCVs
                }=Ctx} ->
            bind(Node, CallId),
            TRef = erlang:send_after(?SANITY_CHECK_PERIOD, self(), 'sanity_check'),
            State = #state{node=Node
                          ,call_id=CallId
                          ,command_q=queue:new()
                          ,start_time=os:timestamp()
                          ,sanity_check_tref=TRef
                          ,fetch_id=FetchId
                          ,controller_q=ControllerQ
                          ,control_q=ControlQ
                          ,initial_ccvs=CCVs
                          ,is_node_up=true
                          ,is_call_up=true
                          ,control_ctx = Ctx
                          },
            call_control_ready(State),
            gen_server:enter_loop(?MODULE, [], State);
        _Other ->
    catch(ExitFun(Payload)),
            lager:debug("callback doesn't want to proceed")
    catch
        _Ex:_Err:_Stacktrace ->
            catch(ExitFun(Payload)),
            lager:debug("error running callback ~p : ~p", [_Ex, _Err])

    end;
init_control(Pid, #{node := Node
                   ,call_id := CallId
                   ,fetch_id := FetchId
                   ,controller_q := ControllerQ
                   ,control_q := ControlQ
                   ,initial_ccvs := CCVs
                   ,init_fun := InitFun
                   }=Payload) ->
    proc_lib:init_ack(Pid, {'ok', self()}),
    InitFun(Payload),
    bind(Node, CallId),
    TRef = erlang:send_after(?SANITY_CHECK_PERIOD, self(), 'sanity_check'),
    State = #state{node=Node
                  ,call_id=CallId
                  ,command_q=queue:new()
                  ,start_time=os:timestamp()
                  ,sanity_check_tref=TRef
                  ,fetch_id=FetchId
                  ,controller_q=ControllerQ
                  ,control_q=ControlQ
                  ,initial_ccvs=CCVs
                  ,is_node_up=true
                  ,is_call_up=true
                  ,control_ctx=Payload
                  },
    call_control_ready(State),
    gen_server:enter_loop(?MODULE, [], State).

%%------------------------------------------------------------------------------
%% @doc Handling call messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call('node', _From, #state{node=Node}=State) ->
    {'reply', Node, State};
handle_call('queue_name', _From, #state{control_q=Q}=State) ->
    {'reply', kapi:encode_pid(Q), State};
handle_call('callid', _From, #state{call_id=CallId}=State) ->
    {'reply', CallId, State};
handle_call('other_legs', _From, #state{other_legs=Legs}=State) ->
    {'reply', Legs, State};
handle_call(_Request, _From, State) ->
    {'reply', {'error', 'not_implemented'}, State}.

%%------------------------------------------------------------------------------
%% @doc Handling cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast('stop', State) ->
    {'stop', 'normal', State};
handle_cast({'update_node', Node}, #state{node=OldNode}=State) ->
    lager:debug("channel has moved from ~s to ~s", [OldNode, Node]),
    {'noreply', State#state{node=Node}};
handle_cast({'dialplan', JObj}, State) ->
    {'noreply', handle_dialplan(JObj, State)};
handle_cast({'fs_nodedown', Node}, #state{node=Node
                                         ,is_node_up='true'
                                         }=State) ->
    lager:debug("lost connection to media node ~s", [Node]),
    TRef = erlang:send_after(?MAX_TIMEOUT_FOR_NODE_RESTART, self(), 'nodedown_restart_exceeded'),
    {'noreply', State#state{is_node_up='false'
                           ,node_down_tref=TRef
                           }};
handle_cast({'fs_nodeup', Node}, #state{node=Node
                                       ,call_id=CallId
                                       ,is_node_up='false'
                                       ,node_down_tref=TRef
                                       }=State) ->
    lager:debug("regained connection to media node ~s", [Node]),
    _ = (catch erlang:cancel_timer(TRef)),
    _ = timer:sleep(100 + rand:uniform(1400)),
    case freeswitch:api(Node, 'uuid_exists', CallId) of
        {'ok', <<"true">>} ->
            {'noreply', force_queue_advance(State#state{is_node_up='true'})};
        _Else ->
            {'noreply', handle_channel_destroyed(State)}
    end;
handle_cast(_, State) ->
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info({'event', CallId, JObj}, #state{call_id=CallId}=State) ->
    handle_event_info(CallId, JObj, State);
handle_info({'event', _CallId, _JObj}, State) ->
    lager:debug("not handling ~s : ~s", [_CallId, kz_api:event_name(_JObj)]),
    {'noreply', State};
handle_info({'call_control', JObj}, State) ->
    handle_call_control(JObj, State),
    {'noreply', State};
handle_info({'kapi', {{<<"callctl">>, _, _}, _, JObj}}, State) ->
    handle_call_control(JObj, State),
    {'noreply', State};
handle_info({'usurp_control', CallId, FetchId, _JObj}, #state{call_id = CallId
                                                             ,fetch_id = FetchId
                                                             } = State) ->
    {'noreply', State};
handle_info({'usurp_control', CallId, _FetchId, _JObj}, #state{call_id = CallId} = State) ->
    lager:debug("the call has been usurped by an external process"),
    {'stop', 'normal', State};
handle_info({'usurp_control', _CallId, _FetchId, _JObj}, State) ->
    {'noreply', State};
handle_info({'force_queue_advance', CallId}, #state{call_id=CallId, current_cmd_uuid='undefined'}=State) ->
    {'noreply', force_queue_advance(State)};
handle_info({'force_queue_advance', CallId}, #state{call_id=CallId
                                                   ,current_cmd_uuid=EventUUID
                                                   ,event_uuids=EventUUIDs
                                                   }=State) ->
    {'noreply', force_queue_advance(State#state{event_uuids=[EventUUID | EventUUIDs]})};
handle_info({'force_queue_advance', _}, State) ->
    {'noreply', State};
handle_info({'forward_queue', CallId}, #state{call_id=CallId}=State) ->
    {'noreply', forward_queue(State)};
handle_info('keep_alive_expired', State) ->
    lager:debug("no new commands received after channel destruction, our job here is done"),
    {'stop', 'normal', State};
handle_info('sanity_check', #state{call_id=CallId}=State) ->
    case ecallmgr_fs_channel:exists(CallId) of
        'true' ->
            lager:debug("listener passed sanity check, call is still up"),
            TRef = erlang:send_after(?SANITY_CHECK_PERIOD, self(), 'sanity_check'),
            {'noreply', State#state{sanity_check_tref=TRef}};
        'false' ->
            lager:debug("call uuid does not exist, executing post-hangup events and terminating"),
            {'noreply', handle_channel_destroyed(State)}
    end;
handle_info('nodedown_restart_exceeded', #state{is_node_up='false'}=State) ->
    lager:debug("we have not received a node up in time, assuming down for good for this call", []),
    {'noreply', handle_channel_destroyed(State)};
handle_info({switch_reply, _}, State) ->
    {'noreply', State};
handle_info({route_resp, _, _}, State) ->
    {'noreply', State};
handle_info(_Msg, State) ->
    lager:debug("unhandled message: ~p", [_Msg]),
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Allows listener to pass options to handlers.
%% @end
%%------------------------------------------------------------------------------
-spec handle_call_control(kz_json:object(), state()) -> gen_server:handle_event_return().
handle_call_control(JObj, _State) ->
    case kz_util:get_event_type(JObj) of
        {<<"call">>, <<"command">>} -> handle_call_command(JObj);
        {<<"conference">>, <<"command">>} -> handle_conference_command(JObj);
        {_Category, _Event} -> lager:debug_unsafe("event ~s : ~s not handled : ~s", [_Category, _Event, kz_json:encode(JObj, ['pretty'])])
    end.

-spec handle_call_command(kz_json:object()) -> 'ok'.
handle_call_command(JObj) ->
    gen_server:cast(self(), {'dialplan', JObj}).

-spec handle_conference_command(kz_json:object()) -> 'ok'.
handle_conference_command(JObj) ->
    gen_server:cast(self(), {'dialplan', JObj}).

%%------------------------------------------------------------------------------
%% @doc This function is called by a `gen_listener' when it is about to
%% terminate. It should be the opposite of `Module:init/1' and do any
%% necessary cleaning up. When it returns, the `gen_listener' terminates
%% with Reason. The return value is ignored.
%%
%% @end
%%------------------------------------------------------------------------------
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, #state{start_time=StartTime, control_ctx=Context}=State) ->
    cancel_timers(State),
    ecallmgr_call_sup:release_context(Context),
    lager:debug("control queue was up for ~p microseconds", [timer:now_diff(os:timestamp(), StartTime)]).

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
-spec call_control_ready(state()) -> 'ok'.
call_control_ready(#state{controller_q='undefined'}) -> 'ok';
call_control_ready(State) ->
    publish_route_win(State),
    publish_usurp(State).

-spec publish_usurp(state()) -> 'ok'.
publish_usurp(#state{call_id=CallId
                    ,fetch_id=FetchId
                    ,node=Node
                    }) ->
    publish_usurp(CallId, FetchId, Node).

-spec publish_usurp(kz_term:ne_binary(), kz_term:ne_binary(), atom()) -> 'ok'.
publish_usurp(CallId, FetchId, Node) ->
    Usurp = [{<<"Call-ID">>, CallId}
            ,{<<"Fetch-ID">>, FetchId}
            ,{<<"Reason">>, <<"Route-Win">>}
            ,{<<"Media-Node">>, kz_term:to_binary(Node)}
             | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
            ],
    lager:debug("sending control usurp for fetch-id ~s(~s)", [FetchId, CallId]),
    kapi_call:publish_usurp_control(CallId, Usurp),
    ecallmgr_usurp_monitor:register('usurp_control', CallId, FetchId).

-spec publish_route_win(state()) -> 'ok'.
publish_route_win(#state{call_id=CallId
                        ,controller_q=ControllerQ
                        ,control_q=Q
                        ,initial_ccvs=CCVs
                        }) ->

    Win = [{<<"Msg-ID">>, CallId}
          ,{<<"Call-ID">>, CallId}
          ,{<<"Control-Queue">>, kapi:encode_pid(Q)}
          ,{<<"Custom-Channel-Vars">>, CCVs}
           | kz_api:default_headers(Q, <<"dialplan">>, <<"route_win">>, ?APP_NAME, ?APP_VERSION)
          ],
    lager:debug("sending route_win to ~s", [ControllerQ]),
    kapi_route:publish_win(ControllerQ, Win).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec handle_channel_destroyed(state()) -> state().
handle_channel_destroyed(#state{sanity_check_tref=SCTRef
                               ,current_app=CurrentApp
                               ,current_cmd=CurrentCmd
                               ,current_cmd_uuid=EventUUID
                               ,call_id=CallId
                               }=State
                        ) ->
    lager:debug("our channel has been destroyed, executing any post-hangup commands"),
    %% if our sanity check timer is running stop it, it will always return false
    %% now that the channel is gone
    catch (erlang:cancel_timer(SCTRef)),

    %% if the current application can not be run without a channel and we have received the
    %% channel_destroy (the last event we will ever receive from freeswitch for this call)
    %% then create an error and force advance. This will happen with dialplan actions that
    %% have not been executed on freeswitch but were already queued (for example in xferext).
    %% Commonly events like masquerade, noop, etc
    _ = case CurrentApp =:= 'undefined'
            andalso EventUUID =:= 'undefined'
        of
            'true' -> 'ok';
            'false' ->
                maybe_send_error_resp(CallId, CurrentCmd),
                self() ! {'force_queue_advance', CallId}
        end,
    State#state{keep_alive_ref=get_keep_alive_ref(State#state{is_call_up='false'})
               ,is_call_up='false'
               ,is_node_up='true'
               }.

-spec force_queue_advance(state()) -> state().
force_queue_advance(#state{call_id=CallId
                          ,current_app=CurrApp
                          ,current_cmd_uuid=CurrUUID
                          ,command_q=CmdQ
                          ,is_node_up=INU
                          ,is_call_up=CallUp
                          }=State) ->
    lager:debug("received control queue unconditional advance, skipping wait for command completion of '~s : ~s'"
               ,[CurrApp, CurrUUID]
               ),
    case INU
        andalso queue:out(CmdQ)
    of
        'false' ->
            %% if the node is down, don't inject the next FS event
            lager:debug("not continuing until the media node becomes available"),
            State#state{current_app='undefined', current_cmd_uuid='undefined'};
        {'empty', _} ->
            lager:debug("no call commands remain queued, hibernating"),
            State#state{current_app='undefined', current_cmd_uuid='undefined'};
        {{'value', Cmd}, CmdQ1} ->
            AppName = kapi_dialplan:application_name(Cmd),
            MsgId = kz_api:msg_id(Cmd),
            case CallUp
                andalso execute_control_request(Cmd, State)
            of
                'false' ->
                    lager:debug("command '~s' is not valid after hangup, skipping", [AppName]),
                    maybe_send_error_resp(CallId, Cmd),
                    self() ! {'force_queue_advance', CallId},
                    State#state{command_q=CmdQ1
                               ,current_app=AppName
                               ,current_cmd_uuid = 'undefined'
                               ,current_cmd=Cmd
                               ,keep_alive_ref=get_keep_alive_ref(State)
                               ,msg_id=MsgId
                               };
                {'error', Error} ->
                    lager:debug("command '~s' returned an error ~p", [AppName, Error]),
                    maybe_send_error_resp(AppName, CallId, Cmd, Error),
                    self() ! {'force_queue_advance', CallId},
                    State#state{command_q=CmdQ1
                               ,current_app=AppName
                               ,current_cmd_uuid = 'undefined'
                               ,current_cmd=Cmd
                               ,keep_alive_ref=get_keep_alive_ref(State)
                               ,msg_id=MsgId
                               };
                'ok' ->
                    lager:debug("command ~s execute complete", [AppName]),
                    State#state{command_q=CmdQ1
                               ,current_app=AppName
                               ,current_cmd_uuid = 'undefined'
                               ,current_cmd=Cmd
                               ,keep_alive_ref=get_keep_alive_ref(State)
                               ,msg_id=MsgId
                               };
                {'ok', EventUUID} ->
                    lager:debug("command ~s queued for completion : ~s", [AppName, EventUUID]),
                    State#state{command_q=CmdQ1
                               ,current_app=AppName
                               ,current_cmd=Cmd
                               ,current_cmd_uuid=EventUUID
                               ,keep_alive_ref=get_keep_alive_ref(State)
                               ,msg_id=MsgId
                               }
            end
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec handle_execute_complete(kz_term:api_binary(), kz_term:api_binary(), kz_json:object(), state()) -> state().
handle_execute_complete(_AppName, <<"null">>, _JObj, State) ->
    lager:debug_unsafe("ignoring ~s completion", [_AppName]),
    State;
handle_execute_complete('undefined', _, _JObj, State) ->
    %% lager:debug_unsafe("call control received undefined : ~s", [kz_json:encode(_JObj, ['pretty'])]),
    State;
handle_execute_complete(_, 'undefined', _JObj, State) ->
    %% lager:debug_unsafe("call control received undefined : ~s", [kz_json:encode(_JObj, ['pretty'])]),
    State;
handle_execute_complete(_AppName, EventUUID, _JObj, #state{current_cmd_uuid='undefined'
                                                          ,event_uuids=EventUUIDs
                                                          }=State) ->
    case lists:member(EventUUID, EventUUIDs) of
        'true' -> State#state{event_uuids=lists:delete(EventUUID, EventUUIDs)};
        'false' ->
            lager:debug("execute complete not handled : ~s:~s", [_AppName, EventUUID]),
            State
    end;
handle_execute_complete(AppName, EventUUID, _, #state{current_app=AppName
                                                     ,current_cmd_uuid=EventUUID
                                                     }=State) ->
    lager:debug("~s execute complete, advancing control queue", [AppName]),
    forward_queue(State);
handle_execute_complete(AppName, EventUUID, JObj, #state{current_app=CurrApp
                                                        ,current_cmd_uuid=EventUUID
                                                        }=State) ->
    RawAppName = kz_json:get_ne_binary_value(<<"Raw-Application-Name">>, JObj, AppName),
    lager:debug("converting app name ~s for ~s", [CurrApp, RawAppName]),
    CurrentAppNames = ecallmgr_util:convert_kazoo_app_name(CurrApp),
    case lists:member(RawAppName, CurrentAppNames) of
        'true' -> handle_execute_complete(CurrApp, EventUUID, JObj, State);
        'false' ->
            lager:warning("couldn't translate the app name ~s for ~s", [CurrApp, RawAppName]),
            State
    end;
handle_execute_complete(_AppName, _EventUUID, _JObj, #state{current_app=_CurrApp
                                                           ,current_cmd_uuid=__EventUUID
                                                           }=State) ->
                                                %    lager:debug_unsafe("call control unhandled exec complete : ~s , ~s => ~s", [_AppName, _EventUUID, kz_json:encode(_JObj, ['pretty'])]),
    State.

-spec forward_queue(state()) -> state().
forward_queue(#state{call_id = CallId
                    ,is_node_up = IsNodeUp
                    ,is_call_up = CallUp
                    ,command_q = CmdQ
                    }=State) ->
    case IsNodeUp
        andalso queue:out(CmdQ)
    of
        'false' ->
            %% if the node is down, don't inject the next FS event
            lager:debug("not continuing until the media node becomes available"),
            State#state{current_app='undefined'
                       ,current_cmd_uuid='undefined'
                       ,msg_id='undefined'
                       };
        {'empty', _} ->
            lager:debug("no call commands remain queued, hibernating"),
            State#state{current_app='undefined'
                       ,current_cmd_uuid='undefined'
                       ,msg_id='undefined'
                       };
        {{'value', Cmd}, CmdQ1} ->
            AppName = kz_json:get_ne_binary_value(<<"Application-Name">>, Cmd),
            MsgId = kz_api:msg_id(Cmd, <<>>),
            case CallUp
                andalso execute_control_request(Cmd, State)
            of
                'false' ->
                    lager:debug("command '~s' is not valid after hangup, skipping", [AppName]),
                    maybe_send_error_resp(CallId, Cmd),
                    self() ! {'force_queue_advance', CallId},
                    State#state{command_q = CmdQ1
                               ,current_app = AppName
                               ,current_cmd_uuid = 'undefined'
                               ,current_cmd = Cmd
                               ,msg_id = MsgId
                               };
                {'error', Error} ->
                    lager:debug("command '~s' returned an error ~p", [AppName, Error]),
                    maybe_send_error_resp(AppName, CallId, Cmd, Error),
                    self() ! {'force_queue_advance', CallId},
                    State#state{command_q=CmdQ1
                               ,current_app=AppName
                               ,current_cmd_uuid = 'undefined'
                               ,current_cmd=Cmd
                               ,keep_alive_ref=get_keep_alive_ref(State)
                               ,msg_id=MsgId
                               };
                'ok' ->
                    lager:debug("command ~s execute complete", [AppName]),
                    State#state{command_q = CmdQ1
                               ,current_app = AppName
                               ,current_cmd_uuid = 'undefined'
                               ,current_cmd = Cmd
                               ,msg_id = MsgId
                               };
                {'ok', EventUUID} ->
                    lager:debug("command ~s queued for completion : ~s", [AppName, EventUUID]),
                    MsgId = kz_api:msg_id(Cmd, <<>>),
                    State#state{command_q = CmdQ1
                               ,current_app = AppName
                               ,current_cmd_uuid = EventUUID
                               ,current_cmd = Cmd
                               ,msg_id = MsgId
                               }
            end
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec handle_sofia_replaced(kz_term:ne_binary(), state()) -> state().
handle_sofia_replaced(<<_/binary>> = CallId, #state{call_id=CallId}=State) ->
    lager:debug("call id hasn't changed, no replacement necessary"),
    State;
handle_sofia_replaced(<<_/binary>> = ReplacedBy, #state{node=Node
                                                       ,call_id=CallId
                                                       ,command_q=CommandQ
                                                       }=State)->
    lager:debug("channel replaced by ~s for ~s in ~s", [ReplacedBy, CallId, Node]),
    unbind(Node, CallId),
    kz_util:put_callid(ReplacedBy),
    bind(Node, ReplacedBy),
    lager:info("...call id updated, continuing post-transfer"),
    set_control_info(ReplacedBy, State),

    Commands = [kz_json:set_value(<<"Call-ID">>, ReplacedBy, JObj)
                || JObj <- queue:to_list(CommandQ)
               ],
    force_queue_advance(State#state{call_id = ReplacedBy
                                   ,command_q=queue:from_list(Commands)
                                   }).

-spec set_control_info(kz_term:ne_binary(), state()) -> 'ok'.
set_control_info(UUID, #state{node=Node
                             ,fetch_id=FetchId
                             ,control_q=ControlQ
                             })->
    Cmd = 'kz_uuid_setvar_multi_encoded',
    Arg = list_to_binary([UUID
                         ," ^^;Call-Control-Queue="
                         ,kapi:encode_pid(ControlQ)
                         ,";Call-Control-PID="
                         ,kz_term:to_binary(self())
                         ,";ecallmgr_Ecallmgr-Node="
                         ,kz_term:to_binary(node())
                         ,";Call-Control-Node="
                         ,kz_term:to_binary(node())
                         ,";"
                         ,?SET_CCV(<<"Fetch-ID">>, FetchId)
                         ]),
    _ = freeswitch:api(Node, Cmd, Arg),
    %% freeswitch:sync_channel(Node, ReplacedBy),
    'ok'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec handle_dialplan(kz_json:object(), state()) -> state().
handle_dialplan(JObj, #state{call_id=CallId
                            ,is_node_up=INU
                            ,is_call_up=CallUp
                            ,command_q=CmdQ
                            ,current_app=CurrApp
                            }=State) ->
    NewCmdQ = try
                  insert_command(State, kz_term:to_atom(kz_json:get_value(<<"Insert-At">>, JObj, 'tail')), JObj)
              catch _T:_R ->
                      lager:debug("failed to insert command into control queue: ~p:~p", [_T, _R]),
                      CmdQ
              end,
    case INU
        andalso (not queue:is_empty(NewCmdQ))
        andalso CurrApp =:= 'undefined'
    of
        'true' ->
            {{'value', Cmd}, NewCmdQ1} = queue:out(NewCmdQ),
            AppName = kapi_dialplan:application_name(Cmd),
            MsgId = kz_api:msg_id(Cmd),
            case CallUp
                andalso execute_control_request(Cmd, State)
            of
                'false' ->
                    lager:debug("command '~s' is not valid after hangup, ignoring", [AppName]),
                    maybe_send_error_resp(CallId, Cmd),
                    self() ! {'force_queue_advance', CallId},
                    State#state{command_q=NewCmdQ1
                               ,current_app=AppName
                               ,current_cmd=Cmd
                               ,keep_alive_ref=get_keep_alive_ref(State)
                               ,msg_id=MsgId
                               };
                {'error', Error} ->
                    lager:debug("command '~s' returned an error ~p", [AppName, Error]),
                    maybe_send_error_resp(AppName, CallId, Cmd, Error),
                    self() ! {'force_queue_advance', CallId},
                    State#state{command_q=NewCmdQ1
                               ,current_app=AppName
                               ,current_cmd=Cmd
                               ,keep_alive_ref=get_keep_alive_ref(State)
                               ,msg_id=MsgId
                               };
                'ok' ->
                    self() ! {'forward_queue', CallId},
                    lager:debug("command ~s execute complete", [AppName]),
                    State#state{command_q=NewCmdQ1
                               ,current_app=AppName
                               ,current_cmd=Cmd
                               ,keep_alive_ref=get_keep_alive_ref(State)
                               ,msg_id=MsgId
                               };
                {'ok', EventUUID} ->
                    lager:debug("command ~s queued for completion : ~s", [AppName, EventUUID]),
                    State#state{command_q=NewCmdQ1
                               ,current_app=AppName
                               ,current_cmd_uuid=EventUUID
                               ,current_cmd=Cmd
                               ,keep_alive_ref=get_keep_alive_ref(State)
                               ,msg_id=MsgId
                               }

            end;
        'false' ->
            State#state{command_q=NewCmdQ
                       ,keep_alive_ref=get_keep_alive_ref(State)
                       }
    end.

%% execute all commands in JObj immediately, irregardless of what is running (if anything).
-spec insert_command(state(), insert_at_options(), kz_json:object()) -> queue:queue().
insert_command(#state{node=Node
                     ,call_id=CallId
                     ,command_q=CommandQ
                     ,is_node_up=IsNodeUp
                     }=State
              ,'now'
              ,JObj
              ) ->
    AName = kapi_dialplan:application_name(JObj),
    case IsNodeUp
        andalso AName
    of
        'false' ->
            lager:debug("node ~s is not available", [Node]),
            lager:debug("sending execution error for command ~s", [AName]),
            {Mega,Sec,Micro} = os:timestamp(),
            Props = [{<<"Event-Name">>, <<"CHANNEL_EXECUTE_ERROR">>}
                    ,{<<"Event-Date-Timestamp">>, ((Mega * 1000000 + Sec) * 1000000 + Micro)}
                    ,{<<"Call-ID">>, CallId}
                    ,{<<"Channel-Call-State">>, <<"ERROR">>}
                    ,{<<"Custom-Channel-Vars">>, JObj}
                    ,{<<"Msg-ID">>, kz_api:msg_id(JObj)}
                    ,{<<"Request">>, JObj}
                     | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
                    ],
            kapi_call:publish_event(Props),
            CommandQ;
        <<"queue">> ->
            'true' = kapi_dialplan:queue_v(JObj),
            Commands = kz_json:get_list_value(<<"Commands">>, JObj, []),
            DefJObj = kz_json:from_list(kz_api:extract_defaults(JObj)),
            _ = execute_queue_commands(Commands, DefJObj, State),
            CommandQ;
        <<"noop">> ->
            _ = execute_control_request(JObj, State),
            maybe_filter_queue(kz_json:get_value(<<"Filter-Applications">>, JObj), CommandQ);
        _ ->
            lager:debug("recv and executing ~s now!", [AName]),
            _ = execute_control_request(JObj, State),
            CommandQ
    end;
insert_command(#state{node=Node, call_id=CallId}, 'flush', JObj) ->
    lager:debug("received control queue flush command, clearing all waiting commands"),
    _ = freeswitch:api(Node, 'uuid_break', <<CallId/binary, " all">>),
    self() ! {'force_queue_advance', CallId},
    insert_command_into_queue(queue:new(), 'tail', JObj);
insert_command(#state{command_q=CommandQ}, 'head', JObj) ->
    insert_command_into_queue(CommandQ, 'head', JObj);
insert_command(#state{command_q=CommandQ}, 'tail', JObj) ->
    insert_command_into_queue(CommandQ, 'tail', JObj);
insert_command(Q, Pos, _) ->
    lager:debug("received command for an unknown queue position: ~p", [Pos]),
    Q.

execute_queue_commands([], _, _) -> 'ok';
execute_queue_commands([Command|Commands], DefJObj, State) ->
    case kz_json:is_empty(Command)
        orelse 'undefined' =:= kapi_dialplan:application_name(Command)
    of
        'true' -> execute_queue_commands(Commands, DefJObj, State);
        'false' ->
            JObj = kz_json:merge_jobjs(Command, DefJObj),
            'true' = kapi_dialplan:v(JObj),
            _Ugly = insert_command(State, 'now', JObj),
            execute_queue_commands(Commands, DefJObj, State)
    end.

-spec insert_command_into_queue(queue:queue(), 'tail' | 'head', kz_json:object()) -> queue:queue().
insert_command_into_queue(Q, Position, JObj) ->
    InsertFun = queue_insert_fun(Position),
    case kapi_dialplan:application_name(JObj) of
        <<"queue">> -> %% list of commands that need to be added
            insert_queue_command_into_queue(InsertFun, Q, JObj);
        _Else -> InsertFun(JObj, Q)
    end.

-spec insert_queue_command_into_queue(function(), queue:queue(), kz_json:object()) -> queue:queue().
insert_queue_command_into_queue(InsertFun, CommandQueue, JObj) ->
    'true' = kapi_dialplan:queue_v(JObj),
    DefJObj = kz_json:from_list(kz_api:extract_defaults(JObj)),
    lists:foldr(fun(CmdJObj, TmpQueue) ->
                        AppCmd = kz_json:merge_jobjs(CmdJObj, DefJObj),
                        InsertFun(AppCmd, TmpQueue)
                end
               ,CommandQueue
               ,kz_json:get_list_value(<<"Commands">>, JObj)
               ).

-type insert_fun() :: fun((kz_json:object(), queue:queue()) -> queue:queue()).

-spec queue_insert_fun('tail' | 'head') -> insert_fun().
queue_insert_fun('tail') ->
    queue_insert_fun('tail', fun queue:in/2);

queue_insert_fun('head') ->
    queue_insert_fun('head', fun queue:in_r/2).

-spec queue_insert_fun('tail' | 'head', insert_fun()) -> insert_fun().
queue_insert_fun(Position, QueueFun) when is_function(QueueFun, 2) ->
    fun(JObj, Queue) ->
            'true' = kapi_dialplan:v(JObj),
            case kapi_dialplan:application_name(JObj) of
                'undefined' -> Queue;
                <<"noop">> = AppName ->
                    MsgId = kz_api:msg_id(JObj),
                    lager:debug("inserting at the head of the control queue call command ~s(~s)", [AppName, MsgId]),
                    QueueFun(JObj, Queue);
                AppName ->
                    lager:debug("inserting at the ~p of the control queue call command ~s"
                               ,[Position, AppName]
                               ),
                    QueueFun(JObj, Queue)
            end
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
%% See Noop documentation for Filter-Applications to get an idea of this function's purpose
-spec maybe_filter_queue(kz_term:api_objects(), queue:queue()) -> queue:queue().
maybe_filter_queue('undefined', CommandQ) -> CommandQ;
maybe_filter_queue([], CommandQ) -> CommandQ;
maybe_filter_queue([AppName|T]=Apps, CommandQ) when is_binary(AppName) ->
    case queue:out(CommandQ) of
        {'empty', _} -> CommandQ;
        {{'value', NextJObj}, CommandQ1} ->
            case AppName =:= kapi_dialplan:application_name(NextJObj) of
                'false' -> maybe_filter_queue(T, CommandQ);
                'true' ->
                    lager:debug("app ~s matched next command, popping off", [AppName]),
                    maybe_filter_queue(Apps, CommandQ1)
            end
    end;
maybe_filter_queue([AppJObj|T]=Apps, CommandQ) ->
    case queue:out(CommandQ) of
        {'empty', _} -> CommandQ;
        {{'value', NextJObj}, CommandQ1} ->
            case (NextAppName = kapi_dialplan:application_name(NextJObj))
                =:= (AppName = kapi_dialplan:application_name(AppJObj))
                orelse kz_json:get_ne_binary_value(<<"Group-ID">>, NextJObj)
                =:= kz_json:get_ne_binary_value(<<"Group-ID">>, AppJObj, <<"nomatch">>)
            of
                'false' -> maybe_filter_queue(T, CommandQ);
                'true' ->
                    lager:debug("app ~s matched next command ~s, checking fields"
                               ,[AppName, NextAppName]
                               ),
                    Fields = kz_json:get_json_value(<<"Fields">>, AppJObj, kz_json:new()),
                    lager:debug("fields: ~p", [Fields]),

                    case kz_json:all(fun({AppField, AppValue}) ->
                                             kz_json:get_value(AppField, NextJObj) =:= AppValue
                                     end
                                    ,Fields
                                    )
                    of
                        'false' -> maybe_filter_queue(T, CommandQ);
                        'true' ->
                            lager:debug("all fields matched queued command, popping it off"),
                            maybe_filter_queue(Apps, CommandQ1) % same app and all fields matched
                    end
            end
    end.

-spec get_module(kz_term:ne_binary(), kz_term:ne_binary()) -> atom().
get_module(Category, Name) ->
    ModuleName = <<"ecallmgr_", Category/binary, "_", Name/binary>>,
    try kz_term:to_atom(ModuleName)
    catch
        'error':'badarg' ->
            kz_term:to_atom(ModuleName, 'true')
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec execute_control_request(kz_json:object(), state()) -> 'ok' | {'ok', kz_term:ne_binary()} | {'error', any()}.
execute_control_request(Cmd, #state{node=Node
                                   ,call_id=CallId
                                   ,other_legs=OtherLegs
                                   }) ->
    kz_util:put_callid(CallId),
    Srv = self(),

    Application = kapi_dialplan:application_name(Cmd),

    lager:debug("executing call command '~s' ~s", [Application, kz_api:msg_id(Cmd)]),

    Mod = get_module(kz_api:event_category(Cmd), kz_api:event_name(Cmd)),

    CmdLeg = kz_api:call_id(Cmd),
    CallLeg = which_call_leg(CmdLeg, OtherLegs, CallId),

    try Mod:exec_cmd(Node, CallLeg, Cmd, self())
    catch
        'throw':{'error', 'baduuid'}:_ ->
            lager:debug("unable to execute command, baduuid"),
            Msg = list_to_binary(["Session ", CallId
                                 ," not found for ", Application
                                 ]),
            send_error_resp(CallId, Cmd, Msg),
            Srv ! {'force_queue_advance', CallId},
            'ok';
        _:{'error', 'nosession'}:_ ->
            lager:debug("unable to execute command, no session"),
            Msg = list_to_binary(["Session ", CallId
                                 ," not found for ", Application
                                 ]),
            send_error_resp(CallId, Cmd, Msg),
            Srv ! {'force_queue_advance', CallId},
            'ok';
        'error':{'badmatch', {'error', 'nosession'}}:_ ->
            lager:debug("unable to execute command, no session"),
            Msg = list_to_binary(["Session ", CallId
                                 ," not found for ", Application
                                 ]),
            send_error_resp(CallId, Cmd, Msg),
            Srv ! {'force_queue_advance', CallId},
            'ok';
        'error':{'badmatch', {'error', ErrMsg}}:ST ->
            lager:debug("invalid command ~s: ~p", [Application, ErrMsg]),
            kz_util:log_stacktrace(ST),
            maybe_send_error_resp(CallId, Cmd),
            Srv ! {'force_queue_advance', CallId},
            'ok';
        'throw':{'msg', ErrMsg} ->
            lager:debug("error while executing command ~s: ~s", [Application, ErrMsg]),
            send_error_resp(CallId, Cmd),
            Srv ! {'force_queue_advance', CallId},
            'ok';
        'throw':Msg ->
            lager:debug("failed to execute ~s: ~s", [Application, Msg]),
            lager:debug("only handling call id(s): ~p", [[CallId | OtherLegs]]),

            send_error_resp(CallId, Cmd, Msg),
            Srv ! {'force_queue_advance', CallId},
            'ok';
        _A:_B:ST ->
            lager:debug("exception (~s) while executing ~s: ~p", [_A, Application, _B]),
            kz_util:log_stacktrace(ST),
            send_error_resp(CallId, Cmd),
            Srv ! {'force_queue_advance', CallId},
            'ok'
    end.

-spec which_call_leg(kz_term:ne_binary(), kz_term:ne_binaries(), kz_term:ne_binary()) -> kz_term:ne_binary().
which_call_leg(CallId, _OtherLegs, CallId) -> CallId;
which_call_leg(CmdLeg, OtherLegs, CallId) ->
    case lists:member(CmdLeg, OtherLegs) of
        'true' ->
            lager:debug("executing against ~s instead", [CmdLeg]),
            CmdLeg;
        'false' ->
            case ecallmgr_fs_channel:fetch(CmdLeg, 'record') of
                {'ok', #channel{other_leg=CallId}} -> CmdLeg;
                _Else -> CallId
            end
    end.

-spec maybe_send_error_resp(kz_term:ne_binary(), kz_json:object()) -> 'ok'.
maybe_send_error_resp(CallId, Cmd) ->
    AppName = kapi_dialplan:application_name(Cmd),
    Msg = <<"Could not execute dialplan action: ", AppName/binary>>,
    maybe_send_error_resp(AppName, CallId, Cmd, Msg).

-spec maybe_send_error_resp(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object(), kz_term:ne_binary()) -> 'ok'.
maybe_send_error_resp(<<"hangup">>, _CallId, _Cmd, _Msg) -> 'ok';
maybe_send_error_resp(_, CallId, Cmd, Msg) -> send_error_resp(CallId, Cmd, Msg).

-spec send_error_resp(kz_term:ne_binary(), kz_json:object()) -> 'ok'.
send_error_resp(CallId, Cmd) ->
    AppName = kapi_dialplan:application_name(Cmd),
    Msg = <<"Could not execute dialplan action: ", AppName/binary>>,
    send_error_resp(CallId, Cmd, Msg).

-spec send_error_resp(kz_term:ne_binary(), kz_json:object(), kz_term:ne_binary()) -> 'ok'.
send_error_resp(CallId, Cmd, Msg) ->
    case ecallmgr_fs_channel:fetch(CallId) of
        {'ok', Channel} -> send_error_resp(CallId, Cmd, Msg, Channel);
        {'error', 'not_found'} -> send_error_resp(CallId, Cmd, Msg, 'undefined')
    end.

-spec send_error_resp(kz_term:ne_binary(), kz_json:object(), kz_term:ne_binary(), kz_term:api_object()) -> 'ok'.
send_error_resp(CallId, Cmd, Msg, _Channel) ->
    Resp = [{<<"Msg-ID">>, kz_api:msg_id(Cmd)}
           ,{<<"Error-Message">>, Msg}
           ,{<<"Request">>, Cmd}
           ,{<<"Call-ID">>, CallId}
            | kz_api:default_headers(<<>>, <<"error">>, <<"dialplan">>, ?APP_NAME, ?APP_VERSION)
           ],
    lager:debug("sending execution error: ~p", [Resp]),
    kapi_dialplan:publish_error(CallId, Resp).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get_keep_alive_ref(state()) -> kz_term:api_reference().
get_keep_alive_ref(#state{is_call_up='true'}) -> 'undefined';
get_keep_alive_ref(#state{keep_alive_ref='undefined'
                         ,is_call_up='false'
                         }) ->
    lager:debug("started post hangup keep alive timer for ~bms", [?KEEP_ALIVE]),
    erlang:send_after(?KEEP_ALIVE, self(), 'keep_alive_expired');
get_keep_alive_ref(#state{keep_alive_ref=TRef
                         ,is_call_up='false'
                         }) ->
    _ = case erlang:cancel_timer(TRef) of
            'false' -> 'ok';
            _ -> %% flush the receive buffer of expiration messages
                receive 'keep_alive_expired' -> 'ok'
                after 0 -> 'ok' end
        end,
    lager:debug("reset post hangup keep alive timer"),
    erlang:send_after(?KEEP_ALIVE, self(), 'keep_alive_expired').

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec bind(atom(), kz_term:ne_binary()) -> 'true'.
bind(Node, CallId) ->
    lager:debug("binding to call ~s events on node ~s", [CallId, Node]),
    'true' = gproc:reg({'p', 'l', {'call_control', CallId}}),
    'true' = gproc:reg({'p', 'l', {'call_event', Node, CallId}}).

-spec unbind(atom(), kz_term:ne_binary()) -> 'true'.
unbind(Node, CallId) ->
    lager:debug("unbinding from call ~s events on node ~s", [CallId, Node]),
    _ = (catch gproc:unreg({'p', 'l', {'call_control', CallId}})),
    _ = (catch gproc:unreg({'p', 'l', {'call_event', Node, CallId}})),
    'true'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec handle_replaced(kz_json:object(), state()) ->
                             {'noreply', state()}.
handle_replaced(JObj, #state{fetch_id=FetchId
                            ,node=_Node
                            ,call_id=_CallId
                            }=State) ->
    case kz_call_event:custom_channel_var(JObj, <<"Fetch-ID">>) of
        FetchId ->
            ReplacedBy = kz_json:get_ne_binary_value(<<"Replaced-By">>, JObj),
            case ecallmgr_fs_channel:fetch(ReplacedBy) of
                {'ok', _Channel} ->
                    {'noreply', handle_sofia_replaced(ReplacedBy, State)};
                _Else ->
                    lager:debug("channel replaced was not handled : ~p", [_Else]),
                    {'noreply', State}
            end;
        _Else ->
            lager:info("sofia replaced on our channel but different fetch id~n"),
            {'noreply', State}
    end.

-spec handle_direct(kz_json:object(), state()) ->
                           {'noreply', state()}.
handle_direct(JObj, #state{fetch_id=FetchId
                          ,node=_Node
                          ,call_id=_CallId
                          }=State) ->
    case kz_call_event:custom_channel_var(JObj, <<"Fetch-ID">>) of
        FetchId ->
            ReplacedBy = kz_json:get_ne_binary_value(<<"Connecting-Leg-A-UUID">>, JObj),
            case ecallmgr_fs_channel:fetch(ReplacedBy) of
                {'ok', _Channel} ->
                    {'noreply', handle_sofia_replaced(ReplacedBy, State)};
                _Else ->
                    lager:debug("channel replaced was not handled : ~p", [_Else]),
                    {'noreply', State}
            end;
        _Else ->
            lager:info("sofia replaced on our channel but different fetch id~n"),
            {'noreply', State}
    end.

-spec handle_sync(kz_json:object(), state()) ->
                         {'noreply', state()}.
handle_sync(JObj, #state{fetch_id=FetchId
                        ,node=_Node
                        ,call_id=_CallId
                        }=State) ->
    case kz_call_event:custom_channel_var(JObj, <<"Fetch-ID">>) of
        FetchId -> {'noreply', State};
        New -> {'noreply', State#state{fetch_id=New}}
    end.

-spec handle_transferee(kz_json:object(), state()) ->
                               {'noreply', state()}.
handle_transferee(JObj, #state{fetch_id=FetchId
                              ,node=_Node
                              ,call_id=CallId
                              }=State) ->
    case kz_call_event:custom_channel_var(JObj, <<"Fetch-ID">>) of
        FetchId ->
            lager:info("we (~s) have been transferred, terminate immediately", [CallId]),
            {'stop', 'normal', State};
        _Else ->
            lager:info("we were a different instance of this transferred call ~s : ~s", [FetchId, _Else]),
            {'noreply', State}
    end.

-spec handle_event_info(kz_term:ne_binary(), kz_evt_freeswitch:data(), state()) ->
                               {'noreply', state()} |
                               {'stop', any(), state()}.
handle_event_info(CallId, JObj, #state{call_id=CallId}=State) ->
    Application = kz_call_event:application_name(JObj),
    case kz_call_event:event_name(JObj) of
        <<"CHANNEL_EXECUTE_COMPLETE">> ->
            {'noreply', handle_execute_complete(Application, kz_call_event:application_uuid(JObj), JObj, State)};
        <<"CHANNEL_DESTROY">> ->
            {'noreply', handle_channel_destroyed(State)};
        <<"CHANNEL_TRANSFEREE">> ->
            handle_transferee(JObj, State);
        <<"CHANNEL_REPLACED">> ->
            handle_replaced(JObj, State);
        <<"CHANNEL_DIRECT">> ->
            handle_direct(JObj, State);
        <<"CHANNEL_EXECUTE">> when Application =:= <<"redirect">> ->
            {'stop', 'normal', State};
        <<"CHANNEL_SYNC">> ->
            handle_sync(JObj, State);
        _Else ->
            {'noreply', State}
    end.

-spec cancel_timers(state()) -> any().
cancel_timers(#state{sanity_check_tref=SCTRef
                    ,keep_alive_ref=KATRef
                    }) ->
    catch (erlang:cancel_timer(SCTRef)),
    catch (erlang:cancel_timer(KATRef)).
