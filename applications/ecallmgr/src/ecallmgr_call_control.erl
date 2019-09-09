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
-behaviour(gen_listener).

%% API
-export([start_link/5, stop/1]).
-export([queue_name/1]).
-export([callid/1]).
-export([node/1]).
-export([hostname/1]).
-export([event_execute_complete/3]).
-export([other_legs/1
        ,update_node/2
        ,control_procs/1
        ,publish_usurp/3
        ]).
-export([fs_nodeup/2]).
-export([fs_nodedown/2]).

%% gen_listener callbacks
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,handle_event/2
        ,terminate/2
        ,code_change/3
        ]).

-include("ecallmgr.hrl").

-define(SERVER, ?MODULE).

-define(KEEP_ALIVE, 2 * ?MILLISECONDS_IN_SECOND).

-type insert_at_options() :: 'now' | 'head' | 'tail' | 'flush'.

-record(state, {node :: atom()
               ,call_id :: kz_term:ne_binary()
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
               ,control_q :: kz_term:api_ne_binary()
               ,initial_ccvs :: kz_term:api_object()
               ,node_down_tref :: kz_term:api_reference()
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
%% @doc Starts the server.
%% @end
%%------------------------------------------------------------------------------
-spec start_link(atom(), kz_term:ne_binary(), kz_term:api_ne_binary(), kz_term:api_ne_binary(), kz_json:object()) ->
                        kz_types:startlink_ret().
start_link(Node, CallId, FetchId, ControllerQ, CCVs) ->
    %% We need to become completely decoupled from ecallmgr_call_events
    %% because the call_events process might have been spun up with A->B
    %% then transferred to A->D, but the route landed in a different
    %% ecallmgr.  Since our call_events will get a bad session if we
    %% try to handle call more than once on a UUID we had to leave the
    %% call_events running on another ecallmgr... fun fun
    Bindings = [{'dialplan', []}
               ,{'self', []}
               ],
    gen_listener:start_link(?SERVER, [{'responders', ?RESPONDERS}
                                     ,{'bindings', Bindings}
                                     ,{'queue_name', ?QUEUE_NAME}
                                     ,{'queue_options', ?QUEUE_OPTIONS}
                                     ,{'consume_options', ?CONSUME_OPTIONS}
                                     ]
                           ,[Node, CallId, FetchId, ControllerQ, CCVs]
                           ).

-spec stop(pid()) -> 'ok'.
stop(Srv) ->
    gen_listener:cast(Srv, 'stop').

-spec callid(pid()) -> kz_term:ne_binary().
callid(Srv) ->
    gen_listener:call(Srv, 'callid', ?MILLISECONDS_IN_SECOND).

-spec node(pid()) -> kz_term:ne_binary().
node(Srv) ->
    gen_listener:call(Srv, 'node', ?MILLISECONDS_IN_SECOND).

-spec hostname(pid()) -> binary().
hostname(Srv) ->
    Node = ?MODULE:node(Srv),
    [_, Hostname] = binary:split(kz_term:to_binary(Node), <<"@">>),
    Hostname.

-spec queue_name(kz_term:api_pid()) -> kz_term:api_ne_binary().
queue_name('undefined') -> 'undefined';
queue_name(Srv) when is_pid(Srv) -> gen_listener:queue_name(Srv).

-spec other_legs(pid()) -> kz_term:ne_binaries().
other_legs(Srv) ->
    gen_listener:call(Srv, 'other_legs', ?MILLISECONDS_IN_SECOND).

-spec event_execute_complete(kz_term:api_pid(), kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
event_execute_complete('undefined', _CallId, _App) -> 'ok';
event_execute_complete(Srv, CallId, App) ->
    gen_listener:cast(Srv, {'event_execute_complete', CallId, App, kz_json:new()}).

-spec update_node(atom(), kz_term:ne_binary() | kz_term:pids()) -> 'ok'.
update_node(Node, CallId) when is_binary(CallId) ->
    update_node(Node, gproc:lookup_pids({'p', 'l', {'call_control', CallId}}));
update_node(Node, Pids) when is_list(Pids) ->
    _ = [gen_listener:cast(Srv, {'update_node', Node}) || Srv <- Pids],
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
%%% gen_listener callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server.
%% @end
%%------------------------------------------------------------------------------
-spec init([atom() | kz_term:ne_binary() | kz_json:object()]) -> {'ok', state()}.
init([Node, CallId, FetchId, ControllerQ, CCVs]) ->
    kz_util:put_callid(CallId),
    lager:debug("starting call control listener"),
    gen_listener:cast(self(), 'init'),

    _ = bind_to_events(Node, CallId),

    _ = reg_for_call_related_events(CallId),

    {'ok', #state{node=Node
                 ,call_id=CallId
                 ,command_q=queue:new()
                 ,start_time=os:timestamp()
                 ,fetch_id=FetchId
                 ,controller_q=ControllerQ
                 ,initial_ccvs=CCVs
                 }}.

%%------------------------------------------------------------------------------
%% @doc Handling call messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call('node', _From, #state{node=Node}=State) ->
    {'reply', Node, State};
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
handle_cast('init', State) ->
    TRef = erlang:send_after(?SANITY_CHECK_PERIOD, self(), 'sanity_check'),
    {'noreply', State#state{sanity_check_tref=TRef}};
handle_cast('stop', State) ->
    {'stop', 'normal', State};
handle_cast({'update_node', Node}, #state{node=OldNode}=State) ->
    lager:debug("channel has moved from ~s to ~s", [OldNode, Node]),
    {'noreply', State#state{node=Node}};
handle_cast({'dialplan', JObj}, State) ->
    {'noreply', handle_dialplan(JObj, State)};
handle_cast({'event_execute_complete', CallId, AppName, JObj}
           ,#state{call_id=CallId}=State
           ) ->
    {'noreply', handle_execute_complete(AppName, JObj, State)};
handle_cast({'event_execute_complete', _, _, _}, State) ->
    {'noreply', State};
handle_cast({'gen_listener', {'created_queue', Q}}, State) ->
    {'noreply', State#state{control_q=Q}};
handle_cast({'gen_listener', {'is_consuming', _IsConsuming}}, State) ->
    call_control_ready(State),
    {'noreply', State};
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
            {'noreply', force_queue_advance(State#state{is_node_up='true'}, kz_json:new())};
        _Else ->
            {'noreply', handle_channel_destroyed(empty_channel_destroyed_object(), State)}
    end;
handle_cast(_, State) ->
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info({'event', [CallId | Props]}, #state{call_id=CallId}=State) ->
    handle_event_info(CallId, Props, State);
handle_info({'event', [CallId | Props]}, State) ->
    handle_other_event_info(CallId, Props, State);
handle_info({'force_queue_advance', CallId, JObj}, #state{call_id=CallId}=State) ->
    {'noreply', force_queue_advance(State, JObj)};
handle_info({'force_queue_advance', _, _}, State) ->
    {'noreply', State};
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
            {'noreply', handle_channel_destroyed(empty_channel_destroyed_object(), State)}
    end;
handle_info(?CHANNEL_MOVE_COMPLETE_MSG(Node, UUID, _Evt), State) ->
    lager:debug("channel move complete recv for node ~s:~s", [Node, UUID]),
    {'noreply', State};
handle_info('nodedown_restart_exceeded', #state{is_node_up='false'}=State) ->
    lager:debug("we have not received a node up in time, assuming down for good for this call", []),
    {'noreply', handle_channel_destroyed(empty_channel_destroyed_object(), State)};
handle_info(?LOOPBACK_BOWOUT_MSG(Node, Props), #state{call_id=ResigningUUID
                                                     ,node=Node
                                                     }=State) ->
    case {props:get_value(?RESIGNING_UUID, Props)
         ,props:get_value(?ACQUIRED_UUID, Props)
         }
    of
        {ResigningUUID, ResigningUUID} ->
            lager:debug("call id after bowout remains the same"),
            {'noreply', State};
        {ResigningUUID, AcquiringUUID} ->
            lager:debug("replacing ~s with ~s", [ResigningUUID, AcquiringUUID]),
            {'noreply', handle_sofia_replaced(AcquiringUUID, State)};
        {_UUID, _AcuiringUUID} ->
            lager:debug("ignoring bowout for ~s", [_UUID]),
            {'noreply', State}
    end;
handle_info({'usurp_control', CallId, FetchId, _JObj}, #state{call_id = CallId
                                                             ,fetch_id = FetchId
                                                             } = State) ->
    {'noreply', State};
handle_info({'usurp_control', CallId, _FetchId, _JObj}, #state{call_id = CallId} = State) ->
    lager:debug("the call has been usurped by an external process"),
    {'stop', 'normal', State};
handle_info({'usurp_control', _CallId, _FetchId, _JObj}, State) ->
    {'noreply', State};
handle_info(_Msg, State) ->
    lager:debug("unhandled message: ~p", [_Msg]),
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Allows listener to pass options to handlers.
%% @end
%%------------------------------------------------------------------------------
-spec handle_event(kz_json:object(), state()) -> gen_listener:handle_event_return().
handle_event(JObj, _State) ->
    _ = case kz_util:get_event_type(JObj) of
            {<<"call">>, <<"command">>} -> handle_call_command(JObj);
            {<<"conference">>, <<"command">>} -> handle_conference_command(JObj);
            {<<"error">>, _EvtName} -> lager:debug("ignoring error event for ~s", [_EvtName]);
            {_Cat, _Name} -> lager:debug("ignoring ~s: ~s", [_Cat, _Name])
        end,
    'ignore'.

-spec handle_call_command(kz_json:object()) -> 'ok'.
handle_call_command(JObj) ->
    gen_listener:cast(self(), {'dialplan', JObj}).

-spec handle_conference_command(kz_json:object()) -> 'ok'.
handle_conference_command(JObj) ->
    gen_listener:cast(self(), {'dialplan', JObj}).

%%------------------------------------------------------------------------------
%% @doc This function is called by a `gen_listener' when it is about to
%% terminate. It should be the opposite of `Module:init/1' and do any
%% necessary cleaning up. When it returns, the `gen_listener' terminates
%% with Reason. The return value is ignored.
%%
%% @end
%%------------------------------------------------------------------------------
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, #state{start_time=StartTime
                         ,sanity_check_tref=SCTRef
                         ,keep_alive_ref=KATRef
                         }) ->
    catch (erlang:cancel_timer(SCTRef)),
    catch (erlang:cancel_timer(KATRef)),
    lager:debug("control queue was up for ~p microseconds", [timer:now_diff(os:timestamp(), StartTime)]),
    'ok'.

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
          ,{<<"Control-Queue">>, Q}
          ,{<<"Custom-Channel-Vars">>, CCVs}
           | kz_api:default_headers(Q, <<"dialplan">>, <<"route_win">>, ?APP_NAME, ?APP_VERSION)
          ],
    lager:debug("sending route_win to ~s", [ControllerQ]),
    kapi_route:publish_win(ControllerQ, Win).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec handle_channel_destroyed(kz_json:object(), state()) -> state().
handle_channel_destroyed(JObj, State) ->
    case kz_json:is_true(<<"Channel-Is-Loopback">>, JObj, 'false') of
        'false' -> do_handle_channel_destroyed(JObj, State);
        'true' -> handle_loopback_destroyed(JObj, State)
    end.

-spec empty_channel_destroyed_object() -> kz_json:object().
empty_channel_destroyed_object() ->
    kz_json:from_list(
      [{<<"Hangup-Cause">>, <<"UNSPECIFIED">>}
      ,{<<"Hangup-Code">>, <<"sip:600">>}
      ]).

-spec handle_loopback_destroyed(kz_json:object(), state()) -> state().
handle_loopback_destroyed(JObj, State) ->
    case {kz_call_event:hangup_cause(JObj)
         ,kz_json:is_true(<<"Channel-Loopback-Bowout-Execute">>, JObj)
         }
    of
        {<<"NORMAL_UNSPECIFIED">>, 'true'} ->
            lager:debug("our loopback has ended but we may not have recv the bowout"),
            State;
        {_Cause, _Bowout} ->
            lager:debug("our loopback has ended with ~s(bowout ~s); treating as done"
                       ,[_Cause, _Bowout]
                       ),
            do_handle_channel_destroyed(JObj, State)
    end.

-spec do_handle_channel_destroyed(kz_json:object(), state()) -> state().
do_handle_channel_destroyed(JObj, #state{sanity_check_tref=SCTRef
                                        ,current_app=CurrentApp
                                        ,current_cmd=CurrentCmd
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
            orelse is_post_hangup_command(CurrentApp)
        of
            'true' -> 'ok';
            'false' ->
                maybe_send_error_resp(JObj, CallId, CurrentCmd),
                self() ! {'force_queue_advance', CallId, JObj}
        end,
    State#state{keep_alive_ref=get_keep_alive_ref(State#state{is_call_up='false'})
               ,is_call_up='false'
               ,is_node_up='true'
               }.

-spec force_queue_advance(state(), kz_json:object()) -> state().
force_queue_advance(#state{call_id=CallId
                          ,current_app=CurrApp
                          ,command_q=CmdQ
                          ,is_node_up=INU
                          ,is_call_up=CallUp
                          }=State
                   ,JObj) ->
    lager:debug("received control queue unconditional advance, skipping wait for command completion of '~s'"
               ,[CurrApp]
               ),
    case INU
        andalso queue:out(CmdQ)
    of
        'false' ->
            %% if the node is down, don't inject the next FS event
            lager:debug("not continuing until the media node becomes available"),
            State#state{current_app='undefined'};
        {'empty', _} ->
            lager:debug("no call commands remain queued, hibernating"),
            State#state{current_app='undefined'};
        {{'value', Cmd}, CmdQ1} ->
            AppName = kapi_dialplan:application_name(Cmd),
            _ = case CallUp
                    orelse is_post_hangup_command(AppName)
                of
                    'true' ->
                        execute_control_request(Cmd, State);
                    'false' ->
                        lager:debug("command '~s' is not valid after hangup, skipping", [AppName]),
                        maybe_send_error_resp(JObj, CallId, Cmd),
                        self() ! {'force_queue_advance', CallId, JObj}
                end,
            MsgId = kz_api:msg_id(Cmd),
            State#state{command_q=CmdQ1
                       ,current_app=AppName
                       ,current_cmd=Cmd
                       ,keep_alive_ref=get_keep_alive_ref(State)
                       ,msg_id=MsgId
                       }
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec handle_execute_complete(kz_term:api_binary(), kz_json:object(), state()) -> state().
handle_execute_complete('undefined', _, State) -> State;
handle_execute_complete(<<"noop">>, JObj, #state{msg_id=CurrMsgId}=State) ->
    case kz_json:get_ne_binary_value(<<"Application-Response">>, JObj) of
        CurrMsgId ->
            lager:debug("noop execution complete for ~s, advancing control queue", [CurrMsgId]),
            forward_queue(JObj, State);
        _NoopId ->
            lager:debug("received noop execute complete with incorrect id ~s (expecting ~s)"
                       ,[_NoopId, CurrMsgId]
                       ),
            State
    end;
handle_execute_complete(<<"playback">> = AppName, JObj, #state{current_app=AppName}=State) ->
    handle_playback_complete(AppName, JObj, State);
handle_execute_complete(<<"play">> = AppName, JObj, #state{current_app=AppName}=State) ->
    handle_playback_complete(AppName, JObj, State);
handle_execute_complete(<<"tts">> = AppName, JObj, #state{current_app=AppName}=State) ->
    handle_playback_complete(AppName, JObj, State);
handle_execute_complete(AppName, JObj, #state{current_app=AppName}=State) ->
    lager:debug("~s execute complete, advancing control queue", [AppName]),
    forward_queue(JObj, State);
handle_execute_complete(AppName, JObj, #state{current_app=CurrApp}=State) ->
    RawAppName = kz_json:get_value(<<"Raw-Application-Name">>, JObj, AppName),
    MappedNames = ecallmgr_util:convert_kazoo_app_name(CurrApp),

    case lists:member(RawAppName, MappedNames) of
        'true' -> handle_execute_complete(CurrApp, JObj, State);
        'false' -> State
    end.

-spec handle_playback_complete(kz_term:ne_binary(), kz_json:object(), state()) -> state().
handle_playback_complete(AppName, JObj, #state{command_q=CmdQ}=State) ->
    lager:debug("~s finished, checking for group-id/DTMF termination", [AppName]),
    case kz_json:get_ne_binary_value(<<"DTMF-Digit">>, JObj) of
        'undefined' -> handle_playback_looping(JObj, State);
        _DTMF ->
            GroupId = kz_json:get_ne_binary_value(<<"Group-ID">>, JObj),
            lager:debug("DTMF ~s terminated playback, flushing all with group id ~s"
                       ,[_DTMF, GroupId]
                       ),
            forward_queue(JObj, State#state{command_q=flush_group_id(CmdQ, GroupId, AppName)})
    end.

-spec handle_playback_looping(kz_json:object(), state()) -> state().
handle_playback_looping(JObj, #state{current_cmd=AppCmd}=State) ->
    case kz_json:is_true(<<"Endless-Playback">>, AppCmd, 'false')
        orelse kz_json:get_integer_value(<<"Loop-Count">>, AppCmd, 0)
    of
        'true' ->
            lager:debug("media is playing back endlessly, looping"),
            _ = execute_control_request(AppCmd, State),
            State;
        Count when is_integer(Count), Count > 1 ->
            lager:debug("media is looped (~p left), looping", [Count-1]),
            UpdatedCmd = kz_json:set_value(<<"Loop-Count">>, Count-1, AppCmd),
            execute_control_request(UpdatedCmd, State#state{current_cmd=UpdatedCmd}),
            State#state{current_cmd=UpdatedCmd};
        _Count ->
            lager:debug("media finished playing, advancing control queue"),
            forward_queue(JObj, State)
    end.

-spec flush_group_id(queue:queue(), kz_term:api_binary(), kz_term:ne_binary()) -> queue:queue().
flush_group_id(CmdQ, 'undefined', _) -> CmdQ;
flush_group_id(CmdQ, GroupId, AppName) ->
    lager:debug("filtering commands ~s for group-id ~s", [AppName, GroupId]),
    Filter = kz_json:from_list([{<<"Application-Name">>, AppName}
                               ,{<<"Group-ID">>, GroupId}
                               ,{<<"Fields">>, kz_json:from_list([{<<"Group-ID">>, GroupId}])}
                               ]),
    maybe_filter_queue([Filter], CmdQ).

-spec forward_queue(kz_json:object(), state()) -> state().
forward_queue(JObj
             ,#state{call_id = CallId
                    ,is_node_up = INU
                    ,is_call_up = CallUp
                    ,command_q = CmdQ
                    }=State) ->
    case INU
        andalso queue:out(CmdQ)
    of
        'false' ->
            %% if the node is down, don't inject the next FS event
            lager:debug("not continuing until the media node becomes available"),
            State#state{current_app='undefined', msg_id='undefined'};
        {'empty', _} ->
            lager:debug("no call commands remain queued, hibernating"),
            State#state{current_app='undefined', msg_id='undefined'};
        {{'value', Cmd}, CmdQ1} ->
            AppName = kapi_dialplan:application_name(Cmd),
            _ = case CallUp
                    orelse is_post_hangup_command(AppName)
                of
                    'true' -> execute_control_request(Cmd, State);
                    'false' ->
                        lager:debug("command '~s' is not valid after hangup, skipping", [AppName]),
                        maybe_send_error_resp(JObj, CallId, Cmd),
                        self() ! {'force_queue_advance', CallId, JObj}
                end,
            MsgId = kz_api:msg_id(Cmd),
            State#state{command_q = CmdQ1
                       ,current_app = AppName
                       ,current_cmd = Cmd
                       ,msg_id = MsgId
                       }
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec handle_sofia_replaced(kz_term:ne_binary(), state()) -> state().
handle_sofia_replaced(<<_/binary>> = CallId, #state{call_id=CallId}=State) ->
    lager:debug("call id hasn't changed, no replacement necessary"),
    State;
handle_sofia_replaced(<<_/binary>> = ReplacedBy, #state{call_id=CallId
                                                       ,node=Node
                                                       ,other_legs=Legs
                                                       ,command_q=CommandQ
                                                       }=State) ->
    lager:info("updating callid from ~s to ~s", [CallId, ReplacedBy]),
    unbind_from_events(Node, CallId),
    unreg_for_call_related_events(CallId),

    kz_util:put_callid(ReplacedBy),
    bind_to_events(Node, ReplacedBy),
    reg_for_call_related_events(ReplacedBy),

    lager:info("...call id updated, continuing post-transfer"),
    Commands = [kz_json:set_value(<<"Call-ID">>, ReplacedBy, JObj)
                || JObj <- queue:to_list(CommandQ)
               ],
    State#state{call_id=ReplacedBy
               ,other_legs=lists:delete(ReplacedBy, Legs)
               ,command_q=queue:from_list(Commands)
               }.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec handle_channel_create(kzd_freeswitch:data(), state()) -> state().
handle_channel_create(Props, #state{call_id=CallId}=State) ->
    LegId = kzd_freeswitch:call_id(Props),
    case ecallmgr_fs_channel:get_other_leg(LegId, Props) of
        'undefined' -> State;
        CallId -> add_leg(Props, LegId, State);
        OtherLeg -> maybe_add_cleg(Props, OtherLeg, LegId, State)
    end.

-spec add_leg(kz_term:proplist(), kz_term:ne_binary(), state()) -> state().
add_leg(Props, LegId, #state{other_legs=Legs
                            ,call_id=CallId
                            ,node=Node
                            }=State) ->
    case lists:member(LegId, Legs) of
        'true' -> State;
        'false' ->
            lager:debug("added leg ~s to call", [LegId]),
            ConsumerPid = kz_amqp_channel:consumer_pid(),
            _ = kz_util:spawn(
                  fun() ->
                          kz_util:put_callid(CallId),
                          _ = kz_amqp_channel:consumer_pid(ConsumerPid),
                          publish_leg_addition(props:set_value(<<"Other-Leg-Unique-ID">>, CallId, Props))
                  end),
            _ = case ecallmgr_fs_channel:fetch(CallId) of
                    {'ok', Channel} ->
                        CDR = kz_json:get_value(<<"interaction_id">>, Channel),
                        ecallmgr_fs_command:set(Node, LegId, [{<<?CALL_INTERACTION_ID>>, CDR}]);
                    _ -> 'ok'
                end,
            State#state{other_legs=[LegId|Legs]}
    end.

-spec publish_leg_addition(kz_term:proplist()) -> 'ok'.
publish_leg_addition(Props) ->
    Event = ecallmgr_call_events:create_event(<<"LEG_CREATED">>
                                             ,'undefined'
                                             ,ecallmgr_call_events:swap_call_legs(Props)
                                             ),
    ecallmgr_call_events:publish_event(Event).

-spec maybe_add_cleg(kz_term:proplist(), kz_term:api_binary(), kz_term:api_binary(), state()) -> state().
maybe_add_cleg(Props, OtherLeg, LegId, #state{other_legs=Legs}=State) ->
    case lists:member(OtherLeg, Legs) of
        'true' -> add_cleg(Props, OtherLeg, LegId, State);
        'false' -> State
    end.

-spec add_cleg(kz_term:proplist(), kz_term:api_binary(), kz_term:api_binary(), state()) -> state().
add_cleg(_Props, _OtherLeg, 'undefined', State) -> State;
add_cleg(Props, OtherLeg, LegId, #state{other_legs=Legs
                                       ,call_id=CallId
                                       }=State) ->
    case lists:member(LegId, Legs) of
        'true' -> State;
        'false' ->
            lager:debug("added cleg ~s to call", [LegId]),
            ConsumerPid = kz_amqp_channel:consumer_pid(),
            _ = kz_util:spawn(
                  fun() ->
                          kz_util:put_callid(CallId),
                          _ = kz_amqp_channel:consumer_pid(ConsumerPid),
                          publish_cleg_addition(Props, OtherLeg, CallId)
                  end),
            State#state{other_legs=[LegId|Legs]}
    end.

-spec publish_cleg_addition(kz_term:proplist(), kz_term:api_binary(), kz_term:ne_binary()) -> 'ok'.
publish_cleg_addition(Props, OtherLeg, CallId) ->
    Event = ecallmgr_call_events:create_event(<<"LEG_CREATED">>
                                             ,'undefined'
                                             ,ecallmgr_call_events:swap_call_legs(Props)
                                             ),
    Event1 = replace_call_id(Event, OtherLeg, CallId, []),
    ecallmgr_call_events:publish_event(Event1).

-spec replace_call_id(kz_term:proplist(), kz_term:api_binary(), kz_term:ne_binary(), kz_term:proplist()) -> kz_term:proplist().
replace_call_id([], _Call1, _Call2, Swap) -> Swap;
replace_call_id([{Key, Call1}|T], Call1, Call2, Swap) ->
    replace_call_id(T, Call1, Call2, [{Key, Call2}|Swap]);
replace_call_id([Prop|T], Call1, Call2, Swap) ->
    replace_call_id(T, Call1, Call2, [Prop|Swap]).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec handle_channel_destroy(kzd_freeswitch:data(), state()) -> state().
handle_channel_destroy(Props, #state{call_id=CallId}=State) ->
    LegId = kzd_freeswitch:call_id(Props),
    case ecallmgr_fs_channel:get_other_leg(LegId, Props) =:= CallId of
        'true' -> remove_leg(Props, State);
        'false' -> State
    end.

-spec remove_leg(kz_term:proplist(), state()) -> state().
remove_leg(Props, #state{other_legs=Legs
                        ,call_id=CallId
                        }=State) ->
    LegId = props:get_value(<<"Caller-Unique-ID">>, Props),
    case lists:member(LegId, Legs) of
        'false' -> State;
        'true' ->
            lager:debug("removed leg ~s from call", [LegId]),
            ConsumerPid = kz_amqp_channel:consumer_pid(),
            _ = kz_util:spawn(
                  fun() ->
                          kz_util:put_callid(CallId),
                          _ = kz_amqp_channel:consumer_pid(ConsumerPid),
                          publish_leg_removal(Props)
                  end),
            State#state{other_legs=lists:delete(LegId, Legs)
                       ,last_removed_leg=LegId
                       }
    end.

-spec publish_leg_removal(kz_term:proplist()) -> 'ok'.
publish_leg_removal(Props) ->
    Event = ecallmgr_call_events:create_event(<<"LEG_DESTROYED">>
                                             ,'undefined'
                                             ,ecallmgr_call_events:swap_call_legs(Props)),
    ecallmgr_call_events:publish_event(Event).

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
            _ = case CallUp
                    orelse is_post_hangup_command(AppName)
                of
                    'true' -> execute_control_request(Cmd, State);
                    'false' ->
                        lager:debug("command '~s' is not valid after hangup, ignoring", [AppName]),
                        maybe_send_error_resp(JObj, CallId, Cmd),
                        self() ! {'force_queue_advance', CallId, JObj}
                end,
            MsgId = kz_api:msg_id(Cmd),
            State#state{command_q=NewCmdQ1
                       ,current_app=AppName
                       ,current_cmd=Cmd
                       ,keep_alive_ref=get_keep_alive_ref(State)
                       ,msg_id=MsgId
                       };
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
            execute_control_request(JObj, State),
            maybe_filter_queue(kz_json:get_value(<<"Filter-Applications">>, JObj), CommandQ);
        _ ->
            lager:debug("recv and executing ~s now!", [AName]),
            execute_control_request(JObj, State),
            CommandQ
    end;
insert_command(#state{node=Node, call_id=CallId}, 'flush', JObj) ->
    lager:debug("received control queue flush command, clearing all waiting commands"),
    _ = freeswitch:api(Node, 'uuid_break', <<CallId/binary, " all">>),
    self() ! {'force_queue_advance', CallId, JObj},
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

-spec queue_insert_fun('tail' | 'head') -> function().
queue_insert_fun('tail') ->
    fun(JObj, Q) ->
            'true' = kapi_dialplan:v(JObj),
            case kapi_dialplan:application_name(JObj) of
                'undefined' -> Q;
                <<"noop">> = AppName ->
                    MsgId = kz_api:msg_id(JObj),
                    lager:debug("inserting at the tail of the control queue call command ~s(~s)", [AppName, MsgId]),
                    queue:in(JObj, Q);
                AppName ->
                    lager:debug("inserting at the tail of the control queue call command ~s", [AppName]),
                    queue:in(JObj, Q)
            end
    end;
queue_insert_fun('head') ->
    fun(JObj, Q) ->
            'true' = kapi_dialplan:v(JObj),
            case kapi_dialplan:application_name(JObj) of
                'undefined' -> Q;
                <<"noop">> = AppName ->
                    MsgId = kz_api:msg_id(JObj),
                    lager:debug("inserting at the head of the control queue call command ~s(~s)", [AppName, MsgId]),
                    queue:in_r(JObj, Q);
                AppName ->
                    lager:debug("inserting at the head of the control queue call command ~s", [AppName]),
                    queue:in_r(JObj, Q)
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

-spec is_post_hangup_command(kz_term:ne_binary()) -> boolean().
is_post_hangup_command(AppName) ->
    lists:member(AppName, ?POST_HANGUP_COMMANDS).

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
-spec execute_control_request(kz_json:object(), state()) -> 'ok'.
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
        'throw':{'error', 'baduuid'} ->
            lager:debug("unable to execute command, baduuid"),
            Msg = list_to_binary(["Session ", CallId
                                 ," not found for ", Application
                                 ]),
            send_error_resp(kz_json:new(), CallId, Cmd, Msg),
            Srv ! {'force_queue_advance', CallId, kz_json:new()},
            'ok';
        _:{'error', 'nosession'} ->
            lager:debug("unable to execute command, no session"),
            Msg = list_to_binary(["Session ", CallId
                                 ," not found for ", Application
                                 ]),
            send_error_resp(kz_json:new(), CallId, Cmd, Msg),
            Srv ! {'force_queue_advance', CallId, kz_json:new()},
            'ok';
        'error':{'badmatch', {'error', 'nosession'}} ->
            lager:debug("unable to execute command, no session"),
            Msg = list_to_binary(["Session ", CallId
                                 ," not found for ", Application
                                 ]),
            send_error_resp(kz_json:new(), CallId, Cmd, Msg),
            Srv ! {'force_queue_advance', CallId, kz_json:new()},
            'ok';
        'error':{'badmatch', {'error', ErrMsg}} ->
            ST = erlang:get_stacktrace(),
            lager:debug("invalid command ~s: ~p", [Application, ErrMsg]),
            kz_util:log_stacktrace(ST),
            maybe_send_error_resp(kz_json:new(), CallId, Cmd),
            Srv ! {'force_queue_advance', CallId, kz_json:new()},
            'ok';
        'throw':{'msg', ErrMsg} ->
            lager:debug("error while executing command ~s: ~s", [Application, ErrMsg]),
            send_error_resp(kz_json:new(), CallId, Cmd),
            Srv ! {'force_queue_advance', CallId, kz_json:new()},
            'ok';
        'throw':Msg ->
            lager:debug("failed to execute ~s: ~s", [Application, Msg]),
            lager:debug("only handling call id(s): ~p", [[CallId | OtherLegs]]),

            send_error_resp(kz_json:new(), CallId, Cmd, Msg),
            Srv ! {'force_queue_advance', CallId, kz_json:new()},
            'ok';
        _A:_B ->
            ST = erlang:get_stacktrace(),
            lager:debug("exception (~s) while executing ~s: ~p", [_A, Application, _B]),
            kz_util:log_stacktrace(ST),
            send_error_resp(kz_json:new(), CallId, Cmd),
            Srv ! {'force_queue_advance', CallId, kz_json:new()},
            'ok'
    end.

-spec which_call_leg(kz_term:ne_binary(), kz_term:ne_binaries(), kz_term:ne_binary()) -> kz_term:ne_binary().
which_call_leg(CmdLeg, OtherLegs, CallId) ->
    case lists:member(CmdLeg, OtherLegs) of
        'true' ->
            lager:debug("executing against ~s instead", [CmdLeg]),
            CmdLeg;
        'false' -> CallId
    end.

-spec maybe_send_error_resp(kz_json:object(), kz_term:ne_binary(), kz_json:object()) -> 'ok'.
maybe_send_error_resp(JObj, CallId, Cmd) ->
    maybe_send_error_resp(JObj, CallId, Cmd, kapi_dialplan:application_name(Cmd)).

-spec maybe_send_error_resp(kz_json:object(), kz_term:ne_binary(), kz_json:object(), kz_term:ne_binary()) -> 'ok'.
maybe_send_error_resp(_JObj, _CallId, _Cmd, <<"hangup">>) -> 'ok';
maybe_send_error_resp(JObj, CallId, Cmd, AppName) ->
    Msg = <<"Could not execute dialplan action: ", AppName/binary>>,
    send_error_resp(JObj, CallId, Cmd, Msg).

-spec send_error_resp(kz_json:object(), kz_term:ne_binary(), kz_json:object()) -> 'ok'.
send_error_resp(JObj, CallId, Cmd) ->
    Msg = <<"Could not execute dialplan action: "
           ,(kapi_dialplan:application_name(Cmd))/binary
          >>,
    send_error_resp(JObj, CallId, Cmd, Msg).

-spec send_error_resp(kz_json:object(), kz_term:ne_binary(), kz_json:object(), kz_term:ne_binary()) -> 'ok'.
send_error_resp(JObj, CallId, Cmd, Msg) ->
    case ecallmgr_fs_channel:fetch(CallId) of
        {'ok', Channel} -> send_error_resp(JObj, CallId, Cmd, Msg, Channel);
        {'error', 'not_found'} -> send_error_resp(JObj, CallId, Cmd, Msg, 'undefined')
    end.

-spec send_error_resp(kz_json:object(), kz_term:ne_binary(), kz_json:object(), kz_term:ne_binary(), kz_term:api_object()) -> 'ok'.
send_error_resp(JObj, CallId, Cmd, Msg, Channel) ->
    CCVs = error_ccvs(Channel),

    Resp = [{<<"Msg-ID">>, kz_api:msg_id(Cmd)}
           ,{<<"Error-Message">>, Msg}
           ,{<<"Request">>, Cmd}
           ,{<<"Call-ID">>, CallId}
           ,{<<"Custom-Channel-Vars">>, CCVs}
            | kz_api:default_headers(<<>>, <<"error">>, <<"dialplan">>, ?APP_NAME, ?APP_VERSION)
            ++ maybe_add_channel_destroy_headers(JObj)
           ],
    lager:debug("sending execution error: ~p", [Resp]),
    kapi_dialplan:publish_error(CallId, Resp).

-spec error_ccvs(kz_term:api_object()) -> kz_term:api_object().
error_ccvs('undefined') -> 'undefined';
error_ccvs(Channel) ->
    kz_json:from_list(ecallmgr_fs_channel:channel_ccvs(Channel)).

-spec maybe_add_channel_destroy_headers(kz_json:object()) -> kz_term:proplist().
maybe_add_channel_destroy_headers(JObj) ->
    props:filter_undefined(
      [{<<"Disposition">>, kz_json:get_value(<<"Disposition">>, JObj)}
      ,{<<"Hangup-Cause">>, kz_json:get_value(<<"Hangup-Cause">>, JObj)}
      ,{<<"Hangup-Code">>, kz_json:get_value(<<"Hangup-Code">>, JObj)}
      ]
     ).

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
-spec bind_to_events(atom(), kz_term:ne_binary()) -> 'true'.
bind_to_events(Node, CallId) ->
    lager:debug("binding to call ~s events on node ~s", [CallId, Node]),
    'true' = gproc:reg({'p', 'l', ?FS_CALL_EVENT_REG_MSG(Node, CallId)}),
    'true' = gproc:reg({'p', 'l', ?FS_EVENT_REG_MSG(Node, <<"CHANNEL_CREATE">>)}),
    'true' = gproc:reg({'p', 'l', ?FS_EVENT_REG_MSG(Node, <<"CHANNEL_DESTROY">>)}).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec unbind_from_events(atom(), kz_term:ne_binary()) -> 'true'.
unbind_from_events(Node, CallId) ->
    lager:debug("unbinding from call ~s events on node ~s", [CallId, Node]),
    _ = (catch gproc:unreg({'p', 'l', ?FS_CALL_EVENT_REG_MSG(Node, CallId)})),
    _ = (catch gproc:unreg({'p', 'l', ?FS_EVENT_REG_MSG(Node, <<"CHANNEL_CREATE">>)})),
    _ = (catch gproc:unreg({'p', 'l', ?FS_EVENT_REG_MSG(Node, <<"CHANNEL_DESTROY">>)})),
    'true'.

-spec reg_for_call_related_events(kz_term:ne_binary()) -> 'ok'.
reg_for_call_related_events(CallId) ->
    gproc:reg({'p', 'l', {'call_control', CallId}}),
    gproc:reg({'p', 'l', ?LOOPBACK_BOWOUT_REG(CallId)}).

-spec unreg_for_call_related_events(kz_term:ne_binary()) -> 'ok'.
unreg_for_call_related_events(CallId) ->
    (catch gproc:unreg({'p', 'l', {'call_control', CallId}})),
    (catch gproc:unreg({'p', 'l', ?LOOPBACK_BOWOUT_REG(CallId)})),
    'ok'.

-spec handle_replaced(kz_term:proplist(), state()) ->
                             {'noreply', state()}.
handle_replaced(Props, #state{fetch_id=FetchId
                             ,node=Node
                             ,call_id=CallId
                             }=State) ->
    case props:get_value(?GET_CCV(<<"Fetch-ID">>), Props) of
        FetchId ->
            ReplacedBy = props:get_value(<<"att_xfer_replaced_by">>, Props),
            case ecallmgr_fs_channel:fetch(ReplacedBy) of
                {'ok', Channel} ->
                    OtherLeg = kz_json:get_value(<<"other_leg">>, Channel),
                    OtherUUID = props:get_value(<<"Other-Leg-Unique-ID">>, Props),
                    CDR = kz_json:get_value(<<"interaction_id">>, Channel),
                    kz_cache:store_local(?ECALLMGR_INTERACTION_CACHE, CallId, CDR),
                    _ = ecallmgr_fs_command:set(Node, OtherUUID, [{<<?CALL_INTERACTION_ID>>, CDR}]),
                    _ = ecallmgr_fs_command:set(Node, OtherLeg, [{<<?CALL_INTERACTION_ID>>, CDR}]),
                    {'noreply', handle_sofia_replaced(ReplacedBy, State)};
                _Else ->
                    lager:debug("channel replaced was not handled : ~p", [_Else]),
                    {'noreply', State}
            end;
        _Else ->
            lager:info("sofia replaced on our channel but different fetch id~n"),
            {'noreply', State}
    end.

-spec handle_transferee(kz_term:proplist(), state()) ->
                               {'noreply', state()}.
handle_transferee(Props, #state{fetch_id=FetchId
                               ,node=_Node
                               ,call_id=CallId
                               }=State) ->
    case props:get_value(?GET_CCV(<<"Fetch-ID">>), Props) of
        FetchId ->
            lager:info("we (~s) have been transferred, terminate immediately", [CallId]),
            {'stop', 'normal', State};
        _Else ->
            lager:info("we were a different instance of this transferred call"),
            {'noreply', State}
    end.

-spec handle_transferor(kz_term:proplist(), state()) ->
                               {'noreply', state()}.
handle_transferor(_Props, #state{fetch_id=_FetchId
                                ,node=_Node
                                ,call_id=_CallId
                                }=State) ->
    {'noreply', State}.

-spec handle_intercepted(atom(), kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
handle_intercepted(Node, CallId, Props) ->
    _ = case {props:get_value(<<"Core-UUID">>, Props)
             ,props:get_value(?GET_CUSTOM_HEADER(<<"Core-UUID">>), Props)
             }
        of
            {A, A} -> 'ok';
            {_, 'undefined'} ->
                UUID = props:get_value(<<"intercepted_by">>, Props),
                case ecallmgr_fs_channel:fetch(UUID) of
                    {'ok', Channel} ->
                        CDR = kz_json:get_value(<<"interaction_id">>, Channel),
                        kz_cache:store_local(?ECALLMGR_INTERACTION_CACHE, CallId, CDR),
                        ecallmgr_fs_command:set(Node, UUID, [{<<?CALL_INTERACTION_ID>>, CDR}]);
                    _ -> 'ok'
                end;
            _ ->
                UUID = props:get_value(<<"intercepted_by">>, Props),
                CDR = props:get_value(?GET_CCV(<<?CALL_INTERACTION_ID>>), Props),
                ecallmgr_fs_command:set(Node, UUID, [{<<?CALL_INTERACTION_ID>>, CDR}])
        end,
    'ok'.

-spec handle_event_info(kz_term:ne_binary(), kzd_freeswitch:data(), state()) ->
                               {'noreply', state()} |
                               {'stop', any(), state()}.
handle_event_info(CallId, Props, #state{call_id=CallId
                                       ,node=Node
                                       }=State) ->
    JObj = ecallmgr_call_events:to_json(Props),
    Application = kapi_dialplan:application_name(JObj),
    case props:get_first_defined([<<"Event-Subclass">>
                                 ,<<"Event-Name">>
                                 ]
                                ,Props
                                )
    of
        <<"kazoo::", _/binary>> ->
            {'noreply', handle_execute_complete(Application, JObj, State)};
        <<"CHANNEL_EXECUTE_COMPLETE">> ->
            {'noreply', handle_execute_complete(Application, JObj, State)};
        <<"RECORD_STOP">> ->
            {'noreply', handle_execute_complete(Application, JObj, State)};
        <<"CHANNEL_DESTROY">> ->
            case kz_json:is_true(<<"Channel-Moving">>, JObj) of
                'false' -> {'noreply', handle_channel_destroyed(JObj, State)};
                'true' ->
                    lager:debug("channel destroy while moving to other node, deferring to new controller"),
                    {'stop', 'normal', State}
            end;
        <<"sofia::transferee">> ->
            handle_transferee(Props, State);
        <<"sofia::replaced">> ->
            handle_replaced(Props, State);
        <<"sofia::intercepted">> ->
            'ok' = handle_intercepted(Node, CallId, Props),
            {'noreply', State};
        <<"CHANNEL_EXECUTE">> when Application =:= <<"redirect">> ->
            gen_listener:cast(self(), {'channel_redirected', JObj}),
            {'stop', 'normal', State};
        <<"sofia::transferor">> ->
            handle_transferor(Props, State);
        _Else ->
            {'noreply', State}
    end.

-spec handle_other_event_info(kz_term:api_binary(), kzd_freeswitch:data(), state()) -> {'noreply', state()}.
handle_other_event_info(CallId, Props, State) ->
    case props:get_first_defined([<<"Event-Subclass">>
                                 ,<<"Event-Name">>
                                 ]
                                ,Props
                                )
    of
        <<"CHANNEL_CREATE">> ->
            {'noreply', handle_channel_create(Props, State)};
        <<"CHANNEL_DESTROY">> ->
            {'noreply', handle_channel_destroy(Props, State)};
        <<"sofia::transferor">> ->
            props:to_log(Props, <<"TRANSFEROR OTHER ", CallId/binary>>),
            {'noreply', State};
        _Else ->
            lager:debug("CALL CONTROL NOT HANDLED ~s", [_Else]),
            {'noreply', State}
    end.
