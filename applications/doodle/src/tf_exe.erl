%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% @author Karl Anderson
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(tf_exe).
-behaviour(gen_server).

%% API
-export([start_link/2]).
-export([queue_name/1]).
-export([continue/1, continue/2]).
-export([stop/1, stop/2]).

%% gen_server callbacks
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3
        ]).

-include("doodle.hrl").
-include_lib("kazoo_stdlib/include/kazoo_json.hrl").
-include_lib("kazoo_amqp/include/kz_api.hrl").

-record(state, {call = kapps_im:new() :: kapps_im:im()
               ,flow = kz_json:new() :: kz_json:object()
               ,flows = [] :: kz_json:objects()
               ,tf_module_pid :: kz_term:api_pid_ref()
               ,tf_module_old_pid :: kz_term:api_pid_ref()
               ,result = 'undefined' :: atom()
               ,queue :: kz_term:api_ne_binary()
               ,self = self() :: pid()
               }).
-type state() :: #state{}.

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the server.
%% @end
%%------------------------------------------------------------------------------
-spec start_link(kapps_im:im(), map()) -> kz_types:startlink_ret().
start_link(Im, Context) ->
    gen_server:start_link(?MODULE, [Im, Context], []).

-spec tf_exe_pid(kapps_im:im()) -> pid().
tf_exe_pid(Im) ->
    kapps_im:kvs_fetch('tf_exe_pid', Im).

-spec continue(kapps_im:im() | pid()) -> 'ok'.
continue(Srv) -> continue(?DEFAULT_CHILD_KEY, Srv).

-spec continue(kz_term:ne_binary(), kapps_im:im() | pid()) -> 'ok'.
continue(Key, Srv) when is_pid(Srv) ->
    gen_server:cast(Srv, {'continue', Key});
continue(Key, Im) ->
    Srv = tf_exe_pid(Im),
    continue(Key, Srv).

-spec stop(kapps_im:im() | pid()) -> 'ok'.
stop(Srv) ->
    stop(Srv, 'undefined').

-spec stop(kapps_im:im() | pid(), any()) -> 'ok'.
stop(Srv, Cause) when is_pid(Srv) ->
    gen_server:cast(Srv, {'stop', Cause});
stop(Im, Cause) ->
    Srv = tf_exe_pid(Im),
    stop(Srv, Cause).

-spec queue_name(kapps_im:im() | pid()) -> kz_term:ne_binary().
queue_name(Srv) when is_pid(Srv) ->
    gen_server:call(Srv, 'queue_name');
queue_name(Im) ->
    Srv = tf_exe_pid(Im),
    queue_name(Srv).

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server.
%% @end
%%------------------------------------------------------------------------------
-spec init([kapps_im:im() | map()]) -> {'ok', state()}.
init([Im, Context]) ->
    process_flag('trap_exit', 'true'),
    kapps_im:put_message_id(Im),
    gen_server:cast(self(), 'initialize'),
    #{channel := Channel, queue := Queue} = Context,
    ControllerQ = kapi:encode_pid(Queue),
    kz_amqp_channel:consumer_channel(Channel),
    Funs = [{fun kapps_im:kvs_store/3, 'consumer_pid', kz_amqp_channel:consumer_pid()}
           ,{fun kapps_im:kvs_store/3, 'consumer_channel', kz_amqp_channel:consumer_channel()}
           ,{fun kapps_im:set_controller_queue/2, ControllerQ}
           ],
    {'ok', #state{call = kapps_im:exec(Funs, Im)
                 ,queue = ControllerQ
                 }}.

%%------------------------------------------------------------------------------
%% @doc Handling call messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call('queue_name', _From, #state{queue=Q}=State) ->
    {'reply', Q, State};
handle_call(_Request, _From, State) ->
    lager:warning("unhandled request in call: ~p : ~p", [_Request, _From]),
    Reply = {'error', 'unimplemented'},
    {'reply', Reply, State}.

%%------------------------------------------------------------------------------
%% @doc Handling cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast({'continue', Key}, #state{flow=Flow}=State) ->
    lager:info("continuing to child '~s'", [Key]),

    case kz_json:get_value([<<"children">>, Key], Flow) of
        'undefined' when Key =:= ?DEFAULT_CHILD_KEY ->
            lager:info("wildcard child does not exist, we are lost... exiting"),
            stop(self()),
            {'noreply', State};
        'undefined' ->
            lager:info("requested child '~s' does not exist, trying wild card", [Key]),
            continue(self()),
            {'noreply', State};
        NewFlow ->
            {'noreply', launch_tf_module(State#state{flow=NewFlow})}
    end;
handle_cast({'stop', Cause}, State) ->
    {'stop', {shutdown, Cause}, State#state{result=Cause}};
handle_cast('initialize', State) ->
    initialize(State);
handle_cast(_Msg, State) ->
    lager:debug("unhandled cast: ~p", [_Msg]),
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info({'amqp_msg', JObj}, State) ->
    _ = handle_event(JObj, State),
    {'noreply', State};
handle_info({'kapi', {_, _, JObj}}, State) ->
    _ = handle_event(JObj, State),
    {'noreply', State};
handle_info({'DOWN', Ref, 'process', Pid, 'normal'}, #state{tf_module_pid={Pid, Ref}
                                                           ,call=Im
                                                           }=State) ->
    erlang:demonitor(Ref, ['flush']),
    lager:debug("cf module ~s down normally", [kapps_im:kvs_fetch('tf_last_action', Im)]),
    {'noreply', State#state{tf_module_pid='undefined'}};
handle_info({'DOWN', Ref, 'process', Pid, 'killed'}, #state{tf_module_pid={Pid, Ref}
                                                           ,call=Im
                                                           }=State) ->
    erlang:demonitor(Ref, ['flush']),
    lager:debug("cf module ~s down normally", [kapps_im:kvs_fetch('tf_last_action', Im)]),
    {'noreply', State#state{tf_module_pid='undefined'}};
handle_info({'DOWN', Ref, 'process', Pid, _Reason}, #state{tf_module_pid={Pid, Ref}
                                                          ,call=Im
                                                          }=State) ->
    erlang:demonitor(Ref, ['flush']),
    LastAction = kapps_im:kvs_fetch('tf_last_action', Im),
    lager:error("action ~s died unexpectedly: ~p", [LastAction, _Reason]),
    continue(self()),
    {'noreply', State#state{tf_module_pid='undefined'}};
handle_info({'DOWN', _Ref, 'process', _Pid, 'normal'}, State) ->
    {'noreply', State};
handle_info({'EXIT', Pid, 'normal'}, #state{tf_module_pid={Pid, Ref}
                                           ,call=Im
                                           }=State) ->
    erlang:demonitor(Ref, ['flush']),
    lager:debug("cf module ~s down normally", [kapps_im:kvs_fetch('tf_last_action', Im)]),
    {'noreply', State#state{tf_module_pid='undefined'}};
handle_info({'EXIT', Pid, 'killed'}, #state{tf_module_pid={Pid, Ref}
                                           ,call=Im
                                           }=State) ->
    erlang:demonitor(Ref, ['flush']),
    lager:debug("cf module ~s killed normally", [kapps_im:kvs_fetch('tf_last_action', Im)]),
    {'noreply', State#state{tf_module_pid='undefined'}};
handle_info({'EXIT', Pid, _Reason}, #state{tf_module_pid={Pid, Ref}
                                          ,call=Im
                                          }=State) ->
    erlang:demonitor(Ref, ['flush']),
    LastAction = kapps_im:kvs_fetch('tf_last_action', Im),
    lager:error_unsafe("action ~s died unexpectedly: ~p", [LastAction, _Reason]),
    continue(self()),
    {'noreply', State#state{tf_module_pid='undefined'}};
handle_info({'EXIT', Pid, 'normal'}, #state{tf_module_old_pid={Pid, Ref}
                                           ,call=Im
                                           }=State) ->
    erlang:demonitor(Ref, ['flush']),
    lager:debug("cf module ~s down normally", [kapps_im:kvs_fetch('tf_old_action', Im)]),
    {'noreply', State#state{tf_module_old_pid='undefined'}};
handle_info({'EXIT', Pid, 'killed'}, #state{tf_module_old_pid={Pid, Ref}
                                           ,call=Im
                                           }=State) ->
    erlang:demonitor(Ref, ['flush']),
    lager:debug("cf module ~s killed normally", [kapps_im:kvs_fetch('tf_old_action', Im)]),
    {'noreply', State#state{tf_module_old_pid='undefined'}};
handle_info({'EXIT', Pid, _Reason}, #state{tf_module_old_pid={Pid, Ref}
                                          ,call=Im
                                          }=State) ->
    erlang:demonitor(Ref, ['flush']),
    LastAction = kapps_im:kvs_fetch('tf_old_action', Im),
    lager:error("action ~s died unexpectedly: ~p", [LastAction, _Reason]),
    {'noreply', State#state{tf_module_old_pid='undefined'}};
handle_info({'amqp_return', _JObj, _Returned} = Msg, #state{tf_module_pid=PidRef
                                                           ,call=Im
                                                           } = State) ->
    Others = kapps_im:kvs_fetch('tf_event_pids', [], Im),
    Notify = case get_pid(PidRef) of
                 'undefined' -> Others;
                 ModPid -> [ModPid | Others]
             end,
    relay_message(Notify, Msg),
    {'noreply', State};

handle_info(_Msg, State) ->
    lager:debug("unhandled message: ~p", [_Msg]),
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handle call messages, sometimes forward them on.
%% @end
%%------------------------------------------------------------------------------
-spec handle_event(kz_call_event:doc(), state()) -> 'ok'.
handle_event(JObj, #state{tf_module_pid=PidRef
                         ,call=Im
                         }) ->
    Others = kapps_im:kvs_fetch('tf_event_pids', [], Im),
    Notify = case get_pid(PidRef) of
                 'undefined' -> Others;
                 ModPid -> [ModPid | Others]
             end,
    relay_message(Notify, JObj).

%%------------------------------------------------------------------------------
%% @doc This function is called by a `gen_server' when it is about to
%% terminate. It should be the opposite of `Module:init/1' and do any
%% necessary cleaning up. When it returns, the `gen_server' terminates
%% with Reason. The return value is ignored.
%%
%% @end
%%------------------------------------------------------------------------------
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, _State) -> 'ok'.

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
%% @doc This function determines if the callflow module specified at the
%% current node is 'available' and attempts to launch it if so.
%% Otherwise it will advance to the next child in the flow.
%% @end
%%------------------------------------------------------------------------------
-spec launch_tf_module(state()) -> state().
launch_tf_module(#state{flow=?EMPTY_JSON_OBJECT}=State) ->
    lager:debug("no flow left to launch, maybe stopping"),
    stop(self(), 'finish'),
    State;
launch_tf_module(#state{flow=Flow
                       }=State) ->
    do_launch_tf_module(State, find_module(Flow)).

-spec do_launch_tf_module(state(), atom()) -> state().
do_launch_tf_module(#state{call=Im
                          ,tf_module_pid=OldPidRef
                          }=State
                   ,'undefined'
                   ) ->
    lager:error("unknown textflow action, reverting to last action"),
    continue(self()),
    OldAction = kapps_im:kvs_fetch('tf_last_action', Im),
    State#state{tf_module_pid='undefined'
               ,tf_module_old_pid=OldPidRef
               ,call=update_actions(OldAction, Im)
               };
do_launch_tf_module(#state{call=Im
                          ,flow=Flow
                          ,tf_module_pid=OldPidRef
                          }=State
                   ,Action
                   ) ->
    Data = kz_json:get_json_value(<<"data">>, Flow, kz_json:new()),
    lager:info("moving to action '~s'", [Action]),
    Im1 = update_actions(Action, Im),
    PidRef = spawn_tf_module(Action, Data, Im1),
    link(get_pid(PidRef)),
    State#state{tf_module_pid=PidRef
               ,tf_module_old_pid=OldPidRef
               ,call=Im1
               }.

-spec find_module(kz_json:object()) -> atom().
find_module(Flow) ->
    Module = kz_json:get_ne_binary_value(<<"module">>, Flow),
    ModuleBin = <<"tf_", Module/binary>>,
    Data = kz_json:get_json_value(<<"data">>, Flow, kz_json:new()),
    TFModule = kz_term:to_atom(ModuleBin, 'true'),
    IsExported = kz_module:is_exported(TFModule, 'handle', 2),
    SkipModule = kz_json:is_true(<<"skip_module">>, Data, 'false'),
    case IsExported
        andalso (not SkipModule)
    of
        'true' -> TFModule;
        'false' ->
            lager:debug("skipping textflow module ~s (handle exported: ~s skip_module: ~s)"
                       ,[TFModule, IsExported, SkipModule]),
            'undefined'
    end.

-spec update_actions(atom(), kapps_im:im()) -> kapps_im:im().
update_actions(Action, Im) ->
    OldAction = kapps_im:kvs_fetch('tf_last_action', Im),
    Routines = [{fun kapps_im:kvs_store/3, 'tf_old_action', OldAction}
               ,{fun kapps_im:kvs_store/3, 'tf_last_action', Action}
               ],
    kapps_im:exec(Routines, Im).

%%------------------------------------------------------------------------------
%% @doc Helper function to spawn a linked callflow module, from the entry
%% point 'handle' having set the callid on the new process first.
%% @end
%%------------------------------------------------------------------------------
-spec spawn_tf_module(atom(), kz_json:object(), kapps_im:im()) -> kz_term:pid_ref().
spawn_tf_module(TFModule, Data, Im) ->
    AMQPConsumer = kapps_im:kvs_fetch('consumer_pid', Im),
    AMQPChannel = kapps_im:kvs_fetch('consumer_channel', Im),
    kz_process:spawn_monitor(fun tf_module_task/5, [TFModule, Data, Im, AMQPConsumer, AMQPChannel]).

-spec tf_module_task(atom(), kz_json:object(), kapps_im:im(), pid(), pid()) -> any().
tf_module_task(TFModule, Data, Im, AMQPConsumer, AMQPChannel) ->
    _ = kz_amqp_channel:consumer_channel(AMQPChannel),
    _ = kz_amqp_channel:consumer_pid(AMQPConsumer),
    kapps_im:put_message_id(Im),
    try TFModule:handle(Data, Im)
    catch
        ?CATCH('exit', 'normal', _ST) ->
            lager:info("action ~s finished", [TFModule]);
        ?CATCH(_E, R, _ST) ->
            lager:info("action ~s died unexpectedly (~s): ~p", [TFModule, _E, R]),
            ?LOGSTACK(_ST),
            throw(R)
    end.


-spec log_call_information(kapps_im:im()) -> 'ok'.
log_call_information(Im) ->
    lager:info("~s request ~s => ~s", [kapps_im:inception_type(Im), kapps_im:from(Im), kapps_im:to(Im)]).

-spec relay_message(kz_term:pids(), kz_json:object() | {'amqp_return', kz_json:object(), kz_json:object()}) -> 'ok'.
relay_message(Notify, Message) ->
    _ = [kapps_im_command:relay_event(Pid, Message)
         || Pid <- Notify,
            is_pid(Pid)
        ],
    'ok'.

-spec get_pid({pid(), reference()} | 'undefined') -> kz_term:api_pid().
get_pid({Pid, _}) when is_pid(Pid) -> Pid;
get_pid(_) -> 'undefined'.

-spec initialize(state()) -> {'stop', 'normal', state()} |
          {'noreply', state()}.
initialize(#state{call=Im}=State) ->
    log_call_information(Im),
    Flow = kapps_im:kvs_fetch('tf_flow', Im),
    {'noreply'
    ,launch_tf_module(State#state{call=kapps_im:kvs_store('tf_exe_pid', self(), Im)
                                 ,flow=Flow
                                 })
    }.
