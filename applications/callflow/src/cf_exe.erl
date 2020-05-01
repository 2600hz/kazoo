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
-module(cf_exe).
-behaviour(gen_server).

%% API
-export([start_link/1]).
-export([amqp_send/3, amqp_call/4]).
-export([get_call/1, set_call/1]).
-export([callid/1, callid/2]).
-export([queue_name/1]).
-export([control_queue/1, control_queue/2]).
-export([continue/1, continue/2]).
-export([continue_with_flow/2]).
-export([branch/2]).
-export([stop/1, stop/2]).
-export([hard_stop/1]).
-export([transfer/1]).
-export([control_usurped/1]).
-export([channel_destroyed/1, channel_destroyed/2]).
-export([is_channel_destroyed/1]).
-export([stop_on_destroy/1
        ,continue_on_destroy/1
        ]).
-export([get_branch_keys/1, get_all_branch_keys/1]).
-export([attempt/1, attempt/2]).
-export([wildcard_is_empty/1]).
-export([callid_update/2]).
-export([add_event_listener/2]).
-export([next/1, next/2]).
-export([update_call/1, update_call/2]).
-export([add_termination_handler/2
        ,remove_termination_handler/2
        ]).

-export([stop_bad_destination/1]).
-export([status/1]).

%% gen_server callbacks
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3
        ]).

-export([init_cf_exe/2]).

-include("callflow.hrl").
-include_lib("kazoo_stdlib/include/kazoo_json.hrl").
-include_lib("kazoo_amqp/include/kz_api.hrl").

-define(CALL_SANITY_CHECK, 30000).

-define(MAX_BRANCH_COUNT, kapps_config:get_integer(?CF_CONFIG_CAT, <<"max_branch_count">>, 50)).

-type callfow_status() :: 'init' | 'running' | 'not_running'.
-export_type([callfow_status/0]).

-type termination_handler() :: {module(), atom(), list()}.
-type termination_handlers() :: [termination_handler()].

-record(state, {call = kapps_call:new() :: kapps_call:call()
               ,flow = kz_json:new() :: kz_json:object()
               ,flows = [] :: kz_json:objects()
               ,cf_module_pid :: kz_term:api_pid_ref()
               ,cf_module_old_pid :: kz_term:api_pid_ref()
               ,hangup_info = 'undefined' :: kz_term:api_object()
               ,status = 'init' :: callfow_status()
               ,queue :: kz_term:api_ne_binary()
               ,self = self() :: pid()
               ,stop_on_destroy = 'true' :: boolean()
               ,destroyed = 'false' :: boolean()
               ,branch_count = ?MAX_BRANCH_COUNT :: non_neg_integer()
               ,termination_handlers = [] :: termination_handlers()
               }).
-type state() :: #state{}.

-spec cf_exe_pid(kapps_call:call()) -> pid().
cf_exe_pid(Call) ->
    kapps_call:kvs_fetch('cf_exe_pid', Call).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the server.
%% @end
%%------------------------------------------------------------------------------
-spec start_link(map()) -> kz_types:startlink_ret().
start_link(Map) ->
    proc_lib:start_link(?MODULE, 'init_cf_exe', [self(), Map]).

-spec get_call(pid() | kapps_call:call()) -> {'ok', kapps_call:call()}.
get_call(Srv) when is_pid(Srv) ->
    gen_server:call(Srv, 'get_call', 1000);
get_call(Call) ->
    Srv = cf_exe_pid(Call),
    get_call(Srv).

-spec set_call(kapps_call:call()) -> 'ok'.
set_call(Call) ->
    Srv = cf_exe_pid(Call),
    gen_server:cast(Srv, {'set_call', Call}).

-spec update_call(kapps_call:call()) -> kapps_call:call().
update_call(Call) ->
    Srv = cf_exe_pid(Call),
    gen_server:call(Srv, {'set_call', Call}).

-spec update_call(kapps_call:call(), list()) -> kapps_call:call().
update_call(Call, Routines) ->
    Srv = cf_exe_pid(Call),
    gen_server:call(Srv, {'update_call', Routines}).

-spec continue(kapps_call:call() | pid()) -> 'ok'.
continue(Srv) -> continue(?DEFAULT_CHILD_KEY, Srv).

-spec continue(kz_term:ne_binary(), kapps_call:call() | pid()) -> 'ok'.
continue(Key, Srv) when is_pid(Srv) ->
    gen_server:cast(Srv, {'continue', Key});
continue(Key, Call) ->
    Srv = cf_exe_pid(Call),
    continue(Key, Srv).

-spec continue_with_flow(kz_json:object(), kapps_call:call() | pid()) -> 'ok'.
continue_with_flow(Flow, Srv) when is_pid(Srv) ->
    gen_server:cast(Srv, {'continue_with_flow', Flow});
continue_with_flow(Flow, Call) ->
    Srv = cf_exe_pid(Call),
    continue_with_flow(Flow, Srv).

-spec branch(kz_json:object(), kapps_call:call() | pid()) -> 'ok'.
branch(Flow, Srv) when is_pid(Srv) ->
    gen_server:cast(Srv, {'branch', Flow});
branch(Flow, Call) ->
    Srv = cf_exe_pid(Call),
    branch(Flow, Srv).

-spec next(kapps_call:call() | pid()) -> kz_term:api_object().
next(Srv) -> next(?DEFAULT_CHILD_KEY, Srv).

-spec next(kz_term:ne_binary(), kapps_call:call() | pid()) -> kz_term:api_object().
next(Key, Srv) when is_pid(Srv) ->
    gen_server:call(Srv, {'next', Key});
next(Key, Call) ->
    Srv = cf_exe_pid(Call),
    next(Key, Srv).

-spec add_event_listener(kapps_call:call() | pid(), {atom(), list()}) -> 'ok'.
add_event_listener(Srv, {_,_}=SpawnInfo) when is_pid(Srv) ->
    gen_server:cast(Srv, {'add_event_listener', SpawnInfo});
add_event_listener(Call, {_,_}=SpawnInfo) ->
    add_event_listener(cf_exe_pid(Call), SpawnInfo).

-spec add_termination_handler(kapps_call:call() | pid(), termination_handler()) -> 'ok'.
add_termination_handler(Srv, {_,_,_}=Handler) when is_pid(Srv) ->
    gen_server:call(Srv, {'add_termination_handler', Handler});
add_termination_handler(Call, {_,_,_}=Handler) ->
    add_termination_handler(cf_exe_pid(Call), Handler).

-spec remove_termination_handler(kapps_call:call() | pid(), termination_handler()) -> 'ok'.
remove_termination_handler(Srv, {_,_,_}=Handler) when is_pid(Srv) ->
    gen_server:call(Srv, {'remove_termination_handler', Handler});
remove_termination_handler(Call, {_,_,_}=Handler) ->
    remove_termination_handler(cf_exe_pid(Call), Handler).

-spec stop(kapps_call:call() | pid()) -> 'ok'.
stop(Srv) ->
    stop(Srv, 'undefined').

-spec stop(kapps_call:call() | pid(), kz_term:api_ne_binary()) -> 'ok'.
stop(Srv, Cause) when is_pid(Srv) ->
    gen_server:cast(Srv, {'stop', Cause});
stop(Call, Cause) ->
    Srv = cf_exe_pid(Call),
    stop(Srv, Cause).

-spec hard_stop(kapps_call:call() | pid()) -> 'ok'.
hard_stop(Srv) when is_pid(Srv) ->
    gen_server:cast(Srv, 'hard_stop');
hard_stop(Call) ->
    Srv = cf_exe_pid(Call),
    hard_stop(Srv).

-spec transfer(kapps_call:call() | pid()) -> 'ok'.
transfer(Srv) when is_pid(Srv) ->
    gen_server:cast(Srv, 'transfer');
transfer(Call) ->
    Srv = cf_exe_pid(Call),
    transfer(Srv).

-spec control_usurped(kapps_call:call() | pid()) -> 'ok'.
control_usurped(Srv) when is_pid(Srv) ->
    gen_server:cast(Srv, 'control_usurped');
control_usurped(Call) ->
    Srv = cf_exe_pid(Call),
    control_usurped(Srv).

-spec channel_destroyed(kapps_call:call() | pid()) -> 'ok'.
channel_destroyed(Srv) ->
    channel_destroyed(Srv, 'undefined').

-spec channel_destroyed(kapps_call:call() | pid(), kz_term:api_object()) -> 'ok'.
channel_destroyed(Srv, JObj) when is_pid(Srv) ->
    gen_server:cast(Srv, {'channel_destroyed', JObj});
channel_destroyed(Call, JObj) ->
    Srv = cf_exe_pid(Call),
    channel_destroyed(Srv, JObj).

-spec stop_on_destroy(kapps_call:call() | pid()) -> 'ok'.
stop_on_destroy(Srv) when is_pid(Srv) ->
    gen_server:cast(Srv, 'stop_on_destroy');
stop_on_destroy(Call) ->
    Srv = cf_exe_pid(Call),
    stop_on_destroy(Srv).

-spec continue_on_destroy(kapps_call:call() | pid()) -> 'ok'.
continue_on_destroy(Srv) when is_pid(Srv) ->
    gen_server:cast(Srv, 'continue_on_destroy');
continue_on_destroy(Call) ->
    Srv = cf_exe_pid(Call),
    continue_on_destroy(Srv).

-spec callid_update(kz_term:ne_binary(), kapps_call:call() | pid()) -> 'ok'.
callid_update(CallId, Srv) when is_pid(Srv) ->
    gen_server:cast(Srv, {'callid_update', CallId});
callid_update(CallId, Call) ->
    Srv = cf_exe_pid(Call),
    callid_update(CallId, Srv).

-spec is_channel_destroyed(kapps_call:call() | pid()) -> boolean().
is_channel_destroyed(Srv) when is_pid(Srv) ->
    gen_server:call(Srv, 'is_channel_destroyed');
is_channel_destroyed(Call) ->
    Srv = cf_exe_pid(Call),
    is_channel_destroyed(Srv).

-spec callid(kapps_call:call() | pid()) -> kz_term:ne_binary().
callid(Srv) when is_pid(Srv) ->
    CallId = gen_server:call(Srv, 'callid', 1000),
    kz_log:put_callid(CallId),
    CallId;
callid(Call) ->
    Srv = cf_exe_pid(Call),
    callid(Srv).

-spec callid(kz_term:api_binary(), kapps_call:call() | pid()) -> kz_term:ne_binary().
callid(_, Call) ->
    callid(Call).

-spec queue_name(kapps_call:call() | pid()) -> kz_term:ne_binary().
queue_name(Srv) when is_pid(Srv) ->
    gen_server:call(Srv, 'queue_name');
queue_name(Call) ->
    Srv = cf_exe_pid(Call),
    queue_name(Srv).

-spec control_queue(kapps_call:call() | pid()) -> kz_term:ne_binary().
control_queue(Srv) when is_pid(Srv) -> gen_server:call(Srv, 'control_queue_name');
control_queue(Call) -> control_queue(cf_exe_pid(Call)).

-spec control_queue(kz_term:api_binary(), kapps_call:call() | pid()) -> kz_term:ne_binary().
control_queue(_, Call) -> control_queue(Call).

-spec get_branch_keys(kapps_call:call() | pid()) -> {'branch_keys', kz_json:keys()}.
get_branch_keys(Srv) when is_pid(Srv) ->
    gen_server:call(Srv, 'get_branch_keys');
get_branch_keys(Call) ->
    Srv = cf_exe_pid(Call),
    get_branch_keys(Srv).

-spec get_all_branch_keys(kapps_call:call() | pid()) ->
          {'branch_keys', kz_json:path()}.
get_all_branch_keys(Srv) when is_pid(Srv) ->
    gen_server:call(Srv, {'get_branch_keys', 'all'});
get_all_branch_keys(Call) ->
    Srv = cf_exe_pid(Call),
    get_all_branch_keys(Srv).

-spec attempt(kapps_call:call() | pid()) ->
          {'attempt_resp', 'ok'} |
          {'attempt_resp', {'error', any()}}.
attempt(Srv) -> attempt(?DEFAULT_CHILD_KEY, Srv).

-spec attempt(kz_term:ne_binary(), kapps_call:call() | pid()) ->
          {'attempt_resp', 'ok'} |
          {'attempt_resp', {'error', any()}}.
attempt(Key, Srv) when is_pid(Srv) ->
    gen_server:call(Srv, {'attempt', Key});
attempt(Key, Call) ->
    Srv = cf_exe_pid(Call),
    attempt(Key, Srv).

-spec wildcard_is_empty(kapps_call:call() | pid()) -> boolean().
wildcard_is_empty(Srv) when is_pid(Srv) ->
    gen_server:call(Srv, 'wildcard_is_empty');
wildcard_is_empty(Call) ->
    Srv = cf_exe_pid(Call),
    wildcard_is_empty(Srv).

-spec amqp_send(pid() | kapps_call:call(), kz_term:api_terms(), kz_amqp_worker:publish_fun()) -> 'ok'.
amqp_send(Srv, API, PubFun) when is_pid(Srv), is_function(PubFun, 1) ->
    gen_server:cast(Srv, {'amqp_send', API, PubFun});
amqp_send(Call, API, PubFun) when is_function(PubFun, 1) ->
    amqp_send(cf_exe_pid(Call), API, PubFun).

-spec amqp_call(pid() | kapps_call:call(), kz_term:api_terms(), kz_amqp_worker:publish_fun(), kz_amqp_worker:validate_fun()) -> 'ok'.
amqp_call(Srv, API, PubFun, VerifyFun) when is_pid(Srv), is_function(PubFun, 1) ->
    gen_server:call(Srv, {'amqp_call', API, PubFun, VerifyFun});
amqp_call(Call, API, PubFun, VerifyFun) when is_function(PubFun, 1) ->
    amqp_call(cf_exe_pid(Call), API, PubFun, VerifyFun).

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server.
%% @end
%%------------------------------------------------------------------------------
-spec init(term()) -> 'ignore'.
init(_) -> 'ignore'.

-spec init_cf_exe(pid(), map()) -> 'ok'.
init_cf_exe(Pid, #{request := JObj
                  ,props := Props
                  ,callback := Fun
                  }) ->
    proc_lib:init_ack(Pid, {'ok', self()}),
    kz_amqp_channel:consumer_channel(props:get_value('channel', Props)),
    CallId = kz_api:call_id(JObj),
    _ = kz_hooks:bind_call_id(CallId),
    case Fun(JObj, Props) of
        {'ok', Call} ->
            process_flag('trap_exit', 'true'),
            State = #state{call=Call
                          ,branch_count = ?MAX_BRANCH_COUNT
                          ,queue = kapps_call:controller_queue(Call)
                          },
            gen_server:cast(self(), 'initialize'),
            gen_server:enter_loop(?MODULE, [], State);
        _Other -> lager:debug("not executing callflow => ~p", [_Other])
    end.

%%------------------------------------------------------------------------------
%% @doc Handling call messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call('is_channel_destroyed', _From, State) ->
    {'reply', State#state.destroyed, State};
handle_call({'update_call', Routines}, _From, #state{call=Call}=State) ->
    NewCall = kapps_call:exec(Routines, Call),
    {'reply', NewCall, State#state{call=NewCall}};
handle_call({'set_call', Call}, _From, State) ->
    {'reply', Call, State#state{call=Call}};
handle_call('get_call', _From, #state{call=Call}=State) ->
    {'reply', {'ok', Call}, State};
handle_call('callid', _From, #state{call=Call}=State) ->
    {'reply', kapps_call:call_id_direct(Call), State};
handle_call('queue_name', _From, #state{queue=Q}=State) ->
    {'reply', Q, State};
handle_call('control_queue_name', _From, #state{call=Call}=State) ->
    {'reply', kapps_call:control_queue_direct(Call), State};
handle_call('get_branch_keys', _From, #state{flow = Flow}=State) ->
    Children = kz_json:get_value(<<"children">>, Flow, kz_json:new()),
    Reply = {'branch_keys', lists:delete(?DEFAULT_CHILD_KEY, kz_json:get_keys(Children))},
    {'reply', Reply, State};
handle_call({'get_branch_keys', 'all'}, _From, #state{flow = Flow}=State) ->
    Children = kz_json:get_value(<<"children">>, Flow, kz_json:new()),
    Reply = {'branch_keys', kz_json:get_keys(Children)},
    {'reply', Reply, State};
handle_call({'attempt', Key}, _From, #state{flow=Flow}=State) ->
    case kz_json:get_ne_value([<<"children">>, Key], Flow) of
        'undefined' ->
            lager:info("attempted 'undefined' child ~s", [Key]),
            Reply = {'attempt_resp', {'error', 'empty'}},
            {'reply', Reply, State};
        NewFlow ->
            lager:info("branching to attempted child ~s", [Key]),
            Reply = {'attempt_resp', 'ok'},
            {'reply', Reply, launch_cf_module(State#state{flow = NewFlow})}
    end;
handle_call('wildcard_is_empty', _From, #state{flow = Flow}=State) ->
    case kz_json:get_json_value([<<"children">>, ?DEFAULT_CHILD_KEY], Flow) of
        'undefined' -> {'reply', 'true', State};
        ChildFlow -> {'reply', kz_json:is_empty(ChildFlow), State}
    end;
handle_call({'next', Key}, _From, #state{flow=Flow}=State) ->
    {'reply'
    ,kz_json:get_first_defined([[<<"children">>, Key]
                               ,[<<"children">>, ?DEFAULT_CHILD_KEY]
                               ]
                              ,Flow
                              )
    ,State
    };
handle_call({'amqp_call', API, PubFun, VerifyFun}, _From, #state{queue=Q}=State) ->
    Reply = amqp_call_message(API, PubFun, VerifyFun, Q),
    {'reply', Reply, State};
handle_call({'add_termination_handler', {_M, _F, _Args}=H}
           ,_From
           ,#state{termination_handlers=Handlers}=State
           ) ->
    {'reply', 'ok', State#state{termination_handlers=lists:usort([H | Handlers])}};
handle_call({'remove_termination_handler', {_M, _F, _Args}=H}
           ,_From
           ,#state{termination_handlers=Handlers}=State
           ) ->
    {'reply', 'ok', State#state{termination_handlers=lists:delete(H, Handlers)}};
handle_call('status', _From, #state{status=Status}=State) ->
    {'reply', Status, State};
handle_call(_Request, _From, State) ->
    lager:warning("unhandled request in call: ~p : ~p", [_Request, _From]),
    Reply = {'error', 'unimplemented'},
    {'reply', Reply, State}.

%%------------------------------------------------------------------------------
%% @doc Handling cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast({'set_call', Call}, State) ->
    {'noreply', State#state{call=Call}};
handle_cast({'continue', _}, #state{stop_on_destroy='true'
                                   ,destroyed='true'
                                   }=State) ->
    lager:info("channel no longer active, not continuing"),
    hard_stop(self()),
    {'noreply', State};
handle_cast({'continue', Key}, #state{flow=Flow}=State) ->
    lager:info("continuing to child '~s'", [Key]),

    case kz_json:get_value([<<"children">>, Key], Flow) of
        'undefined' when Key =:= ?DEFAULT_CHILD_KEY ->
            lager:info("wildcard child does not exist, we are lost... hanging up"),
            stop(self()),
            {'noreply', State};
        'undefined' ->
            lager:info("requested child '~s' does not exist, trying wild card", [Key]),
            continue(self()),
            {'noreply', State};
        NewFlow ->
            {'noreply', launch_cf_module(State#state{flow=NewFlow})}
    end;
handle_cast({'stop', _Cause}, #state{stop_on_destroy='true'
                                    ,destroyed='true'
                                    }=State) ->
    lager:info("instructed to stop after channel already destroyed, ignoring cause: ~p", [_Cause]),
    {'stop', 'normal', State};
handle_cast({'stop', 'undefined'}, #state{flows=[]}=State) ->
    lager:info("instructed to stop and no flows left"),
    {'stop', 'normal', State};
handle_cast({'stop', Cause}, #state{flows=[]
                                   ,call=Call
                                   }=State) ->
    hangup_call(Call, Cause),
    lager:info("sent call hangup: ~s", [Cause]),
    {'noreply', State};
handle_cast({'stop', _Cause}, #state{flows=[Flow|Flows]}=State) ->
    {'noreply', launch_cf_module(State#state{flow=Flow, flows=Flows})};
handle_cast('hard_stop', State) ->
    lager:info("instructed to hard_stop"),
    {'stop', 'normal', State};
handle_cast('transfer', State) ->
    {'stop', {'shutdown', 'transfer'}, State};
handle_cast('control_usurped', State) ->
    {'stop', {'shutdown', 'control_usurped'}, State};
handle_cast({'channel_destroyed', JObj}, #state{stop_on_destroy='true'
                                               ,cf_module_pid='undefined'
                                               }=State) ->
    lager:info("recv channel destroyed, going down"),
    {'stop', 'normal', State#state{hangup_info=JObj}};
handle_cast({'channel_destroyed', JObj}, State) ->
    lager:info("recv channel destroyed, noting but staying up"),
    {'noreply', State#state{destroyed='true', hangup_info=JObj}};
handle_cast('stop_on_destroy', State) ->
    {'noreply', State#state{stop_on_destroy='true'}};
handle_cast('continue_on_destroy', State) ->
    {'noreply', State#state{stop_on_destroy='false'}};
handle_cast({'continue_with_flow', NewFlow}, State) ->
    lager:info("callflow has been reset"),
    {'noreply', launch_cf_module(State#state{flow=NewFlow})};
handle_cast({'branch', _NewFlow}, #state{branch_count=BC}=State) when BC =< 0 ->
    lager:warning("callflow exceeded max branch count, terminating"),
    {'stop', 'normal', State};
handle_cast({'branch', NewFlow}, #state{flow=Flow
                                       ,flows=Flows
                                       ,branch_count=BC
                                       }=State) ->
    lager:info("callflow has been branched"),
    case kz_json:get_ne_value([<<"children">>, ?DEFAULT_CHILD_KEY], Flow) of
        'undefined' ->
            {'noreply', launch_cf_module(State#state{flow=NewFlow
                                                    ,branch_count=BC-1
                                                    })};
        PrevFlow ->
            {'noreply', launch_cf_module(State#state{flow=NewFlow
                                                    ,flows=[PrevFlow|Flows]
                                                    ,branch_count=BC-1
                                                    }
                                        )
            }
    end;
handle_cast({'callid_update', NewCallId}, #state{call=Call}=State) ->
    kz_log:put_callid(NewCallId),
    PrevCallId = kapps_call:call_id_direct(Call),
    lager:info("updating callid to ~s (from ~s), catch you on the flip side", [NewCallId, PrevCallId]),
    _ = kz_hooks:unbind_call_id(PrevCallId),
    _ = kz_hooks:bind_call_id(NewCallId),
    {'noreply', State#state{call=kapps_call:set_call_id(NewCallId, Call)}};
handle_cast({'add_event_listener', {Mod, Args}}, #state{call=Call}=State) ->
    _EvtL = cf_util:start_event_listener(Call, Mod, Args),
    lager:debug("started event listener: ~p", [_EvtL]),
    {'noreply', State};
handle_cast('initialize', State) ->
    initialize(State);
handle_cast({'amqp_send', API, PubFun}, #state{queue=Q}=State) ->
    amqp_send_message(API, PubFun, Q),
    {'noreply', State};
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
handle_info({'DOWN', Ref, 'process', Pid, 'normal'}, #state{cf_module_pid={Pid, Ref}
                                                           ,call=Call
                                                           }=State) ->
    erlang:demonitor(Ref, ['flush']),
    lager:debug("cf module ~s down normally", [kapps_call:kvs_fetch('cf_last_action', Call)]),
    {'noreply', State#state{cf_module_pid='undefined'}};
handle_info({'DOWN', Ref, 'process', Pid, 'killed'}, #state{cf_module_pid={Pid, Ref}
                                                           ,call=Call
                                                           }=State) ->
    erlang:demonitor(Ref, ['flush']),
    lager:debug("cf module ~s down normally", [kapps_call:kvs_fetch('cf_last_action', Call)]),
    {'noreply', State#state{cf_module_pid='undefined'}};
handle_info({'DOWN', Ref, 'process', Pid, _Reason}, #state{cf_module_pid={Pid, Ref}
                                                          ,call=Call
                                                          }=State) ->
    erlang:demonitor(Ref, ['flush']),
    LastAction = kapps_call:kvs_fetch('cf_last_action', Call),
    lager:error("action ~s died unexpectedly: ~p", [LastAction, _Reason]),
    continue(self()),
    {'noreply', State#state{cf_module_pid='undefined'}};
handle_info({'DOWN', _Ref, 'process', _Pid, 'normal'}, State) ->
    {'noreply', State};
handle_info({'EXIT', Pid, 'normal'}, #state{cf_module_pid={Pid, Ref}
                                           ,call=Call
                                           }=State) ->
    erlang:demonitor(Ref, ['flush']),
    lager:debug("cf module ~s down normally", [kapps_call:kvs_fetch('cf_last_action', Call)]),
    {'noreply', State#state{cf_module_pid='undefined'}};
handle_info({'EXIT', Pid, 'killed'}, #state{cf_module_pid={Pid, Ref}
                                           ,call=Call
                                           }=State) ->
    erlang:demonitor(Ref, ['flush']),
    lager:debug("cf module ~s killed normally", [kapps_call:kvs_fetch('cf_last_action', Call)]),
    {'noreply', State#state{cf_module_pid='undefined'}};
handle_info({'EXIT', Pid, _Reason}, #state{cf_module_pid={Pid, Ref}
                                          ,call=Call
                                          }=State) ->
    erlang:demonitor(Ref, ['flush']),
    LastAction = kapps_call:kvs_fetch('cf_last_action', Call),
    lager:error("action ~s died unexpectedly: ~p", [LastAction, _Reason]),
    continue(self()),
    {'noreply', State#state{cf_module_pid='undefined'}};
handle_info({'EXIT', Pid, 'normal'}, #state{cf_module_old_pid={Pid, Ref}
                                           ,call=Call
                                           }=State) ->
    erlang:demonitor(Ref, ['flush']),
    lager:debug("cf module ~s down normally", [kapps_call:kvs_fetch('cf_old_action', Call)]),
    {'noreply', State#state{cf_module_old_pid='undefined'}};
handle_info({'EXIT', Pid, 'killed'}, #state{cf_module_old_pid={Pid, Ref}
                                           ,call=Call
                                           }=State) ->
    erlang:demonitor(Ref, ['flush']),
    lager:debug("cf module ~s killed normally", [kapps_call:kvs_fetch('cf_old_action', Call)]),
    {'noreply', State#state{cf_module_old_pid='undefined'}};
handle_info({'EXIT', Pid, _Reason}, #state{cf_module_old_pid={Pid, Ref}
                                          ,call=Call
                                          }=State) ->
    erlang:demonitor(Ref, ['flush']),
    LastAction = kapps_call:kvs_fetch('cf_old_action', Call),
    lager:error("action ~s died unexpectedly: ~p", [LastAction, _Reason]),
    {'noreply', State#state{cf_module_old_pid='undefined'}};
handle_info({'amqp_return', _JObj, _Returned} = Msg, #state{cf_module_pid=PidRef
                                                           ,call=Call
                                                           } = State) ->
    Others = kapps_call:kvs_fetch('cf_event_pids', [], Call),
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
handle_event(JObj, #state{cf_module_pid=PidRef
                         ,call=Call
                         ,self=Self
                         }) ->
    CallId = kapps_call:call_id_direct(Call),
    Others = kapps_call:kvs_fetch('cf_event_pids', [], Call),
    Notify = case get_pid(PidRef) of
                 'undefined' -> Others;
                 ModPid -> [ModPid | Others]
             end,

    case {kz_util:get_event_type(JObj), kz_call_event:call_id(JObj)} of
        {{<<"call_event">>, <<"CHANNEL_DESTROY">>}, CallId} ->
            handle_channel_destroyed(Self, Notify, JObj);
        {{<<"call_event">>, <<"CHANNEL_DISCONNECTED">>}, CallId} ->
            handle_channel_destroyed(Self, Notify, JObj);
        {{<<"call_event">>, <<"CHANNEL_TRANSFEREE">>}, _} ->
            handle_channel_transfer(Call, JObj);
        {{<<"call_event">>, <<"CHANNEL_REPLACED">>}, _} ->
            handle_channel_replaced(Call, JObj, Notify);
        {{<<"call_event">>, <<"CHANNEL_DIRECT">>}, _} ->
            handle_channel_direct(Call, JObj, Notify),
            relay_message(Others, JObj);
        {{<<"call_event">>, <<"CHANNEL_BRIDGE">>}, _} ->
            handle_channel_bridged(Self, Notify, JObj, Call);
        {{<<"call_event">>, <<"CHANNEL_PIVOT">>}, CallId} ->
            handle_channel_pivoted(Self, PidRef, JObj, Call);
        {{<<"call_event">>, <<"usurp_control">>}, CallId} ->
            handle_usurp(Self, Call, JObj);
        {{<<"error">>, _}, _} ->
            handle_error(CallId, Notify, JObj);
        {_Evt, CallId} ->
            lager:debug_unsafe("relaying ~p to ~p", [_Evt, Notify]),
            relay_message(Notify, JObj);
        {{_Cat, _Name}, _Else} when Others =:= [] ->
            lager:info("received ~s (~s) from call ~s while relaying for ~s"
                      ,[_Cat, _Name, _Else, CallId]
                      );
        {_Evt, _Else} ->
            lager:info("the others want to know about ~p", [_Evt]),
            relay_message(Others, JObj)
    end,
    'ok'.

%%------------------------------------------------------------------------------
%% @doc This function is called by a `gen_server' when it is about to
%% terminate. It should be the opposite of `Module:init/1' and do any
%% necessary cleaning up. When it returns, the `gen_server' terminates
%% with Reason. The return value is ignored.
%%
%% @end
%%------------------------------------------------------------------------------
-spec terminate(any(), state()) -> 'ok'.
terminate({'shutdown', 'transfer'}, _State) ->
    lager:info("callflow execution has been transferred");
terminate({'shutdown', 'control_usurped'}, _State) ->
    lager:info("the call has been usurped by an external process");
terminate(_Reason, #state{call=Call
                         ,cf_module_pid='undefined'
                         ,hangup_info=HangupInfo
                         ,termination_handlers=DestroyHandlers
                         }=State) ->
    lager:info("callflow execution has been stopped: ~p", [_Reason]),
    maybe_hangup_call(State),
    run_destroy_handlers(Call, HangupInfo, DestroyHandlers);
terminate(_Reason, #state{call=Call
                         ,cf_module_pid={Pid, _}
                         ,hangup_info=HangupInfo
                         ,termination_handlers=DestroyHandlers
                         }=State) ->
    lager:info("callflow execution has been stopped: ~p", [_Reason]),
    exit(Pid, 'kill'),
    maybe_hangup_call(State),
    run_destroy_handlers(Call, HangupInfo, DestroyHandlers).

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
-spec launch_cf_module(state()) -> state().
launch_cf_module(#state{flow=?EMPTY_JSON_OBJECT}=State) ->
    lager:debug("no flow left to launch, maybe stopping"),
    stop(self()),
    State;
launch_cf_module(#state{call=Call
                       ,flow=Flow
                       }=State) ->
    case cf_util:token_check(Call, Flow) of
        'true' ->
            do_launch_cf_module(State, find_cf_module(Flow));
        'false' ->
            lager:debug("call does not have enough tokens to proceed, stopping execution"),
            stop(self()),
            State
    end.

-spec do_launch_cf_module(state(), atom()) -> state().
do_launch_cf_module(#state{call=Call
                          ,cf_module_pid=OldPidRef
                          }=State
                   ,'undefined'
                   ) ->
    lager:error("unknown callflow action, reverting to last action"),
    continue(self()),
    OldAction = kapps_call:kvs_fetch('cf_last_action', Call),
    State#state{cf_module_pid='undefined'
               ,cf_module_old_pid=OldPidRef
               ,call=update_actions(OldAction, Call)
               };
do_launch_cf_module(#state{call=Call
                          ,flow=Flow
                          ,cf_module_pid=OldPidRef
                          }=State
                   ,{Where, Action}
                   ) ->
    Data = kz_json:get_json_value(<<"data">>, Flow, kz_json:new()),
    lager:info("moving to action '~s'", [Action]),
    %% Actions need to be updated before the module is spawned, in case
    %% the module calls cf_exe:set_call/1 - that would undo the later
    %% old_action/last_action update
    Call1 = update_actions(Action, Call),
    PidRef = spawn_cf_module(Where, Action, Data, Call1),
    link(get_pid(PidRef)),
    State#state{cf_module_pid=PidRef
               ,cf_module_old_pid=OldPidRef
               ,call=Call1
               }.

-spec find_cf_module(kz_json:object()) -> 'undefined' | {'local' | 'remote', atom()}.
find_cf_module(Flow) ->
    Module = kz_json:get_ne_binary_value(<<"module">>, Flow),
    ModuleBin = <<"cf_", Module/binary>>,
    Data = kz_json:get_json_value(<<"data">>, Flow, kz_json:new()),
    CFModule = kz_term:to_atom(ModuleBin, 'true'),
    IsExported = kz_module:is_exported(CFModule, 'handle', 2),
    IsRemote = kz_json:is_true(<<"remote_execution">>, Flow),
    SkipModule = kz_json:is_true(<<"skip_module">>, Data, 'false'),
    case (IsExported
          orelse IsRemote
         )
        andalso (not SkipModule)
    of
        'true' when IsRemote -> {'remote', kz_term:to_atom(Module, 'true')};
        'true' -> {'local', CFModule};
        'false' ->
            lager:debug("skipping callflow module ~s (handle exported: ~s skip_module: ~s)"
                       ,[CFModule, IsExported, SkipModule]),
            'undefined'
    end.

-spec update_actions(atom(), kapps_call:call()) -> kapps_call:call().
update_actions(Action, Call) ->
    OldAction = kapps_call:kvs_fetch('cf_last_action', Call),
    Routines = [{fun kapps_call:kvs_store/3, 'cf_old_action', OldAction}
               ,{fun kapps_call:kvs_store/3, 'cf_last_action', Action}
               ],
    kapps_call:exec(Routines, Call).

%%------------------------------------------------------------------------------
%% @doc Helper function to spawn a linked callflow module, from the entry
%% point 'handle' having set the callid on the new process first.
%% @end
%%------------------------------------------------------------------------------
-spec spawn_cf_module(atom(), atom(), kz_json:object(), kapps_call:call()) -> kz_term:pid_ref().
spawn_cf_module('local', CFModule, Data, Call0) ->
    Call = kapps_call:kvs_store('context-source', CFModule, Call0),
    spawn_cf_module(CFModule, Data, Call);
spawn_cf_module('remote', CFModule, Data, Call0) ->
    Call = kapps_call:kvs_store('remote-action', CFModule, Call0),
    spawn_cf_module('cf_remote_action', Data, Call).

-spec spawn_cf_module(atom(), kz_json:object(), kapps_call:call()) -> kz_term:pid_ref().
spawn_cf_module(CFModule, Data, Call) ->
    AMQPConsumer = kz_amqp_channel:consumer_pid(),
    AMQPChannel = kz_amqp_channel:consumer_channel(),
    kz_process:spawn_monitor(fun cf_module_task/5, [CFModule, Data, Call, AMQPConsumer, AMQPChannel]).

-spec cf_module_task(atom(), kz_json:object(), kapps_call:call(), pid(), pid()) -> any().
cf_module_task(CFModule, Data, Call, AMQPConsumer, AMQPChannel) ->
    _ = kz_amqp_channel:consumer_channel(AMQPChannel),
    _ = kz_amqp_channel:consumer_pid(AMQPConsumer),
    kz_log:put_callid(kapps_call:call_id_direct(Call)),
    try CFModule:handle(Data, Call)
    catch

        'exit':'normal' ->
            lager:info("action ~s finished", [CFModule]);
        ?STACKTRACE(_E, R, ST)
        lager:info("action ~s died unexpectedly (~s): ~p", [CFModule, _E, R]),
        kz_log:log_stacktrace(ST),
        throw(R)
        end.

%%------------------------------------------------------------------------------
%% @doc Unlike the kapps_call_command this send command does not call the
%% functions of this module to form the headers, nor does it set
%% the reply queue.  Used when this module is terminating to send
%% a hangup command without relying on the (now terminated) cf_exe.
%% @end
%%------------------------------------------------------------------------------
-spec amqp_send_message(kz_term:api_terms(), kz_amqp_worker:publish_fun(), kz_term:ne_binary()) -> 'ok'.
amqp_send_message(API, PubFun, AMQPQueue) ->
    Req = add_server_id(AMQPQueue, API),
    PubFun(Req).

-spec amqp_call_message(kz_term:api_terms(), kz_amqp_worker:publish_fun(), kz_amqp_worker:validate_fun(), kz_term:ne_binary()) ->
          kz_amqp_worker:request_return().
amqp_call_message(API, PubFun, VerifyFun, Q) ->
    Routines = [{fun add_server_id/2, Q}
               ,fun add_message_id/1
               ],
    Request = kz_api:exec(Routines, API),
    %% TODO
    %% Remove kz_amqp_worker
    kz_amqp_worker:call(Request, PubFun, VerifyFun).

-spec add_server_id(kz_term:ne_binary(), kz_term:api_terms()) -> kz_term:api_terms().
add_server_id(AMQPQueue, API) when is_list(API) ->
    [{?KEY_SERVER_ID, AMQPQueue} | props:delete(?KEY_SERVER_ID, API)];
add_server_id(AMQPQueue, API) ->
    kz_json:set_value(?KEY_SERVER_ID, AMQPQueue, API).

-spec add_message_id(kz_term:api_terms()) -> kz_term:api_terms().
add_message_id(API) when is_list(API) ->
    [{<<"Msg-ID">>, kz_binary:rand_hex(16)} | props:delete(<<"Msg-ID">>, API)];
add_message_id(API) ->
    kz_json:set_value(<<"Msg-ID">>, kz_binary:rand_hex(16), API).

-spec log_call_information(kapps_call:call()) -> 'ok'.
log_call_information(Call) ->
    lager:info("executing callflow ~s", [kapps_call:kvs_fetch('cf_flow_id', Call)]),
    lager:info("account id ~s", [kapps_call:account_id(Call)]),
    lager:info("request ~s", [kapps_call:request(Call)]),
    lager:info("to ~s", [kapps_call:to(Call)]),
    lager:info("from ~s", [kapps_call:from(Call)]),
    lager:info("CID ~s ~s", [kapps_call:caller_id_name(Call), kapps_call:caller_id_number(Call)]),
    case kapps_call:inception(Call) of
        'undefined' -> lager:info("inception onnet: using attributes for an internal call", []);
        _Else -> lager:info("inception ~s: using attributes for an external call", [_Else])
    end,
    lager:info("authorizing id ~s", [kapps_call:authorizing_id(Call)]).

-spec handle_channel_destroyed(pid(), kz_term:pids(), kz_json:object()) -> 'ok'.
handle_channel_destroyed(Self, Notify, JObj) ->
    {Cause, Code} = kapps_util:get_call_termination_reason(JObj),
    Prop = [{<<"Disposition">>, kz_call_event:disposition(JObj)}
           ,{<<"Hangup-Cause">>, Cause}
           ,{<<"Hangup-Code">>, Code}
           ],
    channel_destroyed(Self, kz_json:from_list(Prop)),
    relay_message(Notify, JObj).

-spec handle_channel_transfer(kapps_call:call(), kz_call_event:doc()) -> 'ok'.
handle_channel_transfer(Call, CallEvent) ->
    OrgFetchId = kapps_call:custom_channel_var(<<"Fetch-ID">>, Call),
    NewFetchId = kz_call_event:custom_channel_var(CallEvent, <<"Fetch-ID">>),
    case OrgFetchId =:= NewFetchId of
        'true'  -> transfer(Call);
        'false' -> 'ok'
    end.

-spec handle_channel_replaced(kapps_call:call(), kz_call_event:doc(), kz_term:pids()) -> 'ok'.
handle_channel_replaced(Call, CallEvent, Notify) ->
    OrgFetchId = kapps_call:custom_channel_var(<<"Fetch-ID">>, Call),
    NewFetchId = kz_call_event:custom_channel_var(CallEvent, <<"Fetch-ID">>),
    case OrgFetchId =:= NewFetchId of
        'true' ->
            ReplacedBy = kz_call_event:replaced_by(CallEvent),
            callid_update(ReplacedBy, Call),
            relay_message(Notify, CallEvent);
        'false' -> 'ok'
    end.

-spec handle_channel_direct(kapps_call:call(), kz_call_event:doc(), kz_term:pids()) -> 'ok'.
handle_channel_direct(Call, CallEvent, Notify) ->
    OrgFetchId = kapps_call:custom_channel_var(<<"Fetch-ID">>, Call),
    NewFetchId = kz_call_event:custom_channel_var(CallEvent, <<"Fetch-ID">>),
    case OrgFetchId =:= NewFetchId of
        'true' ->
            ReplacedBy = kz_call_event:connecting_b_leg_id(CallEvent),
            callid_update(ReplacedBy, Call),
            relay_message(Notify, CallEvent);
        'false' -> 'ok'
    end.

-spec handle_channel_bridged(pid(), kz_term:pids(), kz_call_event:doc(), kapps_call:call()) -> 'ok'.
handle_channel_bridged(Self, Notify, CallEvent, Call) ->
    gen_server:cast(Self, {'set_call', kapps_call:set_call_bridged('true', Call)}),
    relay_message(Notify, CallEvent).

-spec handle_usurp(pid(), kapps_call:call(), kz_call_event:doc()) -> 'ok'.
handle_usurp(Self, Call, CallEvent) ->
    OrgFetchId = kapps_call:custom_channel_var(<<"Fetch-ID">>, Call),
    NewFetchId = kz_json:get_ne_binary_value(<<"Fetch-ID">>, CallEvent),
    case OrgFetchId =:= NewFetchId of
        'false' -> control_usurped(Self);
        'true'  -> 'ok'
    end.

-spec handle_error(kz_term:ne_binary(), kz_term:pids(), kz_json:object()) -> 'ok'.
handle_error(CallId, Notify, JObj) ->
    case kz_json:get_ne_binary_value(<<"Call-ID">>, JObj, kz_json:get_ne_binary_value([<<"Request">>, <<"Call-ID">>], JObj)) of
        CallId -> relay_message(Notify, JObj);
        'undefined' -> relay_message(Notify, JObj);
        _Else -> 'ok'
    end.

-spec relay_message(kz_term:pids(), kz_json:object() | {'amqp_return', kz_json:object(), kz_json:object()}) -> 'ok'.
relay_message(Notify, Message) ->
    _ = [kapps_call_command:relay_event(Pid, Message)
         || Pid <- Notify,
            is_pid(Pid)
        ],
    'ok'.

-spec run_destroy_handlers(kapps_call:call(), kz_json:object(), termination_handlers()) -> 'ok'.
run_destroy_handlers(Call, JObj, Handlers) ->
    _ = [erlang:apply(M, F, [Call, JObj | Args]) || {M, F, Args} <- Handlers],
    'ok'.

-spec get_pid({pid(), reference()} | 'undefined') -> kz_term:api_pid().
get_pid({Pid, _}) when is_pid(Pid) -> Pid;
get_pid(_) -> 'undefined'.

-spec maybe_hangup_call(state()) -> 'ok'.
maybe_hangup_call(#state{call=Call
                        ,hangup_info='undefined'
                        }) ->
    hangup_call(Call, 'undefined');
maybe_hangup_call(#state{}) -> 'ok'.

-spec hangup_call(kapps_call:call(), kz_term:api_ne_binary()) -> 'ok'.
hangup_call(Call, Cause) ->
    kapps_call_command:queued_hangup(kapps_call:clear_helpers(Call), Cause).

-spec handle_channel_pivoted(kz_types:server_ref(), kz_term:api_pid_ref(), kz_call_event:doc(), kapps_call:call()) -> 'ok'.
handle_channel_pivoted(Self, PidRef, JObj, Call) ->
    case kz_json:get_value(<<"Application-Data">>, JObj) of
        'undefined' -> lager:info("no app data to pivot");
        <<FlowBin/binary>> ->
            _ = maybe_stop_action(PidRef),
            lager:debug("pivoting to ~s", [FlowBin]),
            cf_util:flush_control_queue(Call),
            continue_with_flow(kz_json:decode(FlowBin), Self);
        FlowJObj ->
            _ = maybe_stop_action(PidRef),
            lager:debug("pivoting to ~p", [FlowJObj]),
            cf_util:flush_control_queue(Call),
            continue_with_flow(FlowJObj, Self)
    end.

-spec maybe_stop_action(kz_term:api_pid_ref()) -> 'ok'.
maybe_stop_action({Pid, _Ref}) ->
    exit(Pid, 'kill');
maybe_stop_action('undefined') -> 'ok'.

-spec stop_bad_destination(kapps_call:call()) -> 'ok'.
stop_bad_destination(Call) ->
    _ = kapps_call_command:prompt(<<"fault-can_not_be_completed_as_dialed">>, Call),
    stop(Call).

-spec status(kapps_call:call() | pid() | 'undefined') -> callfow_status().
status('undefined') -> 'not_running';
status(Srv) when is_pid(Srv) ->
    gen_server:call(Srv, 'status');
status(Call) ->
    Srv = cf_exe_pid(Call),
    status(Srv).

-spec initialize(state()) -> {'stop', 'normal', state()} |
          {'noreply', state()}.
initialize(#state{call=Call}=State) ->
    initialize(State, Call, kapps_call_events:is_destroyed(Call)).

initialize(State, _Call, 'true') ->
    lager:info("call has terminated before executor finished initializing"),
    {'stop', 'normal', State};
initialize(State, Call, 'false') ->
    log_call_information(Call),
    Flow = kapps_call:kvs_fetch('cf_flow', Call),
    Updaters = [{fun kapps_call:kvs_store/3, 'cf_exe_pid', self()}
               ,{fun kapps_call:call_id_helper/2, fun callid/2}
               ,{fun kapps_call:control_queue_helper/2, fun control_queue/2}
               ],
    CallWithHelpers = kapps_call:exec(Updaters, Call),
    _ = kz_process:spawn(fun cf_singular_call_hooks:maybe_hook_call/1, [CallWithHelpers]),
    {'noreply'
    ,launch_cf_module(State#state{call=CallWithHelpers
                                 ,flow=Flow
                                 ,status='running'
                                 })
    }.
