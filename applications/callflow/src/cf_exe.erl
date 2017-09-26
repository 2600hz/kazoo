%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2017, 2600Hz
%%% @doc
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(cf_exe).
-behaviour(gen_listener).

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
-export([stop/1]).
-export([hard_stop/1]).
-export([transfer/1]).
-export([control_usurped/1]).
-export([channel_destroyed/1]).
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

%% gen_listener callbacks
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,handle_event/2
        ,terminate/2
        ,code_change/3
        ]).

-include("callflow.hrl").
-include_lib("kazoo_stdlib/include/kazoo_json.hrl").

-define(SERVER, ?MODULE).

-define(CALL_SANITY_CHECK, 30000).

-define(RESPONDERS, []).
-define(QUEUE_NAME, <<>>).
-define(QUEUE_OPTIONS, []).
-define(CONSUME_OPTIONS, []).

-define(MAX_BRANCH_COUNT, kapps_config:get_integer(?CF_CONFIG_CAT, <<"max_branch_count">>, 50)).

-record(state, {call = kapps_call:new() :: kapps_call:call()
               ,flow = kz_json:new() :: kz_json:object()
               ,flows = [] :: kz_json:objects()
               ,cf_module_pid :: api_pid_ref()
               ,cf_module_old_pid :: api_pid_ref()
               ,status = <<"sane">> :: ne_binary()
               ,queue :: api_ne_binary()
               ,self = self() :: pid()
               ,stop_on_destroy = 'true' :: boolean()
               ,destroyed = 'false' :: boolean()
               ,branch_count = ?MAX_BRANCH_COUNT :: non_neg_integer()
               ,termination_handlers = [] :: list()
               }).
-type state() :: #state{}.

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts the server
%%--------------------------------------------------------------------
-spec start_link(kapps_call:call()) -> startlink_ret().
start_link(Call) ->
    CallId = kapps_call:call_id(Call),
    Bindings = [{'call', [{'callid', CallId}]}
               ,{'self', []}
               ],
    gen_listener:start_link(?SERVER, [{'responders', ?RESPONDERS}
                                     ,{'bindings', Bindings}
                                     ,{'queue_name', ?QUEUE_NAME}
                                     ,{'queue_options', ?QUEUE_OPTIONS}
                                     ,{'consume_options', ?CONSUME_OPTIONS}
                                     ], [Call]).

-spec get_call(pid() | kapps_call:call()) -> {'ok', kapps_call:call()}.
get_call(Srv) when is_pid(Srv) ->
    gen_server:call(Srv, 'get_call', 1000);
get_call(Call) ->
    Srv = kapps_call:kvs_fetch('consumer_pid', Call),
    get_call(Srv).

-spec set_call(kapps_call:call()) -> 'ok'.
set_call(Call) ->
    Srv = kapps_call:kvs_fetch('consumer_pid', Call),
    gen_server:cast(Srv, {'set_call', Call}).

-spec update_call(kapps_call:call()) -> kapps_call:call().
update_call(Call) ->
    Srv = kapps_call:kvs_fetch('consumer_pid', Call),
    gen_server:call(Srv, {'set_call', Call}).

-spec update_call(kapps_call:call(), list()) -> kapps_call:call().
update_call(Call, Routines) ->
    Srv = kapps_call:kvs_fetch('consumer_pid', Call),
    gen_server:call(Srv, {'update_call', Routines}).

-spec continue(kapps_call:call() | pid()) -> 'ok'.
-spec continue(ne_binary(), kapps_call:call() | pid()) -> 'ok'.
continue(Srv) -> continue(<<"_">>, Srv).

continue(Key, Srv) when is_pid(Srv) ->
    gen_listener:cast(Srv, {'continue', Key});
continue(Key, Call) ->
    Srv = kapps_call:kvs_fetch('consumer_pid', Call),
    continue(Key, Srv).

-spec continue_with_flow(kz_json:object(), kapps_call:call() | pid()) -> 'ok'.
continue_with_flow(Flow, Srv) when is_pid(Srv) ->
    gen_listener:cast(Srv, {'continue_with_flow', Flow});
continue_with_flow(Flow, Call) ->
    Srv = kapps_call:kvs_fetch('consumer_pid', Call),
    continue_with_flow(Flow, Srv).

-spec branch(kz_json:object(), kapps_call:call() | pid()) -> 'ok'.
branch(Flow, Srv) when is_pid(Srv) ->
    gen_listener:cast(Srv, {'branch', Flow});
branch(Flow, Call) ->
    Srv = kapps_call:kvs_fetch('consumer_pid', Call),
    branch(Flow, Srv).

-spec next(kapps_call:call() | pid()) -> api_object().
-spec next(ne_binary(), kapps_call:call() | pid()) -> api_object().
next(Srv) -> next(<<"_">>, Srv).

next(Key, Srv) when is_pid(Srv) ->
    gen_listener:call(Srv, {'next', Key});
next(Key, Call) ->
    Srv = kapps_call:kvs_fetch('consumer_pid', Call),
    next(Key, Srv).

-spec add_event_listener(kapps_call:call() | pid(), {atom(), list()}) -> 'ok'.
add_event_listener(Srv, {_,_}=SpawnInfo) when is_pid(Srv) ->
    gen_listener:cast(Srv, {'add_event_listener', SpawnInfo});
add_event_listener(Call, {_,_}=SpawnInfo) ->
    add_event_listener(kapps_call:kvs_fetch('consumer_pid', Call), SpawnInfo).

-spec add_termination_handler(kapps_call:call() | pid(), {atom(), atom(), list()}) -> 'ok'.
add_termination_handler(Srv, {_,_,_}=Handler) when is_pid(Srv) ->
    gen_listener:call(Srv, {'add_termination_handler', Handler});
add_termination_handler(Call, {_,_,_}=Handler) ->
    add_termination_handler(kapps_call:kvs_fetch('consumer_pid', Call), Handler).

-spec remove_termination_handler(kapps_call:call() | pid(), {atom(), atom(), list()}) -> 'ok'.
remove_termination_handler(Srv, {_,_,_}=Handler) when is_pid(Srv) ->
    gen_listener:call(Srv, {'remove_termination_handler', Handler});
remove_termination_handler(Call, {_,_,_}=Handler) ->
    remove_termination_handler(kapps_call:kvs_fetch('consumer_pid', Call), Handler).

-spec stop(kapps_call:call() | pid()) -> 'ok'.
stop(Srv) when is_pid(Srv) ->
    gen_listener:cast(Srv, 'stop');
stop(Call) ->
    Srv = kapps_call:kvs_fetch('consumer_pid', Call),
    stop(Srv).

-spec hard_stop(kapps_call:call() | pid()) -> 'ok'.
hard_stop(Srv) when is_pid(Srv) ->
    gen_listener:cast(Srv, 'hard_stop');
hard_stop(Call) ->
    Srv = kapps_call:kvs_fetch('consumer_pid', Call),
    hard_stop(Srv).

-spec transfer(kapps_call:call() | pid()) -> 'ok'.
transfer(Srv) when is_pid(Srv) ->
    gen_listener:cast(Srv, 'transfer');
transfer(Call) ->
    Srv = kapps_call:kvs_fetch('consumer_pid', Call),
    transfer(Srv).

-spec control_usurped(kapps_call:call() | pid()) -> 'ok'.
control_usurped(Srv) when is_pid(Srv) ->
    gen_listener:cast(Srv, 'control_usurped');
control_usurped(Call) ->
    Srv = kapps_call:kvs_fetch('consumer_pid', Call),
    control_usurped(Srv).

-spec channel_destroyed(kapps_call:call() | pid()) -> 'ok'.
channel_destroyed(Srv) when is_pid(Srv) ->
    gen_listener:cast(Srv, 'channel_destroyed');
channel_destroyed(Call) ->
    Srv = kapps_call:kvs_fetch('consumer_pid', Call),
    channel_destroyed(Srv).

-spec stop_on_destroy(kapps_call:call() | pid()) -> 'ok'.
stop_on_destroy(Srv) when is_pid(Srv) ->
    gen_listener:cast(Srv, 'stop_on_destroy');
stop_on_destroy(Call) ->
    Srv = kapps_call:kvs_fetch('consumer_pid', Call),
    stop_on_destroy(Srv).

-spec continue_on_destroy(kapps_call:call() | pid()) -> 'ok'.
continue_on_destroy(Srv) when is_pid(Srv) ->
    gen_listener:cast(Srv, 'continue_on_destroy');
continue_on_destroy(Call) ->
    Srv = kapps_call:kvs_fetch('consumer_pid', Call),
    continue_on_destroy(Srv).

-spec callid_update(ne_binary(), kapps_call:call() | pid()) -> 'ok'.
callid_update(CallId, Srv) when is_pid(Srv) ->
    gen_listener:cast(Srv, {'callid_update', CallId});
callid_update(CallId, Call) ->
    Srv = kapps_call:kvs_fetch('consumer_pid', Call),
    callid_update(CallId, Srv).

-spec is_channel_destroyed(kapps_call:call()) -> boolean().
is_channel_destroyed(Srv) when is_pid(Srv) ->
    gen_listener:call(Srv, 'is_channel_destroyed');
is_channel_destroyed(Call) ->
    Srv = kapps_call:kvs_fetch('consumer_pid', Call),
    is_channel_destroyed(Srv).

-spec callid(kapps_call:call() | pid()) -> ne_binary().
-spec callid(api_binary(), kapps_call:call() | pid()) -> ne_binary().

callid(Srv) when is_pid(Srv) ->
    CallId = gen_server:call(Srv, 'callid', 1000),
    kz_util:put_callid(CallId),
    CallId;
callid(Call) ->
    Srv = kapps_call:kvs_fetch('consumer_pid', Call),
    callid(Srv).

callid(_, Call) ->
    callid(Call).

-spec queue_name(kapps_call:call() | pid()) -> ne_binary().
queue_name(Srv) when is_pid(Srv) ->
    gen_listener:queue_name(Srv);
queue_name(Call) ->
    Srv = kapps_call:kvs_fetch('consumer_pid', Call),
    queue_name(Srv).

-spec control_queue(kapps_call:call() | pid()) -> ne_binary().
-spec control_queue(api_binary(), kapps_call:call() | pid()) -> ne_binary().

control_queue(Srv) when is_pid(Srv) -> gen_listener:call(Srv, 'control_queue_name');
control_queue(Call) -> control_queue(kapps_call:kvs_fetch('consumer_pid', Call)).
control_queue(_, Call) -> control_queue(Call).

-spec get_branch_keys(kapps_call:call() | pid()) -> {'branch_keys', kz_json:keys()}.
get_branch_keys(Srv) when is_pid(Srv) ->
    gen_listener:call(Srv, 'get_branch_keys');
get_branch_keys(Call) ->
    Srv = kapps_call:kvs_fetch('consumer_pid', Call),
    get_branch_keys(Srv).

-spec get_all_branch_keys(kapps_call:call() | pid()) ->
                                 {'branch_keys', kz_json:path()}.
get_all_branch_keys(Srv) when is_pid(Srv) ->
    gen_listener:call(Srv, {'get_branch_keys', 'all'});
get_all_branch_keys(Call) ->
    Srv = kapps_call:kvs_fetch('consumer_pid', Call),
    get_all_branch_keys(Srv).

-spec attempt(kapps_call:call() | pid()) ->
                     {'attempt_resp', 'ok'} |
                     {'attempt_resp', {'error', any()}}.
-spec attempt(ne_binary(), kapps_call:call() | pid()) ->
                     {'attempt_resp', 'ok'} |
                     {'attempt_resp', {'error', any()}}.
attempt(Srv) -> attempt(<<"_">>, Srv).

attempt(Key, Srv) when is_pid(Srv) ->
    gen_listener:call(Srv, {'attempt', Key});
attempt(Key, Call) ->
    Srv = kapps_call:kvs_fetch('consumer_pid', Call),
    attempt(Key, Srv).

-spec wildcard_is_empty(kapps_call:call() | pid()) -> boolean().
wildcard_is_empty(Srv) when is_pid(Srv) ->
    gen_listener:call(Srv, 'wildcard_is_empty');
wildcard_is_empty(Call) ->
    Srv = kapps_call:kvs_fetch('consumer_pid', Call),
    wildcard_is_empty(Srv).

-spec amqp_send(pid() | kapps_call:call(), api_terms(), kz_amqp_worker:publish_fun()) -> 'ok'.
amqp_send(Srv, API, PubFun) when is_pid(Srv), is_function(PubFun, 1) ->
    gen_listener:cast(Srv, {'amqp_send', API, PubFun});
amqp_send(Call, API, PubFun) when is_function(PubFun, 1) ->
    amqp_send(kapps_call:kvs_fetch('consumer_pid', Call), API, PubFun).

-spec amqp_call(pid() | kapps_call:call(), api_terms(), kz_amqp_worker:publish_fun(), kz_amqp_worker:validate_fun()) -> 'ok'.
amqp_call(Srv, API, PubFun, VerifyFun) when is_pid(Srv), is_function(PubFun, 1) ->
    gen_listener:call(Srv, {'amqp_call', API, PubFun, VerifyFun});
amqp_call(Call, API, PubFun, VerifyFun) when is_function(PubFun, 1) ->
    amqp_call(kapps_call:kvs_fetch('consumer_pid', Call), API, PubFun, VerifyFun).

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
%%                     'ignore' |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec init([kapps_call:call()]) -> {'ok', state()}.
init([Call]) ->
    process_flag('trap_exit', 'true'),
    CallId = kapps_call:call_id(Call),
    kz_util:put_callid(CallId),
    gen_listener:cast(self(), 'initialize'),
    {'ok', #state{call=Call
                 ,branch_count = ?MAX_BRANCH_COUNT
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
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec handle_call(any(), pid_ref(), state()) -> handle_call_ret_state(state()).
handle_call('is_channel_destroyed', _From, State) ->
    {'reply', State#state.destroyed, State};
handle_call({'update_call', Routines}, _From, #state{call=Call}=State) ->
    NewCall = kapps_call:exec(Routines, Call),
    {'reply', NewCall, State#state{call=NewCall}};
handle_call({'set_call', Call},  _From, State) ->
    {'reply', Call, State#state{call=Call}};
handle_call('get_call', _From, #state{call=Call}=State) ->
    {'reply', {'ok', Call}, State};
handle_call('callid', _From, #state{call=Call}=State) ->
    {'reply', kapps_call:call_id_direct(Call), State};
handle_call('control_queue_name', _From, #state{call=Call}=State) ->
    {'reply', kapps_call:control_queue_direct(Call), State};
handle_call('get_branch_keys', _From, #state{flow = Flow}=State) ->
    Children = kz_json:get_value(<<"children">>, Flow, kz_json:new()),
    Reply = {'branch_keys', lists:delete(<<"_">>, kz_json:get_keys(Children))},
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
    case kz_json:get_value([<<"children">>, <<"_">>], Flow) of
        'undefined' -> {'reply', 'true', State};
        ChildFlow -> {'reply', kz_json:is_empty(ChildFlow), State}
    end;
handle_call({'next', Key}, _From, #state{flow=Flow}=State) ->
    {'reply'
    ,kz_json:get_first_defined([[<<"children">>, Key]
                               ,[<<"children">>, <<"_">>]
                               ]
                              ,Flow
                              )
    ,State
    };
handle_call({'amqp_call', API, PubFun, VerifyFun}, _From, #state{queue=Q}=State) ->
    Reply = amqp_call_message(API, PubFun, VerifyFun, Q),
    {'reply', Reply, State};
handle_call({'add_termination_handler', {_M, _F, _Args}=H},  _From, #state{termination_handlers=Handlers}=State) ->
    {'reply', 'ok', State#state{termination_handlers=lists:usort([H | Handlers])}};
handle_call({'remove_termination_handler', {_M, _F, _Args}=H},  _From, #state{termination_handlers=Handlers}=State) ->
    {'reply', 'ok', State#state{termination_handlers=lists:delete(H, Handlers)}};
handle_call(_Request, _From, State) ->
    Reply = {'error', 'unimplemented'},
    {'reply', Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {'noreply', State} |
%%                                  {'noreply', State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(any(), state()) -> handle_cast_ret_state(state()).
handle_cast({'set_call', Call}, State) ->
    {'noreply', State#state{call=Call}};
handle_cast({'continue', _}, #state{stop_on_destroy='true'
                                   ,destroyed='true'
                                   }=State) ->
    lager:info("channel no longer active, not continuing"),
    hard_stop(self()),
    {'noreply', State};
handle_cast({'continue', Key}, #state{flow=Flow
                                     ,call=Call
                                     ,termination_handlers=Handlers
                                     }=State) ->
    lager:info("continuing to child '~s'", [Key]),

    case kz_json:get_value([<<"children">>, Key], Flow) of
        'undefined' when Key =:= <<"_">> ->
            lager:info("wildcard child does not exist, we are lost...hanging up"),
            maybe_run_destory_handlers(Call, kz_json:new(), Handlers),
            stop(self()),
            {'noreply', State};
        'undefined' ->
            lager:info("requested child '~s' does not exist, trying wild card", [Key]),
            continue(self()),
            {'noreply', State};
        NewFlow ->
            {'noreply', launch_cf_module(State#state{flow=NewFlow})}
    end;
handle_cast('stop', #state{flows=[]}=State) ->
    {'stop', 'normal', State};
handle_cast('stop', #state{flows=[Flow|Flows]}=State) ->
    {'noreply', launch_cf_module(State#state{flow=Flow, flows=Flows})};
handle_cast('hard_stop', State) ->
    {'stop', 'normal', State};
handle_cast('transfer', State) ->
    {'stop', {'shutdown', 'transfer'}, State};
handle_cast('control_usurped', State) ->
    {'stop', {'shutdown', 'control_usurped'}, State};
handle_cast('channel_destroyed', State) ->
    {'noreply', State#state{destroyed='true'}};
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
    case kz_json:get_ne_value([<<"children">>, <<"_">>], Flow) of
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
    kz_util:put_callid(NewCallId),
    PrevCallId = kapps_call:call_id_direct(Call),
    lager:info("updating callid to ~s (from ~s), catch you on the flip side", [NewCallId, PrevCallId]),
    lager:info("removing call event bindings for ~s", [PrevCallId]),
    gen_listener:rm_binding(self(), 'call', [{'callid', PrevCallId}]),
    lager:info("binding to new call events"),
    gen_listener:add_binding(self(), 'call', [{'callid', NewCallId}]),
    {'noreply', State#state{call=kapps_call:set_call_id(NewCallId, Call)}};
handle_cast({'add_event_listener', {Mod, Args}}, #state{call=Call}=State) ->
    cf_util:start_event_listener(Call, Mod, Args),
    {'noreply', State};
handle_cast('initialize', #state{call=Call}=State) ->
    log_call_information(Call),
    Flow = kapps_call:kvs_fetch('cf_flow', Call),
    Updaters = [fun(C) -> kapps_call:kvs_store('consumer_pid', self(), C) end
               ,fun(C) -> kapps_call:call_id_helper(fun callid/2, C) end
               ,fun(C) -> kapps_call:control_queue_helper(fun control_queue/2, C) end
               ],
    CallWithHelpers = lists:foldr(fun(F, C) -> F(C) end, Call, Updaters),
    _ = kz_util:spawn(fun cf_singular_call_hooks:maybe_hook_call/1, [CallWithHelpers]),
    {'noreply', State#state{call=CallWithHelpers
                           ,flow=Flow
                           }};
handle_cast({'gen_listener', {'created_queue', Q}}, #state{call=Call}=State) ->
    {'noreply', State#state{queue=Q
                           ,call=kapps_call:set_controller_queue(Q, Call)
                           }};
handle_cast({'amqp_send', API, PubFun}, #state{queue=Q}=State) ->
    amqp_send_message(API, PubFun, Q),
    {'noreply', State};
handle_cast({'gen_listener', {'is_consuming', 'true'}}
           ,#state{cf_module_pid='undefined'}=State
           ) ->
    lager:debug("ready to recv events, launching the callflow"),
    {'noreply', launch_cf_module(State)};
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
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec handle_info(any(), state()) -> handle_info_ret_state(state()).
handle_info({'DOWN', Ref, 'process', Pid, 'normal'}, #state{cf_module_pid={Pid, Ref}
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
handle_info({'EXIT', Pid, _Reason}, #state{cf_module_old_pid={Pid, Ref}
                                          ,call=Call
                                          }=State) ->
    erlang:demonitor(Ref, ['flush']),
    LastAction = kapps_call:kvs_fetch('cf_old_action', Call),
    lager:error("action ~s died unexpectedly: ~p", [LastAction, _Reason]),
    {'noreply', State#state{cf_module_old_pid='undefined'}};
handle_info(_Msg, State) ->
    lager:debug("unhandled message: ~p", [_Msg]),
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc Handle call messages, sometimes forward them on.
%%--------------------------------------------------------------------
-spec handle_event(kz_json:object(), state()) -> gen_listener:handle_event_return().
handle_event(JObj, #state{cf_module_pid=PidRef
                         ,call=Call
                         ,self=Self
                         ,termination_handlers=DestoryHandlers
                         }) ->
    CallId = kapps_call:call_id_direct(Call),
    Others = kapps_call:kvs_fetch('cf_event_pids', [], Call),
    Notify = case get_pid(PidRef) of
                 'undefined' -> Others;
                 ModPid -> [ModPid | Others]
             end,

    case {kz_util:get_event_type(JObj), kz_json:get_value(<<"Call-ID">>, JObj)} of
        {{<<"call_event">>, <<"CHANNEL_DESTROY">>}, CallId} ->
            handle_channel_destroyed(Self, Notify, JObj, Call, DestoryHandlers);
        {{<<"call_event">>, <<"CHANNEL_DISCONNECTED">>}, CallId} ->
            handle_channel_destroyed(Self, Notify, JObj, Call, DestoryHandlers);
        {{<<"call_event">>, <<"CHANNEL_TRANSFEREE">>}, _} ->
            handle_channel_transfer(Call, JObj);
        {{<<"call_event">>, <<"CHANNEL_REPLACED">>}, _} ->
            handle_channel_replaced(Call, JObj, Notify);
        {{<<"call_event">>, <<"CHANNEL_BRIDGE">>}, _} ->
            handle_channel_bridged(Self, Notify, JObj, Call);
        {{<<"call_event">>, <<"usurp_control">>}, CallId} ->
            handle_usurp(Self, Call, JObj);
        {{<<"error">>, _}, _} ->
            handle_error(CallId, Notify, JObj);
        {_, CallId} ->
            relay_message(Notify, JObj);
        {{_Cat, _Name}, _Else} when Others =:= [] ->
            lager:info("received ~s (~s) from call ~s while relaying for ~s"
                      ,[_Cat, _Name, _Else, CallId]);
        {_Evt, _Else} ->
            lager:info("the others want to know about ~p", [_Evt]),
            relay_message(Others, JObj)
    end,
    'ignore'.

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
-spec terminate(any(), state()) -> 'ok'.
terminate({'shutdown', 'transfer'}, _) ->
    lager:info("callflow execution has been transferred");
terminate({'shutdown', 'control_usurped'}, _) ->
    lager:info("the call has been usurped by an external process");
terminate(_Reason, #state{call=Call
                         ,cf_module_pid='undefined'
                         }) ->
    hangup_call(Call),
    lager:info("callflow execution has been stopped: ~p", [_Reason]);
terminate(_Reason, #state{call=Call
                         ,cf_module_pid={Pid, _}
                         }) ->
    exit(Pid, 'kill'),
    hangup_call(Call),
    lager:info("callflow execution has been stopped: ~p", [_Reason]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {'ok', NewState}
%% @end
%%--------------------------------------------------------------------
-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @private
%% this function determines if the callflow module specified at the
%% current node is 'available' and attempts to launch it if so.
%% Otherwise it will advance to the next child in the flow
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec launch_cf_module(state()) -> state().
launch_cf_module(#state{flow=?EMPTY_JSON_OBJECT}=State) ->
    lager:debug("no flow left to launch, maybe stopping"),
    gen_listener:cast(self(), 'stop'),
    State;
launch_cf_module(#state{call=Call
                       ,flow=Flow
                       ,cf_module_pid=OldPidRef
                       }=State) ->
    Module = <<"cf_", (kz_json:get_value(<<"module">>, Flow))/binary>>,
    Data = kz_json:get_value(<<"data">>, Flow, kz_json:new()),

    {PidRef, Action} =
        case maybe_start_cf_module(Module, Data, Call) of
            {{Pid, _Ref}=PR, _Action}=Resp ->
                link(get_pid(PR)),
                Resp;
            Resp -> Resp
        end,

    OldAction = kapps_call:kvs_fetch('cf_last_action', Call),
    Routines = [{fun kapps_call:kvs_store/3, 'cf_old_action', OldAction}
               ,{fun kapps_call:kvs_store/3, 'cf_last_action', Action}
               ],
    State#state{cf_module_pid=PidRef
               ,cf_module_old_pid=OldPidRef
               ,call=kapps_call:exec(Routines, Call)
               }.

-spec maybe_start_cf_module(ne_binary(), kz_proplist(), kapps_call:call()) ->
                                   {{pid() | 'undefined', reference() | atom()} | 'undefined', atom()}.
maybe_start_cf_module(ModuleBin, Data, Call) ->
    CFModule = kz_term:to_atom(ModuleBin, 'true'),
    try CFModule:module_info('exports') of
        _ ->
            lager:info("moving to action '~s'", [CFModule]),
            spawn_cf_module(CFModule, Data, Call)
    catch
        'error':'undef' ->
            lager:debug("failed to find callflow module ~s", [CFModule]),
            cf_module_not_found(Call)
    end.

-spec cf_module_not_found(kapps_call:call()) ->
                                 {'undefined', atom()}.
cf_module_not_found(Call) ->
    lager:error("unknown callflow action, reverting to last action"),
    continue(self()),
    {'undefined', kapps_call:kvs_fetch('cf_last_action', Call)}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% helper function to spawn a linked callflow module, from the entry
%% point 'handle' having set the callid on the new process first
%% @end
%%--------------------------------------------------------------------
-spec spawn_cf_module(CFModule, list(), kapps_call:call()) ->
                             {pid_ref(), CFModule}.
spawn_cf_module(CFModule, Data, Call) ->
    AMQPConsumer = kz_amqp_channel:consumer_pid(),
    {kz_util:spawn_monitor(fun cf_module_task/4, [CFModule, Data, Call, AMQPConsumer])
    ,CFModule
    }.

%% @private
-spec cf_module_task(atom(), list(), kapps_call:call(), pid()) -> any().
cf_module_task(CFModule, Data, Call, AMQPConsumer) ->
    _ = kz_amqp_channel:consumer_pid(AMQPConsumer),
    kz_util:put_callid(kapps_call:call_id_direct(Call)),
    try CFModule:handle(Data, Call)
    catch
        _E:R ->
            ST = erlang:get_stacktrace(),
            lager:info("action ~s died unexpectedly (~s): ~p", [CFModule, _E, R]),
            kz_util:log_stacktrace(ST),
            throw(R)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% unlike the kapps_call_command this send command does not call the
%% functions of this module to form the headers, nor does it set
%% the reply queue.  Used when this module is terminating to send
%% a hangup command without relying on the (now terminated) cf_exe.
%% @end
%%--------------------------------------------------------------------
-spec amqp_send_message(api_terms(), kz_amqp_worker:publish_fun(), ne_binary()) -> 'ok'.
amqp_send_message(API, PubFun, Q) ->
    PubFun(add_server_id(Q, API)).

-spec amqp_call_message(api_terms(), kz_amqp_worker:publish_fun(), kz_amqp_worker:validate_fun(), ne_binary()) ->
                               kz_amqp_worker:request_return().
amqp_call_message(API, PubFun, VerifyFun, Q) ->
    Routines = [{fun add_server_id/2, Q}
               ,fun add_message_id/1
               ],
    Request = kz_api:exec(Routines, API),
    kz_amqp_worker:call(Request, PubFun, VerifyFun).

-spec send_command(kz_proplist(), api_binary(), api_binary()) -> 'ok'.
send_command(_, 'undefined', _) -> lager:debug("no control queue to send command to");
send_command(_, _, 'undefined') -> lager:debug("no call id to send command to");
send_command(Command, ControlQ, CallId) ->
    Props = Command ++ [{<<"Call-ID">>, CallId}
                        | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
                       ],
    kapps_util:amqp_pool_send(Props, fun(P) -> kapi_dialplan:publish_command(ControlQ, P) end).

-spec add_server_id(ne_binary(), api_terms()) -> api_terms().
add_server_id(Q, API) when is_list(API) ->
    [{<<"Server-ID">>, Q} | props:delete(<<"Server-ID">>, API)];
add_server_id(Q, API) ->
    kz_json:set_value(<<"Server-ID">>, Q, API).

-spec add_message_id(api_terms()) -> api_terms().
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
        'undefined' -> lager:info("inception on-net: using attributes for an internal call", []);
        _Else -> lager:info("inception ~s: using attributes for an external call", [_Else])
    end,
    lager:info("authorizing id ~s", [kapps_call:authorizing_id(Call)]).

-spec handle_channel_destroyed(pid(), pids(), kz_json:object(), kapps_call:call(), list()) -> 'ok'.
handle_channel_destroyed(Self, Notify, JObj, Call, DestoryHandlers) ->
    channel_destroyed(Self),
    relay_message(Notify, JObj),
    maybe_run_destory_handlers(Call, JObj, DestoryHandlers).

-spec handle_channel_transfer(kapps_call:call(), kz_json:object()) -> 'ok'.
handle_channel_transfer(Call, JObj) ->
    OrgFetchId = kapps_call:custom_channel_var(<<"Fetch-ID">>, Call),
    NewFetchId = kz_json:get_value([<<"Custom-Channel-Vars">>, <<"Fetch-ID">>], JObj),
    case OrgFetchId =:= NewFetchId of
        'true'  -> transfer(Call);
        'false' -> 'ok'
    end.

-spec handle_channel_replaced(kapps_call:call(), kz_json:object(), pids()) -> 'ok'.
handle_channel_replaced(Call, JObj, Notify) ->
    OrgFetchId = kapps_call:custom_channel_var(<<"Fetch-ID">>, Call),
    NewFetchId = kz_json:get_value([<<"Custom-Channel-Vars">>, <<"Fetch-ID">>], JObj),
    case OrgFetchId =:= NewFetchId of
        'true' ->
            ReplacedBy = kz_json:get_value(<<"Replaced-By">>, JObj),
            callid_update(ReplacedBy, Call),
            relay_message(Notify, JObj);
        'false' -> 'ok'
    end.

-spec handle_channel_bridged(pid(), pids(), kz_json:object(), kapps_call:call()) -> 'ok'.
handle_channel_bridged(Self, Notify, JObj, Call) ->
    gen_listener:cast(Self, {set_call, kapps_call:set_call_bridged('true', Call)}),
    relay_message(Notify, JObj).

-spec handle_usurp(pid(), kapps_call:call(), kz_json:object()) -> 'ok'.
handle_usurp(Self, Call, JObj) ->
    OrgFetchId = kapps_call:custom_channel_var(<<"Fetch-ID">>, Call),
    NewFetchId = kz_json:get_value(<<"Fetch-ID">>, JObj),
    case OrgFetchId =:= NewFetchId of
        'false' -> control_usurped(Self);
        'true'  -> 'ok'
    end.

-spec handle_error(ne_binary(), pids(), kz_json:object()) -> 'ok'.
handle_error(CallId, Notify, JObj) ->
    case kz_json:get_value([<<"Request">>, <<"Call-ID">>], JObj) of
        CallId      -> relay_message(Notify, JObj);
        'undefined' -> relay_message(Notify, JObj);
        _Else       -> 'ok'
    end.

-spec relay_message(pids(), kz_json:object()) -> 'ok'.
relay_message(Notify, Message) ->
    _ = [kapps_call_command:relay_event(Pid, Message)
         || Pid <- Notify
                ,is_pid(Pid)
        ],
    'ok'.

-spec maybe_run_destory_handlers(kapps_call:call(), kz_json:object(), list()) -> 'ok'.
maybe_run_destory_handlers(Call, JObj, Handlers) ->
    _ = [erlang:apply(M, F, [Call, JObj | Args]) || {M, F, Args} <- Handlers],
    'ok'.

-spec get_pid({pid(), reference()} | 'undefined') -> api_pid().
get_pid({Pid, _}) when is_pid(Pid) -> Pid;
get_pid(_) -> 'undefined'.

-spec hangup_call(kapps_call:call()) -> 'ok'.
hangup_call(Call) ->
    Cmd = [{<<"Event-Name">>, <<"command">>}
          ,{<<"Event-Category">>, <<"call">>}
          ,{<<"Application-Name">>, <<"hangup">>}
          ,{<<"Insert-At">>, <<"tail">>}
          ],
    send_command(Cmd, kapps_call:control_queue_direct(Call), kapps_call:call_id_direct(Call)).
