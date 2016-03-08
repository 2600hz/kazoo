%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2015, 2600Hz
%%% @doc
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(cf_exe).

-behaviour(gen_listener).

%% API
-export([start_link/1]).
-export([relay_amqp/2, send_amqp/3]).
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
-export([stop_on_destroy/1
         ,continue_on_destroy/1
        ]).
-export([get_branch_keys/1, get_all_branch_keys/1]).
-export([attempt/1, attempt/2]).
-export([wildcard_is_empty/1]).
-export([callid_update/2]).
-export([add_event_listener/2]).
-export([next/1, next/2]).
-export([update_call/2]).

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
-include_lib("whistle/src/wh_json.hrl").

-define(SERVER, ?MODULE).

-define(CALL_SANITY_CHECK, 30000).

-define(RESPONDERS, [{{?MODULE, 'relay_amqp'}
                      ,[{<<"*">>, <<"*">>}]
                     }
                    ]).
-define(QUEUE_NAME, <<>>).
-define(QUEUE_OPTIONS, []).
-define(CONSUME_OPTIONS, []).

-record(state, {call = whapps_call:new() :: whapps_call:call()
                ,flow = wh_json:new() :: wh_json:object()
                ,flows = [] :: wh_json:objects()
                ,cf_module_pid :: {pid(), reference()} | 'undefined'
                ,cf_module_old_pid :: {pid(), reference()} | 'undefined'
                ,status = <<"sane">> :: ne_binary()
                ,queue :: api_binary()
                ,self = self()
                ,stop_on_destroy = 'true' :: boolean()
                ,destroyed = 'false' :: boolean()
                ,branch_count :: non_neg_integer()
               }).
-type state() :: #state{}.

-define(MAX_BRANCH_COUNT, whapps_config:get_integer(?CF_CONFIG_CAT, <<"max_branch_count">>, 50)).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts the server
%%--------------------------------------------------------------------
-spec start_link(whapps_call:call()) -> startlink_ret().
start_link(Call) ->
    CallId = whapps_call:call_id(Call),
    Bindings = [{'call', [{'callid', CallId}]}
                ,{'self', []}
               ],
    gen_listener:start_link(?SERVER, [{'responders', ?RESPONDERS}
                                      ,{'bindings', Bindings}
                                      ,{'queue_name', ?QUEUE_NAME}
                                      ,{'queue_options', ?QUEUE_OPTIONS}
                                      ,{'consume_options', ?CONSUME_OPTIONS}
                                     ], [Call]).

-spec get_call(pid() | whapps_call:call()) -> {'ok', whapps_call:call()}.
get_call(Srv) when is_pid(Srv) ->
    gen_server:call(Srv, 'get_call', 1000);
get_call(Call) ->
    Srv = whapps_call:kvs_fetch('consumer_pid', Call),
    get_call(Srv).

-spec set_call(whapps_call:call()) -> 'ok'.
set_call(Call) ->
    Srv = whapps_call:kvs_fetch('consumer_pid', Call),
    gen_server:cast(Srv, {'set_call', Call}).

-spec update_call(whapps_call:call(), list()) -> whapps_call:call().
update_call(Call, Routines) ->
    Srv = whapps_call:kvs_fetch('consumer_pid', Call),
    gen_server:call(Srv, {'update_call', Routines}).

-spec continue(whapps_call:call() | pid()) -> 'ok'.
-spec continue(ne_binary(), whapps_call:call() | pid()) -> 'ok'.
continue(Srv) -> continue(<<"_">>, Srv).

continue(Key, Srv) when is_pid(Srv) ->
    gen_listener:cast(Srv, {'continue', Key});
continue(Key, Call) ->
    Srv = whapps_call:kvs_fetch('consumer_pid', Call),
    continue(Key, Srv).

-spec continue_with_flow(wh_json:object(), whapps_call:call() | pid()) -> 'ok'.
continue_with_flow(Flow, Srv) when is_pid(Srv) ->
    gen_listener:cast(Srv, {'continue_with_flow', Flow});
continue_with_flow(Flow, Call) ->
    Srv = whapps_call:kvs_fetch('consumer_pid', Call),
    continue_with_flow(Flow, Srv).

-spec branch(wh_json:object(), whapps_call:call() | pid()) -> 'ok'.
branch(Flow, Srv) when is_pid(Srv) ->
    gen_listener:cast(Srv, {'branch', Flow});
branch(Flow, Call) ->
    Srv = whapps_call:kvs_fetch('consumer_pid', Call),
    branch(Flow, Srv).

-spec next(whapps_call:call() | pid()) -> api_object().
-spec next(ne_binary(), whapps_call:call() | pid()) -> api_object().
next(Srv) -> next(<<"_">>, Srv).

next(Key, Srv) when is_pid(Srv) ->
    gen_listener:call(Srv, {'next', Key});
next(Key, Call) ->
    Srv = whapps_call:kvs_fetch('consumer_pid', Call),
    next(Key, Srv).

-spec add_event_listener(whapps_call:call() | pid(), {atom(), list()}) -> 'ok'.
add_event_listener(Srv, {_,_}=SpawnInfo) when is_pid(Srv) ->
    gen_listener:cast(Srv, {'add_event_listener', SpawnInfo});
add_event_listener(Call, {_,_}=SpawnInfo) ->
    add_event_listener(whapps_call:kvs_fetch('consumer_pid', Call), SpawnInfo).

-spec stop(whapps_call:call() | pid()) -> 'ok'.
stop(Srv) when is_pid(Srv) ->
    gen_listener:cast(Srv, 'stop');
stop(Call) ->
    Srv = whapps_call:kvs_fetch('consumer_pid', Call),
    stop(Srv).

-spec hard_stop(whapps_call:call() | pid()) -> 'ok'.
hard_stop(Srv) when is_pid(Srv) ->
    gen_listener:cast(Srv, 'hard_stop');
hard_stop(Call) ->
    Srv = whapps_call:kvs_fetch('consumer_pid', Call),
    hard_stop(Srv).

-spec transfer(whapps_call:call() | pid()) -> 'ok'.
transfer(Srv) when is_pid(Srv) ->
    gen_listener:cast(Srv, 'transfer');
transfer(Call) ->
    Srv = whapps_call:kvs_fetch('consumer_pid', Call),
    transfer(Srv).

-spec control_usurped(whapps_call:call() | pid()) -> 'ok'.
control_usurped(Srv) when is_pid(Srv) ->
    gen_listener:cast(Srv, 'control_usurped');
control_usurped(Call) ->
    Srv = whapps_call:kvs_fetch('consumer_pid', Call),
    control_usurped(Srv).

-spec channel_destroyed(whapps_call:call() | pid()) -> 'ok'.
channel_destroyed(Srv) when is_pid(Srv) ->
    gen_listener:cast(Srv, 'channel_destroyed');
channel_destroyed(Call) ->
    Srv = whapps_call:kvs_fetch('consumer_pid', Call),
    channel_destroyed(Srv).

-spec stop_on_destroy(whapps_call:call() | pid()) -> 'ok'.
stop_on_destroy(Srv) when is_pid(Srv) ->
    gen_listener:cast(Srv, 'stop_on_destroy');
stop_on_destroy(Call) ->
    Srv = whapps_call:kvs_fetch('consumer_pid', Call),
    stop_on_destroy(Srv).

-spec continue_on_destroy(whapps_call:call() | pid()) -> 'ok'.
continue_on_destroy(Srv) when is_pid(Srv) ->
    gen_listener:cast(Srv, 'continue_on_destroy');
continue_on_destroy(Call) ->
    Srv = whapps_call:kvs_fetch('consumer_pid', Call),
    continue_on_destroy(Srv).

-spec callid_update(ne_binary(), whapps_call:call() | pid()) -> 'ok'.
callid_update(CallId, Srv) when is_pid(Srv) ->
    gen_listener:cast(Srv, {'callid_update', CallId});
callid_update(CallId, Call) ->
    Srv = whapps_call:kvs_fetch('consumer_pid', Call),
    callid_update(CallId, Srv).

-spec callid(whapps_call:call() | pid()) -> ne_binary().
-spec callid(api_binary(), whapps_call:call()) -> ne_binary().

callid(Srv) when is_pid(Srv) ->
    CallId = gen_server:call(Srv, 'callid', 1000),
    wh_util:put_callid(CallId),
    CallId;
callid(Call) ->
    Srv = whapps_call:kvs_fetch('consumer_pid', Call),
    callid(Srv).

callid(_, Call) ->
    callid(Call).

-spec queue_name(whapps_call:call() | pid()) -> ne_binary().
queue_name(Srv) when is_pid(Srv) ->
    gen_listener:queue_name(Srv);
queue_name(Call) ->
    Srv = whapps_call:kvs_fetch('consumer_pid', Call),
    queue_name(Srv).

-spec control_queue(whapps_call:call() | pid()) -> ne_binary().
-spec control_queue(api_binary(), whapps_call:call() | pid()) -> ne_binary().

control_queue(Srv) when is_pid(Srv) -> gen_listener:call(Srv, 'control_queue_name');
control_queue(Call) -> control_queue(whapps_call:kvs_fetch('consumer_pid', Call)).
control_queue(_, Call) -> control_queue(Call).

-spec get_branch_keys(whapps_call:call() | pid()) -> {'branch_keys', wh_json:keys()}.
get_branch_keys(Srv) when is_pid(Srv) ->
    gen_listener:call(Srv, 'get_branch_keys');
get_branch_keys(Call) ->
    Srv = whapps_call:kvs_fetch('consumer_pid', Call),
    get_branch_keys(Srv).

-spec get_all_branch_keys(whapps_call:call() | pid()) ->
                                 {'branch_keys', wh_json:keys()}.
get_all_branch_keys(Srv) when is_pid(Srv) ->
    gen_listener:call(Srv, {'get_branch_keys', 'all'});
get_all_branch_keys(Call) ->
    Srv = whapps_call:kvs_fetch('consumer_pid', Call),
    get_all_branch_keys(Srv).

-spec attempt(whapps_call:call() | pid()) ->
                     {'attempt_resp', 'ok'} |
                     {'attempt_resp', {'error', any()}}.
-spec attempt(ne_binary(), whapps_call:call() | pid()) ->
                     {'attempt_resp', 'ok'} |
                     {'attempt_resp', {'error', any()}}.
attempt(Srv) -> attempt(<<"_">>, Srv).

attempt(Key, Srv) when is_pid(Srv) ->
    gen_listener:call(Srv, {'attempt', Key});
attempt(Key, Call) ->
    Srv = whapps_call:kvs_fetch('consumer_pid', Call),
    attempt(Key, Srv).

-spec wildcard_is_empty(whapps_call:call() | pid()) -> boolean().
wildcard_is_empty(Srv) when is_pid(Srv) ->
    gen_listener:call(Srv, 'wildcard_is_empty');
wildcard_is_empty(Call) ->
    Srv = whapps_call:kvs_fetch('consumer_pid', Call),
    wildcard_is_empty(Srv).

-spec relay_amqp(wh_json:object(), wh_proplist()) -> any().
relay_amqp(JObj, Props) ->
    Pids = case props:get_value('cf_module_pid', Props) of
               P when is_pid(P) -> [P | props:get_value('cf_event_pids', Props, [])];
               _ -> props:get_value('cf_event_pids', Props, [])
           end,
    [whapps_call_command:relay_event(Pid, JObj) || Pid <- Pids, is_pid(Pid)].

-spec send_amqp(pid() | whapps_call:call(), api_terms(), wh_amqp_worker:publish_fun()) -> 'ok'.
send_amqp(Srv, API, PubFun) when is_pid(Srv), is_function(PubFun, 1) ->
    gen_listener:cast(Srv, {'send_amqp', API, PubFun});
send_amqp(Call, API, PubFun) when is_function(PubFun, 1) ->
    send_amqp(whapps_call:kvs_fetch('consumer_pid', Call), API, PubFun).

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
init([Call]) ->
    process_flag('trap_exit', 'true'),
    CallId = whapps_call:call_id(Call),
    wh_util:put_callid(CallId),
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
handle_call({'update_call', Routines}, _From, #state{call=Call}=State) ->
    NewCall = whapps_call:exec(Routines, Call),
    {'reply', NewCall, State#state{call=NewCall}};
handle_call('get_call', _From, #state{call=Call}=State) ->
    {'reply', {'ok', Call}, State};
handle_call('callid', _From, #state{call=Call}=State) ->
    {'reply', whapps_call:call_id_direct(Call), State};
handle_call('control_queue_name', _From, #state{call=Call}=State) ->
    {'reply', whapps_call:control_queue_direct(Call), State};
handle_call('get_branch_keys', _From, #state{flow = Flow}=State) ->
    Children = wh_json:get_value(<<"children">>, Flow, wh_json:new()),
    Reply = {'branch_keys', lists:delete(<<"_">>, wh_json:get_keys(Children))},
    {'reply', Reply, State};
handle_call({'get_branch_keys', 'all'}, _From, #state{flow = Flow}=State) ->
    Children = wh_json:get_value(<<"children">>, Flow, wh_json:new()),
    Reply = {'branch_keys', wh_json:get_keys(Children)},
    {'reply', Reply, State};
handle_call({'attempt', Key}, _From, #state{flow=Flow}=State) ->
    case wh_json:get_ne_value([<<"children">>, Key], Flow) of
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
    case wh_json:get_value([<<"children">>, <<"_">>], Flow) of
        'undefined' -> {'reply', 'true', State};
        ChildFlow -> {'reply', wh_json:is_empty(ChildFlow), State}
    end;
handle_call({'next', Key}, _From, #state{flow=Flow}=State) ->
    {'reply'
     ,wh_json:get_first_defined([[<<"children">>, Key]
                                 ,[<<"children">>, <<"_">>]
                                ]
                                ,Flow
                               )
     ,State
    };
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
handle_cast({'set_call', Call}, State) ->
    {'noreply', State#state{call=Call}};
handle_cast({'continue', _}, #state{stop_on_destroy='true'
                                    ,destroyed='true'
                                   }=State) ->
    lager:info("channel no longer active, not continuing"),
    hard_stop(self()),
    {'noreply', State};
handle_cast({'continue', Key}, #state{flow=Flow
                                     }=State) ->
    lager:info("continuing to child '~s'", [Key]),

    case wh_json:get_value([<<"children">>, Key], Flow) of
        'undefined' when Key =:= <<"_">> ->
            lager:info("wildcard child does not exist, we are lost...hanging up"),
            ?MODULE:stop(self()),
            {'noreply', State};
        'undefined' ->
            lager:info("requested child does not exist, trying wild card", [Key]),
            ?MODULE:continue(self()),
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
    case wh_json:get_ne_value([<<"children">>, <<"_">>], Flow) of
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
    wh_util:put_callid(NewCallId),
    PrevCallId = whapps_call:call_id_direct(Call),
    lager:info("updating callid to ~s (from ~s), catch you on the flip side", [NewCallId, PrevCallId]),
    lager:info("removing call event bindings for ~s", [PrevCallId]),
    gen_listener:rm_binding(self(), 'call', [{'callid', PrevCallId}]),
    lager:info("binding to new call events"),
    gen_listener:add_binding(self(), 'call', [{'callid', NewCallId}]),
    {'noreply', State#state{call=whapps_call:set_call_id(NewCallId, Call)}};
handle_cast({'add_event_listener', {Mod, Args}}, #state{call=Call}=State) ->
    cf_util:start_event_listener(Call, Mod, Args),
    {'noreply', State};
handle_cast('initialize', #state{call=Call}=State) ->
    log_call_information(Call),
    Flow = whapps_call:kvs_fetch('cf_flow', Call),
    Updaters = [fun(C) -> whapps_call:kvs_store('consumer_pid', self(), C) end
                ,fun(C) -> whapps_call:call_id_helper(fun ?MODULE:callid/2, C) end
                ,fun(C) -> whapps_call:control_queue_helper(fun ?MODULE:control_queue/2, C) end
               ],
    CallWithHelpers = lists:foldr(fun(F, C) -> F(C) end, Call, Updaters),
    _ = wh_util:spawn(fun cf_singular_call_hooks:maybe_hook_call/1, [CallWithHelpers]),
    {'noreply', State#state{call=CallWithHelpers
                       ,flow=Flow
                      }};
handle_cast({'gen_listener', {'created_queue', Q}}, #state{call=Call}=State) ->
    {'noreply', State#state{queue=Q
                            ,call=whapps_call:set_controller_queue(Q, Call)
                           }};
handle_cast({'send_amqp', API, PubFun}, #state{queue=Q}=State) ->
    send_amqp_message(API, PubFun, Q),
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
handle_info({'DOWN', Ref, 'process', Pid, 'normal'}, #state{cf_module_pid={Pid, Ref}
                                                            ,call=Call
                                                           }=State) ->
    erlang:demonitor(Ref, ['flush']),
    lager:debug("cf module ~s down normally", [whapps_call:kvs_fetch('cf_last_action', Call)]),
    {'noreply', State#state{cf_module_pid='undefined'}};
handle_info({'DOWN', Ref, 'process', Pid, _Reason}, #state{cf_module_pid={Pid, Ref}
                                                           ,call=Call
                                                          }=State) ->
    erlang:demonitor(Ref, ['flush']),
    LastAction = whapps_call:kvs_fetch('cf_last_action', Call),
    lager:error("action ~s died unexpectedly: ~p", [LastAction, _Reason]),
    ?MODULE:continue(self()),
    {'noreply', State#state{cf_module_pid='undefined'}};
handle_info({'DOWN', _Ref, 'process', _Pid, 'normal'}, State) ->
    {'noreply', State};
handle_info({'EXIT', Pid, 'normal'}, #state{cf_module_pid={Pid, Ref}
                                            ,call=Call
                                           }=State) ->
    erlang:demonitor(Ref, ['flush']),
    lager:debug("cf module ~s down normally", [whapps_call:kvs_fetch('cf_last_action', Call)]),
    {'noreply', State#state{cf_module_pid='undefined'}};
handle_info({'EXIT', Pid, _Reason}, #state{cf_module_pid={Pid, Ref}
                                           ,call=Call
                                          }=State) ->
    erlang:demonitor(Ref, ['flush']),
    LastAction = whapps_call:kvs_fetch('cf_last_action', Call),
    lager:error("action ~s died unexpectedly: ~p", [LastAction, _Reason]),
    ?MODULE:continue(self()),
    {'noreply', State#state{cf_module_pid='undefined'}};
handle_info({'EXIT', Pid, 'normal'}, #state{cf_module_old_pid={Pid, Ref}
                                            ,call=Call
                                           }=State) ->
    erlang:demonitor(Ref, ['flush']),
    lager:debug("cf module ~s down normally", [whapps_call:kvs_fetch('cf_old_action', Call)]),
    {'noreply', State#state{cf_module_old_pid='undefined'}};
handle_info({'EXIT', Pid, _Reason}, #state{cf_module_old_pid={Pid, Ref}
                                           ,call=Call
                                          }=State) ->
    erlang:demonitor(Ref, ['flush']),
    LastAction = whapps_call:kvs_fetch('cf_old_action', Call),
    lager:error("action ~s died unexpectedly: ~p", [LastAction, _Reason]),
    {'noreply', State#state{cf_module_old_pid='undefined'}};
handle_info(_Msg, State) ->
    lager:debug("unhandled message: ~p", [_Msg]),
    {'noreply', State}.

handle_channel_destroyed(Self, NotifyPids, JObj) ->
    channel_destroyed(Self),
    relay_message(NotifyPids, JObj).

handle_channel_transfer(Call, JObj) ->
    OrgFetchId = whapps_call:custom_channel_var(<<"Fetch-ID">>, Call),
    NewFetchId = wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Fetch-ID">>], JObj),

    case OrgFetchId =:= NewFetchId of
        'false' -> 'ok';
        'true'  -> transfer(Call)
    end.

handle_channel_replaced(Call, JObj, Notify) ->
    OrgFetchId = whapps_call:custom_channel_var(<<"Fetch-ID">>, Call),
    NewFetchId = wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Fetch-ID">>], JObj),

    case OrgFetchId =:= NewFetchId of
        'false' ->
            'ignore';

        'true' ->
            ReplacedBy = wh_json:get_value(<<"Replaced-By">>, JObj),
            callid_update(ReplacedBy, Call),
            relay_message(Notify, JObj)
    end.

handle_usurp(Self, Call, JObj) ->
    OrgFetchId = whapps_call:custom_channel_var(<<"Fetch-ID">>, Call),
    NewFetchId = wh_json:get_value(<<"Fetch-ID">>, JObj),

    case OrgFetchId =:= NewFetchId of
        'false' -> control_usurped(Self);
        'true'  -> 'ok'
    end.

handle_error(CallId, Notify, JObj) ->
    case wh_json:get_value([<<"Request">>, <<"Call-ID">>], JObj) of
        CallId      -> relay_message(Notify, JObj);
        'undefined' -> relay_message(Notify, JObj);
        _Else       -> 'ignore'
    end.

relay_message(NotifyPids, Message) ->
    [whapps_call_command:relay_event(Pid, Message) || Pid <- NotifyPids, is_pid(Pid)].

%%--------------------------------------------------------------------
%% @private
%% @doc Handle call messages, sometimes forward them on.
%%--------------------------------------------------------------------
handle_event(JObj, #state{cf_module_pid=PidRef, call=Call ,self=Self}) ->
    CallId = whapps_call:call_id_direct(Call),
    Others = whapps_call:kvs_fetch('cf_event_pids', [], Call),
    ModPid = get_pid(PidRef),
    Notify = if is_pid(ModPid) -> [ModPid | Others]; 'true' -> Others end,

    case {whapps_util:get_event_type(JObj), wh_json:get_value(<<"Call-ID">>, JObj)} of
        {{<<"call_event">>, <<"CHANNEL_DESTROY">>}, CallId} ->
            handle_channel_destroyed(Self, Notify, JObj);

        {{<<"call_event">>, <<"CHANNEL_DISCONNECTED">>}, CallId} ->
            handle_channel_destroyed(Self, Notify, JObj);

        {{<<"call_event">>, <<"CHANNEL_TRANSFEREE">>}, _} ->
            handle_channel_transfer(Call, JObj);

        {{<<"call_event">>, <<"CHANNEL_REPLACED">>}, _} ->
            handle_channel_replaced(Call, JObj, Notify);

        {{<<"call_event">>, <<"usurp_control">>}, CallId} ->
            handle_usurp(Self, Call, JObj);

        {{<<"error">>, _}, _} ->
            handle_error(CallId, Notify, JObj);

        {_, CallId} ->
            relay_message(Notify, JObj);

        {{_Cat, _Name}, _Else} when Others =:= [] ->
            lager:info("received ~s (~s) from call ~s while relaying for ~s", [_Cat, _Name, _Else, CallId]);

        {_Evt, _Else} ->
            lager:info("the others want to know about ~p", [_Evt]),
            relay_message(Others, JObj)
    end,
    'ignore'.

-spec get_pid({pid(), any()}) -> pid().
get_pid({Pid, _}) when is_pid(Pid) -> Pid;
get_pid(_) -> 'undefined'.

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

-spec hangup_call(whapps_call:call()) -> 'ok'.
hangup_call(Call) ->
    Cmd = [{<<"Event-Name">>, <<"command">>}
           ,{<<"Event-Category">>, <<"call">>}
           ,{<<"Application-Name">>, <<"hangup">>}
          ],
    send_command(Cmd, whapps_call:control_queue_direct(Call), whapps_call:call_id_direct(Call)).

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
    Module = <<"cf_", (wh_json:get_value(<<"module">>, Flow))/binary>>,
    Data = wh_json:get_value(<<"data">>, Flow, wh_json:new()),
    {PidRef, Action} = maybe_start_cf_module(Module, Data, Call),
    link(get_pid(PidRef)),
    OldAction = whapps_call:kvs_fetch('cf_last_action', Call),
    Routines = [{fun whapps_call:kvs_store/3, 'cf_old_action', OldAction}
                ,{fun whapps_call:kvs_store/3, 'cf_last_action', Action}
               ],
    State#state{cf_module_pid=PidRef
                ,cf_module_old_pid=OldPidRef
                ,call=whapps_call:exec(Routines, Call)
               }.

-spec maybe_start_cf_module(ne_binary(), wh_proplist(), whapps_call:call()) ->
                                   {{pid(), reference()} | 'undefined', atom()}.
maybe_start_cf_module(ModuleBin, Data, Call) ->
    CFModule = wh_util:to_atom(ModuleBin, 'true'),
    try CFModule:module_info('exports') of
        _ ->
            lager:info("moving to action '~s'", [CFModule]),
            spawn_cf_module(CFModule, Data, Call)
    catch
        'error':'undef' ->
            lager:debug("failed to find callflow module ~s", [CFModule]),
            cf_module_not_found(Call)
    end.

-spec cf_module_not_found(whapps_call:call()) ->
                                 {'undefined', atom()}.
cf_module_not_found(Call) ->
    lager:error("unknown callflow action, reverting to last action"),
    ?MODULE:continue(self()),
    {'undefined', whapps_call:kvs_fetch('cf_last_action', Call)}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% helper function to spawn a linked callflow module, from the entry
%% point 'handle' having set the callid on the new process first
%% @end
%%--------------------------------------------------------------------
-spec spawn_cf_module(CFModule, list(), whapps_call:call()) ->
                             {pid_ref(), CFModule}.
spawn_cf_module(CFModule, Data, Call) ->
    AMQPConsumer = wh_amqp_channel:consumer_pid(),
    {wh_util:spawn_monitor(fun cf_module_task/4, [CFModule, Data, Call, AMQPConsumer])
     ,CFModule
    }.

%% @private
-spec cf_module_task(atom(), list(), whapps_call:call(), pid()) -> any().
cf_module_task(CFModule, Data, Call, AMQPConsumer) ->
    _ = wh_amqp_channel:consumer_pid(AMQPConsumer),
    wh_util:put_callid(whapps_call:call_id_direct(Call)),
    try CFModule:handle(Data, Call) of
        Result -> Result
    catch
        _E:R ->
            ST = erlang:get_stacktrace(),
            lager:info("action ~s died unexpectedly (~s): ~p", [CFModule, _E, R]),
            wh_util:log_stacktrace(ST),
            throw(R)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% unlike the whapps_call_command this send command does not call the
%% functions of this module to form the headers, nor does it set
%% the reply queue.  Used when this module is terminating to send
%% a hangup command without relying on the (now terminated) cf_exe.
%% @end
%%--------------------------------------------------------------------
-spec send_amqp_message(api_terms(), wh_amqp_worker:publish_fun(), ne_binary()) -> 'ok'.
send_amqp_message(API, PubFun, Q) ->
    PubFun(add_server_id(API, Q)).

-spec send_command(wh_proplist(), api_binary(), api_binary()) -> 'ok'.
send_command(_, 'undefined', _) -> lager:debug("no control queue to send command to");
send_command(_, _, 'undefined') -> lager:debug("no call id to send command to");
send_command(Command, ControlQ, CallId) ->
    Props = Command ++ [{<<"Call-ID">>, CallId}
                       | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                      ],
    whapps_util:amqp_pool_send(Props, fun(P) -> wapi_dialplan:publish_command(ControlQ, P) end).

-spec add_server_id(api_terms(), ne_binary()) -> api_terms().
add_server_id(API, Q) when is_list(API) ->
    [{<<"Server-ID">>, Q} | props:delete(<<"Server-ID">>, API)];
add_server_id(API, Q) ->
    wh_json:set_value(<<"Server-ID">>, Q, API).

-spec log_call_information(whapps_call:call()) -> 'ok'.
log_call_information(Call) ->
    lager:info("executing callflow ~s", [whapps_call:kvs_fetch('cf_flow_id', Call)]),
    lager:info("account id ~s", [whapps_call:account_id(Call)]),
    lager:info("request ~s", [whapps_call:request(Call)]),
    lager:info("to ~s", [whapps_call:to(Call)]),
    lager:info("from ~s", [whapps_call:from(Call)]),
    lager:info("CID ~s ~s", [whapps_call:caller_id_name(Call), whapps_call:caller_id_number(Call)]),
    case whapps_call:inception(Call) of
        'undefined' -> lager:info("inception on-net: using attributes for an internal call", []);
        _Else -> lager:info("inception ~s: using attributes for an external call", [_Else])
    end,
    lager:info("authorizing id ~s", [whapps_call:authorizing_id(Call)]).
