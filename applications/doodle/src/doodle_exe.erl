%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% @author Karl Anderson
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(doodle_exe).
-behaviour(gen_listener).

%% API
-export([start_link/1]).
-export([relay_amqp/2, send_amqp/3]).
-export([get_call/1, set_call/1]).
-export([callid/1, callid/2]).
-export([queue_name/1]).
-export([control_queue/1, control_queue/2]).
-export([continue/1, continue/2]).
-export([branch/2]).
-export([stop/1]).
-export([transfer/1]).
-export([control_usurped/1]).
-export([get_branch_keys/1, get_all_branch_keys/1]).
-export([attempt/1, attempt/2]).
-export([wildcard_is_empty/1]).
-export([callid_update/2]).
-export([add_event_listener/2]).

%% gen_listener callbacks
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,handle_event/2
        ,terminate/2
        ,code_change/3
        ]).

-include("doodle.hrl").

-define(SERVER, ?MODULE).

-define(CALL_SANITY_CHECK, 30 * ?MILLISECONDS_IN_SECOND).

-define(RESPONDERS, [{{?MODULE, 'relay_amqp'}
                     ,[{<<"*">>, <<"*">>}]
                     }
                    ]).
-define(QUEUE_NAME, <<>>).
-define(QUEUE_OPTIONS, []).
-define(CONSUME_OPTIONS, []).

-record(state, {call = kapps_call:new() :: kapps_call:call()
               ,flow = kz_json:new() :: kz_json:object()
               ,cf_module_pid :: {pid(), reference()} | 'undefined'
               ,cf_module_old_pid :: {pid(), reference()} | 'undefined'
               ,status = <<"sane">> :: kz_term:ne_binary()
               ,queue :: kz_term:api_binary()
               ,self = self()
               }).
-type state() :: #state{}.

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the server.
%% @end
%%------------------------------------------------------------------------------
-spec start_link(kapps_call:call()) -> kz_types:startlink_ret().
start_link(Call) ->
    CallId = kapps_call:call_id(Call),
    Bindings = [{'sms', [{'message_id', CallId}
                        ,{'restrict_to', ['delivery']}
                        ]
                }
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
    gen_server:call(Srv, 'get_call', ?MILLISECONDS_IN_SECOND);
get_call(Call) ->
    Srv = kapps_call:kvs_fetch('consumer_pid', Call),
    get_call(Srv).

-spec set_call(kapps_call:call()) -> 'ok'.
set_call(Call) ->
    Srv = kapps_call:kvs_fetch('consumer_pid', Call),
    gen_server:cast(Srv, {'set_call', Call}).

-spec update_call(kapps_call:call()) -> 'ok'.
update_call(Call) ->
    Srv = kapps_call:kvs_fetch('consumer_pid', Call),
    gen_server:cast(Srv, {'update_call', Call}).

-spec continue(kapps_call:call() | pid()) -> 'ok'.
continue(Srv) -> continue(<<"_">>, Srv).

-spec continue(kz_term:ne_binary(), kapps_call:call() | pid()) -> 'ok'.
continue(Key, Srv) when is_pid(Srv) ->
    gen_listener:cast(Srv, {'continue', Key});
continue(Key, Call) ->
    update_call(Call),
    Srv = kapps_call:kvs_fetch('consumer_pid', Call),
    continue(Key, Srv).

-spec branch(kz_json:object(), kapps_call:call() | pid()) -> 'ok'.
branch(Flow, Srv) when is_pid(Srv) ->
    gen_listener:cast(Srv, {'branch', Flow});
branch(Flow, Call) ->
    update_call(Call),
    Srv = kapps_call:kvs_fetch('consumer_pid', Call),
    branch(Flow, Srv).

-spec add_event_listener(pid(), {any(),any()}) -> 'ok'.
add_event_listener(Srv, {_,_}=SpawnInfo) when is_pid(Srv) ->
    gen_listener:cast(Srv, {'add_event_listener', SpawnInfo});
add_event_listener(Call, {_,_}=SpawnInfo) ->
    add_event_listener(kapps_call:kvs_fetch('consumer_pid', Call), SpawnInfo).

-spec stop(kapps_call:call() | pid()) -> 'ok'.
stop(Srv) when is_pid(Srv) ->
    gen_listener:cast(Srv, 'stop');
stop(Call) ->
    update_call(Call),
    Srv = kapps_call:kvs_fetch('consumer_pid', Call),
    stop(Srv).

-spec transfer(kapps_call:call() | pid()) -> 'ok'.
transfer(Srv) when is_pid(Srv) ->
    gen_listener:cast(Srv, 'transfer');
transfer(Call) ->
    update_call(Call),
    Srv = kapps_call:kvs_fetch('consumer_pid', Call),
    transfer(Srv).

-spec control_usurped(kapps_call:call() | pid()) -> 'ok'.
control_usurped(Srv) when is_pid(Srv) ->
    gen_listener:cast(Srv, 'control_usurped');
control_usurped(Call) ->
    update_call(Call),
    Srv = kapps_call:kvs_fetch('consumer_pid', Call),
    control_usurped(Srv).

-spec callid_update(kz_term:ne_binary(), kapps_call:call() | pid()) -> 'ok'.
callid_update(CallId, Srv) when is_pid(Srv) ->
    gen_listener:cast(Srv, {'callid_update', CallId});
callid_update(CallId, Call) ->
    Srv = kapps_call:kvs_fetch('consumer_pid', Call),
    callid_update(CallId, Srv).


-spec callid(kapps_call:call() | pid()) -> kz_term:ne_binary().
callid(Srv) when is_pid(Srv) ->
    CallId = gen_server:call(Srv, 'callid', ?MILLISECONDS_IN_SECOND),
    kz_log:put_callid(CallId),
    CallId;
callid(Call) ->
    Srv = kapps_call:kvs_fetch('consumer_pid', Call),
    callid(Srv).

-spec callid(kz_term:api_binary(), kapps_call:call()) -> kz_term:ne_binary().
callid(_, Call) ->
    callid(Call).

-spec queue_name(kapps_call:call() | pid()) -> kz_term:ne_binary().
queue_name(Srv) when is_pid(Srv) ->
    gen_listener:queue_name(Srv);
queue_name(Call) ->
    Srv = kapps_call:kvs_fetch('consumer_pid', Call),
    queue_name(Srv).


-spec control_queue(kapps_call:call() | pid()) -> kz_term:ne_binary().
control_queue(Srv) when is_pid(Srv) -> gen_listener:call(Srv, 'control_queue_name');
control_queue(Call) -> control_queue(kapps_call:kvs_fetch('consumer_pid', Call)).

-spec control_queue(kz_term:api_binary(), kapps_call:call() | pid()) -> kz_term:ne_binary().
control_queue(_, Call) -> control_queue(Call).

-spec get_branch_keys(kapps_call:call() | pid()) -> {'branch_keys', kz_json:path()}.
get_branch_keys(Srv) when is_pid(Srv) ->
    gen_listener:call(Srv, 'get_branch_keys');
get_branch_keys(Call) ->
    Srv = kapps_call:kvs_fetch('consumer_pid', Call),
    get_branch_keys(Srv).

-spec get_all_branch_keys(kapps_call:call() | pid()) -> {'branch_keys', kz_json:path()}.
get_all_branch_keys(Srv) when is_pid(Srv) ->
    gen_listener:call(Srv, {'get_branch_keys', 'all'});
get_all_branch_keys(Call) ->
    Srv = kapps_call:kvs_fetch('consumer_pid', Call),
    get_all_branch_keys(Srv).

-spec attempt(kapps_call:call() | pid()) ->
                     {'attempt_resp', 'ok'} |
                     {'attempt_resp', {'error', any()}}.
attempt(Srv) -> attempt(<<"_">>, Srv).

-spec attempt(kz_json:key(), kapps_call:call() | pid()) ->
                     {'attempt_resp', 'ok'} |
                     {'attempt_resp', {'error', any()}}.
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

-spec relay_amqp(kz_json:object(), kz_term:proplist()) -> any().
relay_amqp(JObj, Props) ->
    Pids = case props:get_value('cf_module_pid', Props) of
               P when is_pid(P) -> [P | props:get_value('cf_event_pids', Props, [])];
               _ -> props:get_value('cf_event_pids', Props, [])
           end,
    [kapps_call_command:relay_event(Pid, JObj) || Pid <- Pids, is_pid(Pid)].

-spec send_amqp(pid() | kapps_call:call(), kz_term:api_terms(), kz_amqp_worker:publish_fun()) -> 'ok'.
send_amqp(Srv, API, PubFun) when is_pid(Srv), is_function(PubFun, 1) ->
    gen_listener:cast(Srv, {'send_amqp', API, PubFun});
send_amqp(Call, API, PubFun) when is_function(PubFun, 1) ->
    send_amqp(kapps_call:kvs_fetch('consumer_pid', Call), API, PubFun).

%%%=============================================================================
%%% gen_listener callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server.
%% @end
%%------------------------------------------------------------------------------
-spec init([kapps_call:call()]) -> {'ok', state()}.
init([Call]) ->
    process_flag('trap_exit', 'true'),
    CallId = kapps_call:call_id(Call),
    kz_log:put_callid(CallId),
    gen_listener:cast(self(), 'initialize'),
    {'ok', #state{call=Call}}.

%%------------------------------------------------------------------------------
%% @doc Handling call messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
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
    case kz_json:get_value([<<"children">>, Key], Flow) of
        'undefined' ->
            lager:info("attempted 'undefined' child ~s", [Key]),
            Reply = {'attempt_resp', {'error', 'undefined'}},
            {'reply', Reply, State};
        NewFlow ->
            case kz_json:is_empty(NewFlow) of
                'true' ->
                    lager:info("attempted empty child ~s", [Key]),
                    Reply = {'attempt_resp', {'error', 'empty'}},
                    {'reply', Reply, State};
                'false' ->
                    lager:info("branching to attempted child ~s", [Key]),
                    Reply = {'attempt_resp', 'ok'},
                    {'reply', Reply, launch_cf_module(State#state{flow = NewFlow})}
            end
    end;
handle_call('wildcard_is_empty', _From, #state{flow = Flow}=State) ->
    case kz_json:get_value([<<"children">>, <<"_">>], Flow) of
        'undefined' -> {'reply', 'true', State};
        ChildFlow -> {'reply', kz_json:is_empty(ChildFlow), State}
    end;
handle_call(_Request, _From, State) ->
    Reply = {'error', 'unimplemented'},
    {'reply', Reply, State}.

%%------------------------------------------------------------------------------
%% @doc Handling cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast({'set_call', Call}, State) ->
    {'noreply', State#state{call=Call}};
handle_cast({'update_call', NewCall}, #state{call=OldCall, queue=Q}=State) ->
    Action = kapps_call:kvs_fetch('cf_last_action', OldCall),
    Call1 = kapps_call:set_controller_queue(Q, NewCall),
    Call = kapps_call:kvs_store('cf_last_action', Action, Call1),
    {'noreply', State#state{call=Call}};

handle_cast({'continue', Key}, #state{flow=Flow
                                     }=State) ->
    lager:info("continuing to child '~s'", [Key]),

    case kz_json:get_value([<<"children">>, Key], Flow) of
        'undefined' when Key =:= <<"_">> ->
            lager:info("wildcard child does not exist, we are lost...hanging up"),
            stop(self()),
            {'noreply', State};
        'undefined' ->
            lager:info("requested child does not exist, trying wild card ~s", [Key]),
            continue(self()),
            {'noreply', State};
        NewFlow ->
            case kz_json:is_empty(NewFlow) of
                'false' -> {'noreply', launch_cf_module(State#state{flow=NewFlow})};
                'true' ->
                    stop(self()),
                    {'noreply', State}
            end
    end;
handle_cast('stop', #state{call=Call}=State) ->
    _ = kz_process:spawn(fun doodle_util:save_sms/1, [kapps_call:clear_helpers(Call)]),
    {'stop', 'normal', State};
handle_cast('transfer', State) ->
    {'stop', {'shutdown', 'transfer'}, State};
handle_cast('control_usurped', State) ->
    {'stop', {'shutdown', 'control_usurped'}, State};
handle_cast({'branch', NewFlow}, State) ->
    lager:info("textflow has been branched"),
    {'noreply', launch_cf_module(State#state{flow=NewFlow})};
handle_cast({'callid_update', NewCallId}, #state{call=Call}=State) ->
    kz_log:put_callid(NewCallId),
    PrevCallId = kapps_call:call_id_direct(Call),
    lager:info("updating callid to ~s (from ~s), catch you on the flip side", [NewCallId, PrevCallId]),
    lager:info("removing call event bindings for ~s", [PrevCallId]),
    gen_listener:rm_binding(self(), 'call', [{'callid', PrevCallId}]),
    lager:info("binding to new call events"),
    gen_listener:add_binding(self(), 'call', [{'callid', NewCallId}]),
    {'noreply', State#state{call=kapps_call:set_call_id(NewCallId, Call)}};
handle_cast({'add_event_listener', {M, A}}, #state{call=Call}=State) ->
    lager:debug("trying to start evt listener ~s: ~p", [M, A]),
    try doodle_event_handler_sup:new(event_listener_name(Call, M), M, [kapps_call:clear_helpers(Call) | A]) of
        {'ok', P} when is_pid(P) ->
            lager:debug("started event listener ~p from ~s", [P, M]),
            {'noreply', State};
        _E ->
            lager:debug("error starting event listener: ~p", [_E]),
            {'noreply', State}
    catch
        _:_R ->
            lager:info("failed to spawn ~s:~s: ~p", [M, _R]),
            {'noreply', State}
    end;
handle_cast('initialize', #state{call=Call}) ->
    log_call_information(Call),
    Flow = kapps_call:kvs_fetch('cf_flow', Call),
    Updaters = [fun(C) -> kapps_call:kvs_store('consumer_pid', self(), C) end
               ,fun(C) -> kapps_call:call_id_helper(fun callid/2, C) end
               ,fun(C) -> kapps_call:control_queue_helper(fun control_queue/2, C) end
               ],
    CallWithHelpers = lists:foldr(fun(F, C) -> F(C) end, Call, Updaters),
    {'noreply', #state{call=CallWithHelpers
                      ,flow=Flow
                      }};
handle_cast({'gen_listener', {'created_queue', Q}}, #state{call=Call}=State) ->
    {'noreply', launch_cf_module(State#state{queue=Q
                                            ,call=kapps_call:set_controller_queue(Q, Call)
                                            })};
handle_cast({'send_amqp', API, PubFun}, #state{queue=Q}=State) ->
    send_amqp_message(API, PubFun, Q),
    {'noreply', State};
handle_cast({'gen_listener',{'is_consuming',_IsConsuming}}, State) ->
    {'noreply', State};
handle_cast(_Msg, State) ->
    lager:debug("unhandled cast: ~p", [_Msg]),
    {'noreply', State}.

event_listener_name(Call, Module) ->
    <<(kapps_call:call_id_direct(Call))/binary, "-", (kz_term:to_binary(Module))/binary>>.

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
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
    lager:debug("cf module ~s down normally", [kapps_call:kvs_fetch('cf_last_action', Call)]),
    {'noreply', State#state{cf_module_old_pid='undefined'}};
handle_info({'EXIT', Pid, _Reason}, #state{cf_module_old_pid={Pid, Ref}
                                          ,call=Call
                                          }=State) ->
    erlang:demonitor(Ref, ['flush']),
    LastAction = kapps_call:kvs_fetch('cf_last_action', Call),
    lager:error("action ~s died unexpectedly: ~p", [LastAction, _Reason]),
    {'noreply', State#state{cf_module_old_pid='undefined'}};
handle_info(_Msg, State) ->
    lager:debug("unhandled message: ~p", [_Msg]),
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec handle_event(kz_json:object(), state()) -> gen_listener:handle_event_return().
handle_event(JObj, #state{cf_module_pid=PidRef
                         ,call=Call
                         ,self=Self
                         }) ->
    CallId = kapps_call:call_id_direct(Call),
    SmsId = kapps_call:kvs_fetch(<<"sms_docid">>, Call),
    Others = kapps_call:kvs_fetch('cf_event_pids', [], Call),
    case {kz_util:get_event_type(JObj), kz_json:get_value(<<"Call-ID">>, JObj)} of
        {{<<"call_event">>, <<"CHANNEL_TRANSFEREE">>}, _} ->
            ExeFetchId = kapps_call:custom_channel_var(<<"Fetch-ID">>, Call),
            TransferFetchId = kz_json:get_value([<<"Custom-Channel-Vars">>, <<"Fetch-ID">>], JObj),
            _ = case ExeFetchId =:= TransferFetchId of
                    'false' -> 'ok';
                    'true' -> transfer(Call)
                end,
            'ignore';
        {{<<"call_event">>, <<"CHANNEL_REPLACED">>}, _} ->
            ExeFetchId = kapps_call:custom_channel_var(<<"Fetch-ID">>, Call),
            TransferFetchId = kz_json:get_value([<<"Custom-Channel-Vars">>, <<"Fetch-ID">>], JObj),
            case ExeFetchId =:= TransferFetchId of
                'false' -> 'ignore';
                'true' ->
                    ReplacedBy = kz_json:get_value(<<"Replaced-By">>, JObj),
                    callid_update(ReplacedBy, Call),
                    {'reply', [{'cf_module_pid', get_pid(PidRef)}
                              ,{'cf_event_pids', Others}
                              ]}
            end;
        {{<<"call_event">>, <<"usurp_control">>}, CallId} ->
            _ = case kapps_call:custom_channel_var(<<"Fetch-ID">>, Call)
                    =:= kz_json:get_value(<<"Fetch-ID">>, JObj)
                of
                    'false' -> control_usurped(Self);
                    'true' -> 'ok'
                end,
            'ignore';
        {{<<"error">>, _}, _} ->
            case kz_json:get_value([<<"Request">>, <<"Call-ID">>], JObj) of
                CallId -> {'reply', [{'cf_module_pid', get_pid(PidRef)}
                                    ,{'cf_event_pids', Others}
                                    ]};
                'undefined' -> {'reply', [{'cf_module_pid', get_pid(PidRef)}
                                         ,{'cf_event_pids', Others}
                                         ]};
                _Else -> 'ignore'
            end;
        {_, CallId} ->
            {'reply', [{'cf_module_pid', get_pid(PidRef)}
                      ,{'cf_event_pids', Others}
                      ]};
        {_, SmsId} ->
            {'reply', [{'cf_module_pid', get_pid(PidRef)}
                      ,{'cf_event_pids', Others}
                      ]};
        {{_Cat, _Name}, _Else} when Others =:= [] ->
            lager:info("received ~s (~s) from call ~s while relaying for ~s"
                      , [_Cat, _Name, _Else, CallId]),
            'ignore';
        {_Evt, _Else} ->
            lager:info("the others want to know about ~p", [_Evt]),
            {'reply', [{'cf_event_pids', Others}]}
    end.

-spec get_pid({pid(), any()}) -> pid().
get_pid({Pid, _}) when is_pid(Pid) -> Pid;
get_pid(_) -> 'undefined'.

%%------------------------------------------------------------------------------
%% @doc This function is called by a `gen_listener' when it is about to
%% terminate. It should be the opposite of `Module:init/1' and do any
%% necessary cleaning up. When it returns, the `gen_listener' terminates
%% with Reason. The return value is ignored.
%%
%% @end
%%------------------------------------------------------------------------------
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, #state{cf_module_pid='undefined'
                         }) ->
    lager:info("textflow execution has been stopped: ~p", [_Reason]);
terminate(_Reason, #state{cf_module_pid={Pid, _}
                         }) ->
    exit(Pid, 'kill'),
    lager:info("textflow execution has been stopped: ~p", [_Reason]).

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
%% this function determines if the callflow module specified at the
%% current node is 'available' and attempts to launch it if so.
%% Otherwise it will advance to the next child in the flow
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec launch_cf_module(state()) -> state().
launch_cf_module(#state{call=Call
                       ,flow=Flow
                       ,cf_module_pid=OldPidRef
                       }=State) ->
    Module = <<(cf_module_prefix(Call))/binary, (kz_json:get_value(<<"module">>, Flow))/binary>>,
    Data = kz_json:get_value(<<"data">>, Flow, kz_json:new()),
    {PidRef, Action} = maybe_start_cf_module(Module, Data, Call),
    _ = cf_link(PidRef),
    State#state{cf_module_pid=PidRef
               ,cf_module_old_pid=OldPidRef
               ,call=kapps_call:kvs_store('cf_last_action', Action, Call)
               }.

-spec cf_link('undefined' | kz_term:pid_ref()) -> 'true'.
cf_link('undefined') -> 'true';
cf_link(PidRef) ->
    link(get_pid(PidRef)).

-spec cf_module_prefix(kapps_call:call()) -> kz_term:ne_binary().
cf_module_prefix(Call) ->
    cf_module_prefix(Call, kapps_call:resource_type(Call)).

-spec cf_module_prefix(kapps_call:call(), kz_term:ne_binary()) -> kz_term:ne_binary().
cf_module_prefix(_Call, <<"sms">>) -> <<"cf_sms_">>;
cf_module_prefix(_Call, _) -> <<"cf_">>.

-spec maybe_start_cf_module(kz_term:ne_binary(), kz_term:proplist(), kapps_call:call()) ->
                                   {kz_term:pid_ref() | 'undefined', atom()}.
maybe_start_cf_module(ModuleBin, Data, Call) ->
    CFModule = kz_term:to_atom(ModuleBin, 'true'),
    case kz_module:is_exported(CFModule, 'handle', 2) of
        'true' ->
            lager:info("moving to action '~s'", [CFModule]),
            spawn_cf_module(CFModule, Data, Call);
        'false' ->
            cf_module_skip(ModuleBin, Call)
    end.

-spec cf_module_skip(CFModule, kapps_call:call()) ->
                            {'undefined', CFModule}.
cf_module_skip(CFModule, _Call) ->
    lager:info("unknown textflow action '~s', skipping to next action", [CFModule]),
    continue(self()),
    {'undefined', CFModule}.

%%------------------------------------------------------------------------------
%% @doc helper function to spawn a linked callflow module, from the entry
%% point 'handle' having set the callid on the new process first
%% @end
%%------------------------------------------------------------------------------
-spec spawn_cf_module(CFModule, list(), kapps_call:call()) ->
                             {kz_term:pid_ref(), CFModule}.
spawn_cf_module(CFModule, Data, Call) ->
    AMQPConsumer = kz_amqp_channel:consumer_pid(),
    {kz_process:spawn_monitor(fun cf_module_task/4, [CFModule, Data, Call, AMQPConsumer])
    ,CFModule
    }.

-spec cf_module_task(atom(), list(), kapps_call:call(), pid()) -> any().
cf_module_task(CFModule, Data, Call, AMQPConsumer) ->
    _ = kz_amqp_channel:consumer_pid(AMQPConsumer),
    kz_log:put_callid(kapps_call:call_id_direct(Call)),
    try CFModule:handle(Data, Call) of
        _ -> 'ok'
    catch
        ?STACKTRACE(_E, R, ST)
        lager:info("action ~s died unexpectedly (~s): ~p", [CFModule, _E, R]),
        kz_log:log_stacktrace(ST),
        throw(R)
        end.

%%------------------------------------------------------------------------------
%% @doc unlike the kapps_call_command this send command does not call the
%% functions of this module to form the headers, nor does it set
%% the reply queue.  Used when this module is terminating to send
%% a hangup command without relying on the (now terminated) doodle_exe.
%% @end
%%------------------------------------------------------------------------------
-spec send_amqp_message(kz_term:api_terms(), kz_amqp_worker:publish_fun(), kz_term:ne_binary()) -> 'ok'.
send_amqp_message(API, PubFun, Q) ->
    PubFun(add_server_id(API, Q)).

-spec add_server_id(kz_term:api_terms(), kz_term:ne_binary()) -> kz_term:api_terms().
add_server_id(API, Q) when is_list(API) ->
    [{<<"Server-ID">>, Q} | props:delete(<<"Server-ID">>, API)];
add_server_id(API, Q) ->
    kz_json:set_value(<<"Server-ID">>, Q, API).

-spec log_call_information(kapps_call:call()) -> 'ok'.
log_call_information(Call) ->
    lager:info("executing flow ~s", [kapps_call:kvs_fetch('cf_flow_id', Call)]),
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
