%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2013, 2600Hz
%%% @doc
%%%
%%% Behaviour for setting up an AMQP listener.
%%% Add/rm responders for Event-Cat/Event-Name pairs. Each responder
%%% corresponds to a module that has defined a handle/1 function, receiving
%%% the wh_json:object() from the AMQP request.
%%%
%%% Params :: [
%%%   {bindings, [ {atom(), wh_proplist()}, ...]} -> the type of bindings, with optional properties to pass along
%%%   {responders, [ {responder, [ {<<"event-category">>, <<"event-name">>}, ...]} ]
%%%      responder is the module name to call handle_req/2 on for those category/name combos
%%%      responder can also be {module, function}, to call module:function/2 instead of handle_req/2
%%%      Responder can optionally define a function/3 (or handle_req/3) that will be called with the 3rd arg
%%%      consisting of the delivery options including exchange and routing_key
%%%   {queue_name, <<"some name">>} -> optional, if you want a named queue
%%%   {queue_options, [{key, value}]} -> optional, if the queue requires different params
%%%   {consume_options, [{key, value}]} -> optional, if the consumption requires special params
%%%   {basic_qos, integer()} -> optional, if QoS is being set on this queue
%%% ]
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(gen_listener).

-behaviour(gen_server).

-export([start_link/3
         ,start_link/4
        ]).

-export([queue_name/1
         ,responders/1
         ,is_consuming/1
        ]).

-export([add_queue/4
         ,other_queues/1
         ,rm_queue/2
        ]).

%% gen_server callbacks
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,terminate/2
         ,code_change/3
         ,format_status/2
        ]).

%% gen_server API
-export([call/2
         ,call/3
         ,cast/2
         ,delayed_cast/3
         ,reply/2
        ]).

%% gen_listener API
-export([add_responder/3
         ,rm_responder/2
         ,rm_responder/3
        ]).

-export([add_binding/2
         ,add_binding/3
         ,rm_binding/2
         ,rm_binding/3
        ]).

-export([ack/2
         ,nack/2
        ]).

-export([execute/4
         ,execute/3
         ,execute/2
        ]).

-export([federated_event/3]).
-export([handle_event/4]).
-export([distribute_event/3]).

-include_lib("whistle/include/wh_amqp.hrl").
-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_log.hrl").
-include_lib("rabbitmq_server/plugins-src/rabbitmq-erlang-client/include/amqp_client.hrl").

-define(TIMEOUT_RETRY_CONN, 5000).
-define(CALLBACK_TIMEOUT_MSG, 'callback_timeout').

-define(START_TIMEOUT, 500).
-define(MAX_TIMEOUT, 5000).

-define(BIND_WAIT, 100).

-type module_state() :: term().
-record(state, {
          queue :: api_binary()
         ,is_consuming = 'false' :: boolean()
         ,responders = [] :: listener_utils:responders() %% { {EvtCat, EvtName}, Module }
         ,bindings = [] :: bindings() %% {authentication, [{key, value},...]}
         ,params = [] :: wh_proplist()
         ,module :: atom()
         ,module_state :: module_state()
         ,module_timeout_ref :: reference() % when the client sets a timeout, gen_listener calls shouldn't negate it, only calls that pass through to the client
         ,other_queues = [] :: [{ne_binary(), {wh_proplist(), wh_proplist()}},...] | [] %% {QueueName, {proplist(), wh_proplist()}}
         ,federators = []
         ,self = self()
         ,consumer_key = wh_amqp_channel:consumer_pid()
         }).

-type state() :: #state{}.

-type handle_event_return() :: {'reply', wh_proplist()} | 'ignore'.

-type binding() :: {atom() | ne_binary(), wh_proplist()}. %% {wapi_module, options}
-type bindings() :: [binding(),...] | [].

-type responder_callback_mod() :: atom() | {atom(), atom()}.
-type responder_callback_mapping() :: {ne_binary(), ne_binary()}.
-type responder_callback_mappings() :: [responder_callback_mapping(),...] | [].
-type responder_start_params() :: [{responder_callback_mod(), responder_callback_mappings()},...].

-type start_params() :: [{'responders', responder_start_params()} |
                         {'bindings', bindings()} |
                         {'queue_name', binary()} |
                         {'queue_options', wh_proplist()} |
                         {'consume_options', wh_proplist()} |
                         {'basic_qos', non_neg_integer()}
                        ].

-export_type([handle_event_return/0]).

%%%===================================================================
%%% API
%%%===================================================================
-callback init(term()) ->
    {'ok', module_state()} |
    {'ok', module_state(), timeout() | 'hibernate'} |
    {'stop', term()} |
    'ignore'.

-callback handle_call(term(), {pid(), term()}, module_state()) ->
    {'reply', term(), module_state()} |
    {'reply', term(), module_state(), timeout() | 'hibernate'} |
    {'noreply', module_state()} |
    {'noreply', module_state(), timeout() | 'hibernate'} |
    {'stop', term(), term(), module_state()} |
    {'stop', term(), module_state()}.

-callback handle_cast(term(), module_state()) ->
    {'noreply', module_state()} |
    {'noreply', module_state(), timeout() | 'hibernate'} |
    {'stop', term(), module_state()}.

-callback handle_info(timeout() | term(), module_state()) ->
    {'noreply', module_state()} |
    {'noreply', module_state(), timeout() | 'hibernate'} |
    {'stop', term(), module_state()}.

-callback handle_event(wh_json:object(), module_state()) ->
    handle_event_return().

-callback terminate('normal' | 'shutdown' | {'shutdown', term()} | term(), module_state()) ->
    term().
-callback code_change(term() | {'down', term()}, module_state(), term()) ->
    {'ok', state()} | {'error', term()}.

  -spec start_link(atom(), start_params(), list()) -> startlink_ret().
start_link(Module, Params, InitArgs) ->
    gen_server:start_link(?MODULE, [Module, Params, InitArgs], []).

-spec start_link({'local', atom()} | {'global', term()}, atom(), start_params(), list()) -> startlink_ret().
start_link(Name, Module, Params, InitArgs) ->
    gen_server:start_link(Name, ?MODULE, [Module, Params, InitArgs], []).

-spec queue_name(server_ref()) -> ne_binary().
queue_name(Srv) -> gen_server:call(Srv, 'queue_name').

-spec is_consuming(server_ref()) -> boolean().
is_consuming(Srv) -> gen_server:call(Srv, 'is_consuming').

-spec responders(server_ref()) -> listener_utils:responders().
responders(Srv) -> gen_server:call(Srv, 'responders').

-spec ack(server_ref(), #'basic.deliver'{}) -> 'ok'.
ack(Srv, Delivery) -> gen_server:cast(Srv, {'ack', Delivery}).

-spec nack(server_ref(), #'basic.deliver'{}) -> 'ok'.
nack(Srv, Delivery) -> gen_server:cast(Srv, {'nack', Delivery}).

%% API functions that mirror gen_server:call,cast,reply
-spec call(server_ref(), term()) -> term().
call(Name, Request) -> gen_server:call(Name, Request).

-spec call(server_ref(), term(), wh_timeout()) -> term().
call(Name, Request, Timeout) -> gen_server:call(Name, Request, Timeout).

-spec cast(server_ref(), term()) -> 'ok'.
cast(Name, Request) -> gen_server:cast(Name, Request).

-spec delayed_cast(server_ref(), term(), pos_integer()) -> 'ok'.
delayed_cast(Name, Request, Wait) when is_integer(Wait), Wait > 0 ->
    spawn(fun() ->
                  timer:sleep(Wait),
                  cast(Name, Request)
          end),
    'ok'.

-spec reply({pid(), reference()}, term()) -> no_return().
reply(From, Msg) -> gen_server:reply(From, Msg).

-spec add_responder(server_ref(), responder_callback_mod(), responder_callback_mapping() | responder_callback_mappings()) -> 'ok'.
add_responder(Srv, Responder, Key) when not is_list(Key) ->
    add_responder(Srv, Responder, [Key]);
add_responder(Srv, Responder, [{_,_}|_] = Keys) ->
    gen_server:cast(Srv, {'add_responder', Responder, Keys}).

-spec rm_responder(server_ref(), responder_callback_mod()) -> 'ok'.
%% empty list removes all
rm_responder(Srv, Responder) -> rm_responder(Srv, Responder, []).

-spec rm_responder(server_ref(), responder_callback_mod(), responder_callback_mappings()) -> 'ok'.
rm_responder(Srv, Responder, {_,_}=Key) ->
    rm_responder(Srv, Responder, [Key]);
rm_responder(Srv, Responder, Keys) ->
    gen_server:cast(Srv, {'rm_responder', Responder, Keys}).

-spec add_binding(server_ref(), binding() | ne_binary() | atom()) -> 'ok'.
add_binding(Srv, {Binding, Props}) when is_list(Props)
                                        ,(is_atom(Binding) orelse is_binary(Binding)) ->
    gen_server:cast(Srv, {'add_binding', Binding, Props});
add_binding(Srv, Binding) when is_binary(Binding) orelse is_atom(Binding) ->
    gen_server:cast(Srv, {'add_binding', wh_util:to_binary(Binding), []}).

-spec add_binding(server_ref(), ne_binary() | atom(), wh_proplist()) -> 'ok'.
add_binding(Srv, Binding, Props) when is_binary(Binding) orelse is_atom(Binding) ->
    gen_server:cast(Srv, {'add_binding', wh_util:to_binary(Binding), Props}).

%% It is expected that responders have been set up already, prior to binding the new queue
-spec add_queue(server_ref(), binary(), wh_proplist(), binding() | bindings()) ->
                             {'ok', ne_binary()} |
                             {'error', term()}.
add_queue(Srv, QueueName, QueueProps, {_Type, _Props}=Binding) ->
    add_queue(Srv, QueueName, QueueProps, [Binding]);
add_queue(Srv, QueueName, QueueProps, [{_,_}|_]=Bindings) ->
    gen_server:call(Srv, {'add_queue', QueueName, QueueProps, Bindings}).

-spec rm_queue(server_ref(), ne_binary()) -> 'ok'.
rm_queue(Srv, ?NE_BINARY = QueueName) ->
    gen_server:cast(Srv, {'rm_queue', QueueName}).

-spec other_queues(server_ref()) -> ne_binaries().
other_queues(Srv) -> gen_server:call(Srv, 'other_queues').

-spec rm_binding(server_ref(), binding()) -> 'ok'.
rm_binding(Srv, {Binding, Props}) ->
    rm_binding(Srv, Binding, Props).

-spec rm_binding(server_ref(), ne_binary() | atom(), wh_proplist()) -> 'ok'.
rm_binding(Srv, Binding, Props) ->
    gen_server:cast(Srv, {'rm_binding', wh_util:to_binary(Binding), Props}).

-spec federated_event(server_ref(), wh_json:object(), #'basic.deliver'{}) -> 'ok'.
federated_event(Srv, JObj, BasicDeliver) ->
    gen_server:cast(Srv, {'federated_event', JObj, BasicDeliver}).

-spec execute(server_ref(), module(), atom(), [term()]) -> 'ok'.
execute(Srv, Module, Function, Args) ->
    gen_server:cast(Srv, {'$execute', Module, Function, Args}).

-spec execute(server_ref(), atom(), [term()]) -> 'ok'.
execute(Srv, Function, Args) ->
    gen_server:cast(Srv, {'$execute', Function, Args}).

-spec execute(server_ref(), function()) -> 'ok'.
execute(Srv, Function) when is_function(Function) ->
    gen_server:cast(Srv, {'$execute', Function}).

%%%===================================================================
%%% gen_server callbacks
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
-spec init([atom() | wh_proplist(),...]) ->
                  {'ok', state()} |
                  {'stop', term()} |
                  'ignore'.
init([Module, Params, InitArgs]) ->
    process_flag('trap_exit', 'true'),
    put('callid', Module),
    lager:debug("starting new gen_listener proc"),
    case erlang:function_exported(Module, 'init', 1)
        andalso Module:init(InitArgs)
    of
        {'ok', MS} -> init(Module, Params, MS, 'undefined');
        {'ok', MS, 'hibernate'} -> init(Module, Params, MS, 'undefined');
        {'ok', MS, Timeout} -> init(Module, Params, MS, start_timer(Timeout));
        {'stop', _R} = STOP -> STOP;
        'ignore' -> 'ignore'
    end.

init(Module, Params, ModState, TimeoutRef) ->
    Responders = props:get_value('responders', Params, []),
    _ = [add_responder(self(), Mod, Events)
         || {Mod, Events} <- Responders
        ],
    _ = wh_amqp_channel:requisition(),
    case maybe_start_federators(Params) of
        {'ok', Federators} ->
            {'ok', #state{module=Module
                          ,module_state=ModState
                          ,module_timeout_ref=TimeoutRef
                          ,params=Params
                          ,federators=Federators
                          ,bindings=props:get_value('bindings', Params, [])
                         }};
        'ok' ->
            {'ok', #state{module=Module
                          ,module_state=ModState
                          ,module_timeout_ref=TimeoutRef
                          ,params=Params
                          ,federators=[]
                          ,bindings=props:get_value('bindings', Params, [])
                         }}
    end.

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
-type gen_l_handle_call_ret() :: {'reply', term(), state(), gen_server_timeout()} |
                                 {'noreply', state(), gen_server_timeout()} |
                                 {'stop', term(), state()} | {'stop', term(), term(), state()}.

-spec handle_call(term(), {pid(), reference()}, state()) -> gen_l_handle_call_ret().
handle_call({'add_queue', QueueName, QueueProps, Bindings}, _From, State) ->
    {Q, S} = add_other_queue(QueueName, QueueProps, Bindings, State),
    {'reply', {'ok', Q}, S};
handle_call('other_queues', _From, #state{other_queues=OtherQueues}=State) ->
    {'reply', props:get_keys(OtherQueues), State};
handle_call('queue_name', _From, #state{queue=Q}=State) ->
    {'reply', Q, State};
handle_call('responders', _From, #state{responders=Rs}=State) ->
    {'reply', Rs, State};
handle_call('is_consuming', _From, #state{is_consuming=IsC}=State) ->
    {'reply', IsC, State};
handle_call(Request, From, #state{module=Module
                                  ,module_state=ModState
                                  ,module_timeout_ref=OldRef
                                 }=State) ->
    _ = stop_timer(OldRef),
    case catch Module:handle_call(Request, From, ModState) of
        {'reply', Reply, ModState1} ->
            {'reply', Reply, State#state{module_state=ModState1
                                         ,module_timeout_ref='undefined'
                                        }
             ,'hibernate'};
        {'reply', Reply, ModState1, Timeout} ->
            {'reply', Reply, State#state{module_state=ModState1
                                         ,module_timeout_ref=start_timer(Timeout)
                                        }
             ,'hibernate'};
        {'noreply', ModState1} ->
            {'noreply', State#state{module_state=ModState1}, 'hibernate'};
        {'noreply', ModState1, Timeout} ->
            {'noreply', State#state{module_state=ModState1
                                    ,module_timeout_ref=start_timer(Timeout)
                                   }
             ,'hibernate'};
        {'stop', Reason, ModState1} ->
            {'stop', Reason, State#state{module_state=ModState1}};
        {'stop', Reason, Reply, ModState1} ->
            {'stop', Reason, Reply, State#state{module_state=ModState1}};
        {'EXIT', Why} ->
            lager:alert("exception: ~p", [Why]),
            {'stop', Why, State}
    end.

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
-spec handle_cast(term(), state()) -> handle_cast_ret().
handle_cast({'ack', Delivery}, State) ->
    amqp_util:basic_ack(Delivery),
    {'noreply', State};
handle_cast({'nack', Delivery}, State) ->
    amqp_util:basic_nack(Delivery),
    {'noreply', State};
handle_cast({'add_queue', QueueName, QueueProps, Bindings}, State) ->
    {_, S} = add_other_queue(QueueName, QueueProps, Bindings, State),
    {'noreply', S};
handle_cast({'rm_queue', QueueName}, #state{other_queues=OtherQueues}=State) ->
    _ = [remove_binding(Binding, Props, QueueName)
         || {Binding, Props} <- props:get_value(QueueName, OtherQueues, [])
        ],
    {'noreply', State#state{other_queues=props:delete(QueueName, OtherQueues)}};
handle_cast({'add_responder', Responder, Keys}, #state{responders=Responders}=State) ->
    {'noreply'
     ,State#state{responders=listener_utils:add_responder(Responders, Responder, Keys)}
     ,'hibernate'
    };
handle_cast({'rm_responder', Responder, Keys}, #state{responders=Responders}=State) ->
    {'noreply'
     ,State#state{responders=listener_utils:rm_responder(Responders, Responder, Keys)}
     ,'hibernate'
    };
handle_cast({'add_binding', _, _}=AddBinding, #state{is_consuming='false'}=State) ->
    Time = ?BIND_WAIT + (crypto:rand_uniform(100, 500)), % wait 100 + [100,500) ms before replaying the binding request
    lager:debug("not consuming yet, put binding to end of message queue after ~b ms", [Time]),
    ?MODULE:delayed_cast(self(), AddBinding, Time),
    {'noreply', State};
handle_cast({'add_binding', Binding, Props}, #state{queue=Q
                                                    ,bindings=Bs
                                                   }=State) ->
    case lists:keyfind(Binding, 1, Bs) of
        'false' ->
            create_binding(Binding, Props, Q),
            {'noreply', State#state{bindings=[{Binding, Props}|Bs]}, 'hibernate'};
        {_, Props} -> {'noreply', State};
        {_, _P} ->
            create_binding(Binding, Props, Q),
            {'noreply', State#state{bindings=[{Binding, Props}|Bs]}, 'hibernate'}
    end;
handle_cast({'rm_binding', Binding, Props}, #state{queue=Q
                                                   ,bindings=Bs
                                                  }=State) ->
    KeepBs = lists:filter(fun({B, P}) when B =:= Binding, P =:= Props ->
                                  remove_binding(B, P, Q),
                                  'false';
                             (_) -> 'true'
                          end, Bs),
    {'noreply', State#state{bindings=KeepBs}, 'hibernate'};
handle_cast({'wh_amqp_assignment', {'new_channel', 'true'}}, State) ->
    {'noreply', State};
handle_cast({'wh_amqp_assignment', {'new_channel', 'false'}}
            ,#state{bindings=Bindings
                    ,params=Params
                   }=State) ->
    {'ok', Q} = start_amqp(Params),
    _ = [create_binding(Type, BindProps, Q)
         || {Type, BindProps} <- Bindings
        ],
    _ = erlang:send_after(?TIMEOUT_RETRY_CONN, self(), '$is_gen_listener_consuming'),
    gen_server:cast(self(), {'gen_listener', {'created_queue', Q}}),
    {'noreply', State#state{queue=Q
                            ,is_consuming='false'
                           }};
handle_cast({'federated_event', JObj, BasicDeliver}, State) ->
    spawn(?MODULE, 'distribute_event', [JObj, BasicDeliver, State]),
    {'noreply', State};
handle_cast({'$execute', Module, Function, Args}
            ,#state{federators=[]}=State) ->
    erlang:apply(Module, Function, Args),
    {'noreply', State};
handle_cast({'$execute', Function, Args}
            ,#state{federators=[]}=State) ->
    erlang:apply(Function, Args),
    {'noreply', State};
handle_cast({'$execute', Function}
            ,#state{federators=[]}=State) ->
    Function(),
    {'noreply', State};
handle_cast({'$execute', Module, Function, Args}=Msg
            ,#state{federators=Federators}=State) ->
    erlang:apply(Module, Function, Args),
    _ = [gen_listener:cast(Federator, Msg)
         || Federator <- Federators
        ],
    {'noreply', State};
handle_cast({'$execute', Function, Args}=Msg
            ,#state{federators=Federators}=State) ->
    erlang:apply(Function, Args),
    _ = [gen_listener:cast(Federator, Msg)
         || Federator <- Federators
        ],
    {'noreply', State};
handle_cast({'$execute', Function}=Msg
            ,#state{federators=Federators}=State) ->
    Function(),
    _ = [gen_listener:cast(Federator, Msg)
         || Federator <- Federators
        ],
    {'noreply', State};
handle_cast(Message, #state{module=Module
                            ,module_state=ModState
                            ,module_timeout_ref=OldRef
                           }=State) ->
    _ = stop_timer(OldRef),
    case catch Module:handle_cast(Message, ModState) of
        {'noreply', ModState1} ->
            {'noreply', State#state{module_state=ModState1}, 'hibernate'};
        {'noreply', ModState1, Timeout} ->
            Ref = start_timer(Timeout),
            {'noreply', State#state{module_state=ModState1
                                    ,module_timeout_ref=Ref
                                   }
             ,'hibernate'
            };
        {'stop', Reason, ModState1} ->
            {'stop', Reason, State#state{module_state=ModState1}};
        {'EXIT', {Reason, _ST}} ->
            lager:debug("exception: ~p: ~p", [Reason, _ST]),
            ST = erlang:get_stacktrace(),
            wh_util:log_stacktrace(ST),
            {'stop', Reason, State}
    end.

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
-spec handle_info(term(), state()) -> handle_info_ret().
handle_info({#'basic.deliver'{}=BD, #amqp_msg{props=#'P_basic'{content_type=CT}
                                              ,payload=Payload}}, State) ->
    spawn(?MODULE, 'handle_event', [Payload, CT, BD, State]),
    {'noreply', State, 'hibernate'};
handle_info(#'basic.consume_ok'{consumer_tag=CTag}, #state{queue='undefined'}=State) ->
    lager:debug("received consume ok (~s) for abandoned queue", [CTag]),
    {'noreply', State};
handle_info(#'basic.consume_ok'{}, State) ->
    gen_server:cast(self(), {'gen_listener', {'is_consuming', 'true'}}),
    {'noreply', State#state{is_consuming='true'}};
handle_info(#'basic.cancel_ok'{consumer_tag=CTag}, State) ->
    lager:debug("recv a basic.cancel_ok for tag ~s", [CTag]),
    gen_server:cast(self(), {'gen_listener', {'is_consuming', 'false'}}),
    {'noreply', State#state{is_consuming='false'}};
handle_info('$is_gen_listener_consuming', #state{is_consuming='false'}=State) ->
    _ = (catch wh_amqp_channel:release()),
    _ = wh_amqp_channel:requisition(),
    {'noreply', State#state{queue='undefined'}};
handle_info('$is_gen_listener_consuming', State) ->
    {'noreply', State};
handle_info(?CALLBACK_TIMEOUT_MSG, State) ->
    handle_callback_info('timeout', State);
handle_info(Message, State) ->
    handle_callback_info(Message, State).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handles the AMQP messages prior to the spawning a handler.
%% Allows listeners to pass options to handlers
%%
%% @spec handle_event(JObj, State) -> {'reply', Options} | ignore
%% @end
%%--------------------------------------------------------------------
-spec handle_event(ne_binary(), ne_binary(), #'basic.deliver'{}, #state{}) ->  'ok'.
handle_event(Payload, <<"application/json">>, BasicDeliver, State) ->
    JObj = wh_json:decode(Payload),
    _ = wh_util:put_callid(JObj),
    distribute_event(JObj, BasicDeliver, State);
handle_event(Payload, <<"application/erlang">>, BasicDeliver, State) ->
    JObj = binary_to_term(Payload),
    _ = wh_util:put_callid(JObj),
    distribute_event(JObj, BasicDeliver, State).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(Reason, #state{module=Module
                         ,module_state=ModState
                        }) ->
    _ = (catch Module:terminate(Reason, ModState)),
    _ = (catch wh_amqp_channel:release()),
    lager:debug("~s terminated cleanly, going down", [Module]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {'ok', NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVersion, State, _Extra) ->
    {'ok', State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec maybe_start_federators(wh_proplist()) -> 'ok' | {'ok', pids()}.
maybe_start_federators(Params) ->
    case wh_amqp_connections:federated_brokers() of
        [] -> 'ok';
        FederatedBrokers -> start_federators(FederatedBrokers, Params)
    end.

-spec start_federators(ne_binaries(), wh_proplist()) -> 'ok' | {'ok', pids()}.
start_federators(FederatedBrokers, Params) ->
    Bindings = props:get_value('bindings', Params, []),
    case get_federated_bindings(Bindings) of
        [] -> 'ok';
        FederateBindings ->
            FederateParams = create_federated_params(FederateBindings, Params),
            start_federators(FederatedBrokers, FederateParams, [])
    end.

-spec start_federators(ne_binaries(), wh_proplist(), pids()) -> {'ok', pids()}.
start_federators([], _, Pids) -> {'ok', Pids};
start_federators([Broker|Brokers], FederateParams, Pids) ->
    {'ok', Pid} = listener_federator:start_link(self(), Broker, FederateParams),
    start_federators(Brokers, FederateParams, [Pid|Pids]).

-spec get_federated_bindings(wh_proplist()) -> wh_proplist().
get_federated_bindings(Bindings) ->
    lists:foldr(fun({Binding, Props}, Federate) ->
                        case props:get_is_true('federate', Props, 'false') of
                            'false' -> Federate;
                            'true' ->
                                lager:debug("found federated binding for ~s: ~p"
                                            ,[Binding, Props]),
                                [{Binding, props:delete('federate', Props)}
                                 | Federate
                                ]
                        end
                end, [], Bindings).

-spec create_federated_params(wh_proplist(), wh_proplist()) -> wh_proplist().
create_federated_params(FederateBindings, Params) ->
    [{'responders', []}
     ,{'bindings', FederateBindings}
     ,{'queue_name', federated_queue_name(Params)}
     ,{'queue_options', props:get_value('queue_options', Params, [])}
     ,{'consume_options', props:get_value('consume_options', Params, [])}
    ].

-spec federated_queue_name(wh_proplist()) -> api_binary().
federated_queue_name(Params) ->
    QueueName = props:get_value('queue_name', Params, <<>>),
    case wh_util:is_empty(QueueName) of
        'true' -> QueueName;
        'false' ->
            [Zone] = wh_config:get(wh_config:get_node_section_name(), 'zone'),
            <<QueueName/binary, "-", (wh_util:to_binary(Zone))/binary>>
    end.

handle_callback_info(Message, #state{module=Module
                                     ,module_state=ModState
                                     ,module_timeout_ref=OldRef
                                    }=State) ->
    _ = stop_timer(OldRef),
    case catch Module:handle_info(Message, ModState) of
        {'noreply', ModState1} ->
            {'noreply', State#state{module_state=ModState1}, 'hibernate'};
        {'noreply', ModState1, Timeout} ->
            Ref = start_timer(Timeout),
            {'noreply', State#state{module_state=ModState1
                                    ,module_timeout_ref=Ref
                                   }
             ,'hibernate'
            };
        {'stop', Reason, ModState1} ->
            {'stop', Reason, State#state{module_state=ModState1}};
        {'EXIT', Why} ->
            lager:alert("exception: ~p", [Why]),
            {'stop', Why, State}
    end.

format_status(_Opt, [_PDict, #state{module=Module
                                    ,module_state=ModState
                                   }=State]) ->
    [{'data', [{"Module State", ModState}
               ,{"Module", Module}
              ]}
     ,{'data', [{"Listener State", State}]}
    ].

-spec distribute_event(wh_json:object(), #'basic.deliver'{}, #state{}) -> 'ok'.
distribute_event(JObj, BasicDeliver, State) ->
    case callback_handle_event(JObj, BasicDeliver, State) of
        'ignore' -> 'ok';
        Props -> distribute_event(Props, JObj, BasicDeliver, State)
    end.

-spec distribute_event(wh_proplist(), wh_json:object(), #'basic.deliver'{}, #state{}) -> 'ok'.
distribute_event(Props, JObj, BasicDeliver, #state{responders=Responders
                                                   ,consumer_key=ConsumerKey
                                                  }) ->
    Key = wh_util:get_event_type(JObj),
    _ = [spawn(fun() ->
                       _ = wh_util:put_callid(JObj),
                       _ = wh_amqp_channel:consumer_pid(ConsumerKey),
                       case erlang:function_exported(Module, Fun, 3) of
                           'true' -> Module:Fun(JObj, Props, BasicDeliver);
                           'false' -> Module:Fun(JObj, Props)
                       end
               end)
         || {Evt, {Module, Fun}} <- Responders,
            maybe_event_matches_key(Key, Evt)
        ],
    'ok'.

-spec callback_handle_event(wh_json:object(), #'basic.deliver'{}, state()) -> 'ignore' | wh_proplist().
callback_handle_event(JObj
                      ,BasicDeliver
                      ,#state{module=Module
                              ,module_state=ModState
                              ,queue=Queue
                              ,other_queues=OtherQueues
                              ,self=Self
                             }) ->
    OtherQueueNames = props:get_keys(OtherQueues),
    case
        case erlang:function_exported(Module, 'handle_event', 3) of
            'true' -> catch Module:handle_event(JObj, BasicDeliver, ModState);
            'false' -> catch Module:handle_event(JObj, ModState)
        end
    of
        'ignore' -> 'ignore';
        {'reply', Props} when is_list(Props) ->
            [{'server', Self}
             ,{'queue', Queue}
             ,{'other_queues', OtherQueueNames}
             | Props
            ];
        {'EXIT', _Why} ->
            [{'server', Self}
             ,{'queue', Queue}
             ,{'other_queues', OtherQueueNames}
            ]
    end.

%% allow wildcard (<<"*">>) in the Key to match either (or both) Category and Name
-spec maybe_event_matches_key(responder_callback_mapping(), responder_callback_mapping()) -> boolean().
maybe_event_matches_key(Evt, Evt) -> 'true';
maybe_event_matches_key({_,_}, {<<"*">>, <<"*">>}) -> 'true';
maybe_event_matches_key({_, Name}, {<<"*">>, Name}) -> 'true';
maybe_event_matches_key({Cat, _}, {Cat, <<"*">>}) -> 'true';
maybe_event_matches_key(_A, _B) -> 'false'.

-spec start_amqp(wh_proplist()) -> {'ok', binary()} | {'error', _}.
start_amqp(Props) ->
    QueueProps = props:get_value('queue_options', Props, []),
    QueueName = props:get_value('queue_name', Props, <<>>),
    case amqp_util:new_queue(QueueName, QueueProps) of
        {'error', _}=E -> E;
        Q ->
            set_qos(props:get_value('basic_qos', Props)),
            'ok' = start_consumer(Q, props:get_value('consume_options', Props)),
            lager:debug("queue started: ~s", [Q]),
            {'ok', Q}
    end.

-spec set_qos('undefined' | non_neg_integer()) -> 'ok'.
set_qos('undefined') -> ok;
set_qos(N) when is_integer(N) -> amqp_util:basic_qos(N).

-spec start_consumer(ne_binary(), wh_proplist()) -> 'ok'.
start_consumer(Q, 'undefined') -> amqp_util:basic_consume(Q, []);
start_consumer(Q, ConsumeProps) -> amqp_util:basic_consume(Q, ConsumeProps).

-spec remove_binding(ne_binary(), wh_proplist(), ne_binary()) -> any().
remove_binding(Binding, Props, Q) ->
    Wapi = list_to_binary([<<"wapi_">>, wh_util:to_binary(Binding)]),
    try (wh_util:to_atom(Wapi, 'true')):unbind_q(Q, Props) of
        Return -> Return
    catch
        'error':'undef' ->
            erlang:error({'api_module_undefined', Wapi})
    end.

-spec create_binding(ne_binary(), wh_proplist(), ne_binary()) -> any().
create_binding(Binding, Props, Q) when not is_binary(Binding) ->
    create_binding(wh_util:to_binary(Binding), Props, Q);
create_binding(Binding, Props, Q) ->
    Wapi = list_to_binary([<<"wapi_">>, wh_util:to_binary(Binding)]),
    try (wh_util:to_atom(Wapi, 'true')):bind_q(Q, Props) of
        Return -> Return
    catch
        'error':'undef' ->
            erlang:error({'api_module_undefined', Wapi})
    end.

-spec stop_timer('undefined' | reference()) -> non_neg_integer() | 'false'.
stop_timer('undefined') -> 'false';
stop_timer(Ref) when is_reference(Ref) -> erlang:cancel_timer(Ref).

-spec start_timer(term()) -> reference() | 'undefined'.
start_timer(0) ->
    self() ! ?CALLBACK_TIMEOUT_MSG,
    'undefined';
start_timer(Timeout) when is_integer(Timeout) andalso Timeout >= 0 ->
    erlang:send_after(Timeout, self(), ?CALLBACK_TIMEOUT_MSG);
start_timer(_) -> 'undefined'.

-spec add_other_queue(binary(), wh_proplist(), wh_proplist(), state()) -> {ne_binary(), state()}.
add_other_queue(<<>>, QueueProps, Bindings, #state{other_queues=OtherQueues}=State) ->
    {'ok', Q} = start_amqp(QueueProps),
    gen_server:cast(self(), {'gen_listener', {'created_queue', Q}}),
    _ = [create_binding(Type, BindProps, Q) || {Type, BindProps} <- Bindings],
    {Q, State#state{other_queues=[{Q, {Bindings, QueueProps}}|OtherQueues]}};
add_other_queue(QueueName, QueueProps, Bindings, #state{other_queues=OtherQueues}=State) ->
    {'ok', Q} = start_amqp([{'queue_name', QueueName} | QueueProps]),
    gen_server:cast(self(), {'gen_listener', {'created_queue', Q}}),
    _ = [create_binding(Type, BindProps, Q) || {Type, BindProps} <- Bindings],
    case props:get_value(QueueName, OtherQueues) of
        'undefined' ->
            {Q, State#state{other_queues=[{Q, {Bindings, QueueProps}}|OtherQueues]}};
        OldBindings ->
            {Q, State#state{other_queues=[{Q, {Bindings ++ OldBindings, QueueProps}}
                                          | props:delete(QueueName, OtherQueues)
                                         ]}}
    end.
