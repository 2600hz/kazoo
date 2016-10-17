%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz
%%% @doc
%%%
%%% Behaviour for setting up an AMQP listener.
%%% Add/rm responders for Event-Cat/Event-Name pairs. Each responder
%%% corresponds to a module that has defined a handle/1 function, receiving
%%% the kz_json:object() from the AMQP request.
%%%
%%% Params :: [
%%%   {bindings, [ {atom(), kz_proplist()}, ...]} -> the type of bindings, with optional properties to pass along
%%%   {responders, [ {responder, [ {<<"event-category">>, <<"event-name">>}, ...]} ]
%%%      responder is the module name to call handle_req/2 on for those category/name combos
%%%      responder can also be {module, function}, to call module:function/2 instead of handle_req/2
%%%      Responder can optionally define a function/3 (or handle_req/3) that will be called with the 3rd arg
%%%      consisting of the delivery options including exchange and routing_key
%%%   {queue_name, <<"some name">>} -> optional, if you want a named queue
%%%   {queue_options, [{key, value}]} -> optional, if the queue requires different params
%%%   {consume_options, [{key, value}]} -> optional, if the consumption requires special params
%%%   {basic_qos, integer()} -> optional, if QoS is being set on this queue
%%%   {broker | broker_tag, ne_binary()} -> optional, for binding to specific brokers
%%%   {declare_exchanges, declare_exchanges()} -> optional, for declaring dynamic exchanges used only in this connection
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
        ,start_link/5
        ]).

-export([start_listener/2]).

-export([queue_name/1
        ,bindings/1
        ,responders/1
        ,is_consuming/1
        ,routing_key_used/1
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
        ,reply/2

        ,enter_loop/3, enter_loop/4, enter_loop/5
        ]).

%% gen_listener API
-export([add_responder/3
        ,rm_responder/2
        ,rm_responder/3
        ]).

-export([add_binding/2, add_binding/3
        ,b_add_binding/2, b_add_binding/3
        ,rm_binding/2, rm_binding/3
        ]).

-export([ack/2
        ,nack/2
        ]).

-export([execute/4
        ,execute/3
        ,execute/2
        ]).

-export([federated_event/3]).
-export([delayed_cast/3
        ]).
-export([distribute_event/3]).

-include("listener_types.hrl").

-define(SERVER, ?MODULE).

-define(SERVER_RETRY_PERIOD, 30 * ?MILLISECONDS_IN_SECOND).
-define(TIMEOUT_RETRY_CONN, 5 * ?MILLISECONDS_IN_SECOND).
-define(CALLBACK_TIMEOUT_MSG, 'callback_timeout').

-define(BIND_WAIT, 100).

-type module_state() :: any().

-type federator_listener() :: {ne_binary(), pid()}.
-type federator_listeners() :: [federator_listener()].
-record(state, {
          queue :: api_binary()
               ,is_consuming = 'false' :: boolean()
               ,responders = [] :: listener_utils:responders() %% { {EvtCat, EvtName}, Module }
               ,bindings = [] :: bindings() %% {authentication, [{key, value},...]}
               ,params = [] :: kz_proplist()
               ,module :: atom()
               ,module_state :: module_state()
               ,module_timeout_ref :: reference() % when the client sets a timeout, gen_listener calls shouldn't negate it, only calls that pass through to the client
               ,other_queues = [] :: [{ne_binary(), {kz_proplist(), kz_proplist()}}] %% {QueueName, {proplist(), kz_proplist()}}
               ,federators = [] :: federator_listeners()
               ,self = self() :: pid()
               ,consumer_key = kz_amqp_channel:consumer_pid()
               ,consumer_tags = [] :: binaries()
         }).

-type state() :: #state{}.


-type callback_datum() :: {'server', pid()} |
                          {'queue', api_binary()} |
                          {'other_queues', ne_binaries()}.
-type callback_data() :: kz_proplist() |
                         [callback_datum()].

-export_type([handle_event_return/0
             ,binding/0
             ,bindings/0
             ,basic_deliver/0
             ,callback_data/0
             ]).

%%%===================================================================
%%% API
%%%===================================================================
-callback init(any()) -> {'ok', module_state()} |
                         {'ok', module_state(), timeout() | 'hibernate'} |
                         {'stop', any()} |
                         'ignore'.

-type handle_call_return() :: {'reply', any(), module_state()} |
                              {'reply', any(), module_state(), timeout() | 'hibernate'} |
                              {'noreply', module_state()} |
                              {'noreply', module_state(), timeout() | 'hibernate'} |
                              {'stop', any(), any(), module_state()} |
                              {'stop', any(), module_state()}.

-callback handle_call(any(), {pid(), any()}, module_state()) -> handle_call_return().

-type handle_cast_return() :: {'noreply', module_state()} |
                              {'noreply', module_state(), timeout() | 'hibernate'} |
                              {'stop', any(), module_state()}.

-callback handle_cast(any(), module_state()) -> handle_cast_return().

-type handle_info_return() :: {'noreply', module_state()} |
                              {'noreply', module_state(), timeout() | 'hibernate'} |
                              {'stop', any(), module_state()}.

-callback handle_info(timeout() | any(), module_state()) -> handle_info_return().


-type handle_event_return() :: 'ignore' |
                               {'ignore', module_state()} |
                               {'reply', kz_proplist()} |
                               {'reply', kz_proplist(), module_state()}.

-callback handle_event(kz_json:object(), module_state()) -> handle_event_return().

-callback terminate('normal' | 'shutdown' | {'shutdown', any()} | any(), module_state()) ->
    any().
-callback code_change(any() | {'down', any()}, module_state(), any()) ->
    {'ok', module_state()} | {'error', any()}.

-spec start_link(atom(), start_params(), list()) -> startlink_ret().
start_link(Module, Params, InitArgs) when is_atom(Module),
                                          is_list(Params),
                                          is_list(InitArgs)
                                          ->
    gen_server:start_link(?SERVER, [Module, Params, InitArgs], []).

-spec start_link(gen_server_name() | atom(), atom() | start_params(), start_params() | list(), gen_server_options() | list()) -> startlink_ret().
start_link(Module, Params, InitArgs, Options) when is_atom(Module),
                                                   is_list(Params),
                                                   is_list(InitArgs),
                                                   is_list(Options)
                                                   ->
    gen_server:start_link(?MODULE, [Module, Params, InitArgs], Options);
start_link(Name, Module, Params, InitArgs) when is_atom(Module),
                                                is_list(Params),
                                                is_list(InitArgs)
                                                ->
    gen_server:start_link(Name, ?MODULE, [Module, Params, InitArgs], []).

-spec start_link(gen_server_name(), atom(), start_params(), list(), gen_server_options()) -> startlink_ret().
start_link(Name, Module, Params, InitArgs, Options) when is_atom(Module),
                                                         is_list(Params),
                                                         is_list(InitArgs),
                                                         is_list(Options)
                                                         ->
    gen_server:start_link(Name, ?MODULE, [Module, Params, InitArgs], Options).

-spec queue_name(server_ref()) -> ne_binary().
queue_name(Srv) -> gen_server:call(Srv, 'queue_name').

-spec is_consuming(server_ref()) -> boolean().
is_consuming(Srv) -> gen_server:call(Srv, 'is_consuming').

-spec responders(server_ref()) -> listener_utils:responders().
responders(Srv) -> gen_server:call(Srv, 'responders').

-spec bindings(server_ref()) -> bindings().
bindings(Srv) -> gen_server:call(Srv, 'bindings').

-spec routing_key_used(basic_deliver()) -> ne_binary().
routing_key_used(#'basic.deliver'{routing_key=RoutingKey}) ->
    kz_util:to_binary(RoutingKey).

-spec ack(server_ref(), basic_deliver()) -> 'ok'.
ack(Srv, Delivery) -> gen_server:cast(Srv, {'ack', Delivery}).

-spec nack(server_ref(), basic_deliver()) -> 'ok'.
nack(Srv, Delivery) -> gen_server:cast(Srv, {'nack', Delivery}).

%% API functions that mirror gen_server:call,cast,reply
-spec call(server_ref(), any()) -> any().
call(Name, Request) -> gen_server:call(Name, {'$client_call', Request}).

-spec call(server_ref(), any(), kz_timeout()) -> any().
call(Name, Request, Timeout) -> gen_server:call(Name, {'$client_call', Request}, Timeout).

-spec cast(server_ref(), any()) -> 'ok'.
cast(Name, Request) -> gen_server:cast(Name, {'$client_cast', Request}).

-spec delayed_cast(server_ref(), any(), pos_integer()) -> 'ok'.
delayed_cast(Name, Request, Wait) when is_integer(Wait), Wait > 0 ->
    _P = kz_util:spawn(
           fun() ->
                   kz_util:put_callid(?MODULE),
                   timer:sleep(Wait),
                   gen_server:cast(Name, Request)
           end),
    'ok'.

-spec reply({pid(), reference()}, any()) -> no_return().
reply(From, Msg) -> gen_server:reply(From, Msg).

-type server_name() :: {'global' | 'local', atom()} | pid().

-spec enter_loop(atom(), list(), any()) -> no_return().
-spec enter_loop(atom(), list(), any(), kz_timeout() | server_name()) -> no_return().
-spec enter_loop(atom(), list(), any(), server_name(), kz_timeout()) -> no_return().
enter_loop(Module, Options, ModuleState) ->
    enter_loop(Module, Options, ModuleState, self(), 'infinity').
enter_loop(Module, Options, ModuleState, {Scope, _Name}=ServerName)
  when Scope =:= 'local'
       orelse Scope =:= 'global' ->
    enter_loop(Module, Options, ModuleState, ServerName, 'infinity');
enter_loop(Module, Option, ModuleState, Timeout) ->
    enter_loop(Module, Option, ModuleState, self(), Timeout).

enter_loop(Module, Options, ModuleState, ServerName, Timeout) ->
    {'ok', MyState} = init_state([Module, Options, ModuleState]),
    gen_server:enter_loop(?MODULE, [], MyState, ServerName, Timeout).

-spec add_responder(server_ref(), responder_callback(), responder_callback_mapping() | responder_callback_mappings()) -> 'ok'.
add_responder(Srv, Responder, Key) when not is_list(Key) ->
    add_responder(Srv, Responder, [Key]);
add_responder(Srv, Responder, [{_,_}|_] = Keys) ->
    gen_server:cast(Srv, {'add_responder', Responder, Keys}).

-spec rm_responder(server_ref(), responder_callback()) -> 'ok'.
%% empty list removes all
rm_responder(Srv, Responder) -> rm_responder(Srv, Responder, []).

-spec rm_responder(server_ref(), responder_callback(), responder_callback_mappings()) -> 'ok'.
rm_responder(Srv, Responder, {_,_}=Key) ->
    rm_responder(Srv, Responder, [Key]);
rm_responder(Srv, Responder, Keys) ->
    gen_server:cast(Srv, {'rm_responder', Responder, Keys}).

-spec add_binding(server_ref(), binding() | ne_binary() | atom()) -> 'ok'.
add_binding(Srv, {Binding, Props}) when is_list(Props)
                                        ,(is_atom(Binding)
                                          orelse is_binary(Binding)
                                         ) ->
    gen_server:cast(Srv, {'add_binding', kz_util:to_binary(Binding), Props});
add_binding(Srv, Binding) when is_binary(Binding)
                               orelse is_atom(Binding) ->
    gen_server:cast(Srv, {'add_binding', kz_util:to_binary(Binding), []}).

-spec add_binding(server_ref(), ne_binary() | atom(), kz_proplist()) -> 'ok'.
add_binding(Srv, Binding, Props) when is_binary(Binding)
                                      orelse is_atom(Binding) ->
    gen_server:cast(Srv, {'add_binding', kz_util:to_binary(Binding), Props}).

-spec b_add_binding(server_ref(), binding() | ne_binary() | atom()) -> 'ok'.
b_add_binding(Srv, {Binding, Props}) when is_list(Props)
                                          ,(is_atom(Binding)
                                            orelse is_binary(Binding)
                                           ) ->
    gen_server:call(Srv, {'add_binding', kz_util:to_binary(Binding), Props});
b_add_binding(Srv, Binding) when is_binary(Binding)
                                 orelse is_atom(Binding) ->
    gen_server:call(Srv, {'add_binding', kz_util:to_binary(Binding), []}).

-spec b_add_binding(server_ref(), ne_binary() | atom(), kz_proplist()) -> 'ok'.
b_add_binding(Srv, Binding, Props) when is_binary(Binding)
                                        orelse is_atom(Binding) ->
    gen_server:call(Srv, {'add_binding', kz_util:to_binary(Binding), Props}).

%% It is expected that responders have been set up already, prior to binding the new queue
-spec add_queue(server_ref(), binary(), kz_proplist(), binding() | bindings()) ->
                       {'ok', ne_binary()} |
                       {'error', any()}.
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

-spec rm_binding(server_ref(), ne_binary() | atom(), kz_proplist()) -> 'ok'.
rm_binding(Srv, Binding, Props) ->
    gen_server:cast(Srv, {'rm_binding', kz_util:to_binary(Binding), Props}).

-spec federated_event(server_ref(), kz_json:object(), basic_deliver()) -> 'ok'.
federated_event(Srv, JObj, BasicDeliver) ->
    gen_server:cast(Srv, {'federated_event', JObj, BasicDeliver}).

-spec execute(server_ref(), module(), atom(), [any()]) -> 'ok'.
execute(Srv, Module, Function, Args) ->
    gen_server:cast(Srv, {'$execute', Module, Function, Args}).

-spec execute(server_ref(), atom(), [any()]) -> 'ok'.
execute(Srv, Function, Args) ->
    gen_server:cast(Srv, {'$execute', Function, Args}).

-spec execute(server_ref(), function()) -> 'ok'.
execute(Srv, Function) when is_function(Function) ->
    gen_server:cast(Srv, {'$execute', Function}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% Takes an existing process and turns it into a gen_listener
-spec init_state(list()) -> {'ok', state()} |
                            {'stop', any()} |
                            'ignore'.
init_state([Module, Params, ModuleState]) ->
    process_flag('trap_exit', 'true'),
    put('callid', Module),
    lager:debug("continuing as a gen_listener proc : ~s", [Module]),
    init(Module, Params, ModuleState, 'undefined').

-spec init([atom() | kz_proplist(),...]) ->
                  {'ok', state()} |
                  {'stop', any()} |
                  'ignore'.
init([Module, Params, InitArgs]) ->
    process_flag('trap_exit', 'true'),
    put('callid', Module),
    lager:debug("starting new gen_listener proc : ~s", [Module]),
    case erlang:function_exported(Module, 'init', 1)
        andalso Module:init(InitArgs)
    of
        {'ok', MS} -> init(Module, Params, MS, 'undefined');
        {'ok', MS, 'hibernate'} -> init(Module, Params, MS, 'undefined');
        {'ok', MS, Timeout} -> init(Module, Params, MS, start_timer(Timeout));
        {'stop', _R} = STOP -> STOP;
        'ignore' -> 'ignore'
    end.

-spec init(atom(), kz_proplist(), module_state(), api_reference()) ->
                  {'ok', state()}.
init(Module, Params, ModuleState, TimeoutRef) ->
    _ = channel_requisition(Params),
    _ = [add_responder(self(), Mod, Events)
         || {Mod, Events} <- props:get_value('responders', Params, [])
        ],
    {'ok', #state{module=Module
                 ,module_state=ModuleState
                 ,module_timeout_ref=TimeoutRef
                 ,params=Params
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
-spec handle_call(any(), {pid(), reference()}, state()) -> handle_call_return().
handle_call({'add_queue', QueueName, QueueProps, Bindings}, _From, State) ->
    {Q, S} = add_other_queue(QueueName, QueueProps, Bindings, State),
    {'reply', {'ok', Q}, S};
handle_call('other_queues', _From, #state{other_queues=OtherQueues}=State) ->
    {'reply', props:get_keys(OtherQueues), State};
handle_call('queue_name', _From, #state{queue=Q}=State) ->
    {'reply', Q, State};
handle_call('responders', _From, #state{responders=Rs}=State) ->
    {'reply', Rs, State};
handle_call('bindings', _From, #state{bindings=Bs}=State) ->
    {'reply', Bs, State};
handle_call('is_consuming', _From, #state{is_consuming=IsC}=State) ->
    {'reply', IsC, State};
handle_call({'$client_call', Request}, From, State) ->
    handle_module_call(Request, From, State);
handle_call({'add_binding', _Binding, _Props}=AddBinding, _From, State) ->
    case handle_cast(AddBinding, State) of
        {'noreply', State1} -> {'reply', 'ok', State1};
        {'stop', _Reason, _State1}=Stop -> Stop
    end;
handle_call(Request, From, State) ->
    handle_module_call(Request, From, State).

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
-spec handle_cast(any(), state()) -> handle_cast_return().
handle_cast({'ack', Delivery}, State) ->
    _A = (catch amqp_util:basic_ack(Delivery)),
    {'noreply', State};
handle_cast({'nack', Delivery}, State) ->
    _N = (catch amqp_util:basic_nack(Delivery)),
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
    };
handle_cast({'rm_responder', Responder, Keys}, #state{responders=Responders}=State) ->
    {'noreply'
    ,State#state{responders=listener_utils:rm_responder(Responders, Responder, Keys)}
    };
handle_cast({'add_binding', _, _}=AddBinding, #state{is_consuming='false'}=State) ->
    Time = ?BIND_WAIT + (crypto:rand_uniform(100, 200)), % wait 100 + [100,200) ms before replaying the binding request
    lager:debug("not consuming yet, put binding to end of message queue after ~b ms", [Time]),
    delayed_cast(self(), AddBinding, Time),
    {'noreply', State};
handle_cast({'add_binding', Binding, Props}, State) ->
    {'noreply', handle_add_binding(Binding, Props, State)};
handle_cast({'rm_binding', Binding, Props}, State) ->
    {'noreply', handle_rm_binding(Binding, Props, State)};
handle_cast({'kz_amqp_assignment', {'new_channel', 'true'}}, State) ->
    lager:debug("channel reconnecting"),
    {'noreply', State};
handle_cast({'kz_amqp_assignment', {'new_channel', 'false'}}, State) ->
    {'noreply', handle_amqp_channel_available(State)};
handle_cast({'federated_event', JObj, BasicDeliver}, State) ->
    _ = kz_util:spawn(fun distribute_event/3, [JObj, BasicDeliver, State]),
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
    _ = [?MODULE:cast(Federator, Msg)
         || {_Broker, Federator} <- Federators
        ],
    {'noreply', State};
handle_cast({'$execute', Function, Args}=Msg
           ,#state{federators=Federators}=State) ->
    erlang:apply(Function, Args),
    _ = [?MODULE:cast(Federator, Msg)
         || {_Broker, Federator} <- Federators
        ],
    {'noreply', State};
handle_cast({'$execute', Function}=Msg
           ,#state{federators=Federators}=State
           ) ->
    Function(),
    _ = [?MODULE:cast(Federator, Msg)
         || {_Broker, Federator} <- Federators
        ],
    {'noreply', State};
handle_cast({'$client_cast', Message}, State) ->
    handle_module_cast(Message, State);
handle_cast({'start_listener', Params}, #state{queue='undefined'
                                              ,is_consuming='false'
                                              ,responders=[]
                                              ,bindings=[]
                                              ,params=[]
                                              }=State) ->
    #state{module=Module
          ,module_state=ModuleState
          ,module_timeout_ref=TimeoutRef
          } = State,
    {'ok', #state{}=N} = init(Module, Params, ModuleState, TimeoutRef),
    {'noreply', N};
handle_cast({'start_listener', _Params}, State) ->
    lager:debug("gen listener asked to start listener but it is already initialized"),
    {'noreply', State};

handle_cast({'pause_consumers'}, #state{is_consuming='true', consumer_tags=Tags}=State) ->
    lists:foreach(fun amqp_util:basic_cancel/1, Tags),
    {'noreply', State};

handle_cast({'resume_consumers'}, #state{queue='undefined'}=State) ->
    {'noreply', State};
handle_cast({'resume_consumers'}, #state{is_consuming='false'
                                        ,params=Params
                                        ,queue=Q
                                        ,other_queues=OtherQueues
                                        }=State) ->
    start_consumer(Q, props:get_value('consume_options', Params)),
    _ = [start_consumer(Q1, props:get_value('consume_options', P))
         || {Q1, {_, P}} <- OtherQueues
        ],
    {'noreply', State};

handle_cast(Message, State) ->
    handle_module_cast(Message, State).

-spec maybe_remove_binding(binding(), binding_module(), kz_proplist(), ne_binary()) -> boolean().
maybe_remove_binding({B, P}, B, P, Q) ->
    lager:debug("removing ~s: ~p", [B, P]),
    remove_binding(B, P, Q),
    'false';
maybe_remove_binding(_BP, _B, _P, _Q) -> 'true'.

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
-spec handle_info(any(), state()) -> handle_info_ret().
handle_info({#'basic.deliver'{}=BD, #amqp_msg{props=#'P_basic'{content_type=CT}
                                             ,payload=Payload
                                             }}
           ,#state{params=Params}=State) ->
    case props:is_true('spawn_handle_event', Params, 'false') of
        'true'  -> kz_util:spawn(fun handle_event/4, [Payload, CT, BD, State]),
                   {'noreply', State};
        'false' -> {'noreply', handle_event(Payload, CT, BD, State)}
    end;
handle_info({#'basic.return'{}=BR, #amqp_msg{props=#'P_basic'{content_type=CT}
                                            ,payload=Payload
                                            }}, State) ->
    handle_return(Payload, CT, BR, State);
handle_info(#'basic.consume_ok'{consumer_tag=CTag}, #state{queue='undefined'}=State) ->
    lager:debug("received consume ok (~s) for abandoned queue", [CTag]),
    {'noreply', State};
handle_info(#'basic.consume_ok'{consumer_tag=CTag}, #state{consumer_tags=CTags}=State) ->
    gen_server:cast(self(), {?MODULE, {'is_consuming', 'true'}}),
    {'noreply', State#state{is_consuming='true'
                           ,consumer_tags=[CTag | CTags]
                           }};
handle_info(#'basic.cancel_ok'{consumer_tag=CTag}, #state{consumer_tags=CTags}=State) ->
    lager:debug("recv a basic.cancel_ok for tag ~s", [CTag]),
    gen_server:cast(self(), {?MODULE, {'is_consuming', 'false'}}),
    {'noreply', State#state{is_consuming='false'
                           ,consumer_tags=lists:delete(CTag, CTags)
                           }};
handle_info(#'basic.ack'{}=Ack, #state{}=State) ->
    lager:debug("recv a basic.ack ~p", [Ack]),
    handle_confirm(Ack, State);
handle_info(#'basic.nack'{}=Nack, #state{}=State) ->
    lager:debug("recv a basic.nack ~p", [Nack]),
    handle_confirm(Nack, State);
handle_info(#'channel.flow'{active=Active}, State) ->
    lager:debug("received channel flow (~s)", [Active]),
    amqp_util:flow_control_reply(Active),
    gen_server:cast(self(), {?MODULE,{'channel_flow_control', Active}}),
    {'noreply', State};
handle_info('$is_gen_listener_consuming'
           ,#state{is_consuming='false'
                  ,bindings=ExistingBindings
                  ,params=Params
                  }=State) ->
    _Release = (catch kz_amqp_channel:release()),
    _Requisition = channel_requisition(Params),
    {'noreply', State#state{queue='undefined'
                           ,bindings=[]
                           ,params=props:set_value('bindings', ExistingBindings, Params)
                           }};
handle_info('$is_gen_listener_consuming', State) ->
    {'noreply', State};
handle_info({'$server_confirms', ServerConfirms}, State) ->
    gen_server:cast(self(), {?MODULE,{'server_confirms',ServerConfirms}}),
    {'noreply', State};
handle_info({'$channel_flow', Active}, State) ->
    gen_server:cast(self(), {?MODULE,{'channel_flow', Active}}),
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
-spec handle_event(ne_binary(), ne_binary(), basic_deliver(), state()) ->  state().
handle_event(Payload, <<"application/json">>, BasicDeliver, State) ->
    JObj = kz_json:decode(Payload),
    _ = kz_util:put_callid(JObj),
    distribute_event(JObj, BasicDeliver, State);
handle_event(Payload, <<"application/erlang">>, BasicDeliver, State) ->
    JObj = binary_to_term(Payload),
    _ = kz_util:put_callid(JObj),
    distribute_event(JObj, BasicDeliver, State).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handles the AMQP messages prior to the spawning a handler.
%% Allows listeners to pass options to handlers
%%
%% @spec handle_event(JObj, State) -> {'reply', Options} | ignore
%% @end
%%--------------------------------------------------------------------
-spec handle_return(ne_binary(), ne_binary(), #'basic.return'{}, state()) ->  handle_cast_return().
handle_return(Payload, <<"application/json">>, BR, State) ->
    JObj = kz_json:decode(Payload),
    _ = kz_util:put_callid(JObj),
    handle_return(JObj, BR, State);
handle_return(Payload, <<"application/erlang">>, BR, State) ->
    JObj = binary_to_term(Payload),
    _ = kz_util:put_callid(JObj),
    handle_return(JObj, BR, State).

-spec handle_return(kz_json:object(), #'basic.return'{}, state()) -> handle_cast_return().
handle_return(JObj, BR, State) ->
    Msg = {?MODULE, {'return', JObj, basic_return_to_jobj(BR)}},
    handle_module_cast(Msg, State).

-spec basic_return_to_jobj(#'basic.return'{}) -> kz_json:object().
basic_return_to_jobj(#'basic.return'{reply_code=Code
                                    ,reply_text=Msg
                                    ,exchange=Exchange
                                    ,routing_key=RoutingKey
                                    }) ->
    kz_json:from_list([{<<"code">>, Code}
                      ,{<<"message">>, kz_util:to_binary(Msg)}
                      ,{<<"exchange">>, kz_util:to_binary(Exchange)}
                      ,{<<"routing_key">>, kz_util:to_binary(RoutingKey)}
                      ]).

-spec handle_confirm(#'basic.ack'{} | #'basic.nack'{}, state()) -> handle_cast_return().
handle_confirm(Confirm, State) ->
    Msg = {?MODULE, {'confirm', Confirm}},
    handle_module_cast(Msg, State).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec terminate(any(), state()) -> 'ok'.
terminate(Reason, #state{module=Module
                        ,module_state=ModuleState
                        ,federators=Fs
                        }) ->
    _ = (catch Module:terminate(Reason, ModuleState)),
    _ = (catch kz_amqp_channel:release()),
    _ = [listener_federator:stop(F) || {_Broker, F} <- Fs],
    lager:debug("~s terminated cleanly, going down", [Module]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {'ok', NewState}
%% @end
%%--------------------------------------------------------------------
-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVersion, State, _Extra) ->
    {'ok', State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec handle_callback_info(any(), state()) -> handle_info_return().
handle_callback_info(Message, #state{module=Module
                                    ,module_state=ModuleState
                                    ,module_timeout_ref=OldRef
                                    }=State) ->
    _ = stop_timer(OldRef),
    try Module:handle_info(Message, ModuleState) of
        {'noreply', ModuleState1} ->
            {'noreply', State#state{module_state=ModuleState1}};
        {'noreply', ModuleState1, Timeout} ->
            Ref = start_timer(Timeout),
            {'noreply', State#state{module_state=ModuleState1
                                   ,module_timeout_ref=Ref
                                   }
            };
        {'stop', Reason, ModuleState1} ->
            {'stop', Reason, State#state{module_state=ModuleState1}}
    catch
        _E:R ->
            ST = erlang:get_stacktrace(),
            lager:debug("handle_info exception: ~s: ~p", [_E, R]),
            kz_util:log_stacktrace(ST),
            {'stop', R, State}
    end.

-spec format_status('normal' | 'terminate', [kz_proplist() | state()]) -> any().
format_status(_Opt, [_PDict, #state{module=Module
                                   ,module_state=ModuleState
                                   }=State]) ->
    case erlang:function_exported(Module, 'format_status', 2) of
        'true' -> Module:format_status(_Opt, [_PDict, ModuleState]);
        'false' -> [{'data', [{"Module State", ModuleState}
                             ,{"Module", Module}
                             ,{"Listener State", State}
                             ]
                    }]
    end.


-spec distribute_event(kz_json:object(), basic_deliver(), state()) -> state().
distribute_event(JObj, BasicDeliver, State) ->
    case callback_handle_event(JObj, BasicDeliver, State) of
        'ignore' -> State;
        {'ignore', ModuleState} -> State#state{module_state=ModuleState};
        {CallbackData, ModuleState} -> distribute_event(CallbackData, JObj, BasicDeliver, State#state{module_state=ModuleState});
        CallbackData -> distribute_event(CallbackData, JObj, BasicDeliver, State)
    end.

-spec distribute_event(callback_data(), kz_json:object(), basic_deliver(), state()) -> state().
distribute_event(CallbackData, JObj, BasicDeliver, #state{responders=Responders
                                                         ,consumer_key=ConsumerKey
                                                         }=State) ->
    Key = kz_util:get_event_type(JObj),
    _ = [kz_util:spawn(fun client_handle_event/5, [JObj
                                                  ,ConsumerKey
                                                  ,Callback
                                                  ,CallbackData
                                                  ,BasicDeliver
                                                  ])
         || {Evt, Callback} <- Responders,
            maybe_event_matches_key(Key, Evt)
        ],
    State.

-spec client_handle_event(kz_json:object(), kz_amqp_channel:consumer_pid(), responder_callback(), callback_data(), basic_deliver()) -> any().
client_handle_event(JObj, ConsumerKey, Callback, CallbackData, BasicDeliver) ->
    _ = kz_util:put_callid(JObj),
    _ = kz_amqp_channel:consumer_pid(ConsumerKey),
    client_handle_event(JObj, Callback, CallbackData, BasicDeliver).

-spec client_handle_event(kz_json:object(), responder_callback(), callback_data(), basic_deliver()) -> any().
client_handle_event(JObj, Fun, CallbackData, BasicDeliver)
  when is_function(Fun, 3) -> Fun(JObj, CallbackData, BasicDeliver);
client_handle_event(JObj, Fun, CallbackData, _BasicDeliver)
  when is_function(Fun, 2) -> Fun(JObj, CallbackData);
client_handle_event(JObj, {Module, Fun}, CallbackData, BasicDeliver) ->
    case erlang:function_exported(Module, Fun, 3) of
        'true' -> Module:Fun(JObj, CallbackData, BasicDeliver);
        'false' -> Module:Fun(JObj, CallbackData)
    end.

-spec callback_handle_event(kz_json:object(), basic_deliver(), state()) ->
                                   'ignore' |
                                   {'ignore', module_state()} |
                                   callback_data() |
                                   {callback_data(), module_state()}.
callback_handle_event(JObj
                     ,BasicDeliver
                     ,#state{module=Module
                            ,module_state=ModuleState
                            ,queue=Queue
                            ,other_queues=OtherQueues
                            ,self=Self
                            }
                     ) ->
    case callback_handle_event(JObj, BasicDeliver, Module, ModuleState) of
        'ignore' -> 'ignore';
        {'ignore', _NewModuleState} = Reply -> Reply;
        {'reply', Props} when is_list(Props) ->
            [{'server', Self}
            ,{'queue', Queue}
            ,{'other_queues', props:get_keys(OtherQueues)}
             | Props
            ];
        {'reply', Props, NewModuleState} when is_list(Props) ->
            {[{'server', Self}
             ,{'queue', Queue}
             ,{'other_queues', props:get_keys(OtherQueues)}
              | Props
             ]
            ,NewModuleState
            };
        {'EXIT', _Why} ->
            [{'server', Self}
            ,{'queue', Queue}
            ,{'other_queues', props:get_keys(OtherQueues)}
            ,{'state', ModuleState}
            ]
    end.

-spec callback_handle_event(kz_json:object(), basic_deliver(), atom(), module_state()) ->
                                   handle_event_return() |
                                   {'EXIT', any()}.
callback_handle_event(JObj, BasicDeliver, Module, ModuleState) ->
    case erlang:function_exported(Module, 'handle_event', 3) of
        'true' -> catch Module:handle_event(JObj, BasicDeliver, ModuleState);
        'false' -> catch Module:handle_event(JObj, ModuleState)
    end.

%% allow wildcard (<<"*">>) in the Key to match either (or both) Category and Name
-spec maybe_event_matches_key(responder_callback_mapping(), responder_callback_mapping()) -> boolean().
maybe_event_matches_key(Evt, Evt) -> 'true';
maybe_event_matches_key({_,_}, {<<"*">>, <<"*">>}) -> 'true';
maybe_event_matches_key({_, Name}, {<<"*">>, Name}) -> 'true';
maybe_event_matches_key({Cat, _}, {Cat, <<"*">>}) -> 'true';
maybe_event_matches_key(_A, _B) -> 'false'.

-spec start_amqp(kz_proplist()) ->
                        {'ok', binary()} |
                        {'error', _}.
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
set_qos('undefined') -> 'ok';
set_qos(N) when is_integer(N), N >= 0 -> amqp_util:basic_qos(N).

-spec start_consumer(ne_binary(), kz_proplist()) -> 'ok'.
start_consumer(Q, 'undefined') -> amqp_util:basic_consume(Q, []);
start_consumer(Q, ConsumeProps) -> amqp_util:basic_consume(Q, ConsumeProps).

-spec remove_binding(binding_module(), kz_proplist(), api_binary()) -> 'ok'.
remove_binding(Binding, Props, Q) ->
    Wapi = list_to_binary([<<"kapi_">>, kz_util:to_binary(Binding)]),
    lager:debug("trying to remove bindings with ~s:unbind_q(~s, ~p)", [Wapi, Q, Props]),
    try (kz_util:to_atom(Wapi, 'true')):unbind_q(Q, Props)
    catch
        'error':'undef' ->
            erlang:error({'api_module_undefined', Wapi})
    end.

-spec create_binding(ne_binary(), kz_proplist(), ne_binary()) -> any().
create_binding(Binding, Props, Q) when not is_binary(Binding) ->
    create_binding(kz_util:to_binary(Binding), Props, Q);
create_binding(Binding, Props, Q) ->
    Wapi = list_to_binary([<<"kapi_">>, kz_util:to_binary(Binding)]),
    try (kz_util:to_atom(Wapi, 'true')):bind_q(Q, Props)
    catch
        'error':'undef' ->
            erlang:error({'api_module_undefined', Wapi})
    end.

-spec stop_timer('undefined' | reference()) -> non_neg_integer() | 'false'.
stop_timer('undefined') -> 'false';
stop_timer(Ref) when is_reference(Ref) -> erlang:cancel_timer(Ref).

-spec start_timer(any()) -> reference() | 'undefined'.
start_timer(0) ->
    self() ! ?CALLBACK_TIMEOUT_MSG,
    'undefined';
start_timer(Timeout) when is_integer(Timeout)
                          andalso Timeout >= 0 ->
    erlang:send_after(Timeout, self(), ?CALLBACK_TIMEOUT_MSG);
start_timer(_) -> 'undefined'.

-spec add_other_queue(binary(), kz_proplist(), kz_proplist(), state()) -> {ne_binary(), state()}.
add_other_queue(<<>>, QueueProps, Bindings, #state{other_queues=OtherQueues}=State) ->
    {'ok', Q} = start_amqp(QueueProps),
    gen_server:cast(self(), {?MODULE, {'created_queue', Q}}),

    _ = [create_binding(Type, BindProps, Q) || {Type, BindProps} <- Bindings],
    {Q, State#state{other_queues=[{Q, {Bindings, QueueProps}}|OtherQueues]}};
add_other_queue(QueueName, QueueProps, Bindings, #state{other_queues=OtherQueues}=State) ->
    {'ok', Q} = start_amqp([{'queue_name', QueueName} | QueueProps]),
    gen_server:cast(self(), {?MODULE, {'created_queue', Q}}),
    _ = [create_binding(Type, BindProps, Q) || {Type, BindProps} <- Bindings],
    case props:get_value(QueueName, OtherQueues) of
        'undefined' ->
            {Q, State#state{other_queues=[{Q, {Bindings, QueueProps}}|OtherQueues]}};
        OldBindings ->
            {Q, State#state{other_queues=[{Q, {Bindings ++ OldBindings, QueueProps}}
                                          | props:delete(QueueName, OtherQueues)
                                         ]}}
    end.

-spec handle_module_call(any(), pid_ref(), state()) -> handle_call_return().
handle_module_call(Request, From, #state{module=Module
                                        ,module_state=ModuleState
                                        ,module_timeout_ref=OldRef
                                        }=State) ->
    _ = stop_timer(OldRef),
    try Module:handle_call(Request, From, ModuleState) of
        {'reply', Reply, ModuleState1} ->
            {'reply', Reply
            ,State#state{module_state=ModuleState1
                        ,module_timeout_ref='undefined'
                        }
            };
        {'reply', Reply, ModuleState1, Timeout} ->
            {'reply', Reply
            ,State#state{module_state=ModuleState1
                        ,module_timeout_ref=start_timer(Timeout)
                        }
            };
        {'noreply', ModuleState1} ->
            {'noreply', State#state{module_state=ModuleState1}};
        {'noreply', ModuleState1, Timeout} ->
            {'noreply'
            ,State#state{module_state=ModuleState1
                        ,module_timeout_ref=start_timer(Timeout)
                        }
            };
        {'stop', Reason, ModuleState1} ->
            {'stop', Reason, State#state{module_state=ModuleState1}};
        {'stop', Reason, Reply, ModuleState1} ->
            {'stop', Reason, Reply, State#state{module_state=ModuleState1}}
    catch
        _E:R ->
            ST = erlang:get_stacktrace(),
            lager:debug("handle_call exception: ~s: ~p", [_E, R]),
            kz_util:log_stacktrace(ST),
            {'stop', R, State}
    end.

-spec handle_module_cast(any(), state()) -> handle_cast_return().
handle_module_cast(Msg, #state{module=Module
                              ,module_state=ModuleState
                              ,module_timeout_ref=OldRef
                              }=State) ->
    _ = stop_timer(OldRef),
    try Module:handle_cast(Msg, ModuleState) of
        {'noreply', ModuleState1} ->
            {'noreply', State#state{module_state=ModuleState1}};
        {'noreply', ModuleState1, Timeout} ->
            Ref = start_timer(Timeout),
            {'noreply', State#state{module_state=ModuleState1
                                   ,module_timeout_ref=Ref
                                   }
            };
        {'stop', Reason, ModuleState1} ->
            {'stop', Reason, State#state{module_state=ModuleState1}}
    catch
        _E:R ->
            ST = erlang:get_stacktrace(),
            lager:debug("handle_cast exception: ~s: ~p", [_E, R]),
            kz_util:log_stacktrace(ST),
            {'stop', R, State}
    end.

-spec handle_rm_binding(binding(), kz_proplist(), state()) -> state().
handle_rm_binding(Binding, Props, #state{queue=Q
                                        ,bindings=Bs
                                        }=State) ->
    KeepBs = [BP || BP <- Bs,
                    maybe_remove_binding(BP
                                        ,kz_util:to_binary(Binding)
                                        ,Props
                                        ,Q
                                        )],
    State#state{bindings=KeepBs}.

-spec handle_add_binding(binding_module(), kz_proplist(), state()) ->
                                state().
handle_add_binding(Binding, Props, #state{queue=Q
                                         ,bindings=Bs
                                         }=State) ->
    case lists:keyfind(Binding, 1, Bs) of
        'false' ->
            lager:debug("creating new binding: '~s'", [Binding]),
            create_binding(Binding, Props, Q),
            maybe_update_federated_bindings(State#state{bindings=[{Binding, Props}|Bs]});
        {Binding, ExistingProps} ->
            handle_existing_binding(Binding, Props, State, Q, ExistingProps, Bs)
    end.

-spec handle_existing_binding(binding_module(), kz_proplist(), state(), ne_binary(), kz_proplist(), bindings()) ->
                                     state().
handle_existing_binding(Binding, Props, State, Q, ExistingProps, Bs) ->
    case lists:all(fun({K,V}) ->
                           props:get_value(K, ExistingProps) =:= V;
                      (K) ->
                           props:get_value(K, ExistingProps) =:= 'true'
                   end
                  ,Props
                  )
    of
        'true' ->
            lager:debug("binding ~s with props exists", [Binding]),
            State;
        'false' ->
            lager:debug("creating existing binding '~s' with new props: ~p", [Binding, Props]),
            create_binding(Binding, Props, Q),
            maybe_update_federated_bindings(State#state{bindings=[{Binding, Props}|Bs]})
    end.

-spec maybe_update_federated_bindings(state()) -> state().
maybe_update_federated_bindings(#state{bindings=[{_Binding, Props}|_]}=State) ->
    case is_federated_binding(Props) of
        'false' -> State;
        'true' -> update_federated_bindings(State)
    end.

-spec is_federated_binding(kz_proplist()) -> boolean().
is_federated_binding(Props) ->
    props:get_value('federate', Props) =:= 'true'.

-spec update_federated_bindings(state()) -> state().
update_federated_bindings(#state{bindings=[{Binding, Props}|_]
                                ,federators=Fs
                                }=State) ->
    case kz_amqp_connections:federated_brokers() of
        [] ->
            lager:debug("no federated brokers to connect to, skipping federating binding '~s'", [Binding]),
            State;
        FederatedBrokers ->
            NonFederatedProps = props:delete('federate', Props),
            {_Existing, New} = broker_connections(Fs, FederatedBrokers),
            'ok' = update_existing_listeners_bindings(Fs, Binding, NonFederatedProps),
            {'ok', NewListeners} = start_new_listeners(New, Binding, NonFederatedProps, State),
            State#state{federators=NewListeners ++ Fs}
    end.

-spec broker_connections(federator_listeners(), ne_binaries()) ->
                                {ne_binaries(), ne_binaries()}.
broker_connections(Listeners, Brokers) ->
    lists:partition(fun(Broker) ->
                            props:get_value(Broker, Listeners) =/= 'undefined'
                    end, Brokers).

-spec start_new_listeners(ne_binaries(), binding_module(), kz_proplist(), state()) ->
                                 {'ok', federator_listeners()}.
start_new_listeners(Brokers, Binding, Props, State) ->
    {'ok', [start_new_listener(Broker, Binding, Props, State)
            || Broker <- Brokers
           ]}.

-spec start_new_listener(ne_binary(), binding_module(), kz_proplist(), state()) -> federator_listener().
start_new_listener(Broker, Binding, Props, #state{params=Ps}) ->
    FederateParams = create_federated_params({Binding, Props}, Ps),
    {'ok', Pid} = listener_federator:start_link(self(), Broker, FederateParams),
    lager:debug("started federated listener on broker ~s: ~p", [Broker, Pid]),
    {Broker, Pid}.

-spec update_existing_listeners_bindings(federator_listeners(), binding_module(), kz_proplist()) -> 'ok'.
update_existing_listeners_bindings(Listeners, Binding, Props) ->
    _ = [update_existing_listener_bindings(Listener, Binding, Props)
         || Listener <- Listeners
        ],
    'ok'.

-spec update_existing_listener_bindings(federator_listener(), binding_module(), kz_proplist()) -> 'ok'.
update_existing_listener_bindings({_Broker, Pid}, Binding, Props) ->
    lager:debug("updating listener ~p with ~s", [Pid, Binding]),
    ?MODULE:add_binding(Pid, Binding, Props).

-spec create_federated_params({binding_module(), kz_proplist()}, kz_proplist()) ->
                                     kz_proplist().
create_federated_params(FederateBindings, Params) ->
    [{'responders', []}
    ,{'bindings', [FederateBindings]}
    ,{'queue_name', federated_queue_name(Params)}
    ,{'queue_options', props:get_value('queue_options', Params, [])}
    ,{'consume_options', props:get_value('consume_options', Params, [])}
    ].

-spec federated_queue_name(kz_proplist()) -> api_binary().
federated_queue_name(Params) ->
    QueueName = props:get_value('queue_name', Params, <<>>),
    case kz_util:is_empty(QueueName) of
        'true' -> QueueName;
        'false' ->
            Zone = kz_config:zone('binary'),
            <<QueueName/binary, "-", Zone/binary>>
    end.

-spec handle_amqp_channel_available(state()) -> state().
handle_amqp_channel_available(#state{params=Params}=State) ->
    lager:debug("channel started, let's connect"),
    case maybe_declare_exchanges(props:get_value('declare_exchanges', Params, [])) of
        'ok' ->
            handle_exchanges_ready(State);
        {'error', _E} ->
            lager:debug("error declaring exchanges : ~p", [_E]),
            handle_exchanges_failed(State)
    end.

-spec handle_exchanges_ready(state()) -> state().
handle_exchanges_ready(#state{params=Params}=State) ->
    case start_amqp(Params) of
        {'ok', Q} ->
            handle_amqp_started(State, Q);
        {'error', Reason} ->
            lager:error("start amqp error ~p", [Reason]),
            handle_amqp_errored(State)
    end.

-spec handle_amqp_started(state(), ne_binary()) -> state().
handle_amqp_started(#state{params=Params}=State, Q) ->
    State1 = start_initial_bindings(State#state{queue=Q}, Params),

    gen_server:cast(self(), {?MODULE, {'created_queue', Q}}),

    maybe_server_confirms(props:get_value('server_confirms', Params, 'false')),

    maybe_channel_flow(props:get_value('channel_flow', Params, 'false')),
    erlang:send_after(?TIMEOUT_RETRY_CONN, self(), '$is_gen_listener_consuming'),

    State1#state{is_consuming='false'}.

-spec handle_amqp_errored(state()) -> state().
handle_amqp_errored(#state{params=Params}=State) ->
    #kz_amqp_assignment{channel=Channel} = kz_amqp_assignments:get_channel(),
    _ = (catch kz_amqp_channel:release()),
    kz_amqp_channel:close(Channel),
    timer:sleep(?SERVER_RETRY_PERIOD),
    _ = channel_requisition(Params),
    State#state{is_consuming='false'}.

-spec handle_exchanges_failed(state()) -> state().
handle_exchanges_failed(#state{params=Params}=State) ->
    #kz_amqp_assignment{channel=Channel} = kz_amqp_assignments:get_channel(),
    _ = (catch kz_amqp_channel:release()),
    kz_amqp_channel:close(Channel),
    timer:sleep(?SERVER_RETRY_PERIOD),
    _ = channel_requisition(Params),
    State#state{is_consuming='false'}.

-spec maybe_server_confirms(boolean()) -> 'ok'.
maybe_server_confirms('true') ->
    amqp_util:confirm_select();
maybe_server_confirms(_) -> 'ok'.

-spec maybe_channel_flow(boolean()) -> 'ok'.
maybe_channel_flow('true') ->
    amqp_util:flow_control();
maybe_channel_flow(_) -> 'ok'.

-spec maybe_declare_exchanges(kz_proplist()) ->
                                     command_ret().
-spec maybe_declare_exchanges(kz_amqp_assignment(), kz_proplist()) ->
                                     command_ret().
maybe_declare_exchanges([]) -> 'ok';
maybe_declare_exchanges(Exchanges) ->
    maybe_declare_exchanges(kz_amqp_assignments:get_channel(), Exchanges).

maybe_declare_exchanges(_Channel, []) -> 'ok';
maybe_declare_exchanges(Channel, [{Ex, Type, Opts} | Exchanges]) ->
    declare_exchange(Channel, amqp_util:declare_exchange(Ex, Type, Opts), Exchanges);
maybe_declare_exchanges(Channel, [{Ex, Type} | Exchanges]) ->
    declare_exchange(Channel, amqp_util:declare_exchange(Ex, Type), Exchanges).

-spec declare_exchange(kz_amqp_assignment(), kz_amqp_exchange(), kz_proplist()) -> command_ret().
declare_exchange(Channel, Exchange, Exchanges) ->
    case kz_amqp_channel:command(Channel, Exchange) of
        {'ok', _} -> maybe_declare_exchanges(Channel, Exchanges);
        E -> E
    end.

-spec start_initial_bindings(state(), kz_proplist()) -> state().
start_initial_bindings(State, Params) ->
    lists:foldl(fun({Binding, Props}, StateAcc) ->
                        handle_add_binding(Binding, Props, StateAcc)
                end
               ,State
               ,props:get_value('bindings', Params, [])
               ).

-spec channel_requisition(kz_proplist()) -> boolean().
channel_requisition([]) -> 'false';
channel_requisition(Params) ->
    case props:get_value('broker_tag', Params) of
        'undefined' ->
            case props:get_value('broker', Params) of
                'undefined' -> kz_amqp_channel:requisition();
                Broker -> maybe_add_broker_connection(Broker)
            end;
        Tag ->
            case kz_amqp_connections:broker_with_tag(Tag) of
                'undefined' -> kz_amqp_channel:requisition();
                Broker -> maybe_add_broker_connection(Broker)
            end
    end.

-spec maybe_add_broker_connection(binary()) -> boolean().
-spec maybe_add_broker_connection(binary(), non_neg_integer()) -> boolean().
maybe_add_broker_connection(Broker) ->
    Count = kz_amqp_connections:broker_available_connections(Broker),
    maybe_add_broker_connection(Broker, Count).

maybe_add_broker_connection(Broker, Count) when Count =:= 0 ->
    kz_amqp_connections:add(Broker, kz_util:rand_hex_binary(6), [<<"hidden">>]),
    kz_amqp_channel:requisition(self(), Broker);
maybe_add_broker_connection(Broker, _Count) ->
    kz_amqp_channel:requisition(self(), Broker).

-spec start_listener(pid(), kz_proplist()) -> 'ok'.
start_listener(Srv, Params) ->
    gen_server:cast(Srv, {'start_listener', Params}).
