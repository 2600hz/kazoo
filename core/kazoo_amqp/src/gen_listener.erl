%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc Behaviour for setting up an AMQP listener.
%%% Add/remove responders for `Event-Cat'/`Event-Name' pairs. Each responder
%%% corresponds to a module that has defined a `handle/1' function, receiving
%%% the {@link kz_json:object()} from the AMQP request.
%%%
%%% Options:
%%% <dl>
%%%   <dt>`{bindings, [{atom(), kz_term:proplist()}, ...]}'</dt>
%%%   <dd>The type of bindings, with optional properties to pass along.</dd>
%%%
%%%   <dt>`{responders, [{Responder, [{<<"event-category">>, <<"event-name">>}, ...]}]'</dt>
%%%   <dd>`Responder' is the module name to call `handle_req/2' on for those category/name combos. `Responder' can also be `{module, function}',
%%%       to call `module:function/2' instead of `handle_req/2'.
%%%       `Responder' can optionally define a `function/3' (or `handle_req/3') that will be called with the 3rd argument
%%%       consisting of the delivery options including exchange and routing key.
%%%   </dd>
%%%
%%%   <dt>`{queue_name, <<"some name">>}'</dt>
%%%   <dd>Optional, if you want a named queue.</dd>
%%%
%%%   <dt>`{queue_options, [{key, value}]}'</dt>
%%%   <dd>Optional, if the queue requires different parameters.</dd>
%%%
%%%   <dt>`{consume_options, [{key, value}]}'</dt>
%%%   <dd>Optional, if the consumption requires special parameters.</dd>
%%%
%%%   <dt>`{basic_qos, integer()}'</dt>
%%%   <dd>Optional, if QoS is being set on this queue.</dd>
%%%
%%%   <dt>`{broker | broker_tag, kz_term:ne_binary()}'</dt>
%%%   <dd>Optional, for binding to specific brokers.</dd>
%%%
%%%   <dt>`{declare_exchanges, declare_exchanges()}'</dt>
%%%   <dd>Optional, for declaring dynamic exchanges used only in this connection.</dd>
%%% </dl>
%%%
%%% @author James Aimonetti
%%% @author Karl Anderson
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(gen_listener).
-behaviour(gen_server).

-export([start_link/3
        ,start_link/4
        ,start_link/5
        ,stop/1
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

-export([federated_event/3
        ,notify_of_federator_listener/2
        ]).
-export([delayed_cast/3]).
-export([distribute_event/3]).

-include("listener_types.hrl").

-define(SERVER, ?MODULE).

-define(SERVER_RETRY_PERIOD, 30 * ?MILLISECONDS_IN_SECOND).
-define(TIMEOUT_RETRY_CONN, 5 * ?MILLISECONDS_IN_SECOND).
-define(CALLBACK_TIMEOUT_MSG, 'callback_timeout').

-define(BIND_WAIT, 100).

-type module_state() :: any().

-type federator_listener() :: {kz_term:ne_binary(), {pid(), reference()}}.
-type federator_listeners() :: [federator_listener()].

-record(state, {queue :: kz_term:api_binary()
               ,is_consuming = 'false' :: boolean()
               ,responders = [] :: listener_utils:responders() %% {{EvtCat, EvtName}, Module}
               ,bindings = [] :: bindings() %% {authentication, [{key, value},...]}
               ,params = [] :: kz_term:proplist()
               ,module :: atom()
               ,module_state :: module_state()
               ,module_timeout_ref :: kz_term:api_reference() % when the client sets a timeout, gen_listener calls shouldn't negate it, only calls that pass through to the client
               ,other_queues = [] :: [{kz_term:ne_binary(), {bindings(), kz_term:proplist()}}] %% {QueueName, {Bindings, QueueProps}}
               ,federators = [] :: federator_listeners()
               ,waiting_federators = [] :: list()
               ,self = self() :: pid()
               ,consumer_key = kz_amqp_channel:consumer_pid()
               ,consumer_tags = [] :: kz_term:binaries()
               ,handle_event_mfa = 'undefined' :: mfa() | 'undefined'
               ,auto_ack = 'false' :: boolean()
               }).
-type state() :: #state{}.

-type content() :: {kz_term:api_binary(), kz_term:api_binary()}.

-type deliver() :: {basic_deliver(), amqp_basic()}.

-type callback_datum() :: {'server', pid()} |
                          {'queue', kz_term:api_binary()} |
                          {'other_queues', kz_term:ne_binaries()}.
-type callback_data() :: kz_term:proplist() |
                         [callback_datum()].

-export_type([handle_event_return/0
             ,binding/0
             ,bindings/0
             ,basic_deliver/0
             ,callback_data/0
             ,declare_exchanges/0
             ]).

%%%=============================================================================
%%% API
%%%=============================================================================

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
                               {'ignore', module_state(), timeout() | 'hibernate'} |
                               {'reply', kz_term:proplist()} |
                               {'reply', kz_term:proplist(), module_state()}.

-callback handle_event(kz_json:object(), module_state()) -> handle_event_return().
-callback handle_event(kz_json:object(), kz_term:proplist(), module_state()) -> handle_event_return().

-callback terminate('normal' | 'shutdown' | {'shutdown', any()} | any(), module_state()) ->
    any().
-callback code_change(any() | {'down', any()}, module_state(), any()) ->
    {'ok', module_state()} | {'error', any()}.

-optional_callbacks([handle_event/2, handle_event/3]).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec start_link(atom(), start_params(), list()) -> kz_types:startlink_ret().
start_link(Module, Params, InitArgs) when is_atom(Module),
                                          is_list(Params),
                                          is_list(InitArgs)
                                          ->
    gen_server:start_link(?SERVER, [Module, Params, InitArgs], []).

-spec start_link(kz_types:gen_server_name() | atom(), atom() | start_params(), start_params() | list(), kz_types:gen_server_options() | list()) -> kz_types:startlink_ret().
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

-spec start_link(kz_types:gen_server_name(), atom(), start_params(), list(), kz_types:gen_server_options()) -> kz_types:startlink_ret().
start_link(Name, Module, Params, InitArgs, Options) when is_atom(Module),
                                                         is_list(Params),
                                                         is_list(InitArgs),
                                                         is_list(Options)
                                                         ->
    gen_server:start_link(Name, ?MODULE, [Module, Params, InitArgs], Options).

-spec stop(kz_types:server_ref()) -> 'ok'.
stop(Server) ->
    gen_server:stop(Server).

-spec start_listener(pid(), kz_term:proplist()) -> 'ok'.
start_listener(Srv, Params) ->
    gen_server:cast(Srv, {'start_listener', Params}).

-spec queue_name(kz_types:server_ref()) -> kz_term:api_ne_binary().
queue_name(Srv) -> gen_server:call(Srv, 'queue_name').

-spec is_consuming(kz_types:server_ref()) -> boolean().
is_consuming(Srv) -> gen_server:call(Srv, 'is_consuming').

-spec responders(kz_types:server_ref()) -> listener_utils:responders().
responders(Srv) -> gen_server:call(Srv, 'responders').

-spec bindings(kz_types:server_ref()) -> bindings().
bindings(Srv) -> gen_server:call(Srv, 'bindings').

-spec routing_key_used(basic_deliver()) -> kz_term:ne_binary().
routing_key_used(#'basic.deliver'{routing_key=RoutingKey}) ->
    kz_term:to_binary(RoutingKey).

-spec ack(kz_types:server_ref(), basic_deliver()) -> 'ok'.
ack(Srv, Delivery) -> gen_server:cast(Srv, {'ack', Delivery}).

-spec nack(kz_types:server_ref(), basic_deliver()) -> 'ok'.
nack(Srv, Delivery) -> gen_server:cast(Srv, {'nack', Delivery}).

%%------------------------------------------------------------------------------
%% @doc API functions that mirror `gen_server:call,cast,reply'.
%% @end
%%------------------------------------------------------------------------------
-spec call(kz_types:server_ref(), any()) -> any().
call(Name, Request) -> gen_server:call(Name, {'$client_call', Request}).

-spec call(kz_types:server_ref(), any(), timeout()) -> any().
call(Name, Request, Timeout) -> gen_server:call(Name, {'$client_call', Request}, Timeout).

-spec cast(kz_types:server_ref(), any()) -> 'ok'.
cast(Name, Request) -> gen_server:cast(Name, {'$client_cast', Request}).

-spec delayed_cast(kz_types:server_ref(), any(), pos_integer()) -> 'ok'.
delayed_cast(Name, Request, Wait) when is_integer(Wait), Wait > 0 ->
    _P = kz_process:spawn(
           fun() ->
                   kz_log:put_callid(?MODULE),
                   timer:sleep(Wait),
                   gen_server:cast(Name, Request)
           end),
    'ok'.

-spec reply(kz_term:pid_ref(), any()) -> no_return().
reply(From, Msg) -> gen_server:reply(From, Msg).

-type server_name() :: {'global' | 'local', atom()} | pid().

-spec enter_loop(atom(), list(), any()) -> no_return().
enter_loop(Module, Options, ModuleState) ->
    enter_loop(Module, Options, ModuleState, self(), 'infinity').

-spec enter_loop(atom(), list(), any(), timeout() | server_name()) -> no_return().
enter_loop(Module, Options, ModuleState, {Scope, _Name}=ServerName)
  when Scope =:= 'local'
       orelse Scope =:= 'global' ->
    enter_loop(Module, Options, ModuleState, ServerName, 'infinity');
enter_loop(Module, Option, ModuleState, Timeout) ->
    enter_loop(Module, Option, ModuleState, self(), Timeout).

-spec enter_loop(atom(), list(), any(), server_name(), timeout()) -> no_return().
enter_loop(Module, Options, ModuleState, ServerName, Timeout) ->
    {'ok', MyState} = init_state([Module, Options, ModuleState]),
    gen_server:enter_loop(?MODULE, [], MyState, ServerName, Timeout).

-spec add_responder(kz_types:server_ref(), responder_callback(), responder_callback_mapping() | responder_callback_mappings()) -> 'ok'.
add_responder(Srv, Responder, {_,_}=Key) ->
    add_responder(Srv, Responder, [Key]);
add_responder(Srv, Responder, [{_,_}|_] = Keys) ->
    gen_server:cast(Srv, {'add_responder', Responder, Keys}).

%%------------------------------------------------------------------------------
%% @doc Removes responder from queue.
%% Empty list removes all.
%% @end
%%------------------------------------------------------------------------------
-spec rm_responder(kz_types:server_ref(), responder_callback()) -> 'ok'.
rm_responder(Srv, Responder) -> rm_responder(Srv, Responder, []).

-spec rm_responder(kz_types:server_ref(), responder_callback(), responder_callback_mappings()) -> 'ok'.
rm_responder(Srv, Responder, {_,_}=Key) ->
    rm_responder(Srv, Responder, [Key]);
rm_responder(Srv, Responder, Keys) ->
    gen_server:cast(Srv, {'rm_responder', Responder, Keys}).

-spec add_binding(kz_types:server_ref(), binding() | kz_term:ne_binary() | atom()) -> 'ok'.
add_binding(Srv, {Binding, Props}) when is_list(Props)
                                        andalso (is_atom(Binding)
                                                 orelse is_binary(Binding)
                                                ) ->
    gen_server:cast(Srv, {'add_binding', kz_term:to_binary(Binding), Props});
add_binding(Srv, Binding) when is_binary(Binding)
                               orelse is_atom(Binding) ->
    gen_server:cast(Srv, {'add_binding', kz_term:to_binary(Binding), []}).

-spec add_binding(kz_types:server_ref(), kz_term:ne_binary() | atom(), kz_term:proplist()) -> 'ok'.
add_binding(Srv, Binding, Props) when is_binary(Binding)
                                      orelse is_atom(Binding) ->
    gen_server:cast(Srv, {'add_binding', kz_term:to_binary(Binding), Props}).

-spec b_add_binding(kz_types:server_ref(), binding() | kz_term:ne_binary() | atom()) -> 'ok'.
b_add_binding(Srv, {Binding, Props}) when is_list(Props)
                                          ,(is_atom(Binding)
                                            orelse is_binary(Binding)
                                           ) ->
    gen_server:call(Srv, {'add_binding', kz_term:to_binary(Binding), Props});
b_add_binding(Srv, Binding) when is_binary(Binding)
                                 orelse is_atom(Binding) ->
    gen_server:call(Srv, {'add_binding', kz_term:to_binary(Binding), []}).

-spec b_add_binding(kz_types:server_ref(), kz_term:ne_binary() | atom(), kz_term:proplist()) -> 'ok'.
b_add_binding(Srv, Binding, Props) when is_binary(Binding)
                                        orelse is_atom(Binding) ->
    gen_server:call(Srv, {'add_binding', kz_term:to_binary(Binding), Props}).

%%------------------------------------------------------------------------------
%% @doc Add responder to a queue.
%% It is expected that responders have been set up already, prior to
%% binding the new queue.
%% @end
%%------------------------------------------------------------------------------
-spec add_queue(kz_types:server_ref(), binary(), kz_term:proplist(), binding() | bindings()) ->
          {'ok', kz_term:ne_binary()} |
          {'error', any()}.
add_queue(Srv, QueueName, QueueProps, {_Type, _Props}=Binding) ->
    add_queue(Srv, QueueName, QueueProps, [Binding]);
add_queue(Srv, QueueName, QueueProps, [{_,_}|_]=Bindings) ->
    gen_server:call(Srv, {'add_queue', QueueName, QueueProps, Bindings}).

-spec rm_queue(kz_types:server_ref(), kz_term:ne_binary()) -> 'ok'.
rm_queue(Srv, ?NE_BINARY = QueueName) ->
    gen_server:cast(Srv, {'rm_queue', QueueName}).

-spec other_queues(kz_types:server_ref()) -> kz_term:ne_binaries().
other_queues(Srv) -> gen_server:call(Srv, 'other_queues').

-spec rm_binding(kz_types:server_ref(), binding()) -> 'ok'.
rm_binding(Srv, {Binding, Props}) ->
    rm_binding(Srv, Binding, Props).

-spec rm_binding(kz_types:server_ref(), kz_term:ne_binary() | atom(), kz_term:proplist()) -> 'ok'.
rm_binding(Srv, Binding, Props) ->
    gen_server:cast(Srv, {'rm_binding', kz_term:to_binary(Binding), Props}).

-spec federated_event(kz_types:server_ref(), kz_json:object(), kz_term:proplist()) -> 'ok'.
federated_event(Srv, JObj, Props) ->
    gen_server:cast(Srv, {'federated_event', JObj, Props}).

-spec notify_of_federator_listener(pid(), {kz_term:ne_binary(), pid()}) -> 'ok'.
notify_of_federator_listener(Srv, {_Broker, _Pid}=Child) ->
    gen_server:cast(Srv, {'federator_listener', Child}).

-spec execute(kz_types:server_ref(), module(), atom(), [any()]) -> 'ok'.
execute(Srv, Module, Function, Args) ->
    gen_server:cast(Srv, {'$execute', Module, Function, Args}).

-spec execute(kz_types:server_ref(), atom(), [any()]) -> 'ok'.
execute(Srv, Function, Args) ->
    gen_server:cast(Srv, {'$execute', Function, Args}).

-spec execute(kz_types:server_ref(), function()) -> 'ok'.
execute(Srv, Function) when is_function(Function) ->
    gen_server:cast(Srv, {'$execute', Function}).

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Takes an existing process and turns it into a `gen_listener'.
%% @end
%%------------------------------------------------------------------------------
-spec init_state(list()) -> {'ok', state()} |
          {'stop', any()} |
          'ignore'.
init_state([Module, Params, ModuleState]) ->
    process_flag('trap_exit', 'true'),
    put('callid', Module),
    lager:debug("continuing as a gen_listener proc : ~s", [Module]),
    init(Module, Params, ModuleState, 'undefined').

-spec init([atom() | kz_term:proplist(),...]) ->
          {'ok', state()} |
          {'stop', any()} |
          'ignore'.
init([Module, Params, InitArgs]) ->
    process_flag('trap_exit', 'true'),
    put('callid', Module),
    Application = kapps_util:get_application(),
    kapps_util:put_application(Application),
    lager:debug("starting new gen_listener proc : ~s ~s"
               ,[Application, Module]
               ),
    case erlang:function_exported(Module, 'init', 1)
        andalso Module:init(InitArgs)
    of
        {'ok', MS} -> init(Module, Params, MS, 'undefined');
        {'ok', MS, 'hibernate'} -> init(Module, Params, MS, 'undefined');
        {'ok', MS, Timeout} -> init(Module, Params, MS, start_timer(Timeout));
        {'stop', _R} = STOP -> STOP;
        'ignore' -> 'ignore'
    end.

-spec init(atom(), kz_term:proplist(), module_state(), kz_term:api_reference()) ->
          {'ok', state()}.
init(Module, Params, ModuleState, TimeoutRef) ->
    _ = channel_requisition(Params),
    _ = [add_responder(self(), Mod, Events)
         || {Mod, Events} <- props:get_value('responders', Params, [])
        ],
    {'ok', #state{module=Module
                 ,module_state=ModuleState
                 ,module_timeout_ref=TimeoutRef
                 ,params=maybe_set_queue_name(Module, Params)
                 ,handle_event_mfa = listener_utils:responder_mfa(Module, 'handle_event')
                 ,auto_ack = props:is_true('auto_ack', Params, 'false')
                 }}.

-spec maybe_set_queue_name(atom(), kz_term:proplist()) -> kz_term:proplist().
maybe_set_queue_name(Module, Params) ->
    case props:is_false('queue_name_always_generate', Params, 'true')
        andalso props:get_value('queue_name', Params) =:= <<>>
    of
        'false' -> Params;
        'true' -> props:set_value('queue_name', kz_amqp_util:new_queue_name(Module), Params)
    end.

%%------------------------------------------------------------------------------
%% @doc Handling call messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_call(any(), kz_term:pid_ref(), state()) -> handle_call_return().
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

%%------------------------------------------------------------------------------
%% @doc Handling cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_cast(any(), state()) -> handle_cast_return().
handle_cast({'ack', Delivery}, State) ->
    _A = (catch kz_amqp_util:basic_ack(Delivery)),
    {'noreply', State};
handle_cast({'nack', Delivery}, State) ->
    _N = (catch kz_amqp_util:basic_nack(Delivery)),
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
    %% wait 100 + [100,200) ms before replaying the binding request
    Time = ?BIND_WAIT + 100 + rand:uniform(100),
    lager:debug("not consuming yet, put binding to end of message queue after ~b ms", [Time]),
    delayed_cast(self(), AddBinding, Time),
    {'noreply', State};
handle_cast({'add_binding', Binding, Props}, State) ->
    {'noreply', handle_add_binding(Binding, Props, State)};
handle_cast({'rm_binding', Binding, Props}, State) ->
    {'noreply', handle_rm_binding(Binding, Props, State)};
handle_cast({'federated_event', JObj, Props}, #state{params=Params}=State) ->
    Deliver = props:get_value('deliver', Props),
    Basic = props:get_value('basic', Props),
    case props:is_true('spawn_handle_event', Params, 'false') of
        'true'  ->
            kz_process:spawn(fun distribute_event/3, [JObj, {Deliver, Basic}, State]),
            {'noreply', State};
        'false' -> distribute_event(JObj, {Deliver, Basic}, State)
    end;
handle_cast({'$execute', Module, Function, Args}
           ,#state{federators=[]}=State
           ) ->
    erlang:apply(Module, Function, Args),
    {'noreply', State};
handle_cast({'$execute', Function, Args}
           ,#state{federators=[]}=State
           ) ->
    erlang:apply(Function, Args),
    {'noreply', State};
handle_cast({'$execute', Function}
           ,#state{federators=[]}=State
           ) ->
    Function(),
    {'noreply', State};
handle_cast({'$execute', Module, Function, Args}=Msg
           ,#state{federators=Federators}=State
           ) ->
    erlang:apply(Module, Function, Args),
    _ = [?MODULE:cast(Federator, Msg)
         || {_Broker, {Federator, _Ref}} <- Federators
        ],
    {'noreply', State};
handle_cast({'$execute', Function, Args}=Msg
           ,#state{federators=Federators}=State
           ) ->
    erlang:apply(Function, Args),
    _ = [?MODULE:cast(Federator, Msg)
         || {_Broker, {Federator, _Ref}} <- Federators
        ],
    {'noreply', State};
handle_cast({'$execute', Function}=Msg
           ,#state{federators=Federators}=State
           ) ->
    Function(),
    _ = [?MODULE:cast(Federator, Msg)
         || {_Broker, {Federator, _Ref}} <- Federators
        ],
    {'noreply', State};
handle_cast({'$client_cast', Message}, State) ->
    handle_module_cast(Message, State);
handle_cast({'start_listener', Params}
           ,#state{queue='undefined'
                  ,is_consuming='false'
                  ,responders=[]
                  ,bindings=[]
                  ,params=[]
                  ,module=Module
                  ,module_state=ModuleState
                  ,module_timeout_ref=TimeoutRef
                  }
           ) ->
    {'ok', #state{}=NewState} = init(Module, Params, ModuleState, TimeoutRef),
    {'noreply', NewState};
handle_cast({'start_listener', _Params}, State) ->
    lager:debug("gen listener asked to start listener but it is already initialized"),
    {'noreply', State};
handle_cast({'pause_consumers'}, #state{is_consuming='true', consumer_tags=Tags}=State) ->
    lists:foreach(fun kz_amqp_util:basic_cancel/1, Tags),
    {'noreply', State};
handle_cast({'resume_consumers'}, #state{queue='undefined'}=State) ->
    {'noreply', State};
handle_cast({'resume_consumers'}
           ,#state{is_consuming='false'
                  ,params=Params
                  ,queue=Q
                  ,other_queues=OtherQueues
                  ,auto_ack=AutoAck
                  }=State
           ) ->
    ConsumeOptions = props:get_value('consume_options', Params, []),
    _ = start_consumer(Q, maybe_configure_auto_ack(ConsumeOptions, AutoAck)),
    _ = [start_consumer(Q1, maybe_configure_auto_ack(props:get_value('consume_options', P, []), AutoAck))
         || {Q1, {_, P}} <- OtherQueues
        ],
    {'noreply', State};
handle_cast({'federator_is_consuming', Broker, 'true'}, State) ->
    Filter = fun(X) ->
                     kz_amqp_connections:broker_available_connections(X) > 0
             end,

    Waiting = lists:filter(Filter, State#state.waiting_federators),

    case lists:subtract(Waiting, [Broker]) of
        [] ->
            lager:info("federator for ~s is consuming, all waiting federators are available!", [Broker]),
            handle_module_cast({?MODULE, {'federators_consuming', 'true'}}, State);
        Remaining ->
            lager:info("federator for ~s is consuming, still waiting for federators: ~p", [Broker, Remaining]),
            {'noreply', State#state{waiting_federators = Remaining}}
    end;
handle_cast({'federator_listener', {Broker, Pid}}, #state{federators=Fs}=State) ->
    case lists:keytake(Pid, 2, Fs) of
        'false' ->
            Ref = monitor('process', Pid),
            lager:info("federator ~p(~p) on broker ~s started", [Pid, Ref, Broker]),

            {'noreply', State#state{federators=[{Broker, {Pid, Ref}} | Fs]}};
        {'value', _FL, _Feds} ->
            lager:debug("ignoring federator_listener message about ~p; have ~p already"
                       ,[Pid, _FL]
                       ),
            {'noreply', State}
    end;
handle_cast(Message, State) ->
    handle_module_cast(Message, State).

-spec maybe_remove_binding(binding(), binding_module(), kz_term:proplist(), kz_term:ne_binary()) -> boolean().
maybe_remove_binding({B, P}, B, P, Q) ->
    lager:debug("removing ~s: ~p", [B, P]),
    remove_binding(B, P, Q),
    'false';
maybe_remove_binding(_BP, _B, _P, _Q) -> 'true'.

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> kz_types:handle_info_ret().
handle_info('retry', State) ->
    {'noreply', handle_amqp_channel_available(State, 'true')};
handle_info({'kz_amqp_assignment', {'new_channel', Reconnected, Channel}}, State) ->
    lager:debug("acquired channel"),
    _ = kz_amqp_channel:consumer_channel(Channel),
    {'noreply', handle_amqp_channel_available(State, Reconnected)};
handle_info({'kz_amqp_assignment', 'lost_channel'}
           ,#state{bindings=ExistingBindings
                  ,params=Params
                  }=State
           ) ->
    lager:debug("lost channel assignment"),
    kz_amqp_channel:remove_consumer_channel(),
    gen_server:cast(self(), {?MODULE, {'is_consuming', 'false'}}),
    %% if there's an error before the start consume
    %% we can potentially have initial Params
    %% set to empty array and therefore loose the bindings
    ParamBindings = props:get_value('bindings', Params, []),
    NewParams = lists:usort(ParamBindings ++ ExistingBindings),
    {'noreply', State#state{is_consuming='false'
                           ,consumer_tags=[]
                           ,bindings=[]
                           ,params=props:set_value('bindings', NewParams, Params)
                           }};
handle_info({#'basic.deliver'{}=BD
            ,#amqp_msg{props=#'P_basic'{content_type=CT
                                       ,content_encoding=CE
                                       }=Basic
                      ,payload=Payload
                      }
            }
           ,#state{params=Params, auto_ack=AutoAck}=State
           ) ->

    _ = case AutoAck of
            'true' -> (catch kz_amqp_util:basic_ack(BD));
            'false' -> 'ok'
        end,
    case props:is_true('spawn_handle_event', Params, 'false') of
        'false' ->
            Reply = handle_event(binary:copy(Payload), {CT, CE}, {BD, Basic}, State),
            maybe_gc(Reply);
        'true'  ->
            kz_process:spawn(fun handle_event/4, [Payload, {CT, CE}, {BD, Basic}, State]),
            {'noreply', State}
    end;
handle_info({#'basic.return'{}=BR
            ,#amqp_msg{props=#'P_basic'{content_type=CT
                                       ,content_encoding=CE
                                       }
                      ,payload=Payload
                      }
            }
           ,State
           ) ->
    handle_return(binary:copy(Payload), {CT, CE}, BR, State);
handle_info(#'basic.consume_ok'{consumer_tag=CTag}
           ,#state{queue='undefined'}=State
           ) ->
    lager:debug("received consume ok (~s) for abandoned queue", [CTag]),
    {'noreply', State};
handle_info(#'basic.consume_ok'{consumer_tag=CTag}
           ,#state{consumer_tags=CTags}=State
           ) ->
    lager:debug("received consume ok (~s) for queue : ~p", [CTag, CTags]),
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
    kz_amqp_util:flow_control_reply(Active),
    gen_server:cast(self(), {?MODULE,{'channel_flow_control', Active}}),
    {'noreply', State};
handle_info({'$server_confirms', ServerConfirms}, State) ->
    gen_server:cast(self(), {?MODULE,{'server_confirms',ServerConfirms}}),
    {'noreply', State};
handle_info({'$channel_flow', Active}, State) ->
    gen_server:cast(self(), {?MODULE,{'channel_flow', Active}}),
    {'noreply', State};
handle_info({'kz_amqp_channel', {Client, Ref, 'consumer_tags'}}, #state{consumer_tags=CTags}=State) ->
    Client ! {?MODULE, Ref, CTags},
    {'noreply', State};
handle_info(?CALLBACK_TIMEOUT_MSG, State) ->
    handle_callback_info('timeout', State);
handle_info({'DOWN', Ref, 'process', Pid, Reason}, State) ->
    handle_down({Pid, Ref}, Reason, State);
handle_info(Message, State) ->
    handle_callback_info(Message, State).

%%------------------------------------------------------------------------------
%% @doc Handles the AMQP messages prior to the spawning a handler.
%% Allows listeners to pass options to handlers.
%% @end
%%------------------------------------------------------------------------------
-spec handle_event(kz_term:ne_binary(), content(), deliver(), state()) ->  kz_types:handle_info_ret().
handle_event(Payload, {<<"application/json">>, <<"gzip">>}, Deliver, State) ->
    JObj = kz_json:decode(zlib:gunzip(Payload)),
    _ = kz_log:put_callid(JObj),
    distribute_event(JObj, Deliver, State);
handle_event(Payload, {<<"application/json">>, _}, Deliver, State) ->
    JObj = kz_json:decode(Payload),
    _ = kz_log:put_callid(JObj),
    distribute_event(JObj, Deliver, State);
handle_event(Payload,  {<<"application/erlang">>, <<"gzip">>}, Deliver, State) ->
    JObj = binary_to_term(zlib:gunzip(Payload)),
    _ = kz_log:put_callid(JObj),
    distribute_event(JObj, Deliver, State);
handle_event(Payload,  {<<"application/erlang">>, _}, Deliver, State) ->
    JObj = binary_to_term(Payload),
    _ = kz_log:put_callid(JObj),
    distribute_event(JObj, Deliver, State).

%%------------------------------------------------------------------------------
%% @doc Handles the AMQP messages prior to the spawning a handler.
%% Allows listeners to pass options to handlers.
%% @end
%%------------------------------------------------------------------------------
-spec handle_return(kz_term:ne_binary(), content(), #'basic.return'{}, state()) ->  handle_cast_return().
handle_return(Payload, {<<"application/json">>, <<"gzip">>}, BR, State) ->
    JObj = kz_json:decode(zlib:gunzip(Payload)),
    _ = kz_log:put_callid(JObj),
    handle_return(JObj, BR, State);
handle_return(Payload, {<<"application/json">>, _}, BR, State) ->
    JObj = kz_json:decode(Payload),
    _ = kz_log:put_callid(JObj),
    handle_return(JObj, BR, State);
handle_return(Payload, {<<"application/erlang">>, <<"gzip">>}, BR, State) ->
    JObj = binary_to_term(zlib:gunzip(Payload)),
    _ = kz_log:put_callid(JObj),
    handle_return(JObj, BR, State);
handle_return(Payload, {<<"application/erlang">>, _}, BR, State) ->
    JObj = binary_to_term(Payload),
    _ = kz_log:put_callid(JObj),
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
                      ,{<<"message">>, kz_term:to_binary(Msg)}
                      ,{<<"exchange">>, kz_term:to_binary(Exchange)}
                      ,{<<"routing_key">>, kz_term:to_binary(RoutingKey)}
                      ]).

-spec handle_confirm(#'basic.ack'{} | #'basic.nack'{}, state()) -> handle_cast_return().
handle_confirm(Confirm, State) ->
    Msg = {?MODULE, {'confirm', Confirm}},
    handle_module_cast(Msg, State).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec terminate(any(), state()) -> 'ok'.
terminate(Reason, #state{module=Module
                        ,module_state=ModuleState
                        ,federators=Fs
                        ,consumer_tags=Tags
                        }) ->
    Terminated = (catch Module:terminate(Reason, ModuleState)),
    kz_amqp_assignments:release_consumer(Tags),
    _ = [catch listener_federator:stop(F) || {_Broker, {F, _Ref}} <- Fs],
    maybe_log_module_terminate(Module, Reason, Terminated).

-spec maybe_log_module_terminate(module(), any(), any()) -> 'ok'.
maybe_log_module_terminate(_Module, _Reason, 'ok') -> 'ok';
maybe_log_module_terminate(Module, Reason, Terminated) ->
    lager:debug("~s terminated (~p): ~p", [Module, Reason, Terminated]).

%%------------------------------------------------------------------------------
%% @doc Convert process state when code is changed.
%% @end
%%------------------------------------------------------------------------------
-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVersion, State, _Extra) ->
    {'ok', State}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
handle_down({Pid, Ref}, Reason, #state{federators=Fs}=State) ->
    case lists:keytake({Pid, Ref}, 2, Fs) of
        'false' -> handle_callback_info({'DOWN', Ref, 'process', Pid, Reason}, State);
        {'value', {_Broker, _PidRef}, UpdatedFs} ->
            lager:warning("federator ~p to ~s down: ~p", [_PidRef, _Broker, Reason]),
            {'noreply', State#state{federators=UpdatedFs}}
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec handle_callback_info(any(), state()) -> handle_info_return().
handle_callback_info(Message
                    ,#state{module=Module
                           ,module_state=ModuleState
                           ,module_timeout_ref=OldRef
                           }=State
                    ) ->
    _ = stop_timer(OldRef),
    try Module:handle_info(Message, ModuleState) of
        {'noreply', ModuleState1} ->
            {'noreply', State#state{module_state=ModuleState1}};
        {'noreply', ModuleState1, 'hibernate'} ->
            {'noreply', State#state{module_state=ModuleState1}, 'hibernate'};
        {'noreply', ModuleState1, Timeout} ->
            Ref = start_timer(Timeout),
            {'noreply', State#state{module_state=ModuleState1
                                   ,module_timeout_ref=Ref
                                   }
            };
        {'stop', Reason, ModuleState1} ->
            {'stop', Reason, State#state{module_state=ModuleState1}}
    catch
        ?STACKTRACE(_E, R, ST)
        lager:debug("handle_info exception: ~s: ~p", [_E, R]),
        kz_log:log_stacktrace(ST),
        {'stop', R, State}
        end.

-spec format_status('normal' | 'terminate', [kz_term:proplist() | state()]) -> any().
format_status(_Opt
             ,[_PDict
              ,#state{module=Module
                     ,module_state=ModuleState
                     }=State
              ]) ->
    case erlang:function_exported(Module, 'format_status', 2) of
        'true' -> Module:format_status(_Opt, [_PDict, ModuleState]);
        'false' ->
            [{'data', [{"Module State", ModuleState}
                      ,{"Module", Module}
                      ,{"Listener State", State}
                      ]
             }]
    end.

-spec distribute_event(kz_json:object(), deliver(), state()) ->  kz_types:handle_info_ret().
distribute_event(JObj, {_ , #'P_basic'{headers='undefined'}}=BasicDeliver, State) ->
    case callback_handle_event(JObj, BasicDeliver, State) of
        'ignore' -> {'noreply', State};
        {'ignore', ModuleState} -> {'noreply', State#state{module_state=ModuleState}};
        {'ignore', ModuleState, 'hibernate'} -> {'noreply', State#state{module_state=ModuleState}, 'hibernate'};
        {'ignore', ModuleState, Timeout} -> {'noreply', State#state{module_state=ModuleState}, Timeout};
        {CallbackData, ModuleState} -> distribute_event(CallbackData, JObj, BasicDeliver, State#state{module_state=ModuleState});
        CallbackData -> distribute_event(CallbackData, JObj, BasicDeliver, State)
    end;
distribute_event(JObj, {Deliver, #'P_basic'{headers=Headers}=Basic}=BasicDeliver, State) ->
    case lists:keyfind(?KEY_DELIVER_TO_PID, 1, Headers) of
        {?KEY_DELIVER_TO_PID, _, Pid} ->
            Props = [{'basic', Basic}
                    ,{'deliver', Deliver}
                    ],
            kz_term:to_pid(Pid) ! {'kapi', kapi:delivery_message(JObj, Props)},
            {'noreply', State};
        'false' ->
            case callback_handle_event(JObj, BasicDeliver, State) of
                'ignore' -> {'noreply', State};
                {'ignore', ModuleState} -> {'noreply', State#state{module_state=ModuleState}};
                {'ignore', ModuleState, 'hibernate'} -> {'noreply', State#state{module_state=ModuleState}, 'hibernate'};
                {'ignore', ModuleState, Timeout} -> {'noreply', State#state{module_state=ModuleState}, Timeout};
                {CallbackData, ModuleState} -> distribute_event(CallbackData, JObj, BasicDeliver, State#state{module_state=ModuleState});
                CallbackData -> distribute_event(CallbackData, JObj, BasicDeliver, State)
            end
    end.

-spec distribute_event(callback_data(), kz_json:object(), deliver(), state()) -> {'noreply', state()}.
distribute_event(CallbackData, JObj, Deliver, #state{responders=Responders
                                                    ,consumer_key=ConsumerKey
                                                    }=State) ->
    Key = kz_util:get_event_type(JObj),
    Channel = kz_amqp_channel:consumer_channel(),
    _ = [kz_process:spawn(fun client_handle_event/6, [JObj
                                                     ,Channel
                                                     ,ConsumerKey
                                                     ,Callback
                                                     ,CallbackData
                                                     ,Deliver
                                                     ])
         || {Evt, Callback} <- Responders,
            maybe_event_matches_key(Key, Evt)
        ],
    {'noreply', State}.

-spec client_handle_event(kz_json:object(), pid() | kz_amqp_channel:consumer_channel(), kz_amqp_channel:consumer_pid(), responder_mfa(), callback_data(), deliver()) -> any().
client_handle_event(JObj, 'undefined', ConsumerKey, Callback, CallbackData, Deliver) ->
    lager:warning("no consumer channel to provide to spawned process"),
    _ = kz_log:put_callid(JObj),
    _ = kz_amqp_channel:consumer_pid(ConsumerKey),
    client_handle_event(JObj, Callback, CallbackData, Deliver);
client_handle_event(JObj, #kz_amqp_assignment{channel=Channel}, ConsumerKey, Callback, CallbackData, Deliver)
  when is_pid(Channel)->
    _ = kz_log:put_callid(JObj),
    _ = kz_amqp_channel:consumer_pid(ConsumerKey),
    _ = is_process_alive(Channel)
        andalso kz_amqp_channel:consumer_channel(Channel),
    client_handle_event(JObj, Callback, CallbackData, Deliver);
client_handle_event(JObj, Channel, ConsumerKey, Callback, CallbackData, Deliver)
  when is_pid(Channel) ->
    _ = kz_log:put_callid(JObj),
    _ = kz_amqp_channel:consumer_pid(ConsumerKey),
    _ = is_process_alive(Channel)
        andalso kz_amqp_channel:consumer_channel(Channel),
    client_handle_event(JObj, Callback, CallbackData, Deliver);
client_handle_event(JObj, _, ConsumerKey, Callback, CallbackData, Deliver) ->
    lager:warning("no usable consumer channel to provide to spawned process"),
    _ = kz_log:put_callid(JObj),
    _ = kz_amqp_channel:consumer_pid(ConsumerKey),
    client_handle_event(JObj, Callback, CallbackData, Deliver).

-spec client_handle_event(kz_json:object(), responder_mfa(), callback_data(), deliver()) -> any().
client_handle_event(JObj, {Fun, 4}, CallbackData, {BasicDeliver, Basic}) ->
    Fun(JObj, CallbackData, BasicDeliver, Basic);
client_handle_event(JObj, {Fun, 3}, CallbackData, {BasicDeliver, _Basic}) ->
    Fun(JObj, CallbackData, BasicDeliver);
client_handle_event(JObj, {Fun, 2}, CallbackData, _Deliver) ->
    Fun(JObj, CallbackData);
client_handle_event(JObj, {Module, Fun, 4}, CallbackData, {BasicDeliver, Basic}) ->
    Module:Fun(JObj, CallbackData, BasicDeliver, Basic);
client_handle_event(JObj, {Module, Fun, 3}, CallbackData, {BasicDeliver, _Basic}) ->
    Module:Fun(JObj, CallbackData, BasicDeliver);
client_handle_event(JObj, {Module, Fun, 2}, CallbackData, _Deliver) ->
    Module:Fun(JObj, CallbackData).

-spec callback_handle_event(kz_json:object(), deliver(), state()) ->
          'ignore' |
          {'ignore', module_state()} |
          {'ignore', module_state(), 'hibernate' | timeout()} |
          callback_data() |
          {callback_data(), module_state()}.
callback_handle_event(_JObj
                     ,{Deliver, Basic}
                     ,#state{module_state=ModuleState
                            ,queue=Queue
                            ,other_queues=OtherQueues
                            ,self=Self
                            ,handle_event_mfa='undefined'
                            }
                     ) ->
    [{'server', Self}
    ,{'queue', Queue}
    ,{'basic', Basic}
    ,{'deliver', Deliver}
    ,{'other_queues', props:get_keys(OtherQueues)}
    ,{'state', ModuleState}
    ];

callback_handle_event(JObj
                     ,{Deliver, Basic}
                     ,#state{module_state=ModuleState
                            ,queue=Queue
                            ,other_queues=OtherQueues
                            ,self=Self
                            ,handle_event_mfa=MFA
                            }=_State
                     ) ->
    Props = [{'queue', Queue}
            ,{'basic', Basic}
            ,{'deliver', Deliver}
            ,{'other_queues', props:get_keys(OtherQueues)}
            ],

    case callback_handle_event(JObj, Props, MFA, ModuleState) of
        'ignore' -> 'ignore';
        {'ignore', _NewModuleState} = Reply -> Reply;
        {'ignore', _NewModuleState, _TimeoutOrHibernate} = Reply -> Reply;
        {'reply', NewProps} when is_list(NewProps) ->
            [{'server', Self}
            ,{'queue', Queue}
            ,{'basic', Basic}
            ,{'deliver', Deliver}
            ,{'other_queues', props:get_keys(OtherQueues)}
             | NewProps
            ];
        {'reply', NewProps, NewModuleState} when is_list(Props) ->
            {[{'server', Self}
             ,{'queue', Queue}
             ,{'basic', Basic}
             ,{'deliver', Deliver}
             ,{'other_queues', props:get_keys(OtherQueues)}
              | NewProps
             ]
            ,NewModuleState
            };
        {'EXIT', Why} ->
            lager:error("CRASH in handle_event ~p : ~p", [Why, _State]),
            [{'server', Self}
            ,{'queue', Queue}
            ,{'basic', Basic}
            ,{'deliver', Deliver}
            ,{'other_queues', props:get_keys(OtherQueues)}
            ,{'state', ModuleState}
            ]
    end.

-spec callback_handle_event(kz_json:object(), kz_term:proplist(), mfa(), module_state()) ->
          handle_event_return() |
          {'EXIT', any()}.
callback_handle_event(JObj, _, {Module, Fun, 2}, ModuleState) ->
    catch Module:Fun(JObj, ModuleState);
callback_handle_event(JObj, Props, {Module, Fun, 3}, ModuleState) ->
    catch Module:Fun(JObj, Props, ModuleState);
callback_handle_event(_, _, _, _) ->
    {'EXIT', 'not_exported'}.

%% allow wildcard (<<"*">>) in the Key to match either (or both) Category and Name
-spec maybe_event_matches_key(responder_callback_mapping(), responder_callback_mapping()) -> boolean().
maybe_event_matches_key(Evt, Evt) -> 'true';
maybe_event_matches_key({_,_}, {<<"*">>, <<"*">>}) -> 'true';
maybe_event_matches_key({_, Name}, {<<"*">>, Name}) -> 'true';
maybe_event_matches_key({Cat, _}, {Cat, <<"*">>}) -> 'true';
maybe_event_matches_key(_A, _B) -> 'false'.

-spec start_amqp(kz_term:proplist(), boolean()) ->
          {'ok', binary()} |
          {'error', _}.
start_amqp(Props, AutoAck) ->
    QueueProps = props:get_value('queue_options', Props, []),
    QueueName = props:get_value('queue_name', Props, <<>>),
    ConsumeOptions = props:get_value('consume_options', Props, []),

    case kz_amqp_util:new_queue(QueueName, QueueProps) of
        {'error', _}=E -> E;
        Q ->
            set_qos(QueueName, props:get_value('basic_qos', Props)),
            case start_consumer(Q, maybe_configure_auto_ack(ConsumeOptions, AutoAck)) of
                'ok' ->
                    lager:debug("queue started: ~s", [Q]),
                    {'ok', Q};
                {'error', _}=E -> E
            end
    end.

-spec set_qos(binary(), 'undefined' | non_neg_integer()) -> 'ok'.
set_qos(<<>>, 'undefined') ->
    case kz_config:get_integer(<<"amqp">>, <<"prefetch">>) of
        [N] when is_integer(N) ->
            lager:debug("random queue getting config.ini QoS settings(~p) applied", [N]),
            kz_amqp_util:basic_qos(N);
        _ ->
            lager:debug("random queue getting default QoS settings(1) applied"),
            kz_amqp_util:basic_qos(?DEFAULT_PREFETCH)
    end;
set_qos(_QueueName, 'undefined') ->
    lager:debug("named queue has no QoS settings");
set_qos(_QueueName, N) when is_integer(N), N >= 0 ->
    lager:debug("applying QoS prefetch of ~p", [N]),
    kz_amqp_util:basic_qos(N).

-spec start_consumer(kz_term:ne_binary(), kz_term:proplist()) -> command_ret().
start_consumer(Q, 'undefined') -> kz_amqp_util:basic_consume(Q, []);
start_consumer(Q, ConsumeProps) -> kz_amqp_util:basic_consume(Q, ConsumeProps).

-spec remove_binding(binding_module(), kz_term:proplist(), kz_term:api_binary()) -> 'ok'.
remove_binding(Binding, Props, Q) ->
    Wapi = list_to_binary([<<"kapi_">>, kz_term:to_binary(Binding)]),
    lager:debug("trying to remove bindings with ~s:unbind_q(~s, ~p)", [Wapi, Q, Props]),
    try (kz_term:to_atom(Wapi, 'true')):unbind_q(Q, Props)
    catch
        'error':'undef' ->
            erlang:error({'api_module_undefined', Wapi})
    end.

-spec create_binding(kz_term:ne_binary(), kz_term:proplist(), kz_term:ne_binary()) -> any().
create_binding(Binding, Props, QueueName) when not is_binary(Binding) ->
    create_binding(kz_term:to_binary(Binding), Props, QueueName);
create_binding(Binding, Props, QueueName) ->
    APIModule = kz_term:to_atom(list_to_binary([<<"kapi_">>, kz_term:to_binary(Binding)]), 'true'),
    declare_binding_exchanges(APIModule),
    'true' = kz_module:is_exported(APIModule, 'bind_q', 2),
    APIModule:bind_q(QueueName, Props).

-spec declare_binding_exchanges(module()) -> 'ok'.
declare_binding_exchanges(APIModule) ->
    case kz_module:is_exported(APIModule, 'declare_exchanges', 0) of
        'true' -> APIModule:declare_exchanges();
        'false' -> 'ok'
    end.

-spec stop_timer(kz_term:api_reference()) -> non_neg_integer() | 'false'.
stop_timer('undefined') -> 'false';
stop_timer(Ref) when is_reference(Ref) -> erlang:cancel_timer(Ref).

-spec start_timer(timeout()) -> kz_term:api_reference().
start_timer(0) ->
    self() ! ?CALLBACK_TIMEOUT_MSG,
    'undefined';
start_timer(Timeout) when is_integer(Timeout)
                          andalso Timeout >= 0 ->
    erlang:send_after(Timeout, self(), ?CALLBACK_TIMEOUT_MSG);
start_timer(_) -> 'undefined'.

-spec add_other_queue(binary(), kz_term:proplist(), kz_term:proplist(), state()) -> {kz_term:ne_binary(), state()}.
add_other_queue(<<>>, QueueProps, Bindings, #state{other_queues=OtherQueues, auto_ack=AutoAck}=State) ->
    {'ok', Q} = start_amqp(QueueProps, AutoAck),
    gen_server:cast(self(), {?MODULE, {'created_queue', Q}}),

    _ = [create_binding(Type, BindProps, Q) || {Type, BindProps} <- Bindings],
    {Q, State#state{other_queues=[{Q, {Bindings, QueueProps}}|OtherQueues]}};
add_other_queue(QueueName, QueueProps, Bindings, #state{other_queues=OtherQueues, auto_ack=AutoAck}=State) ->
    {'ok', Q} = start_amqp([{'queue_name', QueueName} | QueueProps], AutoAck),
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

-spec handle_module_call(any(), kz_term:pid_ref(), state()) -> handle_call_return().
handle_module_call(Request, From, #state{module=Module
                                        ,module_state=ModuleState
                                        ,module_timeout_ref=OldRef
                                        }=State) ->
    _ = stop_timer(OldRef),
    try Module:handle_call(Request, From, ModuleState) of
        {'reply', Reply, ModuleState1} ->
            {'reply'
            ,Reply
            ,State#state{module_state=ModuleState1
                        ,module_timeout_ref='undefined'
                        }
            };
        {'reply', Reply, ModuleState1, 'hibernate'} ->
            {'reply'
            ,Reply
            ,State#state{module_state=ModuleState1
                        ,module_timeout_ref='undefined'
                        }
            ,'hibernate'
            };
        {'reply', Reply, ModuleState1, Timeout} ->
            {'reply', Reply
            ,State#state{module_state=ModuleState1
                        ,module_timeout_ref=start_timer(Timeout)
                        }
            };
        {'noreply', ModuleState1} ->
            {'noreply', State#state{module_state=ModuleState1}};
        {'noreply', ModuleState1, 'hibernate'} ->
            {'noreply', State#state{module_state=ModuleState1}, 'hibernate'};
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
        ?STACKTRACE(_E, R, ST)
        lager:debug("handle_call exception: ~s: ~p", [_E, R]),
        kz_log:log_stacktrace(ST),
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
        {'noreply', ModuleState1, 'hibernate'} ->
            {'noreply', State#state{module_state=ModuleState1}, 'hibernate'};
        {'noreply', ModuleState1, Timeout} ->
            Ref = start_timer(Timeout),
            {'noreply', State#state{module_state=ModuleState1
                                   ,module_timeout_ref=Ref
                                   }
            };
        {'stop', Reason, ModuleState1} ->
            {'stop', Reason, State#state{module_state=ModuleState1}}
    catch
        ?STACKTRACE(_E, R, ST)
        lager:debug("handle_cast exception: ~s: ~p", [_E, R]),
        kz_log:log_stacktrace(ST),
        {'stop', R, State}
        end.

-spec handle_rm_binding(binding(), kz_term:proplist(), state()) -> state().
handle_rm_binding(Binding, Props, #state{queue=Q
                                        ,bindings=Bs
                                        }=State) ->
    KeepBs = [BP || BP <- Bs,
                    maybe_remove_binding(BP
                                        ,kz_term:to_binary(Binding)
                                        ,Props
                                        ,Q
                                        )],
    maybe_remove_federated_binding(Binding, Props, State),
    State#state{bindings=KeepBs}.

-spec handle_add_binding(binding_module(), kz_term:proplist(), state()) ->
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

-spec handle_existing_binding(binding_module(), kz_term:proplist(), state(), kz_term:ne_binary(), kz_term:proplist(), bindings()) ->
          state().
handle_existing_binding(Binding, Props, State, Q, ExistingProps, Bs) ->
    case binding_props_match(Props, ExistingProps) of
        'true' ->
            lager:debug("binding ~s with the same properties exists", [Binding]),
            State#state{bindings=[{Binding, Props}|Bs]};
        'false' ->
            lager:debug("creating existing binding '~s' with new props: ~p", [Binding, Props]),
            create_binding(Binding, Props, Q),
            maybe_update_federated_bindings(State#state{bindings=[{Binding, Props}|Bs]})
    end.

-spec binding_props_match(kz_term:proplist(), kz_term:proplist()) -> boolean().
binding_props_match([], []) -> 'true';
binding_props_match([_|_], []) -> 'false';
binding_props_match([], [_|_]) -> 'false';
binding_props_match([{K, V} | Props], ExistingProps) ->
    case props:take_value(K, ExistingProps) of
        {V, EProps} -> binding_props_match(Props, EProps);
        _ -> 'false'
    end;
binding_props_match([K | Props], ExistingProps) ->
    case props:take_value(K, ExistingProps) of
        {'true', EProps} -> binding_props_match(Props, EProps);
        _ -> 'false'
    end.

-spec maybe_update_federated_bindings(state()) -> state().
maybe_update_federated_bindings(#state{bindings=[{_Binding, Props}|_]}=State) ->
    case is_federated_binding(Props) of
        'false' -> State;
        'true' -> update_federated_bindings(State)
    end.

-spec is_federated_binding(kz_term:proplist()) -> boolean().
is_federated_binding(Props) ->
    props:get_value('federate', Props) =:= 'true'.

-spec update_federated_bindings(state()) -> state().
update_federated_bindings(#state{}=State) ->
    update_federated_bindings(State, kz_amqp_connections:federated_brokers()).

update_federated_bindings(#state{bindings=[{Binding, _Props}|_]}=State, []) ->
    lager:debug("no federated brokers to connect to, skipping federating binding '~s'", [Binding]),
    State;
update_federated_bindings(#state{bindings=[{Binding, Props}|_]
                                ,federators=Fs
                                }=State
                         ,FederatedBrokers
                         ) ->
    NonFederatedProps = props:delete('federate', Props),
    {_Existing, New} = broker_connections(Fs, FederatedBrokers),
    'ok' = update_existing_listeners_bindings(Fs, Binding, NonFederatedProps),
    {'ok', NewListeners} = start_new_listeners(New, Binding, NonFederatedProps, State),
    State#state{federators=NewListeners ++ Fs
               ,waiting_federators=New ++ State#state.waiting_federators
               }.

-spec maybe_remove_federated_binding(binding(), kz_term:proplist(), state()) -> 'ok'.
maybe_remove_federated_binding(Binding, Props, State) ->
    maybe_remove_federated_binding(is_federated_binding(Props), Binding, Props, State).

-spec maybe_remove_federated_binding(boolean(), binding(), kz_term:proplist(), state()) -> 'ok'.
maybe_remove_federated_binding(_Flag, _Binding, _Props, #state{federators=[]}) -> 'ok';
maybe_remove_federated_binding('true', Binding, Props, #state{federators=Fs}) ->
    NonFederatedProps = props:delete('federate', Props),
    remove_federated_binding(Fs, Binding, NonFederatedProps);
maybe_remove_federated_binding(_Flag, _Binging, _Props, _State) -> 'ok'.

-spec broker_connections(federator_listeners(), kz_term:ne_binaries()) ->
          {kz_term:ne_binaries(), kz_term:ne_binaries()}.
broker_connections(Listeners, Brokers) ->
    lists:partition(fun(Broker) ->
                            props:get_value(Broker, Listeners) =/= 'undefined'
                    end
                   ,Brokers
                   ).

-spec start_new_listeners(kz_term:ne_binaries(), binding_module(), kz_term:proplist(), state()) ->
          {'ok', federator_listeners()}.
start_new_listeners(Brokers, Binding, Props, State) ->
    {'ok', [start_new_listener(Broker, Binding, Props, State)
            || Broker <- Brokers
           ]}.

-spec start_new_listener(kz_term:ne_binary(), binding_module(), kz_term:proplist(), state()) -> federator_listener().
start_new_listener(Broker, Binding, Props, #state{params=Ps}) ->
    FederateParams = create_federated_params({Binding, Props}, Ps),
    {'ok', Pid} = kz_amqp_federated_listeners_sup:start_child(self(), Broker, FederateParams),
    Ref = monitor('process', Pid),
    lager:debug("started federated listener on broker ~s: ~p(~p)", [Broker, Pid, Ref]),
    {Broker, {Pid, Ref}}.

-spec remove_federated_binding(federator_listeners(), binding_module(), kz_term:proplist()) -> 'ok'.
remove_federated_binding(Listeners, Binding, Props) ->
    _ = [?MODULE:rm_binding(Pid, Binding, Props)
         || {_Broker, {Pid, _Ref}} <- Listeners
        ],
    'ok'.

-spec update_existing_listeners_bindings(federator_listeners(), binding_module(), kz_term:proplist()) -> 'ok'.
update_existing_listeners_bindings(Listeners, Binding, Props) ->
    _ = [update_existing_listener_bindings(Listener, Binding, Props)
         || Listener <- Listeners
        ],
    'ok'.

-spec update_existing_listener_bindings(federator_listener(), binding_module(), kz_term:proplist()) -> 'ok'.
update_existing_listener_bindings({_Broker, {Pid, _Ref}}, Binding, Props) ->
    lager:debug("updating listener ~p with ~s", [Pid, Binding]),
    ?MODULE:add_binding(Pid, Binding, Props).

-spec create_federated_params({binding_module(), kz_term:proplist()}, kz_term:proplist()) ->
          kz_term:proplist().
create_federated_params(FederateBindings, Params) ->
    QueueOptions = props:get_value('queue_options', Params, []),
    [{'responders', []}
    ,{'bindings', [FederateBindings]}
    ,{'queue_name', federated_queue_name(Params, QueueOptions)}
    ,{'queue_options', QueueOptions}
    ,{'consume_options', props:get_value('consume_options', Params, [])}
    ].

-spec federated_queue_name(kz_term:proplist(), kz_term:proplist()) -> kz_term:api_binary().
federated_queue_name(Params, Options) ->
    QueueName = props:get_value('queue_name', Params, <<>>),
    IsGlobalQueue = props:is_true('federated_queue_name_is_global', Options, 'false'),
    case kz_term:is_empty(QueueName) of
        'true' -> QueueName;
        'false' when IsGlobalQueue -> QueueName;
        'false' ->
            Zone = kz_config:zone('binary'),
            <<QueueName/binary, "-", Zone/binary>>
    end.

-spec handle_amqp_channel_available(state(), boolean()) -> state().
handle_amqp_channel_available(#state{params=Params}=State, Reconnected) ->
    log_channel_status(Reconnected),

    case maybe_declare_exchanges(props:get_value('declare_exchanges', Params, [])) of
        'ok' ->
            handle_exchanges_ready(State);
        {'error', _E} ->
            lager:debug("error declaring exchanges : ~p", [_E]),
            handle_amqp_errored(State)
    end.

-spec maybe_retry(boolean()) -> reference() | 'ok'.
maybe_retry('true') ->
    erlang:send_after(?SERVER_RETRY_PERIOD, self(), 'retry');
maybe_retry('false') ->
    'ok'.

log_channel_status('true') ->
    lager:debug("channel restarted, let's re-connect");
log_channel_status('false') ->
    lager:debug("channel started, let's connect").

-spec handle_exchanges_ready(state()) -> state().
handle_exchanges_ready(#state{params=Params
                             ,auto_ack=AutoAck
                             }=State
                      ) ->
    case start_amqp(Params, AutoAck) of
        {'ok', Q} ->
            State1 = handle_amqp_started(State, Q),
            maybe_start_other_queues(State1);
        {'error', Reason} ->
            lager:warning("start amqp error ~p", [Reason]),
            handle_amqp_errored(State)
    end.

-spec maybe_start_other_queues(state()) -> state().
maybe_start_other_queues(#state{other_queues=[]}=State) ->
    lager:debug("no other queues to start"),
    State;
maybe_start_other_queues(#state{other_queues=Queues}=State) ->
    lists:foldl(fun start_other_queue_fold/2
               ,State#state{other_queues=[]}
               ,Queues
               ).

start_other_queue_fold({QueueName, {Bindings, QueueParams}}, State) ->
    {_Q, State1} = add_other_queue(QueueName, QueueParams, Bindings, State),
    lager:debug("started other queue ~s as ~s", [QueueName, _Q]),
    State1.

-spec handle_amqp_started(state(), kz_term:ne_binary()) -> state().
handle_amqp_started(#state{params=Params}=State, Q) ->
    State1 = start_initial_bindings(State#state{queue=Q}, Params),

    gen_server:cast(self(), {?MODULE, {'created_queue', Q}}),

    maybe_server_confirms(props:get_value('server_confirms', Params, 'false')),

    maybe_channel_flow(props:get_value('channel_flow', Params, 'false')),

    State1#state{is_consuming='false'}.

-spec handle_amqp_errored(state()) -> state().
handle_amqp_errored(State) ->
    _ = maybe_retry(kz_amqp_channel:is_consumer_channel_valid()),
    State#state{is_consuming='false'}.

-spec maybe_server_confirms(boolean()) -> 'ok'.
maybe_server_confirms('true') ->
    kz_amqp_util:confirm_select();
maybe_server_confirms(_) -> 'ok'.

-spec maybe_channel_flow(boolean()) -> 'ok'.
maybe_channel_flow('true') ->
    kz_amqp_util:flow_control();
maybe_channel_flow(_) -> 'ok'.

-spec maybe_declare_exchanges(declare_exchanges()) ->
          command_ret().
maybe_declare_exchanges([]) -> 'ok';
maybe_declare_exchanges(Exchanges) ->
    lists:foldl(fun maybe_declare_exchange/2, 'ok', Exchanges).

-spec maybe_declare_exchange(declare_exchange(), command_ret()) -> command_ret().
maybe_declare_exchange(Exchange, {'ok', _}) ->
    declare_exchange(Exchange);
maybe_declare_exchange(Exchange, 'ok') ->
    declare_exchange(Exchange);
maybe_declare_exchange(_Exchange, Error) ->
    Error.

-spec declare_exchange(declare_exchange()) -> command_ret().
declare_exchange({Ex, Type, Opts}) ->
    ExchangeCmd = kz_amqp_util:declare_exchange(Ex, Type, Opts),
    kz_amqp_channel:command(ExchangeCmd);
declare_exchange({Ex, Type}) ->
    ExchangeCmd = kz_amqp_util:declare_exchange(Ex, Type),
    kz_amqp_channel:command(ExchangeCmd).

-spec start_initial_bindings(state(), kz_term:proplist()) -> state().
start_initial_bindings(State, Params) ->
    lists:foldl(fun({Binding, Props}, StateAcc) ->
                        handle_add_binding(kz_term:to_binary(Binding), Props, StateAcc)
                end
               ,State
               ,props:get_value('bindings', Params, [])
               ).

-spec channel_requisition(kz_term:proplist()) -> boolean().
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

-spec maybe_add_broker_connection(binary()) -> 'ok'.
maybe_add_broker_connection(Broker) ->
    _ = kz_amqp_channel:consumer_broker(Broker),
    Count = kz_amqp_connections:broker_connections(Broker),
    maybe_add_broker_connection(Broker, Count).

-spec maybe_add_broker_connection(binary(), non_neg_integer()) -> 'ok'.
maybe_add_broker_connection(Broker, Count) when Count =:= 0 ->
    _Connection = kz_amqp_connections:add(Broker, kz_binary:rand_hex(6), [<<"hidden">>]),
    kz_amqp_channel:requisition(self(), Broker);
maybe_add_broker_connection(Broker, _Count) ->
    kz_amqp_channel:requisition(self(), Broker).

maybe_configure_auto_ack(Props, 'false') -> Props;
maybe_configure_auto_ack(Props, 'true') ->
    [{'no_ack', 'false'} | props:delete('no_ack', Props)].

%% @doc force GC of process if total_heap_size exceeds 10K
%%
%% In production, when compacting kz_amqp_workers and other
%% gen_listener processes, we see a reduction in total_heap_size to
%% about 4K on most processes.
%%
%% Old heap is used for "long-lived" terms. One of the issues was that
%% when a binary Payload was received from AMQP, we would decode it
%% directly which causes refc binaries to be put in the gen_listener's
%% heap, keeping the binary alive. By using binary:copy/1 before
%% decoding, Payload can be freed sooner. I suspect that Payload
%% itself is a refc into the larger AMQP binary payload parsed by the
%% AMQP client library.
%%
%% It is possible that apps may load more memory into their state
%% which would cause gen_listener's state to exceed 10K in the steady
%% state. We advise app developers to not do this :)
-spec maybe_gc(Reply) -> Reply.
maybe_gc(Reply) ->
    maybe_gc(process_info(self(), ['total_heap_size'])
            ,100 * ?BYTES_K
            ),
    Reply.

maybe_gc([{'total_heap_size', Words}], MaxBytes) ->
    maybe_gc(kz_term:words_to_bytes(Words), MaxBytes);
maybe_gc(Heap, Max) when Heap > Max ->
    erlang:garbage_collect(self()),
    [{'total_heap_size', NewHeapWords}] = process_info(self(), ['total_heap_size']),
    NewHeap = kz_term:words_to_bytes(NewHeapWords),
    lager:debug("new heap size ~p (delta ~p)", [NewHeap, Heap-NewHeap]);
maybe_gc(_Heap, _Max) -> 'ok'.
