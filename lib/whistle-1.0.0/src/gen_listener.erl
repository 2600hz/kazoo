%%%-------------------------------------------------------------------
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%% Behaviour for setting up an AMQP listener.
%%% Add/rm responders for Event-Cat/Event-Name pairs. Each responder
%%% corresponds to a module that has defined a handle/1 function, receiving
%%% the wh_json:json_object() from the AMQP request.
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

-include_lib("amqp_client/include/amqp_client.hrl").
-include_lib("whistle/include/wh_amqp.hrl").
-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_log.hrl").

-export([behaviour_info/1]).

-export([start_link/3, start_link/4, stop/1]).

-export([queue_name/1, responders/1]).

-export([add_queue/4, other_queues/1, rm_queue/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2
         ,code_change/3, format_status/2
        ]).

%% gen_server API
-export([call/2, call/3, cast/2, reply/2]).

%% gen_listener API
-export([add_responder/3, rm_responder/2, rm_responder/3]).

-export([add_binding/2, add_binding/3, rm_binding/2, rm_binding/3]).

-export([ack/2, nack/2]).

behaviour_info(callbacks) ->
    [{init, 1}
     ,{handle_event, 2} %% Module passes back {reply, Proplist}, passed as 2nd param to Responder:handle_req/2
     ,{handle_call, 3}
     ,{handle_cast, 2}
     ,{handle_info, 2}
     ,{terminate, 2}
    ];
behaviour_info(_) ->
    undefined.

-type binding() :: {atom() | ne_binary(), wh_proplist()}. %% {wapi_module, options}
-type bindings() :: [binding(),...] | [].

-type responder_callback_mod() :: atom() | {atom(), atom()}.
-type responder_callback_mapping() :: {ne_binary(), ne_binary()}.
-type responder_callback_mappings() :: [responder_callback_mapping(),...] | [].
-type responder_start_params() :: [{responder_callback_mod(), responder_callback_mappings()},...].

-type start_params() :: [{responders, responder_start_params()} |
                         {bindings, bindings()} |
                         {queue_name, binary()} |
                         {queue_options, wh_proplist()} |
                         {consume_options, wh_proplist()} |
                         {basic_qos, non_neg_integer()}
                         ,...].

-record(state, {
          queue = <<>> :: binary()
         ,is_consuming = 'false' :: boolean()
         ,responders = [] :: listener_utils:responders() %% { {EvtCat, EvtName}, Module }
         ,bindings = [] :: bindings() %% {authentication, [{key, value},...]}
         ,params = [] :: wh_proplist()
         ,module = 'undefined' :: atom()
         ,module_state = 'undefined' :: term()
         ,module_timeout_ref = 'undefined' :: 'undefined' | reference() % when the client sets a timeout, gen_listener calls shouldn't negate it, only calls that pass through to the client
         ,active_responders = [] :: [pid(),...] | [] %% list of pids processing requests
         ,other_queues = [] :: [{ne_binary(), bindings()},...] | [] %% {QueueName, Binding()}
         ,last_call_event = [] :: proplist()
         }).

-define(TIMEOUT_RETRY_CONN, 5000).
-define(CALLBACK_TIMEOUT_MSG, callback_timeout).

-define(START_TIMEOUT, 500).
-define(MAX_TIMEOUT, 5000).

%% API functions for requesting data from the gen_listener
-spec queue_name/1 :: (pid()) -> ne_binary().
queue_name(Srv) ->
    gen_server:call(Srv, queue_name).

responders(Srv) ->
    gen_server:call(Srv, responders).

ack(Srv, Delivery) ->
    gen_server:cast(Srv, {ack, Delivery}).

nack(Srv, Delivery) ->
    gen_server:cast(Srv, {nack, Delivery}).

%% API functions that mirror gen_server:call,cast,reply
-spec call/2 :: (pid(), term()) -> term().
call(Name, Request) ->
    gen_server:call(Name, Request).

-spec call/3 :: (pid(), term(), 'infinity' | non_neg_integer()) -> term().
call(Name, Request, Timeout) ->
    gen_server:call(Name, Request, Timeout).

-spec cast/2 :: (pid(), term()) -> 'ok'.
cast(Name, Request) ->
    gen_server:cast(Name, Request).

-spec reply/2 :: ({pid(), reference()}, term()) -> no_return().
reply(From, Msg) ->
    gen_server:reply(From, Msg).

%% Starting the gen_server
-spec start_link/3 :: (atom(), start_params(), term()) -> startlink_ret().
start_link(Module, Params, InitArgs) ->
    gen_server:start_link(?MODULE, [Module, Params, InitArgs], []).

-spec start_link/4 :: ({local, atom()} | {global, term()}, atom(), start_params(), term()) -> startlink_ret().
start_link(Name, Module, Params, InitArgs) ->
    gen_server:start_link(Name, ?MODULE, [Module, Params, InitArgs], []).

-spec stop/1 :: (pid()) -> 'ok'.
stop(Srv) when is_pid(Srv) ->
    gen_server:cast(Srv, stop).

-spec add_responder/3 :: (pid(), responder_callback_mod(), responder_callback_mapping() | responder_callback_mappings()) -> 'ok'.
add_responder(Srv, Responder, Key) when not is_list(Key) ->
    add_responder(Srv, Responder, [Key]);
add_responder(Srv, Responder, [{_,_}|_] = Keys) ->
    gen_server:cast(Srv, {add_responder, Responder, Keys}).

-spec rm_responder/2 :: (pid(), responder_callback_mod()) -> 'ok'.
-spec rm_responder/3 :: (pid(), responder_callback_mod(), responder_callback_mappings()) -> 'ok'.
rm_responder(Srv, Responder) ->
    rm_responder(Srv, Responder, []).  %% empty list removes all
rm_responder(Srv, Responder, {_,_}=Key) ->
    rm_responder(Srv, Responder, [Key]);
rm_responder(Srv, Responder, Keys) ->
    gen_server:cast(Srv, {rm_responder, Responder, Keys}).

-spec add_binding/2 :: (pid(), binding() | ne_binary() | atom()) -> 'ok'.
-spec add_binding/3 :: (pid(), ne_binary() | atom(), wh_proplist()) -> 'ok'.
add_binding(Srv, {Binding, Props}) ->
    gen_server:cast(Srv, {add_binding, Binding, Props});
add_binding(Srv, Binding) when is_binary(Binding) orelse is_atom(Binding) ->
    gen_server:cast(Srv, {add_binding, wh_util:to_binary(Binding), []}).

add_binding(Srv, Binding, Props) when is_binary(Binding) orelse is_atom(Binding) ->
    gen_server:cast(Srv, {add_binding, wh_util:to_binary(Binding), Props}).

%% It is expected that responders have been set up already, prior to binding the new queue
-spec add_queue/4 :: (pid(), binary(), proplist(), binding() | bindings()) -> {'ok', ne_binary()} | {'error', term()}.
add_queue(Srv, QueueName, QueueProps, {_Type, _Props}=Binding) ->
    add_queue(Srv, QueueName, QueueProps, [Binding]);
add_queue(Srv, QueueName, QueueProps, [{_,_}|_]=Bindings) ->
    gen_server:call(Srv, {add_queue, QueueName, QueueProps, Bindings}).

-spec rm_queue/2 :: (pid(), ne_binary()) -> 'ok'.
rm_queue(Srv, ?NE_BINARY = QueueName) ->
    gen_server:cast(Srv, {rm_queue, QueueName}).

-spec other_queues/1 :: (pid()) -> [ne_binary(),...] | [].
other_queues(Srv) ->
    gen_server:call(Srv, other_queues).

-spec rm_binding/2 :: (pid(), binding()) -> 'ok'.
-spec rm_binding/3 :: (pid(), ne_binary() | atom(), wh_proplist()) -> 'ok'.
rm_binding(Srv, {Binding, Props}) ->
    rm_binding(Srv, Binding, Props).
rm_binding(Srv, Binding, Props) ->
    gen_server:cast(Srv, {rm_binding, wh_util:to_binary(Binding), Props}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
-spec init/1 :: ([atom() | wh_proplist(),...]) -> {'ok', #state{}, 'hibernate'}.
init([Module, Params, InitArgs]) ->
    process_flag(trap_exit, true),
    put(callid, ?LOG_SYSTEM_ID),
    lager:debug("starting new gen_listener proc: ~s", [wh_util:to_binary(Module)]),
    {ModState, TimeoutRef} = case erlang:function_exported(Module, init, 1) andalso Module:init(InitArgs) of
                                 {ok, MS} ->
                                     {MS, undefined};
                                 {ok, MS, hibernate} ->
                                     {MS, undefined};
                                 {ok, MS, Timeout} ->
                                     {MS, start_timer(Timeout)};
                                 Err ->
                                     throw(Err)
                             end,
    Responders = props:get_value(responders, Params, []),
    Bindings = props:get_value(bindings, Params, []),
    case start_amqp(Params) of
        {error, _R} ->
            lager:alert("lost our channel, but its back up; rebinding"),
            _Ref = erlang:send_after(?TIMEOUT_RETRY_CONN, self(), {amqp_channel_event, initial_conn_failed}),
            {ok, #state{module=Module, module_state=ModState, module_timeout_ref=TimeoutRef
                        ,responders=[], bindings=Bindings
                        ,params=lists:keydelete(responders, 1, lists:keydelete(bindings, 1, Params))}};
        {ok, Q} ->
            _ = erlang:send_after(?TIMEOUT_RETRY_CONN, self(), is_consuming),
            _ = [add_responder(self(), Mod, Evts) || {Mod, Evts} <- Responders],
            _ = [create_binding(wh_util:to_binary(Type), BindProps, Q) || {Type, BindProps} <- Bindings],
            {ok, #state{queue=Q, module=Module, module_state=ModState, module_timeout_ref=TimeoutRef
                        ,responders=[], bindings=Bindings
                        ,params=lists:keydelete(responders, 1, lists:keydelete(bindings, 1, Params))}
             ,hibernate}
    end.

-type gen_l_handle_call_ret() :: {'reply', term(), #state{}, gen_server_timeout()} |
                                 {'noreply', #state{}, gen_server_timeout()} |
                                 {'stop', term(), #state{}} | {'stop', term(), term(), #state{}}.

-spec handle_call/3 :: (term(), {pid(), reference()}, #state{}) -> gen_l_handle_call_ret().
handle_call({add_queue, QueueName, QueueProps, Bindings}, _From, State) ->
    {Q, S} = add_other_queue(QueueName, QueueProps, Bindings, State),
    {reply, {ok, Q}, S};

handle_call(other_queues, _From, #state{other_queues=OtherQueues}=State) ->
    {reply, props:get_keys(OtherQueues), State};

handle_call(queue_name, _From, #state{queue=Q}=State) ->
    {reply, Q, State};
handle_call(responders, _From, #state{responders=Rs}=State) ->
    {reply, Rs, State};
handle_call(Request, From, #state{module=Module, module_state=ModState, module_timeout_ref=OldRef}=State) ->
    _ = stop_timer(OldRef),
    case catch Module:handle_call(Request, From, ModState) of
        {reply, Reply, ModState1} ->
            {reply, Reply, State#state{module_state=ModState1, module_timeout_ref=undefined}, hibernate};
        {reply, Reply, ModState1, Timeout} when is_integer(Timeout) andalso Timeout >= 0 ->
            Ref = start_timer(Timeout),
            {reply, Reply, State#state{module_state=ModState1, module_timeout_ref=Ref}, hibernate};
        {noreply, ModState1} ->
            {noreply, State#state{module_state=ModState1}, hibernate};
        {noreply, ModState1, Timeout} ->
            Ref = start_timer(Timeout),
            {noreply, State#state{module_state=ModState1, module_timeout_ref=Ref}, hibernate};
        {stop, Reason, ModState1} ->
            {stop, Reason, State#state{module_state=ModState1}};
        {stop, Reason, Reply, ModState1} ->
            {stop, Reason, Reply, State#state{module_state=ModState1}};
        {'EXIT', Why} ->
            lager:alert("exception: ~p", [Why]),
            {stop, Why, State}
    end.

-spec handle_cast/2 :: (term(), #state{}) -> handle_cast_ret().

handle_cast({ack, Delivery}, State) ->
    amqp_util:basic_ack(Delivery),
    {noreply, State};

handle_cast({nack, Delivery}, State) ->
    amqp_util:basic_nack(Delivery),
    {noreply, State};

handle_cast({rm_queue, QueueName}, #state{other_queues=OtherQueues}=State) ->
    _ = [remove_binding(Binding, Props, QueueName) || {Binding, Props} <- props:get_value(QueueName, OtherQueues, [])],
    {noreply, State#state{other_queues=props:delete(QueueName, OtherQueues)}};

handle_cast(stop, #state{active_responders=[]}=State) ->
    {stop, normal, State};
handle_cast(stop, #state{queue = <<>>}=State) ->
    self() ! stop, % put a message in the queue to check length again
    {noreply, State, 50};
handle_cast(stop, #state{queue=Q, bindings=Bindings}=State) ->
    self() ! stop, % put a message in the queue to check length again
    _ = [remove_binding(Binding, Props, Q) || {Binding, Props} <- Bindings],
    {noreply, State#state{queue = <<>>}, 0};

handle_cast({add_responder, Responder, Keys}, #state{responders=Responders}=State) ->
    {noreply, State#state{responders=listener_utils:add_responder(Responders, Responder, Keys)}, hibernate};

handle_cast({rm_responder, Responder, Keys}, #state{responders=Responders}=State) ->
    {noreply, State#state{responders=listener_utils:rm_responder(Responders, Responder, Keys)}, hibernate};

handle_cast({add_binding, Binding, Props}, #state{queue=Q, bindings=Bs}=State) ->
    case lists:keyfind(Binding, 1, Bs) of
        false ->
            lager:debug("creating new binding: ~s", [Binding]),
            create_binding(Binding, Props, Q),
            {noreply, State#state{bindings=[{Binding, Props}|Bs]}};
        {_, P} ->
            case Props =:= P of
                true ->
                    lager:debug("binding ~s exists", [Binding]),
                    {noreply, State};
                false ->
                    lager:debug("adding binding ~s with new props: ~p", [Binding, Props]),
                    create_binding(Binding, Props, Q),
                    {noreply, State#state{bindings=[{Binding, Props}|Bs]}}
            end
    end;

handle_cast({rm_binding, Binding, Props}, #state{queue=Q, bindings=Bs}=State) ->
    KeepBs = lists:filter(fun({B, P}) when B =:= Binding, P =:= Props ->
                                  lager:debug("removing binding ~s (~p)", [B, P]),
                                  remove_binding(B, P, Q),
                                  false;
                             (_) -> true
                          end, Bs),
    {noreply, State#state{bindings=KeepBs}};

handle_cast({add_queue, QueueName, QueueProps, Bindings}, State) ->
    {_, S} = add_other_queue(QueueName, QueueProps, Bindings, State),
    {noreply, S};

handle_cast(Message, #state{module=Module, module_state=ModState, module_timeout_ref=OldRef}=State) ->
    _ = stop_timer(OldRef),
    case catch Module:handle_cast(Message, ModState) of
        {noreply, ModState1} ->
            {noreply, State#state{module_state=ModState1}, hibernate};
        {noreply, ModState1, Timeout} ->
            Ref = start_timer(Timeout),
            {noreply, State#state{module_state=ModState1, module_timeout_ref=Ref}, hibernate};
        {stop, Reason, ModState1} ->
            {stop, Reason, State#state{module_state=ModState1}};
        {'EXIT', {Reason, ST}} ->
            lager:debug("exception: ~p", [Reason]),
            _ = [lager:debug("st: ~p", [T]) || T <- ST],
            {stop, Reason, State}
    end.

-spec handle_info/2 :: (term(), #state{}) -> handle_info_ret().
handle_info({#'basic.deliver'{}=BD, #amqp_msg{props = #'P_basic'{content_type=CT}, payload = Payload}}, State) ->
    case catch handle_event(Payload, CT, BD, State) of
        #state{}=S ->
            {noreply, S, hibernate};
        {'EXIT', Why} ->
            lager:alert("exception: ~p", [Why]),
            {stop, Why, State}
    end;

handle_info({'EXIT', Pid, _Reason}=Message, #state{active_responders=ARs}=State) ->
    case lists:member(Pid, ARs) of
        true -> {noreply, State#state{active_responders=lists:delete(Pid, ARs)}, hibernate};
        false -> handle_callback_info(Message, State)
    end;

handle_info({amqp_channel_event, initial_conn_failed}, State) ->
    lager:alert("failed to create initial connection to AMQP, waiting for connection"),
    _Ref = erlang:send_after(?START_TIMEOUT, self(), {'$maybe_connect_amqp', ?START_TIMEOUT}),
    {noreply, State#state{queue = <<>>, is_consuming=false}, hibernate};
handle_info({amqp_channel_event, restarted}, #state{params=Params, bindings=Bindings, other_queues=OtherQueues}=State) ->
    case start_amqp(Params) of
        {ok, Q} ->
            lager:debug("lost our channel, but its back up; rebinding"),
            [add_binding(self(), Type, BindProps) 
             || {Type, BindProps} <- Bindings
            ],
            [gen_server:cast(self(), {add_queue, Name, Props, Bind})
             || {Name, {Props, Bind}} <- OtherQueues
            ],
            _ = erlang:send_after(?TIMEOUT_RETRY_CONN, self(), is_consuming),
            {noreply, State#state{queue=Q, is_consuming=false, bindings=[], other_queues=[]}, hibernate};
        {error, _R} ->
            lager:alert("failed to rebind after channel restart: ~p", [_R]),
            _Ref = erlang:send_after(?START_TIMEOUT, self(), {'$maybe_connect_amqp', ?START_TIMEOUT}),
            {noreply, State#state{queue = <<>>, is_consuming=false}, hibernate}
    end;
handle_info({amqp_channel_event, _Reason}, State) ->
    lager:alert("notified AMQP channel died: ~p", [_Reason]),
    _Ref = erlang:send_after(?START_TIMEOUT, self(), {'$maybe_connect_amqp', ?START_TIMEOUT}),
    {noreply, State#state{queue = <<>>, is_consuming=false}, hibernate};

handle_info({'$maybe_connect_amqp', Timeout}, #state{bindings=Bindings, params=Params, other_queues=OtherQueues}=State) ->
    case start_amqp(Params) of
        {ok, Q} ->            
            lager:info("reconnected to AMQP channel, rebinding"),
            [add_binding(self(), Type, BindProps) 
             || {Type, BindProps} <- Bindings
            ],
            [gen_server:cast(self(), {add_queue, Name, Props, Bind})
             || {Name, {Bind, Props}} <- OtherQueues
            ],
            _ = erlang:send_after(?TIMEOUT_RETRY_CONN, self(), is_consuming),
            {noreply, State#state{queue=Q, is_consuming=false, bindings=[], other_queues=[]}, hibernate};
        {error, _} ->
            _Ref = erlang:send_after(Timeout, self(), {'$maybe_connect_amqp', next_timeout(Timeout)}),
            {noreply, State#state{queue = <<>>, is_consuming=false}, hibernate}
    end;

handle_info(#'basic.consume_ok'{}, S) ->
    lager:debug("consuming from our queue"),
    {noreply, S#state{is_consuming=true}};

handle_info(is_consuming, #state{is_consuming=false, queue=Q}=State) ->
    lager:debug("huh, we're not consuming. Queue: ~p", [Q]),
    _Ref = erlang:send_after(?START_TIMEOUT, self(), {'$maybe_connect_amqp', ?START_TIMEOUT}),
    {noreply, State};

handle_info(is_consuming, State) ->
    {noreply, State};

handle_info(callback_timeout, State) ->
    handle_callback_info(timeout, State);

handle_info(Message, State) ->
    handle_callback_info(Message, State).

handle_callback_info(Message, #state{module=Module, module_state=ModState, module_timeout_ref=OldRef}=State) ->
    _ = stop_timer(OldRef),
    case catch Module:handle_info(Message, ModState) of
        {noreply, ModState1} ->
            {noreply, State#state{module_state=ModState1}, hibernate};
        {noreply, ModState1, Timeout} ->
            Ref = start_timer(Timeout),
            {noreply, State#state{module_state=ModState1, module_timeout_ref=Ref}, hibernate};
        {stop, Reason, ModState1} ->
            {stop, Reason, State#state{module_state=ModState1}};
        {'EXIT', Why} ->
            lager:alert("exception: ~p", [Why]),
            {stop, Why, State}
    end.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

format_status(_Opt, [_PDict, #state{module=Module, module_state=ModState}=State]) ->
    [{data, [{"Module State", ModState}
             ,{"Module", Module}
            ]}
     ,{data, [{"Listener State", State}]}
    ].

terminate(Reason, #state{module=Module, module_state=ModState
                         ,queue=Q, bindings=Bs
                        }) ->
    _ = (catch Module:terminate(Reason, ModState)),
    lists:foreach(fun({B, P}) ->
                          lager:debug("terminating binding ~s (~p)", [B, P]),
                          (catch remove_binding(B, P, Q))
                  end, Bs),
    lager:debug("~s terminated cleanly, going down", [Module]).

-spec handle_event/4 :: (ne_binary(), ne_binary(), #'basic.deliver'{}, #state{}) ->  #state{}.
handle_event(Payload, <<"application/json">>, BD, State) ->
    JObj = mochijson2:decode(Payload),
    process_req(State, JObj, BD);
handle_event(Payload, <<"application/erlang">>, BD, State) ->
    JObj = binary_to_term(Payload),
    process_req(State, JObj, BD).

-spec process_req/3 :: (#state{}, wh_json:json_object(), #'basic.deliver'{}) -> #state{}.
process_req(#state{responders=Responders, active_responders=ARs}=State, JObj, BD) ->
    case handle_callback_event(State, JObj) of
        ignore -> State;
        Props ->
            Pid = proc_lib:spawn_link(fun() -> 
                                              _ = wh_util:put_callid(JObj),
                                              process_req(Props, Responders, JObj, BD) 
                                      end),
            State#state{active_responders=[Pid | ARs]}
    end.

-spec process_req/4 :: (wh_proplist(), listener_utils:responders(), wh_json:json_object(), #'basic.deliver'{}) -> 'ok'.
process_req(Props, Responders, JObj, BD) ->
    Key = wh_util:get_event_type(JObj),

    Handlers = [spawn_monitor(fun() ->
                                      _ = wh_util:put_callid(JObj),
                                      case erlang:function_exported(Responder, Fun, 3) of
                                          true -> Responder:Fun(JObj, Props, BD);
                                          false -> Responder:Fun(JObj, Props)
                                      end
                              end)
                || {Evt, {Responder, Fun}} <- Responders,
                   maybe_event_matches_key(Key, Evt)
               ],
    wait_for_handlers(Handlers).

-spec handle_callback_event/2 :: (#state{}, wh_json:json_object()) -> 'ignore' | proplist().
handle_callback_event(#state{module=Module, module_state=ModState, queue=Queue, other_queues=OtherQueues}, JObj) ->
    OtherQueueNames = props:get_keys(OtherQueues),
    case catch Module:handle_event(JObj, ModState) of
        {reply, Props} when is_list(Props) -> [{server, self()}
                                               ,{queue, Queue}
                                               ,{other_queues, OtherQueueNames}
                                               | Props
                                              ];
        {'EXIT', _Why} -> [{server, self()}
                           ,{queue, Queue}
                           ,{other_queues, OtherQueueNames}
                          ];
        ignore -> ignore
    end.

%% allow wildcard (<<"*">>) in the Key to match either (or both) Category and Name
-spec maybe_event_matches_key/2 :: (responder_callback_mapping(), responder_callback_mapping()) -> boolean().
maybe_event_matches_key(Evt, Evt) -> true;
maybe_event_matches_key({_, Name}, {<<"*">>, Name}) -> true;
maybe_event_matches_key({Cat, _}, {Cat, <<"*">>}) -> true;
maybe_event_matches_key({_,_}, {<<"*">>, <<"*">>}) -> true;
maybe_event_matches_key(_A, _B) -> false.

%% Collect the spawned handlers going down so the main process_req proc doesn't end until all
%% handlers have completed (for graceful stopping).
-spec wait_for_handlers/1 :: ([{pid(), reference()},...] | []) -> 'ok'.
wait_for_handlers([{Pid, Ref} | Hs]) ->
    receive
        {'DOWN', Ref, process, Pid, _Reason} ->
            wait_for_handlers(Hs)
    end;
wait_for_handlers([]) -> ok.

-spec start_amqp/1 :: (wh_proplist()) -> {'ok', binary()} | {'error', 'amqp_error'}.
start_amqp(Props) ->
    case wh_amqp_mgr:is_available() of
        false -> {error, amqp_down};
        true ->
            QueueProps = props:get_value(queue_options, Props, []),
            QueueName = props:get_value(queue_name, Props, <<>>),
            case amqp_util:new_queue(QueueName, QueueProps) of
                {error, _}=E -> E;
                Q ->
                    ConsumeProps = props:get_value(consume_options, Props, []),
                    set_qos(props:get_value(basic_qos, Props)),
                    ok = amqp_util:basic_consume(Q, ConsumeProps),
                    {ok, Q}
            end
    end.

-spec set_qos/1 :: ('undefined' | non_neg_integer()) -> 'ok'.
set_qos(undefined) -> ok;
set_qos(N) when is_integer(N) -> amqp_util:basic_qos(N).

-spec remove_binding/3 :: (ne_binary(), proplist(), ne_binary()) -> any().
remove_binding(Binding, Props, Q) ->
    Wapi = list_to_binary([<<"wapi_">>, wh_util:to_binary(Binding)]),
    try
        ApiMod = wh_util:to_atom(Wapi),
        ApiMod:unbind_q(Q, Props)
    catch
        error:badarg ->
            lager:debug("api module ~s not found", [Wapi]),
            case code:where_is_file(wh_util:to_list(<<Wapi/binary, ".beam">>)) of
                non_existing ->
                    lager:debug("beam file not found for ~s, trying old method", [Wapi]),
                    erlang:error(api_module_undefined);
                _Path ->
                    lager:debug("beam file found: ~s", [_Path]),
                    wh_util:to_atom(Wapi, true), %% put atom into atom table
                    remove_binding(Binding, Props, Q)
            end;
        error:undef ->
            lager:debug("module ~s doesn't exist or unbind_q/2 isn't exported", [Wapi]),
            erlang:error(api_call_undefined)
    end.

-spec create_binding/3 :: (ne_binary(), wh_proplist(), ne_binary()) -> any().
create_binding(Binding, Props, Q) ->
    Wapi = list_to_binary([<<"wapi_">>, wh_util:to_binary(Binding)]),
    try
        ApiMod = wh_util:to_atom(Wapi),
        ApiMod:bind_q(Q, Props)
    catch
        error:badarg ->
            lager:debug("api module ~s not found", [Wapi]),
            case code:where_is_file(wh_util:to_list(<<Wapi/binary, ".beam">>)) of
                non_existing ->
                    lager:debug("beam file not found for ~s, trying old method", [Wapi]),
                    erlang:error(api_module_undefined);
                _Path ->
                    lager:debug("beam file found: ~s", [_Path]),
                    wh_util:to_atom(Wapi, true), %% put atom into atom table
                    create_binding(Binding, Props, Q)
            end;
        error:undef ->
            lager:debug("module ~s doesn't exist or bind_q/2 isn't exported", [Wapi]),
            erlang:error(api_call_undefined)
    end.

-spec stop_timer/1 :: ('undefined' | reference()) -> non_neg_integer() | 'false'.
stop_timer(undefined) ->
    false;
stop_timer(Ref) when is_reference(Ref) ->
    erlang:cancel_timer(Ref).

-spec start_timer/1 :: (term()) -> reference() | 'undefined'.
start_timer(Timeout) when is_integer(Timeout) andalso Timeout >= 0 ->
    erlang:send_after(Timeout, self(), ?CALLBACK_TIMEOUT_MSG);
start_timer(_) -> 'undefined'.

-spec next_timeout/1 :: (pos_integer()) -> ?START_TIMEOUT..?MAX_TIMEOUT.
next_timeout(?MAX_TIMEOUT=Timeout) ->
    Timeout;
next_timeout(Timeout) when Timeout*2 > ?MAX_TIMEOUT ->
    ?MAX_TIMEOUT;
next_timeout(Timeout) when Timeout < ?START_TIMEOUT ->
    ?START_TIMEOUT;
next_timeout(Timeout) ->
    Timeout * 2.


-spec add_other_queue/4 :: (binary(), proplist(), proplist(), #state{}) -> {ne_binary(), #state{}}.
add_other_queue(<<>>, QueueProps, Bindings, #state{other_queues=OtherQueues}=State) ->
    {ok, Q} = start_amqp(QueueProps),
    _ = [create_binding(wh_util:to_binary(Type), BindProps, Q) || {Type, BindProps} <- Bindings],
    {Q, State#state{other_queues=[{Q, {Bindings, QueueProps}}|OtherQueues]}};
add_other_queue(QueueName, QueueProps, Bindings, #state{other_queues=OtherQueues}=State) ->
    {ok, _} = start_amqp([{queue_name, QueueName} | QueueProps]),
    _ = [create_binding(wh_util:to_binary(Type), BindProps, QueueName) || {Type, BindProps} <- Bindings],
    case props:get_value(QueueName, OtherQueues) of
        undefined ->
            {QueueName, State#state{other_queues=[{QueueName, {Bindings, QueueProps}}|OtherQueues]}};
        OldBindings ->
            {QueueName, State#state{other_queues=[{QueueName, {Bindings ++ OldBindings, QueueProps}}
                                                  | props:delete(QueueName, OtherQueues)
                                                 ]}}
    end.
