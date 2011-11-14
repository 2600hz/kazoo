%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Behaviour for setting up an AMQP listener.
%%% Add/rm responders for Event-Cat/Event-Name pairs. Each responder
%%% corresponds to a module that has defined a handle/1 function, receiving
%%% the json_object() from the AMQP request.
%%%
%%% Params :: [
%%%   {bindings, [ {atom(), proplist()}, ...]} -> the type of bindings, with optional properties to pass along
%%%   {responders, [ {responder, [ {<<"event-category">>, <<"event-name">>}, ...]} ]
%%%      responder is the module name to call handle_req/2 on for those category/name combos
%%%      responder can also be {module, function}, to call module:function/2 instead of handle_req/2
%%%   {queue_name, <<"some name">>} -> optional, if you want a named queue
%%%   {queue_options, [{key, value}]} -> optional, if the queue requires different params
%%%   {consume_options, [{key, value}]} -> optional, if the consumption requires special params
%%%   {basic_qos, integer()} -> optional, if QoS is being set on this queue
%%% ]
%%% @end
%%% Created : 19 Aug 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(gen_listener).

-behaviour(gen_server).

-include_lib("rabbitmq_erlang_client/include/amqp_client.hrl").
-include_lib("whistle/include/wh_amqp.hrl").
-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_log.hrl").

-export([behaviour_info/1]).

-export([start_link/3, stop/1]).

-export([queue_name/1, responders/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2
         ,code_change/3
	]).

%% gen_server API
-export([call/2, call/3, cast/2, reply/2]).

%% gen_listener API
-export([add_responder/3, rm_responder/2, rm_responder/3]).

-export([add_binding/2, add_binding/3, rm_binding/2, rm_binding/3]).

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

-type responders() :: [listener_utils:responder(),...] | [].
-type binding() :: {queue_bindings:bind_types(), proplist()}.
-type bindings() :: [binding(),...] | [].
-type start_params() :: [{responders, responders()} |
			 {bindings, bindings()} |
			 {queue_name, binary()} |
			 {queue_options, proplist()} |
			 {consume_options, proplist()} |
			 {basic_qos, non_neg_integer()}
			 ,...] | [].

-record(state, {
	  queue = <<>> :: binary()
         ,is_consuming = false :: boolean()
	 ,responders = [] :: responders() %% { {EvtCat, EvtName}, Module }
         ,bindings = [] :: bindings() %% authentication | {authentication, [{key, value},...]}
         ,params = [] :: proplist()
	 ,module = 'undefined' :: atom()
         ,module_state = 'undefined' :: term()
         ,active_responders = [] :: [pid(),...] | [] %% list of pids processing requests
	 }).

-define(TIMEOUT_RETRY_CONN, 1000).

%% API functions for requesting data from the gen_listener
-spec queue_name/1 :: (Srv) -> {'ok', binary()} | {'error', atom()} when
      Srv :: atom() | pid().
queue_name(Srv) ->
    gen_server:call(Srv, queue_name).

responders(Srv) ->
    gen_server:call(Srv, responders).

%% API functions that mirror gen_server:call,cast,reply
-spec call/2 :: (Name, Request) -> term() when
      Name :: atom() | pid(),
      Request :: term().
call(Name, Request) ->
    gen_server:call(Name, Request).

-spec call/3 :: (Name, Request, Timeout) -> term() when
      Name :: atom() | pid(),
      Request :: term(),
      Timeout :: 'infinity' | non_neg_integer().
call(Name, Request, Timeout) ->
    gen_server:call(Name, Request, Timeout).

-spec cast/2 :: (Name, Request) -> 'ok' when
      Name :: atom() | pid(),
      Request :: term().
cast(Name, Request) ->
    gen_server:cast(Name, Request).

-spec reply/2 :: (From, Msg) -> no_return() when
      From :: {pid(), reference()},
      Msg :: term().
reply(From, Msg) ->
    gen_server:reply(From, Msg).

%% Starting the gen_server
-spec start_link/3 :: (Module, Params, InitArgs) -> startlink_ret() when
      Module :: atom(),
      Params :: start_params(),
      InitArgs :: term().
start_link(Module, Params, InitArgs) ->
    gen_server:start_link(?MODULE, [Module, Params, InitArgs], []).

-spec stop/1 :: (Srv) -> 'ok' when
      Srv :: atom() | pid().
stop(Srv) when is_atom(Srv) ->
    stop(whereis(Srv));
stop(Srv) when is_pid(Srv) ->
    gen_server:cast(Srv, stop).

-spec add_responder/3 :: (Srv, Responder, Key) -> 'ok' when
      Srv :: atom() | pid(),
      Responder :: atom() | {atom(), atom()},
      Key :: {binary(), binary()} | [{binary(), binary()},...].
add_responder(Srv, Responder, Key) when not is_list(Key) ->
    add_responder(Srv, Responder, [Key]);
add_responder(Srv, Responder, [{_,_}|_] = Keys) ->
    gen_server:cast(Srv, {add_responder, Responder, Keys}).

-spec rm_responder/2 :: (Srv, Responder) -> 'ok' when
      Srv :: atom() | pid(),
      Responder :: atom() | {atom(), atom()}.
-spec rm_responder/3 :: (Srv, Responder, Key) -> 'ok' when
      Srv :: atom() | pid(),
      Responder :: atom(),
      Key :: [{binary(), binary()},...] | []. %% empty list removes all
rm_responder(Srv, Responder) ->
    rm_responder(Srv, Responder, []).
rm_responder(Srv, Responder, {_,_}=Key) ->
    rm_responder(Srv, Responder, [Key]);
rm_responder(Srv, Responder, Keys) ->
    gen_server:cast(Srv, {rm_responder, Responder, Keys}).

-spec add_binding/2 :: (Srv, Binding) -> 'ok' when
      Srv :: atom() | pid(),
      Binding :: binding().
add_binding(Srv, {Binding, Props}) ->
    gen_server:cast(Srv, {add_binding, Binding, Props}).

-spec add_binding/3 :: (Srv, Binding, Props) -> 'ok' when
      Srv :: atom() | pid(),
      Binding :: queue_bindings:bind_types(),
      Props :: proplist().
add_binding(Srv, Binding, Props) ->
    gen_server:cast(Srv, {add_binding, Binding, Props}).

-spec rm_binding/2 :: (Srv, Binding) -> 'ok' when
      Srv :: atom() | pid(),
      Binding :: atom().
rm_binding(Srv, Binding) ->
    gen_server:cast(Srv, {rm_binding, Binding}).

-spec rm_binding/3 :: (Srv, Binding, Props) -> 'ok' when
      Srv :: atom() | pid(),
      Binding :: atom(),
      Props :: proplist().
rm_binding(Srv, Binding, []) ->
    gen_server:cast(Srv, {rm_binding, Binding});
rm_binding(Srv, Binding, Props) ->
    gen_server:cast(Srv, {rm_binding, Binding, Props}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
-spec init/1 :: (Args) -> {'ok', #state{}, 'hibernate'} when
      Args :: [atom() | proplist(),...].
init([Module, Params, InitArgs]) ->
    process_flag(trap_exit, true),
    ModState = case erlang:function_exported(Module, init, 1) andalso Module:init(InitArgs) of
		   {ok, MS} ->
		       MS;
		   {ok, MS, hibernate} ->
		       MS;
		   {ok, MS, Timeout} when is_integer(Timeout) andalso Timeout > -1 ->
		       erlang:send_after(Timeout, self(), timeout),
		       MS;
		   Err ->
		       throw(Err)
	       end,

    put(callid, wh_util:to_binary(Module)), %% identify the client module for this gen_listener

    Responders = props:get_value(responders, Params, []),
    Bindings = props:get_value(bindings, Params, []),

    {ok, Q} = start_amqp(Params),

    _ = erlang:send_after(?TIMEOUT_RETRY_CONN, self(), is_consuming),

    Self = self(),
    spawn(fun() -> [add_responder(Self, Mod, Evts) || {Mod, Evts} <- Responders] end),
    spawn(fun() -> [add_binding(Self, Type, BindProps) || {Type, BindProps} <- Bindings] end),

    {ok, #state{queue=Q, module=Module, module_state=ModState
		,responders=[], bindings=Bindings
		,params=lists:keydelete(responders, 1, lists:keydelete(bindings, 1, Params))}
     ,hibernate}.

-type gen_l_handle_call_ret() :: {'reply', term(), #state{}, gen_server_timeout()} |
				 {'noreply', #state{}, gen_server_timeout()} |
				 {'stop', term(), #state{}} | {'stop', term(), term(), #state{}}.

-spec handle_call/3 :: (Request, From, State) -> gen_l_handle_call_ret() when
      Request :: term(),
      From :: {pid(), reference()},
      State :: #state{}.
handle_call(queue_name, _From, #state{queue=Q}=State) ->
    {reply, Q, State};
handle_call(responders, _From, #state{responders=Rs}=State) ->
    {reply, Rs, State};
handle_call(Request, From, #state{module=Module, module_state=ModState}=State) ->
    case catch Module:handle_call(Request, From, ModState) of
	{reply, Reply, ModState1} ->
	    {reply, Reply, State#state{module_state=ModState1}, hibernate};
	{reply, Reply, ModState1, Timeout} ->
	    {reply, Reply, State#state{module_state=ModState1}, Timeout};
	{noreply, ModState1} ->
	    {noreply, State#state{module_state=ModState1}, hibernate};
	{noreply, ModState1, Timeout} ->
	    {noreply, State#state{module_state=ModState1}, Timeout};
	{stop, Reason, ModState1} ->
	    {stop, Reason, State#state{module_state=ModState1}};
	{stop, Reason, Reply, ModState1} ->
	    {stop, Reason, Reply, State#state{module_state=ModState1}};
	{'EXIT', Why} ->
	    ?LOG("exception: ~p", [Why]),
	    {stop, Why, State}
    end.

-spec handle_cast/2 :: (Request, State) -> handle_cast_ret() when
      Request :: term(),
      State :: #state{}.
handle_cast(stop, #state{active_responders=[]}=State) ->
    {stop, normal, State};
handle_cast(stop, #state{queue = <<>>}=State) ->
    self() ! stop, % put a message in the queue to check length again
    {noreply, State, 50};
handle_cast(stop, #state{queue=Q, bindings=Bindings}=State) ->
    self() ! stop, % put a message in the queue to check length again
    stop_amqp(Q, Bindings), % make sure we're not accepting new requests
    {noreply, State#state{queue = <<>>}, 0};

handle_cast({add_responder, Responder, Keys}, #state{responders=Responders}=State) ->
    {noreply, State#state{responders=listener_utils:add_responder(Responders, Responder, Keys)}, hibernate};

handle_cast({rm_responder, Responder, Keys}, #state{responders=Responders}=State) ->
    {noreply, State#state{responders=listener_utils:rm_responder(Responders, Responder, Keys)}, hibernate};

handle_cast({add_binding, Binding, Props}=Req, #state{queue=Q}=State) ->
    ?LOG("adding binding ~s, ~p", [Binding, Props]),
    Wapi = <<"wapi_", (wh_util:to_binary(Binding))/binary>>,
    try
        ApiMod = wh_util:to_atom(Wapi),
	ApiMod:bind_q(Q, Props),
	{noreply, State}
    catch
	error:badarg ->
	    ?LOG_SYS("api module ~s not found", [Wapi]),
	    case code:where_is_file(wh_util:to_list(<<Wapi/binary, ".beam">>)) of
		non_existing ->
		    ?LOG_SYS("beam file not found for ~s, trying old method", [Wapi]),
		    queue_bindings:add_binding_to_q(Q, Binding, Props),
		    {noreply, State};
		_Path ->
		    ?LOG_SYS("beam file found: ~s", [_Path]),
		    wh_util:to_atom(Wapi, true), %% put atom into atom table
		    handle_cast(Req, State)
	    end;
	error:undef ->
	    ?LOG_SYS("Module ~s doesn't exist of bind_q/2 isn't exported", [Wapi]),
	    ?LOG_SYS("Trying old school add_binding for ~s", [Binding]),
	    queue_bindings:add_binding_to_q(Q, Binding, Props),
	    {noreply, State};
        E:R ->
            io:format("~p ~p~n", [E, R]),
            {noreply, State}
    end;

handle_cast({rm_binding, Binding}=Req, #state{queue=Q}=State) ->
    Wapi = <<"wapi_", (wh_util:to_binary(Binding))/binary>>,
    try
	ApiMod = wh_util:to_atom(Wapi),
	ApiMod:unbind_q(Q),
	{noreply, State}
    catch
	error:badarg ->
	    ?LOG_SYS("Atom ~s not found", [Wapi]),
	    case code:where_is_file(wh_util:to_list(<<Wapi/binary, ".beam">>)) of
		non_existing ->
		    {noreply, State};
		_Path ->
		    ?LOG_SYS("beam file found: ~s", [_Path]),
		    wh_util:to_atom(Wapi, true),
		    handle_cast(Req, State)
	    end;
	error:undef ->
	    ?LOG_SYS("Module ~s doesn't exist or unbind_q/1 isn't exported", [Wapi]),
	    queue_bindings:rm_binding_from_q(Q, Binding),
	    {noreply, State}
    end;

handle_cast({rm_binding, Binding, Props}=Req, #state{queue=Q}=State) ->
    Wapi = <<"wapi_", (wh_util:to_binary(Binding))/binary>>,
    try
	ApiMod = wh_util:to_atom(Wapi),
	ApiMod:unbind_q(Q, Props),
	{noreply, State}
    catch
	error:badarg ->
	    ?LOG_SYS("Atom ~s not found", [Wapi]),
	    case code:where_is_file(wh_util:to_list(<<Wapi/binary, ".beam">>)) of
		non_existing ->
		    {noreply, State};
		_Path ->
		    ?LOG_SYS("beam file found: ~s", [_Path]),
		    wh_util:to_atom(Wapi, true),
		    handle_cast(Req, State)
	    end;
	error:undef ->
	    ?LOG_SYS("Module ~s doesn't exist or unbind_q/2 isn't exported", [Wapi]),
	    queue_bindings:rm_binding_from_q(Q, Binding, Props),
	    {noreply, State}
    end;

handle_cast(Message, #state{module=Module, module_state=ModState}=State) ->
    case catch Module:handle_cast(Message, ModState) of
	{noreply, ModState1} ->
	    {noreply, State#state{module_state=ModState1}, hibernate};
	{noreply, ModState1, Timeout} ->
	    {noreply, State#state{module_state=ModState1}, Timeout};
	{stop, Reason, ModState1} ->
	    {stop, Reason, State#state{module_state=ModState1}};
	{'EXIT', Why} ->
	    ?LOG("exception: ~p", [Why]),
	    {stop, Why, State}
    end.

-spec handle_info/2 :: (Request, State) -> handle_info_ret() when
      Request :: term(),
      State :: #state{}.
handle_info({#'basic.deliver'{}, #amqp_msg{props = #'P_basic'{content_type=CT}, payload = Payload}}, #state{active_responders=ARs}=State) ->
    case catch handle_event(Payload, CT, State) of
	Pid when is_pid(Pid) ->
	    {noreply, State#state{active_responders=[Pid | ARs]}, hibernate};
	{'EXIT', Why} ->
	    ?LOG("exception: ~p", [Why]),
	    {stop, Why, State}
    end;

handle_info({'EXIT', Pid, _Reason}=Message, #state{active_responders=ARs}=State) ->
    case lists:member(Pid, ARs) of
	true -> {noreply, State#state{active_responders=lists:delete(Pid, ARs)}, hibernate};
	false -> handle_callback_info(Message, State)
    end;

handle_info({amqp_host_down, _H}=Down, #state{bindings=Bindings, params=Params}=State) ->
    ?LOG("amqp host down msg: ~p", [_H]),
    case amqp_util:is_host_available() of
	true ->
	    ?LOG("Host is available, let's try wiring up"),
	    case start_amqp(Params) of
		{ok, Q} ->
		    Self = self(),
		    _ = erlang:send_after(?TIMEOUT_RETRY_CONN, Self, is_consuming),
		    spawn(fun() -> [ add_binding(Self, Type, BindProps) || {Type, BindProps} <- Bindings ] end),
		    {noreply, State#state{queue=Q, is_consuming=false}, hibernate};
		{error, _} ->
		    ?LOG("Failed to start amqp, waiting another second"),
		    _ = erlang:send_after(?TIMEOUT_RETRY_CONN, self(), Down),
		    {noreply, State#state{queue = <<>>, is_consuming=false}, hibernate}
	    end;
	false ->
	    ?LOG("No AMQP host ready, waiting another second"),
	    erlang:send_after(?TIMEOUT_RETRY_CONN, self(), Down),
	    {noreply, State#state{queue = <<>>, is_consuming=false}, hibernate}
    end;

handle_info({amqp_lost_channel, no_connection}, State) ->
    ?LOG("Lost our channel, checking every second for a host to come back up"),
    _Ref = erlang:send_after(?TIMEOUT_RETRY_CONN, self(), {amqp_host_down, ok}),
    {noreply, State#state{queue = <<>>, is_consuming=false}, hibernate};

handle_info(#'basic.consume_ok'{}, S) ->
    {noreply, S#state{is_consuming=true}};

handle_info(is_consuming, #state{is_consuming=false, queue=Q}=State) ->
    ?LOG("WTF, we're not consuming. Queue: ~p", [Q]),
    _Ref = erlang:send_after(?TIMEOUT_RETRY_CONN, self(), {amqp_host_down, ok}),
    {noreply, State};

handle_info(is_consuming, State) ->
    {noreply, State};

handle_info(Message, State) ->
    handle_callback_info(Message, State).

handle_callback_info(Message, #state{module=Module, module_state=ModState}=State) ->
    case catch Module:handle_info(Message, ModState) of
	{noreply, ModState1} ->
	    {noreply, State#state{module_state=ModState1}, hibernate};
	{noreply, ModState1, Timeout} ->
	    {noreply, State#state{module_state=ModState1}, Timeout};
	{stop, Reason, ModState1} ->
	    {stop, Reason, State#state{module_state=ModState1}};
	{'EXIT', Why} ->
	    ?LOG("exception: ~p", [Why]),
	    {stop, Why, State}
    end.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

terminate(Reason, #state{module=Module, module_state=ModState}) ->
    ?LOG_SYS("terminating with ~p", [Reason]),
    Module:terminate(Reason, ModState),
    ok.

-spec handle_event/3 :: (Payload, ContentType, State) -> pid() when
      Payload :: binary(),
      ContentType :: binary(),
      State :: #state{}.
handle_event(Payload, <<"application/json">>, State) ->
    JObj = mochijson2:decode(Payload),
    process_req(State, JObj);
handle_event(Payload, <<"application/erlang">>, State) ->
    JObj = binary_to_term(Payload),
    process_req(State, JObj).

-spec process_req/2 :: (State, JObj) -> pid() when
      State :: #state{},
      JObj :: json_object().
process_req(#state{queue=Queue, responders=Responders, module=Module, module_state=ModState}, JObj) ->
    Props1 = case catch Module:handle_event(JObj, ModState) of
		 {reply, Props} when is_list(Props) -> [{queue, Queue} | Props];
		 {'EXIT', _Why} -> [{queue, Queue}]
	     end,
    spawn_link(fun() -> _ = wh_util:put_callid(JObj), process_req(Props1, Responders, JObj) end).

-spec process_req/3 :: (Props, Responders, JObj) -> 'ok' when
      Props :: proplist(),
      Responders :: responders(),
      JObj :: json_object().
process_req(Props, Responders, JObj) ->
    Key = wh_util:get_event_type(JObj),

    Handlers = [spawn_monitor(fun() ->
				      _ = wh_util:put_callid(JObj),
				      Responder:Fun(JObj, Props)
			      end) || {Evt, {Responder, Fun}} <- Responders,
				      maybe_event_matches_key(Key, Evt)
	       ],
    wait_for_handlers(Handlers).

%% allow wildcard (<<"*">>) in the Key to match either (or both) Category and Name
-spec maybe_event_matches_key/2 :: (Key, Event) -> boolean() when
      Key :: {binary(), binary()},
      Event :: {binary(), binary()}.
maybe_event_matches_key(Evt, Evt) -> true;
maybe_event_matches_key({_, Name}, {<<"*">>, Name}) -> true;
maybe_event_matches_key({Cat, _}, {Cat, <<"*">>}) -> true;
maybe_event_matches_key({_,_}, {<<"*">>, <<"*">>}) -> true;
maybe_event_matches_key(_A, _B) -> false.

%% Collect the spawned handlers going down so the main process_req proc doesn't end until all
%% handlers have completed (for graceful stopping).
-spec wait_for_handlers/1 :: (Handlers) -> 'ok' when
      Handlers :: [{pid(), reference()},...] | [].
wait_for_handlers([{Pid, Ref} | Hs]) ->
    receive
	{'DOWN', Ref, process, Pid, _Reason} ->
	    wait_for_handlers(Hs)
    end;
wait_for_handlers([]) -> ok.

-spec start_amqp/1 :: (proplist()) -> {'ok', binary()} | {'error', 'amqp_error'}.
start_amqp(Props) ->
    QueueProps = props:get_value(queue_options, Props, []),
    QueueName = props:get_value(queue_name, Props, <<>>),
    case catch amqp_util:new_queue(QueueName, QueueProps) of
	{error, amqp_error}=E -> ?LOG("Failed to start new queue"), E;
	{'EXIT', _Why} -> ?LOG("Exit: ~p", [_Why]), {error, amqp_error};
	Queue ->
	    ConsumeProps = props:get_value(consume_options, Props, []),

	    set_qos(props:get_value(basic_qos, Props)),
	    amqp_util:basic_consume(Queue, ConsumeProps),
	    ?LOG("Consuming on ~s", [Queue]),
	    {ok, Queue}
    end.

-spec stop_amqp/2 :: (binary(), bindings()) -> 'ok'.
stop_amqp(<<>>, _) -> ok;
stop_amqp(Q, Bindings) ->
    Self = self(),
    spawn(fun() -> [ gen_listener:rm_binding(Self, Type, Prop) || {Type, Prop} <- Bindings] end),
    amqp_util:queue_delete(Q).

-spec set_qos/1 :: ('undefined' | non_neg_integer()) -> 'ok'.
set_qos(undefined) -> ok;
set_qos(N) when is_integer(N) -> amqp_util:basic_qos(N).
