%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(blackhole_listener).
-behaviour(gen_listener).

-export([start_link/0
        ,handle_amqp_event/3
        ,add_binding/1, remove_binding/1
        ,add_bindings/1, remove_bindings/1
        ]).
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,handle_event/2
        ,terminate/2
        ,code_change/3
        ]).

-include("blackhole.hrl").

-define(SERVER, ?MODULE).

-record(state, {bindings :: ets:tid()}).
-type state() :: #state{}.

%% By convention, we put the options here in macros, but not required.
-define(BINDINGS, []).
-define(RESPONDERS, [{{?MODULE, 'handle_amqp_event'}
                     ,[{<<"*">>, <<"*">>}]
                     }
                    ]).
-define(QUEUE_NAME, <<>>).
-define(QUEUE_OPTIONS, []).
-define(CONSUME_OPTIONS, []).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts the server
%%--------------------------------------------------------------------
-spec start_link() -> startlink_ret().
start_link() ->
    gen_listener:start_link({'local', ?SERVER}
                           ,?MODULE
                           ,[{'bindings', ?BINDINGS}
                            ,{'responders', ?RESPONDERS}
                            ,{'queue_name', ?QUEUE_NAME}       % optional to include
                            ,{'queue_options', ?QUEUE_OPTIONS} % optional to include
                            ,{'consume_options', ?CONSUME_OPTIONS} % optional to include
                            ], []).

-spec handle_amqp_event(kz_json:object(), kz_proplist(), gen_listener:basic_deliver() | ne_binary()) -> any().
handle_amqp_event(EventJObj, Props, #'basic.deliver'{routing_key=RoutingKey}) ->
    handle_amqp_event(EventJObj, Props, RoutingKey);
handle_amqp_event(EventJObj, _Props, <<_/binary>> = RoutingKey) ->
    Evt = kz_util:get_event_type(EventJObj),
    lager:debug("recv event ~p (~s)", [Evt, RoutingKey]),
    RK = <<"blackhole.event.", RoutingKey/binary, ".*">>,
    Res = blackhole_bindings:map(RK, [RoutingKey, EventJObj]),
    lager:debug("delivered the event ~p (~s) to ~b subscriptions", [Evt, RoutingKey, length(Res)]).

-type bh_amqp_binding() :: {'amqp', atom(), kz_proplist()}.
-type bh_hook_binding() :: {'hook', ne_binary()} | {'hook', ne_binary(), ne_binary()}.
-type bh_event_binding() :: bh_amqp_binding() | bh_hook_binding().
-type bh_event_bindings() :: [bh_event_binding()].

-spec add_binding(bh_event_binding()) -> 'ok'.
add_binding(Binding) ->
    gen_listener:cast(?SERVER, {'add_bh_binding', Binding}).

-spec add_bindings(bh_event_bindings()) -> 'ok'.
add_bindings(Bindings) ->
    gen_listener:cast(?SERVER, {'add_bh_bindings', Bindings}).

-spec remove_binding(bh_event_binding()) -> 'ok'.
remove_binding(Binding) ->
    gen_listener:cast(?SERVER, {'remove_bh_binding', Binding}).

-spec remove_bindings(bh_event_bindings()) -> 'ok'.
remove_bindings(Bindings) ->
    gen_listener:cast(?SERVER, {'remove_bh_bindings', Bindings}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {'ok', #state{bindings=ets:new(bindings, [])}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec handle_call(any(), pid_ref(), state()) -> handle_call_ret_state(state()).
handle_call(_Request, _From, State) ->
    {'reply', {'error', 'not_implemented'}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(any(), state()) -> handle_cast_ret_state(state()).
handle_cast({'add_bh_bindings', Bindings}, #state{bindings=ETS}=State) ->
    _ = add_bh_bindings(ETS, Bindings),
    {'noreply', State};
handle_cast({'add_bh_binding', Binding}, #state{bindings=ETS}=State) ->
    _ = add_bh_binding(ETS, Binding),
    {'noreply', State};
handle_cast({'remove_bh_bindings', Bindings}, #state{bindings=ETS}=State) ->
    _ = remove_bh_bindings(ETS, Bindings),
    {'noreply', State};
handle_cast({'remove_bh_binding', Binding}, #state{bindings=ETS}=State) ->
    _ = remove_bh_binding(ETS, Binding),
    {'noreply', State};
handle_cast({'gen_listener', {'created_queue', _QueueNAme}}, State) ->
    {'noreply', State};
handle_cast({'gen_listener', {'is_consuming', _IsConsuming}}, State) ->
    {'noreply', State};
handle_cast(_Msg, State) ->
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec handle_info(?HOOK_EVT(ne_binary(), ne_binary(), kz_json:object()), state()) ->
                         {'noreply', state()}.
handle_info(?HOOK_EVT(AccountId, EventType, JObj), State) ->
    _ = kz_util:spawn(fun handle_hook_event/3, [AccountId, EventType, JObj]),
    {'noreply', State};
handle_info(_Info, State) ->
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Allows listener to pass options to handlers
%%
%% @spec handle_event(JObj, State) -> {reply, Options}
%% @end
%%--------------------------------------------------------------------
-spec handle_event(kz_json:object(), kz_proplist()) -> handle_event_ret().
handle_event(_JObj, _State) ->
    {'reply', []}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, _State) ->
    lager:debug("listener terminating: ~p", [_Reason]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec encode_call_id(kz_json:object()) -> ne_binary().
encode_call_id(JObj) ->
    amqp_util:encode(kz_call_event:call_id(JObj)).

-spec handle_hook_event(ne_binary(), ne_binary(), kz_json:object()) -> any().
handle_hook_event(AccountId, EventType, JObj) ->
    RK = kz_util:join_binary([<<"call">>
                             ,AccountId
                             ,EventType
                             ,encode_call_id(JObj)
                             ], <<".">>),
    handle_amqp_event(JObj, [], RK).

binding_key(Binding) -> base64:encode(term_to_binary(Binding)).

add_bh_binding(ETS, Binding) ->
    Key = binding_key(Binding),
    case ets:update_counter(ETS, Key, 1, {Key, 0}) of
        1 -> add_bh_binding(Binding);
        _ -> 'ok'
    end.

remove_bh_binding(ETS, Binding) ->
    Key = binding_key(Binding),
    case ets:update_counter(ETS, Key, -1, {Key, 0}) of
        0 -> remove_bh_binding(Binding),
             ets:delete(ETS, Key);
        _ -> 'ok'
    end.

add_bh_binding({'hook', AccountId}) ->
    kz_hooks:register(AccountId);
add_bh_binding({'hook', AccountId, Event}) ->
    kz_hooks:register(AccountId, Event);
add_bh_binding({'amqp', Wapi, Options}) ->
    gen_listener:add_binding(self(), Wapi, Options).

remove_bh_binding({'hook', AccountId}) ->
    kz_hooks:deregister(AccountId);
remove_bh_binding({'hook', AccountId, Event}) ->
    kz_hooks:deregister(AccountId, Event);
remove_bh_binding({'amqp', Wapi, Options}) ->
    gen_listener:rm_binding(self(), Wapi, Options).

add_bh_bindings(ETS, Bindings) ->
    [add_bh_binding(ETS, Binding) || Binding <- Bindings].

remove_bh_bindings(ETS, Bindings) ->
    [remove_bh_binding(ETS, Binding) || Binding <- Bindings].
