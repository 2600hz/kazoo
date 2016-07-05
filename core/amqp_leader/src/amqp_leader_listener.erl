%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(amqp_leader_listener).

-behaviour(gen_listener).

-export([is_ready/0]).
-export([start_link/1]).
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,handle_event/2
        ,terminate/2
        ,code_change/3
        ]).

-include("amqp_leader.hrl").

-define(SERVER, ?MODULE).

-record(state, {self = self()           :: pid()
               ,name                   :: atom()
               ,pending = []           :: [{pid(), any()}]
               ,has_queue = 'false' :: boolean()
               ,is_consuming = 'false' :: boolean()
               }).

-type state() :: #state{}.

%% By convention, we put the options here in macros, but not required.
-define(BINDINGS(Name), [{'leader', [{'name', Name}]}
                        ]).
-define(RESPONDERS, []).
-define(QUEUE_NAME(Name), kapi_leader:queue(Name)).
-define(QUEUE_OPTIONS, []).
-define(CONSUME_OPTIONS, []).

%%%===================================================================
%%% API
%%%===================================================================
-spec is_ready() -> 'true'.
is_ready() ->
    {'registered_name', Name} = erlang:process_info(self(), 'registered_name'),
    Sup = element(2, hd([X || X <- supervisor:which_children(amqp_leader_sup), element(1, X) =:= Name])),
    Pid = element(2, hd([X || X <- supervisor:which_children(Sup), element(1, X) =:= ?MODULE])),
    Ref = make_ref(),
    gen_listener:cast(Pid, {'is_ready', self(), Ref}),
    recv_ready(Ref).

%%--------------------------------------------------------------------
%% @doc Starts the server
%%--------------------------------------------------------------------
-spec start_link(atom()) -> startlink_ret().
start_link(Name) ->
    gen_listener:start_link(?SERVER, [{'bindings', ?BINDINGS(Name)}
                                     ,{'responders', ?RESPONDERS}
                                     ,{'queue_name', ?QUEUE_NAME(Name)}       % optional to include
                                     ,{'queue_options', ?QUEUE_OPTIONS} % optional to include
                                     ,{'consume_options', ?CONSUME_OPTIONS} % optional to include
                                      %%,{basic_qos, 1}                % only needed if prefetch controls
                                     ], [Name]).

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
-spec init([atom()]) -> {'ok', state()}.
init([Name]) ->
    kz_util:put_callid(kapi_leader:queue(Name, node())),
    kz_nodes:notify_expire(),
    {'ok', #state{self = self(), name = Name}}.

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
-spec handle_call(any(), {pid(), any()}, state()) -> handle_call_ret_state(state()).
handle_call('is_ready', From, #state{pending = Pids} = State) ->
    NewState = maybe_ready(State#state{pending = [From | Pids]}),
    {'noreply', NewState};
handle_call(_Request, _From, State) ->
    lager:warning("unhandled call ~p", [_Request]),
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
handle_cast({'is_ready', Pid, Ref}, #state{pending = Pending} = State) ->
    NewState = maybe_ready(State#state{pending = [{Pid, Ref} | Pending]}),
    {'noreply', NewState};
handle_cast({'kz_nodes', {'expire', #kz_node{node = Node}}}, #state{name = Name} = State) ->
    Name ! {'DOWN', Node},
    {'noreply', State};
handle_cast({'gen_listener', {'created_queue', _QueueNAme}}, State) ->
    lager:debug("connected"),
    NewState = maybe_ready(State#state{has_queue = 'true'}),
    {'noreply', NewState};
handle_cast({'gen_listener', {'is_consuming', _IsConsuming}}, State) ->
    lager:debug("consuming"),
    NewState = maybe_ready(State#state{is_consuming = 'true'}),
    {'noreply', NewState};
handle_cast(_Msg, State) ->
    lager:warning("unhandled cast ~p", [_Msg]),
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
-spec handle_info(any(), state()) -> handle_info_ret_state(state()).
handle_info(_Info, State) ->
    lager:warning("~s unhandled info ~p", [node(), _Info]),
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Allows listener to pass options to handlers
%%
%% @spec handle_event(JObj, State) -> {reply, Options}
%% @end
%%--------------------------------------------------------------------
-spec handle_event(kz_json:object(), state()) -> {'reply', []}.
handle_event(JObj, #state{name = Name}) ->
    kz_util:put_callid(kapi_leader:queue(Name, node())),
    NodeBin = kz_util:to_binary(node()),
    case kz_json:get_value(<<"Node">>, JObj) of
        NodeBin -> 'ok';
        _ ->
            Msg = erlang:binary_to_term(kz_util:from_hex_binary(kz_json:get_value(<<"Message">>, JObj))),
            Name ! Msg
    end,
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
-spec recv_ready(reference()) -> 'true'.
recv_ready(Ref) ->
    receive
        {Ref, 'ready'} -> ready_when_node_is_up()
    after
        1 * ?MILLISECONDS_IN_SECOND ->
            lager:debug("listener not ready yet"),
            recv_ready(Ref)
    end.

-spec ready_when_node_is_up() -> 'true'.
ready_when_node_is_up() ->
    case kz_nodes:is_up(node()) of
        'false' ->
            lager:debug("node is not up yet"),
            timer:sleep(1 * ?MILLISECONDS_IN_SECOND),
            ready_when_node_is_up();
        'true' ->
            'true'
    end.

-spec maybe_ready(state()) -> state().
maybe_ready(#state{pending = Pids
                  ,has_queue = 'true'
                  ,is_consuming = 'true'
                  } = State) ->
    _ = [gen_server:reply(Pid, 'ready') || Pid <- Pids],
    State#state{pending = []};
maybe_ready(State) ->
    case State#state.has_queue of
        'true' -> lager:debug("not consuming");
        'false' -> lager:debug("no queue")
    end,
    State.
