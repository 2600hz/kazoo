%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(blackhole_listener).

-behaviour(gen_listener).

-export([start_link/0
        ,handle_amqp_event/3
        ,add_call_binding/1, remove_call_binding/1
        ,add_binding/2, remove_binding/2
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

-record(state, {}).
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
    lager:debug("recv event ~p (~s)", [kz_util:get_event_type(EventJObj), RoutingKey]),
    blackhole_bindings:map(RoutingKey, EventJObj).

-spec add_call_binding(ne_binary()) -> 'ok'.
add_call_binding(AccountId) ->
    gen_listener:cast(?SERVER, {'add_call_binding', AccountId}).

-spec remove_call_binding(ne_binary()) -> 'ok'.
remove_call_binding(AccountId) ->
    gen_listener:cast(?SERVER, {'remove_call_binding', AccountId}).

-spec add_binding(atom(), kz_proplist()) -> 'ok'.
add_binding(Wapi, Options) ->
    gen_listener:add_binding(?SERVER, Wapi, Options).

-spec remove_binding(atom(), kz_proplist()) -> 'ok'.
remove_binding(Wapi, Options) ->
    gen_listener:rm_binding(?SERVER, Wapi, Options).

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
    {'ok', #state{}}.

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
handle_cast({'add_call_binding', AccountId}, State) ->
    kz_hooks:register(AccountId),
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
handle_info(?HOOK_EVT(_AccountId, EventType, JObj), State) ->
    _ = kz_util:spawn(fun handle_amqp_event/3, [JObj, [], call_routing(EventType, JObj)]),
    {'noreply', State};
handle_info(_Info, State) ->
    {'noreply', State}.

-spec call_routing(ne_binary(), kz_json:object()) -> ne_binary().
call_routing(EventType, JObj) ->
    kapi_call:event_routing_key(EventType, kz_json:get_value(<<"Call-ID">>, JObj)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Allows listener to pass options to handlers
%%
%% @spec handle_event(JObj, State) -> {reply, Options}
%% @end
%%--------------------------------------------------------------------
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
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
