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
-include_lib("rabbitmq_server/include/rabbit_framing.hrl").

-record(state, {}).

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
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_listener:start_link({'local', ?MODULE}
                           ,?MODULE
                           ,[{'bindings', ?BINDINGS}
                            ,{'responders', ?RESPONDERS}
                            ,{'queue_name', ?QUEUE_NAME}       % optional to include
                            ,{'queue_options', ?QUEUE_OPTIONS} % optional to include
                            ,{'consume_options', ?CONSUME_OPTIONS} % optional to include
                            ], []).

-spec handle_amqp_event(wh_json:object(), wh_proplist(), #'basic.deliver'{} | ne_binary()) -> any().
handle_amqp_event(EventJObj, Props, #'basic.deliver'{routing_key=RoutingKey}) ->
    handle_amqp_event(EventJObj, Props, RoutingKey);
handle_amqp_event(EventJObj, _Props, RoutingKey) when is_binary(RoutingKey) ->
    lager:debug("recv event ~p (~s)", [wh_util:get_event_type(EventJObj), RoutingKey]),
    blackhole_bindings:map(RoutingKey, EventJObj).

-spec add_call_binding(ne_binary()) -> 'ok'.
add_call_binding(AccountId) ->
    gen_listener:cast(?MODULE, {'add_call_binding', AccountId}).

-spec remove_call_binding(ne_binary()) -> 'ok'.
remove_call_binding(AccountId) ->
    gen_listener:cast(?MODULE, {'remove_call_binding', AccountId}).

-spec add_binding(atom(), wh_proplist()) -> 'ok'.
add_binding(Wapi, Options) ->
    gen_listener:add_binding(?MODULE, Wapi, Options).

-spec remove_binding(atom(), wh_proplist()) -> 'ok'.
remove_binding(Wapi, Options) ->
    gen_listener:rm_binding(?MODULE, Wapi, Options).

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
    wh_hooks:register(AccountId),
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
handle_info(?HOOK_EVT(_AccountId, EventType, JObj), State) ->
    spawn(?MODULE, 'handle_amqp_event', [JObj, 'ok', call_routing(EventType, JObj)]),
    {'noreply', State};
handle_info(_Info, State) ->
    {'noreply', State}.

call_routing(EventType, JObj) ->
    wapi_call:event_routing_key(EventType, wh_json:get_value(<<"Call-ID">>, JObj)).

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
