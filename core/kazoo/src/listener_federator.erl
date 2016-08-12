%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2016, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(listener_federator).

-behaviour(gen_listener).

-export([start_link/3
        ,stop/1
        ,broker/1
        ]).
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,handle_event/2, handle_event/3
        ,terminate/2
        ,code_change/3
        ]).

-include_lib("kazoo/include/kz_amqp.hrl").
-include_lib("kazoo/include/kz_types.hrl").
-include_lib("kazoo/include/kz_log.hrl").

-define(SERVER, ?MODULE).

-record(state, {parent :: pid()
               ,broker :: ne_binary()
               ,self_binary = kz_util:to_binary(pid_to_list(self())) :: ne_binary()
               ,zone :: ne_binary()
               }).
-type state() :: #state{}.

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts the server
%%--------------------------------------------------------------------
-spec start_link(pid(), ne_binary(), kz_proplist()) -> startlink_ret().
start_link(Parent, Broker, Params) ->
    gen_listener:start_link(?SERVER, Params, [Parent, Broker]).

-spec broker(server_ref()) -> ne_binary().
broker(Pid) ->
    gen_listener:call(Pid, 'get_broker').

-spec stop(server_ref()) -> 'ok'.
stop(Pid) ->
    gen_listener:call(Pid, {'stop', self()}).

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
-spec init([pid() | ne_binary()]) -> {'ok', state()}.
init([Parent, Broker]=L) ->
    lager:debug("federating listener ~p on broker ~s", L),
    kz_amqp_channel:consumer_broker(Broker),
    Zone = kz_util:to_binary(kz_amqp_connections:broker_zone(Broker)),
    {'ok', #state{parent=Parent
                 ,broker=Broker
                 ,zone=Zone
                 }}.

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
-spec handle_call(any(), any(), state()) -> handle_call_ret_state(state()).
handle_call({'stop', Parent}, _From, #state{parent=Parent}=State) ->
    {'stop', 'normal', 'ok', State};
handle_call('get_broker', _From, #state{broker=Broker}=State) ->
    {'reply', Broker, State};
handle_call(_Request, _From, State) ->
    {'noreply', State}.

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
handle_cast({'gen_listener', {'created_queue', _}}, State) ->
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
-spec handle_info(any(), state()) -> handle_info_ret_state(state()).
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

handle_event(JObj, BasicDeliver, #state{parent=Parent
                                       ,broker=Broker
                                       ,self_binary=Self
                                       ,zone=Zone
                                       }) ->
    lager:debug("relaying federated ~s event ~s from ~s to ~p with consumer pid ~p",
                [kz_api:event_category(JObj), kz_api:event_name(JObj), Zone, Parent, Self]
               ),
    RemoteServerId = <<"consumer://"
                       ,(Self)/binary, "/"
                       ,(kz_json:get_value(<<"Server-ID">>, JObj, <<>>))/binary
                     >>,
    gen_listener:federated_event(Parent
                                ,kz_json:set_values([{<<"Server-ID">>, RemoteServerId}
                                                    ,{<<"AMQP-Broker">>, Broker}
                                                    ,{<<"AMQP-Broker-Zone">>, Zone}
                                                    ], JObj)
                                ,BasicDeliver
                                ),
    'ignore'.

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
    lager:debug("listener federator terminating: ~p", [_Reason]).

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
