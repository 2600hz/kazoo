%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.com>
%%% @copyright (C) 2010, Karl Anderson
%%% @doc
%%% Responsible for managing the monitoring application
%%% @end
%%% Created : 11 Nov 2010 by Karl Anderson <karl@2600hz.com>
%%%-------------------------------------------------------------------
-module(monitor_master).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([set_amqp_host/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
     terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-import(logger, [format_log/3]).
-import(proplists, [get_value/2, get_value/3]).

-include("../include/monitor_amqp.hrl").

-record(state, {
        amqp_host = "" :: string()
        ,monitor_q = <<>> :: binary()
    }).

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
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

set_amqp_host(AHost) ->
    gen_server:call(?SERVER, {set_amqp_host, AHost}, infinity).

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
    {ok, []}.

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
handle_call({set_amqp_host, AHost}, _From, #state{amqp_host=""}=State) ->
    format_log(info, "MONITOR_MASTER(~p): Setting amqp host to ~p~n", [self(), AHost]),
    {ok, Monitor_Q} = start_amqp(AHost),
    {reply, ok, State#state{amqp_host=AHost, monitor_q=Monitor_Q}};
handle_call({set_amqp_host, AHost}, _From, #state{amqp_host=CurrentAHost, monitor_q=CurrentMonitorQ}=State) ->
    format_log(info, "MONITOR_MASTER(~p): Updating amqp host from ~p to ~p~n", [self(), CurrentAHost, AHost]),
    amqp_util:queue_delete(CurrentAHost, CurrentMonitorQ),
    amqp_manager:close_channel(self(), CurrentAHost),
    {ok, Monitor_Q} = start_amqp(AHost),
    {reply, ok, State#state{amqp_host=AHost, monitor_q=Monitor_Q}};
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

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
handle_cast(_Msg, State) ->
    {noreply, State}.

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
handle_info(_Info, State) ->
    {noreply, State}.

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
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
start_amqp(AHost) ->
    amqp_util:monitor_exchange(AHost),
    amqp_util:targeted_exchange(AHost),

    Monitor_Q = amqp_util:new_monitor_queue(AHost),

    %% Bind the queue to the targeted exchange
    format_log(info, "MONITOR_MASTER(~p): Bind ~p as a targeted queue~n", [self(), Monitor_Q]),
    amqp_util:bind_q_to_targeted(AHost, Monitor_Q),

    %% Bind the queue to an exchange
    format_log(info, "MONITOR_MASTER(~p): Bind ~p for ~p~n", [self(), Monitor_Q, ?KEY_MONITOR_MASTER_REQ]),
    amqp_util:bind_q_to_monitor(AHost, Monitor_Q, ?KEY_MONITOR_MASTER_REQ),

    %% Register a consumer to listen to the queue
    format_log(info, "MONITOR_MASTER(~p): Consume on ~p~n", [self(), Monitor_Q]),
    amqp_util:basic_consume(AHost, Monitor_Q),

    {ok, Monitor_Q}.
