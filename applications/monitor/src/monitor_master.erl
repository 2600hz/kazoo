%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.com>
%%% @copyright (C) 2010, Karl Anderson
%%% @doc
%%% Responsible for managing the whistle monitoring application
%%% @end
%%% Created : 11 Nov 2010 by Karl Anderson <karl@2600hz.com>
%%%-------------------------------------------------------------------
-module(monitor_master).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
     terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-import(logger, [format_log/3]).
-import(proplists, [get_value/2, get_value/3]).

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
handle_call(_Request, _From, State) ->
    {reply, {error, unsupported}, State}.

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
-spec(start_amqp/1 :: (AHost :: string()) -> tuple(ok, binary(), binary())).
start_amqp(AHost) ->
    CallmgrName = ["monitor_responder.callmgr.", net_adm:localhost()],
    TarName = ["ts_responder.callctl.", net_adm:localhost()],

    amqp_util:callmgr_exchange(AHost),
    amqp_util:targeted_exchange(AHost),

    CallmgrQueue = amqp_util:new_callmgr_queue(AHost, CallmgrName),
    TarQueue = amqp_util:new_targeted_queue(AHost, TarName),

    %% Bind the queue to an exchange
    amqp_util:bind_q_to_callmgr(AHost, CallmgrQueue, ?KEY_AUTH_REQ),
    amqp_util:bind_q_to_callmgr(AHost, CallmgrQueue, ?KEY_ROUTE_REQ),
    format_log(info, "TS_RESPONDER(~p): Bound ~p for ~p and ~p~n", [self(), CallmgrQueue, ?KEY_AUTH_REQ, ?KEY_ROUTE_REQ]),
    amqp_util:bind_q_to_targeted(AHost, TarQueue, TarQueue),

    %% Register a consumer to listen to the queue
    amqp_util:basic_consume(AHost, CallmgrQueue),
    amqp_util:basic_consume(AHost, TarQueue),

    format_log(info, "TS_RESPONDER(~p): Consuming on B(~p) and T(~p)~n", [self(), CallmgrQueue, TarQueue]),
    {ok, CallmgrQueue, TarQueue}.
