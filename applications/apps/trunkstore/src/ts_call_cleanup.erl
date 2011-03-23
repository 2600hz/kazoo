%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, James Aimonetti
%%% @doc
%%% Unfortunately, trunks get out of sync. This server queries all
%%% active calls and shuts them down if the calls are no longer active
%%% @end
%%% Created : 15 Feb 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(ts_call_cleanup).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-import(logger, [format_log/3]).

-define(SERVER, ?MODULE). 

-record(state, {amqp_h = <<>> :: binary()
		,amqp_q = <<>> :: binary()
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
    {ok, #state{}, 0}.

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
    Reply = ok,
    {reply, Reply, State}.

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
handle_info(timeout, #state{amqp_q=Q}=S) ->
    H = whapps_controller:get_amqp_host(),
    {noreply, S#state{amqp_h=H}};
handle_info({amqp_host_down, H}, S) ->
    format_log(info, "TS_CALL(~p): Amqp Host ~s went down, restarting amqp in a bit~n", [self(), H]),
    {noreply, S, 0};
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
-spec(get_amqp_queue/2 :: (AmqpHost :: string(), CallID :: binary()) -> binary()).
get_amqp_queue(AmqpHost, CallID) ->
    get_amqp_queue(AmqpHost, CallID, <<>>).

get_amqp_queue(AmqpHost, CallID, Q) ->
    EvtQ = amqp_util_old:new_callevt_queue(AmqpHost, <<>>),
    format_log(info, "TS_CLEANUP(~p): Listening on Q: ~p for call events relating to ~p~n", [self(), EvtQ, CallID]),

    amqp_util_old:bind_q_to_callevt(AmqpHost, EvtQ, CallID, events),
    amqp_util_old:bind_q_to_callevt(AmqpHost, EvtQ, CallID, cdr),
    amqp_util_old:bind_q_to_targeted(AmqpHost, EvtQ),

    amqp_util_old:basic_consume(AmqpHost, EvtQ),
    amqp_util_old:delete_queue(AmqpHost, Q),
    EvtQ.
