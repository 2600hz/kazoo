%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% Listen for queue requests from callflows, starting handlers for the queue
%%% if needed.
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(fifo_listener).

-behaviour(gen_listener).

%% API
-export([start_link/0, stop/1]).

%% Process AMQP API messages
-export([handle_agent_update/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, handle_event/2
         ,terminate/2, code_change/3]).

-include("fifo.hrl").

-define(RESPONDERS, [{{?MODULE, handle_agent_update}, [{<<"queue">>, <<"agent_login">>}
                                                       ,{<<"queue">>, <<"agent_logout">>}
                                                      ]}
                    ]).
-define(BINDINGS, [{queue, []}]).

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
    gen_listener:start_link(?MODULE, [{responders, ?RESPONDERS}
                                      ,{bindings, ?BINDINGS}
                                     ], []).

-spec stop/1 :: (pid()) -> 'ok'.
stop(Srv) ->
    gen_listener:stop(Srv).

handle_agent_update(JObj, _Props) ->
    ?LOG("agent update: ~p", [JObj]).

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
    _ = spawn(fun() ->
                      [ maybe_init_fifo_queue(Acct) || Acct <- whapps_util:get_all_accounts(encoded)]
              end),
    {'ok', 'ok'}.

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
handle_info(_Info, State) ->
    {noreply, State}.

handle_event(_, _) ->
    {reply, []}.

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
-spec maybe_init_fifo_queue/1 :: (ne_binary()) -> 'ok'.
maybe_init_fifo_queue(AcctDB) ->
    case couch_mgr:get_results(AcctDB, <<"queues/crossbar_listing">>, []) of
        {ok, []} ->
            ?LOG("acct ~s has no queues to manage", [AcctDB]);
        {ok, Queues} ->
            ?LOG("acct ~s has some queues to start, doing so now", [AcctDB]),
            {ok, Cache} = fifo_sup:cache_proc(),
            _ = [maybe_start_queue(AcctDB, wh_json:get_value(<<"id">>, QJObj), Cache) || QJObj <- Queues],
            ok;
        {error, _E} ->
            ?LOG("failed to query view for ~s: ~p", [AcctDB, _E])
    end.

-spec maybe_start_queue/3 :: (ne_binary(), ne_binary(), pid()) -> 'ok'.
maybe_start_queue(AcctDB, QueueID, Cache) ->
    case fifo_util:find_fifo_queue(AcctDB, QueueID, Cache) of
        {error, not_found} ->
            {ok, Pid} = fifo_queues_sup:new(AcctDB, QueueID),
            ?LOG("queue for ~s:~s started: ~p", [AcctDB, QueueID, Pid]),
            fifo_util:store_fifo_queue(AcctDB, QueueID, Pid, Cache);
        {ok, Pid} ->
            ?LOG("queue for ~s:~s already started: ~p", [AcctDB, QueueID, Pid])
    end.
