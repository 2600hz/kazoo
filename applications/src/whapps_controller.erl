%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2010, James Aimonetti
%%% @doc
%%%
%%% @end
%%% Created :  1 Dec 2010 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(whapps_controller).

-behaviour(gen_server).

%% API
-export([start_link/0, start_app/1, set_amqp_host/1, set_couch_host/1, stop_app/1, running_apps/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-import(logger, [format_log/3]).

-define(SERVER, ?MODULE). 

-record(state, {
	  apps=[] :: list()
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

-spec(start_app/1 :: (App :: atom()) -> ok | tuple(error, term())).
start_app(App) ->
    gen_server:cast(?MODULE, {start_app, App}).

stop_app(App) ->
    gen_server:cast(?MODULE, {stop_app, App}).

set_amqp_host(H) ->
    gen_server:cast(?MODULE, {set_amqp_host, H}).

set_couch_host(H) ->
    couch_mgr:set_host(H).

running_apps() ->
    gen_server:call(?SERVER, running_apps).

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
    {ok, #state{}, 0}. % causes a timeout immediately, which we can use to do initialization things for state

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
handle_call(running_apps, _, #state{apps=As}=S) ->
    {reply, As, S};
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
handle_cast({start_app, App}, #state{apps=As}=State) ->
    As1 = add_app(App, As),
    {noreply, State#state{apps=As1}};
handle_cast({stop_app, App}, #state{apps=As}=State) ->
    As1 = rm_app(App, As),
    {noreply, State#state{apps=As1}};
handle_cast({set_amqp_host, H}, #state{apps=As}=State) ->
    lists:foreach(fun(A) -> A:set_amqp_host(H) end, As),
    {noreply, State};
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
handle_info(timeout, State) -> handle_info(start_apps, State);
handle_info(start_apps, #state{apps=As}=State) ->
    Config = lists:concat([filename:dirname(filename:dirname(code:which(whistle_apps))), "/priv/startup.config"]),
    As1 = case file:consult(Config) of
	      {ok, Ts} ->
		  couch_mgr:set_host(props:get_value(default_couch_host, Ts, "")),
		  start_mnesia(),
		  lists:foldl(fun(App, Acc) ->
				      add_app(App, Acc)
			      end, As, props:get_value(start, Ts, []));
	      _ -> As
	  end,
    {noreply, State#state{apps=As1}};
handle_info(_Info, State) ->
    format_log(info, "WHAPPS(~p): Unhandled info ~p~n", [self(), _Info]),
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
terminate(_Reason, #state{apps=As}) ->
    format_log(info, "WHAPPS(~p): Terminating(~p)~n", [self(), _Reason]),
    lists:foreach(fun(App) -> spawn(fun() -> rm_app(App, []) end) end, As),
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
-spec(add_app/2 :: (App :: atom(), As :: list(atom())) -> list()).
add_app(App, As) ->
    format_log(info, "APPS(~p): Starting app ~p if not in ~p~n", [self(), App, As]),
    case not lists:member(App, As) andalso whistle_apps_sup:start_app(App) of
	false -> As;
	{ok, _} -> application:start(App), [App  | As];
	{ok, _, _} -> application:start(App), [App | As];
	_ -> As
    end.

-spec(rm_app/2 :: (App :: atom(), As :: list(atom())) -> list()).
rm_app(App, As) ->
    format_log(info, "APPS(~p): Stopping app ~p if in ~p~n", [self(), App, As]),
    whistle_apps_sup:stop_app(App),
    application:stop(App),
    case lists:member(App, As) of
	true -> lists:delete(App, As);
	false -> As
    end.

start_mnesia() ->
    whistle_apps_mnesia:init().
