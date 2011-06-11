%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2010-2011, VoIP INC
%%% @doc
%%%
%%% @end
%%% Created :  1 Dec 2010 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(whapps_controller).

-behaviour(gen_server).

%% API
-export([start_link/0, start_app/1, set_amqp_host/1, set_couch_host/1, set_couch_host/3, stop_app/1, running_apps/0]).
-export([get_amqp_host/0, restart_app/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 
-define(MILLISECS_PER_DAY, 1000 * 60 * 60 * 24).
-define(STARTUP_FILE, [code:lib_dir(whistle_apps, priv), "/startup.config"]).

-include_lib("whistle/include/wh_log.hrl").

-record(state, {
          apps = [] :: list()
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

-spec(start_app/1 :: (App :: atom()) -> ok).
start_app(App) when is_atom(App) ->
    gen_server:cast(?MODULE, {start_app, App}).

stop_app(App) when is_atom(App) ->
    gen_server:cast(?MODULE, {stop_app, App}).

restart_app(App) when is_atom(App) ->
    gen_server:cast(?MODULE, {restart_app, App}).

set_amqp_host(H) ->
    amqp_manager:set_host(H).

get_amqp_host() ->
    amqp_manager:get_host().

set_couch_host(H) ->
    set_couch_host(H, "", "").
set_couch_host(H, U, P) ->
    couch_mgr:set_host(whistle_util:to_list(H), whistle_util:to_list(U), whistle_util:to_list(P)).

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
    ?LOG_SYS("starting whapps controller"),    
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
    {reply, ok, State}.

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
    add_app(App, As),
    {noreply, State};
handle_cast({stop_app, App}, #state{apps=As}=State) ->
    As1 = rm_app(App, As),
    {noreply, State#state{apps=As1}};
handle_cast({restart_app, App}, #state{apps=As}=State) ->
    As1 = rm_app(App, As),
    whistle_util:reload_app(App),
    add_app(App, As1),
    {noreply, State#state{apps=As1}};
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
handle_info(timeout, State) ->
    spawn(fun() ->                  
                  ?LOG_SYS("loaded whistle controller configuration from ~s", [?STARTUP_FILE]),                  
                  {ok, Startup} = file:consult(?STARTUP_FILE),
                  WhApps = props:get_value(whapps, Startup, []),
                  lists:foreach(fun(WhApp) -> start_app(WhApp) end, WhApps)
          end),
    {noreply, State};
handle_info({add_successful_app, undefined}, State) ->
    {noreply, State};
handle_info({add_successful_app, A}, State) ->
    {noreply, State#state{apps=[A | State#state.apps]}};
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
terminate(_Reason, #state{apps=As}) ->
    lists:foreach(fun(App) -> spawn(fun() -> rm_app(App, []) end) end, As),
    ?LOG_SYS("whapps controller ~p termination", [_Reason]),
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
-spec(add_app/2 :: (App :: atom(), As :: list(atom())) -> no_return()).
add_app(App, As) ->
    Srv = self(),
    spawn(fun() ->
                  ?LOG_SYS("starting whapp ~s", [App]),
		  A = case (not lists:member(App, As)) andalso whistle_apps_sup:start_app(App) of
			  false -> ?LOG_SYS("whapp ~s is already running", [App]), undefined;
			  {ok, _} -> _ = application:start(App),  App;
			  {ok, _, _} -> _ = application:start(App), App;
			  _E -> ?LOG_SYS("failed to start whapp ~s ~p", [App, _E]), undefined
		      end,
		  Srv ! {add_successful_app, A}
	  end).
		  

-spec(rm_app/2 :: (App :: atom(), As :: list(atom())) -> list()).
rm_app(App, As) ->
    ?LOG_SYS("stopping whapp ~s", [App]),
    whistle_apps_sup:stop_app(App),
    application:stop(App),
    lists:delete(App, As).
