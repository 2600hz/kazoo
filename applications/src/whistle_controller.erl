%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2010, James Aimonetti
%%% @doc
%%%
%%% @end
%%% Created :  1 Dec 2010 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(whistle_controller).

-behaviour(gen_server).

%% API
-export([start_link/0, start_app/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

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
    gen_server:call(?MODULE, {start_app, App}, infinity).

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
    timer:send_after(1000, ?MODULE, start_apps),
    {ok, #state{}}.

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
handle_call({start_app, App}, _From, #state{apps=As}=State) ->
    {Resp, As1} = add_app(App, As),
    {reply, Resp, State#state{apps=As1}};
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
handle_info(start_apps, #state{apps=As}=State) ->
    Config = lists:concat([filename:dirname(filename:dirname(code:which(whistle_apps))), "/priv/startup.config"]),
    As1 = case file:consult(Config) of
	      {ok, Ts} ->
		  lists:foldl(fun(App, Acc) ->
				      {_, Acc1} = add_app(App, Acc),
				      Acc1
			      end, As, proplists:get_value(start, Ts, []));
	      _ -> As
	  end,
    {noreply, State#state{apps=As1}};
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
-spec(add_app/2 :: (App :: atom(), As :: list(atom())) -> tuple(ok, list(atom())) | tuple(tuple(), list(atom()))).
add_app(App, As) ->
    case not lists:member(App, As) andalso whistle_apps_sup:start_app(App) of
	false -> {ok, As};
	{ok, _} -> application:start(App), {ok, [App  | As]};
	{ok, _, _} -> application:start(App), {ok, [App | As]};
	E -> {E, As}
    end.
