%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(wh_couch_bootstrap).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,terminate/2
         ,code_change/3
        ]).

-include_lib("whistle_couch/include/wh_couch.hrl").

-define(SERVER, ?MODULE).
-record(state, {}).

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
    put(callid, ?LOG_SYSTEM_ID),
    {ok, Config} = get_config(),
    wh_couch_connections:add(Config),
    lager:info("waiting for first bigcouch/haproxy connection...", []),
    wh_couch_connections:wait_for_connection(),
    {ok, #state{}, 100}.

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
    {reply, {error, not_implemented}, State}.

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
handle_info(timeout, State) ->
%%    _ = wh_couch_sup:stop_bootstrap(),
    {noreply, State};
handle_info(_Info, State) ->
    lager:debug("unhandled message: ~p", [_Info]),
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
    lager:debug("couch bootstrap terminating: ~p", [_Reason]).

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
-spec get_config/0 :: () -> wh_proplist().
get_config() ->
    case file:consult(?CONFIG_FILE_PATH) of
        {ok, Startup} ->
            lager:info("successfully loaded couch config ~s", [?CONFIG_FILE_PATH]),
            DefaultHost = props:get_value(default_couch_host, Startup),
            CouchHost = props:get_value(couch_host, Startup),
            {ok, lists:foldr(fun(Props, C) -> import_config(Props, C) end
                             ,#wh_couch_connection{}
                             ,[DefaultHost, CouchHost])};
        {error, enoent}=E ->
            lager:error("couch config ~s is missing", [?CONFIG_FILE_PATH]),
            E;
        {error, _E}=E ->
            lager:error("failed to load couch config ~s: ~p", [?CONFIG_FILE_PATH, _E]),
            E
    end.

import_config({Host}, Connection) ->
    wh_couch_connection:config(Host, Connection);
import_config({Host, Port}, Connection) ->
    wh_couch_connection:config(Host, Port, Connection);
import_config({Host, User, Pass}, Connection) ->
    wh_couch_connection:config(Host, User, Pass, Connection);
import_config({Host, Port, User, Pass}, Connection) ->
    wh_couch_connection:config(Host, Port, User, Pass, Connection);
import_config({Host, Port, User, Pass, AdminPort}, Connection) ->
    Connection;
import_config(undefined, Connection) -> Connection.
