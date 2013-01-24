%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% 
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(wh_couch_connection).

-behaviour(gen_server).

-export([start_link/1]).
-export([config/0
         ,config/1
         ,config/2
         ,config/3
         ,config/4
         ,config/5
        ]).
-export([set_admin/2]).
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,terminate/2
         ,code_change/3
        ]).

-include_lib("whistle_couch/include/wh_couch.hrl").

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
-spec start_link/1 :: (#wh_couch_connection{}) -> startlink_ret().
start_link(#wh_couch_connection{}=Connection) ->
    gen_server:start_link(?MODULE, [Connection], []).

config() -> config(undefined).
     
config(undefined) -> 
    #wh_couch_connection{};
config(URI) ->
    config(URI, #wh_couch_connection{}).

config(URI, #wh_couch_connection{}=Connection) ->
    case http_uri:parse(URI) of
        {error, no_scheme} ->
            Connection#wh_couch_connection{host = URI};
        {ok, {_, UserInfo, Host, Port, _, _}} ->
            {Username, Password} = split_user_info(UserInfo),
            Connection#wh_couch_connection{host = Host
                                           ,port = Port
                                           ,username = Username
                                           ,password = Password}
    end;
config(Host, Port) ->
    config(Host, Port, #wh_couch_connection{}).

config(Host, Port, #wh_couch_connection{}=Connection) ->
    Connection#wh_couch_connection{host = Host
                                   ,port = wh_util:to_integer(Port)
                                  };
config(Host, User, Pass) ->
    config(Host, User, Pass, #wh_couch_connection{}).

config(Host, User, Pass, #wh_couch_connection{}=Connection) ->
    Connection#wh_couch_connection{host = Host
                                   ,username = User
                                   ,password = Pass
                                  };
config(Host, Port, User, Pass) ->    
    config(Host, Port, User, Pass, #wh_couch_connection{}).

config(Host, Port, User, Pass, #wh_couch_connection{}=Connection) ->
    Connection#wh_couch_connection{host = Host
                                   ,port = wh_util:to_integer(Port)
                                   ,username = User
                                   ,password = Pass
                                  }.

set_admin(IsAdmin, #wh_couch_connection{}=Connection) ->
    Connection#wh_couch_connection{admin=wh_util:is_true(IsAdmin)}.
    
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
init([Connection]) ->
    self() ! maintain_connection,
    {ok, Connection}.

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
handle_call(_Request, _From, Connection) ->
    {reply, {error, not_implemented}, Connection}.

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
handle_cast(_Msg, Connection) ->
    {noreply, Connection}.

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
handle_info(maintain_connection, #wh_couch_connection{connection = undefined}=Connection) ->
    case maybe_reconnect(Connection) of
        {error, _} ->
            erlang:send_after(1000, self(), maintain_connection),
            {noreply, Connection};
        {ok, C} ->
            self() ! maintain_connection,
            {noreply, C}
    end;
handle_info(maintain_connection, #wh_couch_connection{ready = false
                                                      ,connection = Conn
                                                     }=Connection) ->
    C = case couch_util:server_info(Conn) of
            {ok, _} ->
                wh_couch_connections:update(Connection#wh_couch_connection{ready = true}),
                Connection#wh_couch_connection{ready = true};
            _Else ->
                Connection
        end,
    erlang:send_after(1000, self(), maintain_connection),
    {noreply, C};
handle_info(maintain_connection, #wh_couch_connection{connection = Conn}=Connection) ->
    case couch_util:server_info(Conn) of
        {ok, _} -> 
            erlang:send_after(5000, self(), maintain_connection),
            {noreply, Connection};
        _Else ->
            C = Connection#wh_couch_connection{ready = false, connection = undefined},
            wh_couch_connections:update(C),
            self() ! maintain_connection,
            {noreply, C}
    end;
handle_info(_Info, Connection) ->
    {noreply, Connection}.

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
terminate(_Reason, _Connection) ->
    lager:debug("couch connection terminating: ~p", [_Reason]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, Connection, _Extra) ->
    {ok, Connection}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec maybe_reconnect/1 :: (#wh_couch_connection{}) -> {'ok', #wh_couch_connection{}} | {'error', _}.
maybe_reconnect(#wh_couch_connection{host = Host, port = Port
                                     ,username = User, password = Pass}=Connection) ->
    try couch_util:get_new_connection(Host, Port, User, Pass) of
        Conn -> {ok, Connection#wh_couch_connection{connection = Conn}}
    catch
        error:{badmatch,{error,{conn_failed,{error,econnrefused}}}} ->
            lager:info("connection to BigCouch at ~s:~p refused. Is BigCouch/HAProxy running at these host:port combos?", [Host, Port]),
            {error, conn_failed};
        error:{badmatch,{error,{ok,Status,_,_}}} ->
            lager:info("received HTTP error ~p from BigCouch/HAProxy at ~s:~p.", [Status, Host, Port]),
            {error, bad_status};
        _:Reason ->
            ST = erlang:get_stacktrace(),
            lager:info("failed to connect to BigCouch/HAProxy at ~s:~p: ~p", [Host, Port, Reason]),
            _ = [lager:info("st: ~p", [S]) || S <- ST],
            {error, Reason}
    end.    

split_user_info([]) -> {"", ""};
split_user_info(UserInfo) ->
    case string:tokens(UserInfo, ":") of
        [Username] -> {Username, ""};
        [Username, Password] -> {Username, Password}
    end.
