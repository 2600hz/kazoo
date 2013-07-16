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

-include("wh_couch.hrl").

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
-spec start_link(couch_connection()) -> startlink_ret().
start_link(#wh_couch_connection{}=Connection) ->
    gen_server:start_link(?MODULE, [Connection], []).

-spec config() -> couch_connection().
config() -> config('undefined').

-spec config('undefined' | string()) -> couch_connection().
config('undefined') -> #wh_couch_connection{};
config(URI) -> config(URI, #wh_couch_connection{}).

-spec config(string(), integer() | string() | couch_connection()) ->
                    couch_connection().
config(URI, #wh_couch_connection{}=Connection) ->
    case http_uri:parse(URI) of
        {'error', 'no_scheme'} ->
            Connection#wh_couch_connection{host = URI};
        {'error', {'not_supported_scheme', _Scheme}=E} ->
            lager:debug("unsupported scheme: ~p", [_Scheme]),
            throw(E);
        {'error', {'malformed_url', _Abs}} ->
            lager:error("malformed url: ~s", [_Abs]),
            throw({'error', 'malformed_url'});
        {'error', {'malformed_url', _Scheme, _Uri}} ->
            lager:error("malformed url: ~s", [_Uri]),
            throw({'error', 'malformed_url'});
        {'error', {Reason, _Scheme, _Uri}} ->
            lager:error("error parsing uri ~s: ~p", [_Uri, Reason]),
            throw({'error', Reason});
        {'ok', {_, UserInfo, Host, Port, _, _}} ->
            update_connection(Connection, Host, Port, UserInfo);
        {'http', UserInfo, H, P, _Path, _Q} ->
            update_connection(Connection, H, P, UserInfo);
        {'https', UserInfo, H, P, _Path, _Q} ->
            update_connection(Connection, H, P, UserInfo)
    end;
config(Host, Port) -> config(Host, Port, #wh_couch_connection{}).

update_connection(Connection, Host, Port, UserInfo) ->
    {Username, Password} = split_user_info(UserInfo),
    Connection#wh_couch_connection{host = Host
                                   ,port = Port
                                   ,username = Username
                                   ,password = Password
                                  }.

-spec config(string(), integer() | string(), couch_connection() | string()) ->
                    couch_connection().
config(Host, Port, #wh_couch_connection{}=Connection) ->
    Connection#wh_couch_connection{host = Host
                                   ,port = wh_util:to_integer(Port)
                                  };
config(Host, User, Pass) ->
    config(Host, User, Pass, #wh_couch_connection{}).

-spec config(string(), integer() | string(), string(), couch_connection() | string()) -> couch_connection().
config(Host, User, Pass, #wh_couch_connection{}=Connection) ->
    Connection#wh_couch_connection{host = Host
                                   ,username = User
                                   ,password = Pass
                                  };
config(Host, Port, User, Pass) ->
    config(Host, Port, User, Pass, #wh_couch_connection{}).

-spec config(string(), integer(), string(), string(), couch_connection()) ->
                    couch_connection().
config(Host, Port, User, Pass, #wh_couch_connection{}=Connection) ->
    Connection#wh_couch_connection{host = Host
                                   ,port = wh_util:to_integer(Port)
                                   ,username = User
                                   ,password = Pass
                                  }.

-spec set_admin(boolean(), couch_connection()) -> couch_connection().
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
handle_info(maintain_connection, #wh_couch_connection{connected = false}=Connection) ->
    case maybe_reconnect(Connection) of
        {error, _} ->
            erlang:send_after(1000, self(), maintain_connection),
            {noreply, Connection};
        {ok, C} ->
            self() ! maintain_connection,
            {noreply, connection_established(C)}
    end;
handle_info(maintain_connection, #wh_couch_connection{ready = Ready
                                                      ,server = Server
                                                     }=Connection) ->
    case couch_util:server_info(Server) of
        {ok, _} when not Ready ->
            erlang:send_after(5000, self(), maintain_connection),
            {noreply, connection_ready(Connection)};
        {ok, _} ->
            erlang:send_after(5000, self(), maintain_connection),
            {noreply, Connection};
        _Else ->
            erlang:send_after(1000, self(), maintain_connection),
            {noreply, reset_connection(Connection)}
    end;
handle_info(_Info, Connection) ->
    lager:debug("unhandled message: ~p", [_Info]),
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
    {'ok', Connection}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec maybe_reconnect(couch_connection()) ->
                             {'ok', couch_connection()} |
                             {'error', _}.
maybe_reconnect(#wh_couch_connection{host=Host
                                     ,port=Port
                                     ,username=User
                                     ,password=Pass
                                    }=Connection) ->
    try couch_util:get_new_connection(Host, Port, User, Pass) of
        #server{}=Server ->
            {'ok', Connection#wh_couch_connection{server = Server}}
    catch
        'error':{'badmatch',{'error',{'conn_failed',{'error','econnrefused'}}}} ->
            lager:info("connection to BigCouch at ~s:~p refused. Is BigCouch/HAProxy running at these host:port combos?", [Host, Port]),
            {'error', 'conn_failed'};
        'error':{'badmatch',{'error',{'conn_failed',{'error','timeout'}}}} ->
            lager:info("network error connecting to BigCouch at ~s:~p. Is BigCouch/HAProxy running at these host:port combos?", [Host, Port]),
            {'error', 'conn_failed'};
        'error':{'badmatch',{'error',{'ok',"401",_,_}}} ->
            lager:info("401 unauthorized using username ~s to authenticate to ~s: ~p", [User, Host, Connection]),
            {'error', 'bad_status'};
        'error':{'badmatch',{'error',{'ok',Status,_,_}}} ->
            lager:info("received HTTP error ~p from BigCouch/HAProxy at ~s:~p.", [Status, Host, Port]),
            {'error', 'bad_status'};
        _:Reason ->
            ST = erlang:get_stacktrace(),
            lager:info("failed to connect to BigCouch/HAProxy at ~s:~p: ~p", [Host, Port, Reason]),
            _ = [lager:debug("st: ~p", [S]) || S <- ST],
            {'error', Reason}
    end.

-spec split_user_info(string()) -> {string(), string()}.
split_user_info([]) -> {"", ""};
split_user_info(UserInfo) ->
    case string:tokens(UserInfo, ":") of
        [Username] -> {Username, ""};
        [Username, Password] -> {Username, Password}
    end.

-spec connection_established(couch_connection()) -> couch_connection().
connection_established(Connection) ->
    Connection#wh_couch_connection{connected = true}.

-spec connection_ready(couch_connection()) -> couch_connection().
connection_ready(Connection) ->
    C = Connection#wh_couch_connection{ready = true},
    wh_couch_connections:update(C),
    C.

-spec reset_connection(couch_connection()) -> couch_connection().
reset_connection(Connection) ->
    C = Connection#wh_couch_connection{connected = false, ready = false
                                       ,server = #server{}},
    %% TODO: this is disabled for the moment to maintain backward
    %% compatablity with couch_mgr which always assumed the connection
    %% was available
%%    wh_couch_connections:update(C),
    C.
