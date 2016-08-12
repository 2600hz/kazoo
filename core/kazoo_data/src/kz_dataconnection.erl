%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2016, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(kz_dataconnection).

-behaviour(gen_server).

-export([start_link/1]).

-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3
        ]).

-include("kz_data.hrl").

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts the server
%%--------------------------------------------------------------------
-spec start_link(data_connection()) -> startlink_ret().
start_link(#data_connection{}=Connection) ->
    gen_server:start_link(?SERVER, [Connection], []).

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
    lager:info("start connection"),
    self() ! 'maintain_connection',
    {'ok', Connection}.

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
    {'reply', {'error', 'not_implemented'}, Connection}.

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
    {'noreply', Connection}.

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
-spec handle_info(any(), state()) -> handle_info_ret_state(state()).
handle_info('maintain_connection', #data_connection{connected = 'false'}=Connection) ->
    case try_connection(Connection) of
        {'error', _} ->
            erlang:send_after(?MILLISECONDS_IN_SECOND, self(), 'maintain_connection'),
            {'noreply', Connection};
        {'ok', C} ->
            self() ! 'maintain_connection',
            {'noreply', connection_established(C#data_connection{connected='true'})}
    end;
handle_info('maintain_connection', #data_connection{ready=Ready
                                                   ,server=Server
                                                   ,app=App
                                                   }=Connection) ->
    case App:server_info(Server) of
        {'ok', _} when not Ready ->
            erlang:send_after(5 * ?MILLISECONDS_IN_SECOND, self(), 'maintain_connection'),
            {'noreply', connection_ready(Connection)};
        {'ok', _} ->
            erlang:send_after(5 * ?MILLISECONDS_IN_SECOND, self(), 'maintain_connection'),
            {'noreply', Connection};
        _Else ->
            erlang:send_after(?MILLISECONDS_IN_SECOND, self(), 'maintain_connection'),
            {'noreply', reset_connection(Connection)}
    end;
handle_info(_Info, Connection) ->
    {'noreply', Connection}.

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

-spec try_connection(data_connection()) ->  {'ok', data_connection()} | {'error', any()}.
try_connection(#data_connection{app=App, props=Props}=Connection) ->
    lager:info("trying to connect ~s", [App]),
    try App:new_connection(Props) of
        {'ok', Server} -> {'ok', Connection#data_connection{server=Server}};
        Error -> handle_error(Connection, Error)
    catch
        _:Error -> handle_error(Connection, Error)
    end.

-spec handle_error(data_connection(), tuple()) -> {'error', any()}.
handle_error(#data_connection{app=App, props=Props}, Error) ->
    lager:info("failed to connect with ~s : ~p : ~p", [App, Error, Props]),
    {'error', Error}.

-spec connection_established(data_connection()) -> data_connection().
connection_established(Connection) ->
    Connection#data_connection{connected = true}.

-spec connection_ready(data_connection()) -> data_connection().
connection_ready(Connection) ->
    C = Connection#data_connection{ready = true},
    kz_dataconnections:update(C),
    C.

-spec reset_connection(data_connection()) -> data_connection().
reset_connection(Connection) ->
    C = Connection#data_connection{connected = false, ready = false},
    %% TODO: this is disabled for the moment to maintain backward
    %% compatablity with couch_mgr which always assumed the connection
    %% was available
    %%    kz_dataconnections:update(C),
    C.
