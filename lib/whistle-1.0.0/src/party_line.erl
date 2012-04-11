%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2012, VoIP INC
%%% @doc
%%%
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-----------------------------------------------------------------------------
-module(party_line).

-behaviour(gen_server).

-compile({no_auto_import, [register/2]}).

-define(SERVER, ?MODULE).

-export([start_link/0, start_link/1]).
-export([register/1, register/2]).
-export([unregister/0, unregister/1, unregister/2]).
-export([send/2, send/3]).
-export([list/1, list/2]).
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,terminate/2
         ,code_change/3
        ]).

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
-spec start_link/0 :: () -> {'ok', pid()}.
-spec start_link/1 :: (atom()) -> {'ok', pid()}.

start_link() ->
    start_link(?SERVER).

start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [Name], []).

-spec register/1 :: (term()) -> 'ok'.
register(Key) ->
    register(?SERVER, Key).

-spec register/2 :: (atom(), term()) -> 'ok'.
register(Srv, Key) ->
    case get(Srv) of
        undefined -> put(Srv, [Key]);
        Keys -> put(Srv, [Key|Keys])
    end,
    gen_server:cast(Srv, {register, Key, self()}).

-spec unregister/0 :: () -> 'ok'.
unregister() ->
    unregister(?SERVER, undefined).

-spec unregister/1 :: (term()) -> 'ok'.
unregister(Key) ->
    unregister(?SERVER, Key).

-spec unregister/2 :: (atom(), term()) -> 'ok'.
unregister(Srv, Key) ->
    case get(party_line) of
        undefined -> ok;
        Keys -> put(Srv, lists:delete(Key, Keys))
    end,
    gen_server:cast(Srv, {unregister, Key, self()}).

-spec send/2 :: (term(), term()) -> 'ok'.
send(Key, Msg) ->
    send(?SERVER, Key, Msg).

-spec send/3 :: (atom(), term(), term()) -> 'ok'.
send(Srv, Key, Msg) ->
    Pids = list(Srv, Key),
    [P ! Msg || P <- Pids, P =/= self()],
    ok.

-spec list/1 :: (term()) -> [pid(),...] | [].
list(Key) ->
    list(?SERVER, Key).

-spec list/2 :: (atom(), term()) -> [pid(),...] | [].
list(Srv, Key) ->
    gen_server:call(Srv, {list, Key}).    

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
init([_Name]) ->
    %% TODO: search for a key in every process dict and init the registrar?? maybe...
    {ok, {dict:new(), dict:new()}}.

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
handle_call({list, Key}, _From, {Registrar, _}=State) ->
    case dict:find(Key, Registrar) of
        error -> 
            {reply, [], State};
        {ok, Pids} ->
            {reply, Pids, State}
    end;    
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
handle_cast({register, Key, Pid}, {Registrar, Monitors}) ->
    case dict:find(Pid, Monitors) of
        error ->
            MRef = erlang:monitor(process, Pid),
            {noreply, {dict:append(Key, Pid, Registrar)
                       ,dict:store(Pid, MRef, Monitors)}};
        {ok, _} ->
            {noreply, {dict:append(Key, Pid, Registrar), Monitors}}
    end;
handle_cast({unregister, undefined, Pid}, {Registrar, Monitors}) ->
    NewRegistrar = dict:map(fun(_, Pids) ->
                                    [P || P <- Pids, P =/= Pid]
                            end, Registrar),    
    {noreply, {NewRegistrar, maybe_demonitor(Pid, Monitors)}};
handle_cast({unregister, Key, Pid}, {Registrar, Monitors}) ->
    case dict:find(Key, Registrar) of
        error -> {noreply, {Registrar, Monitors}};
        {ok, Pids} ->
            NewRegistrar = dict:store(Key, [P || P <- Pids, P =/= Pid], Registrar),
            case lists:any(fun({_, Ps}) -> 
                                   [P || P <- Ps, P =:= Pid] =/= [] 
                           end, dict:to_list(NewRegistrar)) of
                false -> 
                    %% the pid has removed its last key    
                    {noreply, {NewRegistrar
                               ,maybe_demonitor(Pid, Monitors)}};
                true ->
                    %% the pid still has other keys
                    {noreply, {NewRegistrar, Monitors}}
            end
    end;
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
handle_info({'DOWN', _, _, Pid, _}, State) ->
    gen_server:cast(self(), {unregister, undefined, Pid}),
    {noreply, State};
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
    lager:debug("party line terminating: ~p", [_Reason]).

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
-spec maybe_demonitor/2 :: (pid(), dict()) -> dict().
maybe_demonitor(Pid, Monitors) ->
    case dict:find(Pid, Monitors) of
        error -> Monitors;
        {ok, MRef} ->
            erlang:demonitor(MRef, [flush]),            
            dict:erase(Pid, Monitors)
    end.
