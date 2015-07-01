%% @private
%% @doc A server that keeps track of handler nodes.
%%   Handler nodes should call {@link eradius:modules_ready/2} from their application master
%%   as soon as they are ready, which makes them available for request processing.
%%   The node_mon server monitors the application master and removes it from
%%   request processing when it goes down.
-module(eradius_node_mon).
-export([start_link/0, modules_ready/2, set_nodes/1, get_module_nodes/1]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(NODE_TAB, eradius_node_mon).
-define(PING_INTERVAL, 3000). % 3 sec
-define(PING_TIMEOUT, 300).   % 0.3 sec
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------------------------------
%% -- API
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec modules_ready(pid(), list(module())) -> ok.
modules_ready(ApplicationMaster, Modules) when is_pid(ApplicationMaster), is_list(Modules) ->
    gen_server:cast(?SERVER, {modules_ready, ApplicationMaster, Modules}).

-spec set_nodes(list(node())) -> ok.
set_nodes(Nodes) ->
    gen_server:call(?SERVER, {set_nodes, Nodes}).

-spec get_module_nodes(module()) -> [node()].
get_module_nodes(Module) ->
    try
        ets:lookup_element(?NODE_TAB, Module, 2)
    catch
        error:badarg ->
            []
    end.

%% ------------------------------------------------------------------------------------------
%% -- gen_server callbacks
-record(state, {
    live_registrar_nodes = sets:new() :: set(),
    dead_registrar_nodes = sets:new() :: set(),
    app_masters = dict:new()          :: dict(),
    ping_timer                        :: reference()
}).

init([]) ->
    ets:new(?NODE_TAB, [bag, named_table, protected, {read_concurrency, true}]),
    PingTimer = erlang:send_after(?PING_INTERVAL, self(), ping_dead_nodes),
    {ok, #state{ping_timer = PingTimer}}.

handle_call(remote_get_regs_v1, _From, State) ->
    Registrations = dict:to_list(State#state.app_masters),
    {reply, {ok, Registrations}, State};
handle_call({set_nodes, Nodes}, _From, State) ->
    NewState = State#state{live_registrar_nodes = sets:new(),
                           dead_registrar_nodes = sets:from_list(Nodes)},
    self() ! ping_dead_nodes,
    {reply, ok, NewState}.

handle_cast({remote_modules_ready_v1, ApplicationMaster, Modules}, State) ->
    NewState = State#state{app_masters = register_locally({ApplicationMaster, Modules}, State#state.app_masters)},
    {noreply, NewState};
handle_cast({modules_ready, ApplicationMaster, Modules}, State) ->
    NewState = State#state{app_masters = register_locally({ApplicationMaster, Modules}, State#state.app_masters)},
    lists:foreach(fun (Node) ->
                      gen_server:cast({?SERVER, Node}, {remote_modules_ready_v1, ApplicationMaster, Modules})
                  end, nodes()),
    {noreply, NewState}.

handle_info({'DOWN', _MRef, process, {?SERVER, Node}, _Reason}, State = #state{live_registrar_nodes = LiveRegistrars}) ->
    case sets:is_element(Node, LiveRegistrars) of
        false ->
            %% ignore the 'DOWN', it's from a node we don't really want to monitor anymore
            %% and that shouldn't get into dead_registrar_nodes
            {noreply, State};
        true ->
            {noreply, State#state{live_registrar_nodes = sets:del_element(Node, LiveRegistrars),
                                  dead_registrar_nodes = sets:add_element(Node, State#state.dead_registrar_nodes)}}
    end;
handle_info({'DOWN', _MRef, process, Pid, _Reason}, State = #state{app_masters = AppMasters}) when is_pid(Pid) ->
    case dict:find(Pid, AppMasters) of
        error ->
            {noreply, State};
        {ok, Modules} ->
            ServerNode = node(Pid),
            lists:foreach(fun (Mod) -> ets:delete_object(?NODE_TAB, {Mod, ServerNode}) end, Modules),
            NewState = State#state{app_masters = dict:erase(Pid, AppMasters)},
            {noreply, NewState}
    end;
handle_info(ping_dead_nodes, State = #state{app_masters = AppMasters, live_registrar_nodes = LiveRegistrars}) ->
    erlang:cancel_timer(State#state.ping_timer),
    {NewLive, NewDead, NewAppMasters} =
        sets:fold(fun (Node, {Live, Dead, AppMastersAcc}) ->
                          case (catch gen_server:call({?SERVER, Node}, remote_get_regs_v1, ?PING_TIMEOUT)) of
                              {ok, Registrations} ->
                                  NewAppMastersAcc = lists:foldl(fun register_locally/2, AppMastersAcc, Registrations),
                                  erlang:monitor(process, {?SERVER, Node}),
                                  {sets:add_element(Node, Live), Dead, NewAppMastersAcc};
                              {'EXIT', _Reason} ->
                                  {Live, sets:add_element(Node, Dead), AppMastersAcc}
                          end
                  end, {LiveRegistrars, sets:new(), AppMasters}, State#state.dead_registrar_nodes),
    NewPingTimer = erlang:send_after(?PING_INTERVAL, self(), ping_dead_nodes),
    NewState = State#state{live_registrar_nodes = NewLive,
                           dead_registrar_nodes = NewDead,
                           app_masters = NewAppMasters,
                           ping_timer = NewPingTimer},
    {noreply, NewState};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% ------------------------------------------------------------------------------------------
%% -- helpers
-spec dict_prepend(term(), list(term()), dict()) -> dict().
dict_prepend(Key, List, Dict) ->
    dict:update(Key, fun (Old) -> List ++ Old end, List, Dict).

register_locally({ApplicationMaster, Modules}, AppMasters) ->
    case dict:is_key(ApplicationMaster, AppMasters) of
        true ->
            ok; %% already monitored
        false ->
            monitor(process, ApplicationMaster)
    end,
    ServerNode = node(ApplicationMaster),
    ets:insert(?NODE_TAB, [{Mod, ServerNode} || Mod <- Modules]),
    dict_prepend(ApplicationMaster, Modules, AppMasters).
