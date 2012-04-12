%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2012, VoIP INC
%%% @doc
%%%
%%% When connecting to a FreeSWITCH node, we create three processes: one to
%%% handle authentication (directory) requests; one to handle route (dialplan)
%%% requests, and one to monitor the node and various stats about the node.
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-----------------------------------------------------------------------------
-module(ecallmgr_fs_nodes).

-behaviour(gen_server).

-export([start_link/0]).
-export([connected/0]).
-export([add/1, add/2, add/3]).
-export([remove/1]).
-export([is_node_up/1]).
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,terminate/2
         ,code_change/3
        ]).

-include("ecallmgr.hrl").

-define(SERVER, ?MODULE).

-record(node, {node = 'undefined' :: atom()
               ,cookie = 'undefined' :: atom()
               ,options = [] :: proplist()
              }).

-record(state, {nodes = [] :: [#node{},...] | []
                ,preconfigured_lookup :: pid()
               }).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% returns ok or {error, some_error_atom_explaining_more}
-spec add/1 :: (atom()) -> 'ok' | {'error', 'no_connection'}.
-spec add/2 :: (atom(), proplist() | atom()) -> 'ok' | {'error', 'no_connection'}.
-spec add/3 :: (atom(), atom(), proplist() | atom()) -> 'ok' | {'error', 'no_connection'}.

add(Node) -> 
    add(Node, []).

add(Node, Opts) when is_list(Opts) ->
    add(Node, erlang:get_cookie(), Opts);
add(Node, Cookie) when is_atom(Cookie) ->
    add(Node, Cookie, []).

add(Node, Cookie, Opts) ->
    gen_server:call(?MODULE, {add_fs_node, Node, Cookie, Opts}, 30000).

%% returns ok or {error, some_error_atom_explaining_more}
-spec remove/1 :: (atom()) -> 'ok'.
remove(Node) ->
    gen_server:cast(?MODULE, {rm_fs_node, Node}).

-spec connected/0 :: () -> [atom(),...] | [].
connected() ->
    gen_server:call(?MODULE, connected_nodes).

-spec is_node_up/1 :: (atom()) -> boolean().
is_node_up(Node) ->
    gen_server:call(?MODULE, {is_node_up, Node}).

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
    lager:debug("starting new fs handler"),
    Pid = spawn(fun() -> start_preconfigured_servers() end),
    {ok, #state{preconfigured_lookup=Pid}}.

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
%% #state{nodes=[{FSNode, HandlerPid}]}
%%--------------------------------------------------------------------
handle_call({is_node_up, Node}, _From, #state{nodes=Nodes}=State) ->
    {reply, [ Node1 || #node{node=Node1} <- Nodes, Node1 =:= Node ] =/= [], State};
handle_call(connected_nodes, _From, #state{nodes=Nodes}=State) ->
    {reply, [ Node || #node{node=Node} <- Nodes ], State};
handle_call({add_fs_node, Node, Cookie, Options}, {Pid, _}, #state{preconfigured_lookup=Pid}=State) ->
    lager:debug("trying to add ~s(~s)", [Node, Cookie]),
    {Resp, State1} = add_fs_node(Node, Cookie, Options, State),
    {reply, Resp, State1, hibernate};
handle_call({add_fs_node, Node, Cookie, Options}, _From, #state{preconfigured_lookup=Pid}=State) ->
    lager:debug("trying to add ~s(~s)", [Node, Cookie]),
    {Resp, State1} = add_fs_node(Node, Cookie, Options, State),
    Pid1 = maybe_stop_preconfigured_lookup(Resp, Pid),
    {reply, Resp, State1#state{preconfigured_lookup=Pid1}, hibernate};
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
handle_cast({rm_fs_node, Node}, State) ->
    {noreply, rm_fs_node(Node, State), hibernate}.

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
handle_info({nodedown, Node}, #state{nodes=Nodes}=State) ->
    _ = ecallmgr_fs_sup:remove_node(Node),
    Opts = case lists:keyfind(Node, #node.node, Nodes) of 
               #node{options=O} -> O;
               false -> []
           end,
    case ecallmgr_fs_pinger_sup:add_node(Node, Opts) of
        {ok, _} -> 
            lager:debug("started fs pinger for node '~s'", [Node]),
            {noreply, State#state{nodes=lists:keydelete(Node, #node.node, Nodes)}};
        {error, {already_started, _}} ->
            lager:debug("fs pinger for node '~s' already exists", [Node]),
            {noreply, State#state{nodes=lists:keydelete(Node, #node.node, Nodes)}};
        _Else ->
            _ = ecallmgr_fs_pinger_sup:remove_node(Node),
            self() ! {nodedown, Node},
            lager:debug("failed to start fs pinger for node '~s': ~p", [Node, _Else]),
            {noreply, State}
    end;
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
    lager:debug("fs nodes termination: ~p", [ _Reason]).

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
-spec add_fs_node/4 :: (atom(), atom(), proplist(), #state{}) -> {'ok', #state{}} | 
                                                                 {{'error', 'no_connection'}, #state{}} |
                                                                 {{'error', 'failed_starting_handlers'}, #state{}}.
add_fs_node(Node, Cookie, Options, #state{nodes=Nodes}=State) ->
    case [N || #node{node=Node1}=N <- Nodes, Node =:= Node1] of
        [] ->
            erlang:set_cookie(Node, Cookie),
            case net_adm:ping(Node) of
                pong ->
                    lager:debug("no node matching ~p found, adding", [Node]),
                    case ecallmgr_fs_sup:add_node(Node, Options) of
                        {ok, _} -> 
                            erlang:monitor_node(Node, true),
                            lager:info("successfully connected to node '~s'", [Node]),
                            {ok, State#state{nodes=[#node{node=Node, cookie=Cookie, options=Options} | Nodes]}};
                        {error, {already_started, _}} ->
                            lager:info("already connected to node '~s'", [Node]),
                            {ok, State#state{nodes=[#node{node=Node, cookie=Cookie, options=Options} | Nodes]}};
                        _Else ->
                            lager:warning("failed to add node '~s'", [Node]),
                            self() ! {nodedown, Node},
                            {{error, failed_starting_handlers}, State}
                    end;
                pang ->
                    lager:info("unable to connect to node '~s'; ensure it is reachable from this server and using cookie '~s'", [Node, Cookie]),
                    self() ! {nodedown, Node},
                    {{error, no_connection}, State}
            end;
        [#node{node=Node}] ->
            lager:info("already connected to node '~s'", [Node]),
            {ok, State}
    end.

-spec rm_fs_node/2 :: (atom(), #state{}) -> #state{}.
rm_fs_node(Node, #state{nodes=Nodes}=State) ->
    lager:debug("closing node handler for ~s", [Node]),
    _ = close_node(Node),
    State#state{nodes=lists:keydelete(Node, 2, Nodes)}.    

-spec close_node/1 :: (atom() | #node{}) -> 'ok' | {'error','not_found' | 'running' | 'simple_one_for_one'}.
close_node(#node{node=Node}) ->
    close_node(Node);
close_node(Node) ->
    erlang:monitor_node(Node, false),
    _ = ecallmgr_fs_pinger_sup:remove_node(Node),
    ecallmgr_fs_sup:remove_node(Node).

start_preconfigured_servers() ->
    put(callid, ?LOG_SYSTEM_ID),
    case ecallmgr_config:get(<<"fs_nodes">>, []) of
        [] ->
            lager:info("no preconfigured servers available. Is the sysconf whapp running?"),
            timer:sleep(5000),
            start_preconfigured_servers();
        Nodes when is_list(Nodes) ->
            lager:info("successfully retrieved FreeSWITCH nodes to connect with, doing so..."),
            [start_node_from_config(N) || N <- Nodes];
        _E ->
            lager:debug("recieved a non-list for fs_nodes: ~p", [_E]),
            timer:sleep(5000),
            start_preconfigured_servers()
    end.

start_node_from_config(MaybeJObj) ->
    case wh_json:is_json_object(MaybeJObj) of
        false -> ?MODULE:add(wh_util:to_atom(MaybeJObj, true));
        true ->
            {[Cookie], [Node]} = wh_json:get_values(MaybeJObj),
            ?MODULE:add(wh_util:to_atom(Node, true), wh_util:to_atom(Cookie, true))
    end.
            
-spec maybe_stop_preconfigured_lookup/2 :: ('ok' | {'error', _}, pid() | 'undefined') -> pid() | 'undefined'.
maybe_stop_preconfigured_lookup(_, undefined) -> undefined;
maybe_stop_preconfigured_lookup(ok, Pid) ->
    case is_pid(Pid) andalso is_process_alive(Pid) of
        true ->
            exit(Pid, kill),
            undefined;
        false ->
            undefined
    end;
maybe_stop_preconfigured_lookup(_, Pid) ->
    Pid.
