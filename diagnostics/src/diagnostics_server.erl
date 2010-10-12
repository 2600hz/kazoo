%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.com>
%%% @copyright (C) 2010, James Aimonetti
%%% @doc
%%% Maintain a list of nodes to query for diagnostic information, and
%%% present the data in a meaningful way to a human
%%% @end
%%% Created : 11 Oct 2010 by James Aimonetti <james@2600hz.com>
%%%-------------------------------------------------------------------
-module(diagnostics_server).

-behaviour(gen_server).

%% API
-export([start_link/0, add_node/1, rm_node/1, view/0, view/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-import(logger, [format_log/3]).
-import(props, [get_value/2, get_value/3]).

-define(SERVER, ?MODULE).

-define(CELL_FORMAT, " ~11.s |").
-define(NODE_LINE, lists:flatten(["  | ", ?CELL_FORMAT, ?CELL_FORMAT
				  ,?CELL_FORMAT, ?CELL_FORMAT, ?CELL_FORMAT
				  ,?CELL_FORMAT, "~n"])).
-define(NODE_LINE_HEADER, io_lib:format(?NODE_LINE, ["Type", "Requested", "Successful"
						     ,"Timed Out", "Failed", "Active"])).

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

add_node(Node) ->
    gen_server:call(?MODULE, {add_node, Node}).

rm_node(Node) ->
    gen_server:call(?MODULE, {rm_node, Node}).

view() ->
    gen_server:call(?MODULE, {view}).

view(Node) ->
    gen_server:call(?MODULE, {view, Node}).

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
    {ok, []}.

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
handle_call({add_node, Node}, _From, Nodes) ->
    case lists:member(Node, Nodes) of
	true ->
	    {reply, {error, {node_known, Node}}, Nodes};
	false ->
	    case net_adm:ping(Node) of
		pong ->
		    {reply, {ok, node_added}, [Node | Nodes]};
		pang ->
		    {reply, {error, {ping_failed, Node}}, Nodes}
	    end
    end;
handle_call({rm_node, Node}, _From, Nodes) ->
    case lists:member(Node, Nodes) of
	true ->
	    {reply, {ok, node_removed}, lists:delete(Node, Nodes)};
	false ->
	    {reply, {error, {node_unkown, Node}}, Nodes}
    end;
handle_call({view}, _From, Nodes) ->
    format_log(info, "DIAG_SERVER: Nodes to view: ~p~n", [Nodes]),
    lists:foreach(fun display_node/1, Nodes),
    {reply, ok, Nodes};
handle_call({view, Node}, _From, Nodes) ->
    format_log(info, "DIAG_SERVER: Node to view: ~p~n", [Node]),
    case lists:member(Node, Nodes) of
	true ->
	    case display_node(Node) of
		error ->
		    {reply, {error, {node_error, Node}}, lists:delete(Node, Nodes)};
		ok ->
		    {reply, ok, Nodes}
	    end;
	false ->
	    {reply, {error, {unknown_node, Node}}, Nodes}
    end;
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
-spec(display_node/1 :: (Node :: atom()) -> ok | error).
display_node(Node) ->
    case rpc:call(Node, ecallmgr, diagnostics, []) of
	{badrpc, Reason} ->
	    io:format("DIAG_SERVER: Error getting data from ~p: ~p~n", [Node, Reason]),
	    error;
	Data ->
	    lists:foreach(fun({freeswitch_nodes, FSData}) ->
				  display_fs_data(FSData);
			     (X) -> io:format("DIAG_SERVER: Unknown result.~n~p~n", [X])
			  end, Data),
	    ok
    end.

display_fs_data(Data) ->
    GenSrv = get_value(gen_server, Data),
    Vsn = get_value(version, Data),
    Host = get_value(host, Data),
    {{Y, M, D}, {H, Min, S}} = calendar:now_to_datetime(get_value(recorded, Data)),
    io:format("Diagnostics for ~p (~s) on ~p at ~p:~p:~p on ~p-~p-~p~n", [GenSrv, Vsn, Host, H,Min,S, Y,M,D]),

    KnownNodes = string:join(lists:map(fun erlang:atom_to_list/1, get_value(known_fs_nodes, Data)), ", "),
    io:format("  Known FS Nodes: ~s~n", [KnownNodes]),

    lists:map(fun({Node, {auth_handler, AuthData}, {route_handler, RouteData}}) ->
		      io:format("  Node Diagnostics for ~p~n", [Node]),
		      io:format(?NODE_LINE_HEADER, []),
		      show_line("Auth", AuthData),
		      show_line("Route", RouteData)
	      end, get_value(handler_diagnostics, Data)).

show_line(Type, {error, Error, Reason}) ->
    io:format("  |  ~11.s | ERROR(~p): ~p~n", [Type, Error, Reason]);
show_line(Type, Data) ->
    LR = get_value(lookups_requested, Data, 0),
    R = integer_to_list(LR),

    Format = fun(0) -> "0 (0%)";
		(X) when is_integer(X) -> io_lib:format("~p (~p%)", [X, round(X / LR * 100)]);
		(Y) -> io_lib:format("Huh: ~p", [Y])
	     end,

    S = Format(get_value(lookups_success, Data, 0)),
    T = Format(get_value(lookups_timeout, Data, 0)),
    F = Format(get_value(lookups_failed, Data, 0)),
    A = Format(length(get_value(active_lookups, Data, []))),
    io:format(?NODE_LINE, [Type, R, S, T, F, A]).
%%
%% Diagnostics for GEN_SERVER (VSN) on HOST at RecordedTime
%%   Known Nodes: KNOWN_NODES
%%   Type  | Requested | Successful | Timed Out | Failed | Active
%%   Auth  |    30     |  10 (33%)  |  15 (50%) | 3 (10%)|    2
%%   Route |    30     |  10 (33%)  |  15 (50%) | 3 (10%)|    2
