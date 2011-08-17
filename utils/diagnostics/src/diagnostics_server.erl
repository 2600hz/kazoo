%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2010, James Aimonetti
%%% @doc
%%% Maintain a list of nodes to query for diagnostic information, and
%%% present the data in a meaningful way to a human
%%% @end
%%% Created : 11 Oct 2010 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(diagnostics_server).

-behaviour(gen_server).

%% API
-export([start_link/0, add_node/1, rm_node/1, view/0, view/1, display_fs_data/1, display_fs_data/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-import(logger, [format_log/3]).
-import(props, [get_value/2, get_value/3]).

-define(SERVER, ?MODULE).

-define(CELL_FORMAT, " ~11.s |").
-define(HANDLER_LINE, lists:flatten(["  | ", ?CELL_FORMAT, ?CELL_FORMAT
				     ,?CELL_FORMAT, ?CELL_FORMAT, ?CELL_FORMAT
				     ,?CELL_FORMAT, ?CELL_FORMAT, "~n"])).
-define(HANDLER_LINE_HEADER, io_lib:format(?HANDLER_LINE, ["Type", "Requested", "Successful"
							,"Timed Out", "Failed", "Active", "Uptime"])).
-define(NODE_LINE, lists:flatten(["  | ", ?CELL_FORMAT, ?CELL_FORMAT
				  ,?CELL_FORMAT, ?CELL_FORMAT, ?CELL_FORMAT, "~n"])).
-define(NODE_LINE_HEADER, io_lib:format(?NODE_LINE, ["Type", "Created", "Destroyed", "Active", "Uptime"])).

-define(MICRO_TO_SEC, 1000000).

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

%% Diagnostics for GEN_SERVER (VSN) on HOST at HH:MM:SS on YYYY-MM-DD
%%   Node Diagnostics for NODE
%%   Type  | Requested | Successful | Timed Out | Failed | Active | Uptime
%%   Auth  |    30     |  10 (33%)  |  15 (50%) | 3 (10%)|    2   |  32m
%%   Route |    30     |  10 (33%)  |  15 (50%) | 3 (10%)|    2   |  32m
%%   Node  | CHAN_CREA |  CHAN_DEST |           |        | ACTIVE |  1m2s

display_fs_data(Data) -> display_fs_data(Data, all).

display_fs_data(Data, Opt) ->
    GenSrv = get_value(gen_server, Data),
    Vsn = get_value(version, Data),
    Host = get_value(host, Data),
    {{Y, M, D}, {H, Min, S}} = calendar:now_to_datetime(get_value(recorded, Data)),

    BaseAcc = [{node_handler, {ok, [{active_channels, 0}, {created_channels, 0}, {destroyed_channels, 0}]}}
	       ,{auth_handler, {ok, [{lookups_success,0}, {lookups_failed,0}, {lookups_timeout,0}, {lookups_requested,0}]}}
	       ,{route_handler, {ok, [{lookups_success,0}, {lookups_failed,0}, {lookups_timeout,0}, {lookups_requested,0}]}}
	      ],

    io:format("Diagnostics for ~p (~s) on ~p at ~2.2.0w:~2.2.0w:~2.2.0w on ~p-~p-~p~n", [GenSrv, Vsn, Host, H,Min,S, Y,M,D]),
    AccNodes = lists:foldr(fun(T, Acc)  ->
				   case Opt of
				       all -> show_node(T), merge_data(T, Acc);
				       acc -> merge_data(T, Acc);
				       Node when element(1, T) =:= Node -> show_node(T);
				       _ -> Acc
				   end
			   end, BaseAcc, get_value(handler_diagnostics, Data)),

    case Opt of
	Node when Node =/= all andalso Node =/= acc -> ok;
	_ ->
	    AccData = list_to_tuple([accumulated
				     ,{auth_handler, get_value(auth_handler, AccNodes)}
				     ,{route_handler, get_value(route_handler, AccNodes)}
				     ,{node_handler, get_value(node_handler, AccNodes)}]),
	    
	    io:format("~n", []),
	    show_node(AccData)
    end.

merge_data(T, Acc0) ->
    [_Node | L] = tuple_to_list(T),
    lists:foldl(fun({_Type, {error, _, _}}, Acc) -> Acc;
		   ({_Type, {'EXIT', _, _}}, Acc) -> Acc;
		   ({node_handler, {ok, Data}}, Acc) ->
			{ok, NodeAcc} = get_value(node_handler, Acc),
			AC = get_value(active_channels, Data, 0) + get_value(active_channels, NodeAcc, 0),
			CC = get_value(created_channels, Data, 0) + get_value(created_channels, NodeAcc, 0),
			DC = get_value(destroyed_channels, Data, 0) + get_value(destroyed_channels, NodeAcc, 0),

			[{node_handler, {ok, [{active_channels, AC}, {created_channels, CC}, {destroyed_channels, DC}]}}
			 | lists:keydelete(node_handler, 1, Acc)];
		   ({H, {ok, Data}}, Acc) ->
			{ok, HAcc} = get_value(H, Acc),
			LS = get_value(lookups_success, Data, 0) + get_value(lookups_success, HAcc, 0),
			LF = get_value(lookups_failed, Data, 0) + get_value(lookups_failed, HAcc, 0),
			LT = get_value(lookups_timeout, Data, 0) + get_value(lookups_timeout, HAcc, 0),
			LR = get_value(lookups_requested, Data, 0) + get_value(lookups_requested, HAcc, 0),

			[{H, {ok, [{lookups_success,LS}, {lookups_failed,LF}, {lookups_timeout,LT}, {lookups_requested,LR}]}}
			 | lists:keydelete(H, 1, Acc)]
		end, Acc0, L).

show_node(T) ->
    [Node | L] = tuple_to_list(T),
    io:format("  Node Diagnostics for ~p~n", [Node]),
    io:format(?HANDLER_LINE_HEADER, []),
    lists:foreach(fun({H, Data}) -> show_line(H, Data) end, L).

show_line(Type, {error, Error, Reason}) ->
    io:format("  |  ~11.s | ERROR(~p): ~p~n", [Type, Error, Reason]);
show_line(Type, {'EXIT', _Pid, Cause}) ->
    io:format("  |  ~11.s | ERROR(exit): ~p~n", [Type, Cause]);
show_line(node_handler=Handler, {ok, Data}) ->
    io:format("~n", []),
    io:format(?NODE_LINE_HEADER, []),
    Type = lists:takewhile(fun(C) -> C =/= $_ end, atom_to_list(Handler)),
    R = wh_util:to_list(get_value(created_channels, Data, 0)),
    S = wh_util:to_list(get_value(destroyed_channels, Data, 0)),
    A = wh_util:to_list(get_value(active_channels, Data, 0)),
    U = get_uptime(get_value(uptime, Data, 0)),
    io:format(?NODE_LINE, [Type, R, S, A, U]);
show_line(Handler, {ok, Data}) when is_list(Data) ->
    Type = lists:takewhile(fun(C) -> C =/= $_ end, atom_to_list(Handler)),
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
    U = get_uptime(get_value(uptime, Data, 0)),
    io:format(?HANDLER_LINE, [Type, R, S, T, F, A, U]);
show_line(Handler, {error, E, _}) ->
    io:format(" ~p: ~p~n", [Handler, E]).


%% uptime, in microseconds
get_uptime(0) ->
    "N/A";
%% when less than 1 second, time in milliseconds
get_uptime(Micro) when Micro < ?MICRO_TO_SEC ->
    io_lib:format("~pms", [Micro div 1000]);
%% when less than 1 minute, time in seconds
get_uptime(Micro) when Micro < 60 * ?MICRO_TO_SEC ->
    io_lib:format("~ps", [Micro div ?MICRO_TO_SEC]);
%% when less than 10 minutes, time in MmSs
get_uptime(Micro) when Micro < 600 * ?MICRO_TO_SEC ->
    S = Micro div ?MICRO_TO_SEC,
    M = S div 60,
    io_lib:format("~pm~ps", [M rem 60, S rem 60]);
%% when less than 1 hour, time in mins
get_uptime(Micro) when Micro < 3600 * ?MICRO_TO_SEC ->
    io_lib:format("~pm", [Micro div (60 * ?MICRO_TO_SEC)]);
%% when less than 1 day, time in HhMm
get_uptime(Micro) when Micro < 86400 * ?MICRO_TO_SEC ->
    M = Micro div (60 * ?MICRO_TO_SEC),
    H = M div 60,
    io_lib:format("~ph~pm", [H rem 24, M rem 60]);
%% when greater than 1 day, time in DdHhMm
get_uptime(Micro) ->
    M = Micro div (60 * ?MICRO_TO_SEC),
    H = M div 60,
    D = H div 24,
    io_lib:format("~pd~ph~pm", [D, H rem 24, M rem 60]).
