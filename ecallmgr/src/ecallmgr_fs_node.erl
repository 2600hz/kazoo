%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2010, VoIP INC
%%% @doc
%%% Manage a FreeSWITCH node and its resources
%%% @end
%%% Created : 11 Nov 2010 by James Aimonetti <james@2600hz.org>
-module(ecallmgr_fs_node).

-behaviour(gen_server).

%% API
-export([start_link/1, start_link/2]).
-export([resource_consume/3, show_channels/1, fs_node/1, uuid_exists/2
	 ,hostname/1
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("ecallmgr.hrl").

-record(state, {
	  node = 'undefined' :: atom()
	  ,stats = #node_stats{} :: #node_stats{}
	  ,options = [] :: proplist()
	 }).

-define(SERVER, ?MODULE).

-define(YR_TO_MICRO(Y), wh_util:to_integer(Y)*365*24*3600*1000000).
-define(DAY_TO_MICRO(D), wh_util:to_integer(D)*24*3600*1000000).
-define(HR_TO_MICRO(Hr), wh_util:to_integer(Hr)*3600*1000000).
-define(MIN_TO_MICRO(Min), wh_util:to_integer(Min)*60*1000000).
-define(SEC_TO_MICRO(Sec), wh_util:to_integer(Sec)*1000000).
-define(MILLI_TO_MICRO(Mil), wh_util:to_integer(Mil)*1000).

-define(FS_TIMEOUT, 5000).

%% keep in sync with wh_api.hrl OPTIONAL_CHANNEL_QUERY_REQ_HEADERS
-define(CALL_STATUS_HEADERS, [<<"Unique-ID">>, <<"Call-Direction">>, <<"Caller-Caller-ID-Name">>, <<"Caller-Caller-ID-Number">>
				  ,<<"Caller-Network-Addr">>, <<"Caller-Destination-Number">>, <<"FreeSWITCH-Hostname">>
			     ]).
-define(CALL_STATUS_MAPPING, lists:zip(?CALL_STATUS_HEADERS, [<<"Call-ID">> | ?OPTIONAL_CHANNEL_QUERY_REQ_HEADERS])).

-spec resource_consume/3 :: (FsNodePid, Route, JObj) -> {'resource_consumed', binary(), binary(), integer()} |
							{'resource_error', binary() | 'error'} when
      FsNodePid :: pid(),
      Route :: binary(),
      JObj :: json_object().
resource_consume(FsNodePid, Route, JObj) ->
    FsNodePid ! {resource_consume, self(), Route, JObj},
    receive Resp -> Resp
    after   10000 -> {resource_error, timeout}
    end.

-spec show_channels/1 :: (pid()) -> [proplist(),...] | [].
show_channels(Srv) ->
    gen_server:call(Srv, show_channels).

-spec hostname/1 :: (pid()) -> fs_api_ret().
hostname(Srv) ->
    gen_server:call(Srv, hostname).

-spec fs_node/1 :: (pid()) -> atom().
fs_node(Srv) ->
    gen_server:call(Srv, fs_node).

-spec uuid_exists/2 :: (pid(), binary()) -> boolean().
uuid_exists(Srv, UUID) ->
    gen_server:call(Srv, {uuid_exists, UUID}).

-spec start_link/1 :: (Node :: atom()) -> {'ok', pid()} | {'error', term()}.
start_link(Node) ->
    gen_server:start_link(?SERVER, [Node, []], []).

-spec start_link/2 :: (Node :: atom(), Options :: proplist()) -> {'ok', pid()} | {error, term()}.
start_link(Node, Options) ->
    gen_server:start_link(?SERVER, [Node, Options], []).

init([Node, Options]) ->
    ?LOG_SYS("starting new fs node ~s", [Node]),
    Stats = #node_stats{started = erlang:now()},
    {ok, #state{node=Node, stats=Stats, options=Options}, 0}.

-spec handle_call/3 :: (term(), {pid(), reference()}, #state{}) -> {'noreply', #state{}} | {'reply', atom(), #state{}}.
handle_call(hostname, From, #state{node=Node}=State) ->
    spawn(fun() -> gen_server:reply(From, freeswitch:api(Node, hostname, "")) end),
    {noreply, State};
handle_call({uuid_exists, UUID}, From, #state{node=Node}=State) ->
    spawn(fun() ->
		  case freeswitch:api(Node, uuid_exists, wh_util:to_list(UUID)) of
		      {'ok', Result} -> ?LOG(UUID, "Result of uuid_exists: ~s", [Result]), gen_server:reply(From, wh_util:is_true(Result));
		      _ -> ?LOG(UUID, "Failed to get result from uuid_exists", []), gen_server:reply(From, false)
		  end
	  end),
    {noreply, State};
handle_call(fs_node, _From, #state{node=Node}=State) ->
    {reply, Node, State};
handle_call(show_channels, From, #state{node=Node}=State) ->
    spawn(fun() ->
		  case freeswitch:api(Node, show, "channels") of
		      {ok, Rows} -> gen_server:reply(From, convert_rows(Node, Rows));
		      _ -> gen_server:reply(From, [])
		  end
	  end),
    {noreply, State}.

handle_cast(_Req, State) ->
    {noreply, State}.

handle_info(timeout, #state{stats=Stats, node=Node}=State) ->
    erlang:monitor_node(Node, true),

    {foo, Node} ! register_event_handler,
    receive
	ok ->
	    Res = run_start_cmds(Node),
	    ?LOG_SYS("Start cmd results:"),
	    _ = [ ?LOG("~p", [ApiRes]) || ApiRes <- Res],

	    NodeData = extract_node_data(Node),
	    Active = get_active_channels(Node),

	    ok = freeswitch:event(Node, ['CHANNEL_CREATE', 'CHANNEL_DESTROY', 'HEARTBEAT', 'CHANNEL_HANGUP_COMPLETE'
					 ,'CUSTOM', 'sofia::register'
					]),

	    {noreply, State#state{stats=(Stats#node_stats{
					   created_channels = Active
					   ,fs_uptime = props:get_value(uptime, NodeData, 0)
					  })}, hibernate};
	{error, Reason} ->
	    {stop, Reason, State};
	timeout ->
	    {stop, timeout, State}
    after ?FS_TIMEOUT ->
	    {stop, timeout, State}
    end;

%% If we start up while there are active channels, we'll have negative active_channels in our stats.
%% The first clause fixes that situation
handle_info(Msg, #state{stats=#node_stats{created_channels=Cr, destroyed_channels=De}=Stats}=S) when De > Cr ->
    handle_info(Msg, S#state{stats=Stats#node_stats{created_channels=De, destroyed_channels=De}});

handle_info({event, [undefined | Data]}, #state{stats=Stats}=State) ->
    EvtName = props:get_value(<<"Event-Name">>, Data),
    case EvtName of
	<<"HEARTBEAT">> ->
	    {noreply, State#state{stats=Stats#node_stats{last_heartbeat=erlang:now()}}, hibernate};
	<<"CUSTOM">> ->
	    spawn(fun() ->
                          put(callid, props:get_value(<<"call-id">>, Data)),
                          ?LOG("custom event received ~s", [EvtName]),
                          process_custom_data(Data, ?APP_VERSION)
                  end),
	    {noreply, State, hibernate};
	_ ->
	    {noreply, State, hibernate}
    end;

handle_info({event, [UUID | Data]}, #state{stats=#node_stats{created_channels=Cr, destroyed_channels=De}=Stats}=State) ->
    EvtName = props:get_value(<<"Event-Name">>, Data),
    case EvtName of
	<<"CHANNEL_CREATE">> ->
	    ?LOG(UUID, "received channel create event", []),
	    {noreply, State#state{stats=Stats#node_stats{created_channels=Cr+1}}, hibernate};
	<<"CHANNEL_DESTROY">> ->
	    ChanState = props:get_value(<<"Channel-State">>, Data),
	    case ChanState of
		<<"CS_NEW">> -> % ignore
		    ?LOG(UUID, "ignoring channel destroy because of CS_NEW", []),
		    {noreply, State, hibernate};
		<<"CS_DESTROY">> ->
		    ?LOG(UUID, "received channel destroyed", []),
		    {noreply, State#state{stats=Stats#node_stats{destroyed_channels=De+1}}, hibernate}
	    end;
	<<"CHANNEL_HANGUP_COMPLETE">> ->
	    {noreply, State};
	<<"CUSTOM">> ->
	    spawn(fun() ->
                          put(callid, props:get_value(<<"call-id">>, Data)),
                          ?LOG("custom event received ~s", [EvtName]),
                          process_custom_data(Data, ?APP_VERSION)
                  end),
	    {noreply, State};
	_ ->
	    {noreply, State}
    end;
handle_info({resource_request, Pid, <<"audio">>, ChanOptions}
	    ,#state{options=Opts, stats=#node_stats{created_channels=Cr, destroyed_channels=De}}=State) ->
    ActiveChan = Cr - De,
    MaxChan = props:get_value(max_channels, Opts),
    AvailChan =  MaxChan - ActiveChan,
    Utilized =  round(ActiveChan / MaxChan * 100),

    MinReq = props:get_value(min_channels_requested, ChanOptions),
    FSHandlerPid = self(),
    spawn(fun() -> channel_request(Pid, FSHandlerPid, AvailChan, Utilized, MinReq) end),
    {noreply, State};
handle_info({resource_consume, Pid, Route, JObj}, #state{node=Node, options=Opts, stats=#node_stats{created_channels=Cr, destroyed_channels=De}}=State) ->
    ActiveChan = Cr - De,
    MaxChan = props:get_value(max_channels, Opts, 1),
    AvailChan =  MaxChan - ActiveChan,

    spawn(fun() -> originate_channel(Node, Pid, Route, AvailChan, JObj) end),
    {noreply, State};

handle_info({update_options, NewOptions}, State) ->
    {noreply, State#state{options=NewOptions}, hibernate};

handle_info({diagnostics, Pid}, #state{stats=Stats}=State) ->
    spawn(fun() -> diagnostics(Pid, Stats) end),
    {noreply, State};

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ?LOG_SYS("fs node ~p termination", [_Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-spec originate_channel/5 :: (Node, Pid, Route, AvailChan, JObj) -> no_return() when
      Node :: atom(),
      Pid :: pid(),
      Route :: binary() | list(),
      AvailChan :: integer(),
      JObj :: json_object().
originate_channel(Node, Pid, Route, AvailChan, JObj) ->
    Action = get_originate_action(wh_json:get_value(<<"Application-Name">>, JObj), wh_json:get_value(<<"Application-Data">>, JObj)),
    OrigStr = binary_to_list(list_to_binary([Route, " &", Action])),
    ?LOG_SYS("originate ~s on node ~s", [OrigStr, Node]),
    Result = freeswitch:bgapi(Node, originate, OrigStr),
    case Result of
	{ok, JobId} ->
	    receive
		{bgok, JobId, X} ->
		    CallID = erlang:binary_part(X, {4, byte_size(X)-5}),
		    ?LOG_START(CallID, "originate with job id ~s received bgok ~s", [Node, JobId, X]),
		    CtlQ = start_call_handling(Node, CallID),
		    Pid ! {resource_consumed, CallID, CtlQ, AvailChan-1};
		{bgerror, JobId, Y} ->
		    ErrMsg = erlang:binary_part(Y, {5, byte_size(Y)-6}),
		    ?LOG_SYS("failed to originate, ~p", [Node, ErrMsg]),
		    Pid ! {resource_error, ErrMsg}
	    after
		9000 ->
		    ?LOG_SYS("originate timed out", [Node]),
		    Pid ! {resource_error, timeout}
	    end;
	{error, Y} ->
	    ErrMsg = erlang:binary_part(Y, {5, byte_size(Y)-6}),
	    ?LOG_SYS("failed to originate ~p", [Node, ErrMsg]),
	    Pid ! {resource_error, ErrMsg};
	timeout ->
	    ?LOG_SYS("originate timed out", [Node]),
	    Pid ! {resource_error, timeout}
    end.

-spec start_call_handling/2 :: (Node :: atom(), UUID :: binary()) -> CtlQueue :: binary() | {'error', 'amqp_error'}.
start_call_handling(Node, UUID) ->
    try
	true = is_binary(CtlQueue = amqp_util:new_callctl_queue(<<>>)),
	_ = amqp_util:bind_q_to_callctl(CtlQueue),

	{ok, CtlPid} = ecallmgr_call_sup:start_control_process(Node, UUID, CtlQueue),
	{ok, _} = ecallmgr_call_sup:start_event_process(Node, UUID, CtlPid),
	CtlQueue
    catch
	_:_ -> {error, amqp_error}
    end.

-spec diagnostics/2 :: (Pid :: pid(), Stats :: tuple()) -> no_return().
diagnostics(Pid, Stats) ->
    Resp = ecallmgr_diagnostics:get_diagnostics(Stats),
    Pid ! Resp.

channel_request(Pid, FSHandlerPid, AvailChan, Utilized, MinReq) ->
    ?LOG("requested for ~p channels with ~p avail", [MinReq, AvailChan]),
    case MinReq > AvailChan of
	true -> Pid ! {resource_response, FSHandlerPid, []};
	false -> Pid ! {resource_response, FSHandlerPid, [{node, FSHandlerPid}
							  ,{available_channels, AvailChan}
							  ,{percent_utilization, Utilized}
							 ]}
    end.

-spec extract_node_data/1 :: (Node) -> [{'cpu',string()} |
					{'sessions_max',integer()} |
					{'sessions_per_thirty',integer()} |
					{'sessions_since_startup',integer()} |
					{'uptime',number()}
					,...] when
      Node :: atom().
extract_node_data(Node) ->
    {ok, Status} = freeswitch:api(Node, status),
    Lines = string:tokens(wh_util:to_list(Status), [$\n]),
    process_status(Lines).

-spec process_status/1 :: (Lines) -> [{'cpu',string()} |
				      {'sessions_max',integer()} |
				      {'sessions_per_thirty',integer()} |
				      {'sessions_since_startup',integer()} |
				      {'uptime',number()}
				      ,...] when
      Lines :: [nonempty_string(),...].
process_status([Uptime, _, SessSince, Sess30, SessMax, CPU]) ->
    process_status([Uptime, SessSince, Sess30, SessMax, CPU]);
process_status(["UP " ++ Uptime, SessSince, Sess30, SessMax, CPU]) ->
    {match, [[Y],[D],[Hour],[Min],[Sec],[Milli],[Micro]]} = re:run(Uptime, "([\\d]+)", [{capture, [1], list}, global]),
    UpMicro = ?YR_TO_MICRO(Y) + ?DAY_TO_MICRO(D) + ?HR_TO_MICRO(Hour) + ?MIN_TO_MICRO(Min)
	+ ?SEC_TO_MICRO(Sec) + ?MILLI_TO_MICRO(Milli) + wh_util:to_integer(Micro),
    {match, SessSinceNum} = re:run(SessSince, "([\\d]+)", [{capture, [1], list}]),
    {match, Sess30Num} = re:run(Sess30, "([\\d]+)", [{capture, [1], list}]),
    {match, SessMaxNum} = re:run(SessMax, "([\\d]+)", [{capture, [1], list}]),
    {match, CPUNum} = re:run(CPU, "([\\d\.]+)", [{capture, [1], list}]),

    [{uptime, UpMicro}
     ,{sessions_since_startup, wh_util:to_integer(lists:flatten(SessSinceNum))}
     ,{sessions_per_thirty, wh_util:to_integer(lists:flatten(Sess30Num))}
     ,{sessions_max, wh_util:to_integer(lists:flatten(SessMaxNum))}
     ,{cpu, lists:flatten(CPUNum)}
    ].

process_custom_data(Data, AppVsn) ->
    case props:get_value(<<"Event-Subclass">>, Data) of
	undefined -> ok;
	<<"sofia::register">> -> publish_register_event(Data, AppVsn);
	_ -> ok
    end.

publish_register_event(Data, AppVsn) ->
    Keys = ?OPTIONAL_REG_SUCCESS_HEADERS ++ ?REG_SUCCESS_HEADERS,
    DefProp = wh_api:default_headers(<<>>, <<"directory">>, <<"reg_success">>, wh_util:to_binary(?MODULE), AppVsn),
    ApiProp = lists:foldl(fun(K, Api) ->
				  case props:get_value(wh_util:binary_to_lower(K), Data) of
				      undefined ->
					  case props:get_value(K, Data) of
					      undefined -> Api;
					      V -> [{K, V} | Api]
					  end;
				      V -> [{K, V} | Api]
				  end
			  end, [{<<"Event-Timestamp">>, round(wh_util:current_tstamp())} | DefProp], Keys),
    ?LOG("sending successful registration"),
    case wh_api:reg_success(ApiProp) of
	{error, E} ->
            ?LOG("failed API message creation: ~p", [E]);
	{ok, JSON} ->
	    amqp_util:callmgr_publish(JSON, <<"application/json">>, ?KEY_REG_SUCCESS)
    end.

get_originate_action(<<"transfer">>, Data) ->
    case wh_json:get_value(<<"Route">>, Data) of
	undefined -> <<"error">>;
	Route ->
	    list_to_binary([ <<"transfer(">>, wh_util:to_e164(Route), <<" XML context_2)">>])
    end;
get_originate_action(<<"bridge">>, Data) ->
    case ecallmgr_fs_xml:build_route(Data, wh_json:get_value(<<"Invite-Format">>, Data)) of
	{error, timeout} -> <<"error">>;
	EndPoint ->
	    CVs = ecallmgr_fs_xml:get_leg_vars(Data),
	    list_to_binary([<<"bridge(">>, CVs, EndPoint, <<")">>])
    end;
get_originate_action(_, _) ->
    <<"park">>.

-spec run_start_cmds/1 :: (atom()) -> [fs_api_ret(),...].
run_start_cmds(Node) ->
    [freeswitch:api(Node, wh_util:to_atom(ApiCmd, true), wh_util:to_list(ApiArg)) || {ApiCmd, ApiArg} <- ecallmgr_config:fetch(fs_cmds, [])].

-spec get_active_channels/1 :: (atom()) -> integer().
get_active_channels(Node) ->
    case freeswitch:api(Node, show, "channels") of
	{ok, Chans} ->
	    {ok, R} = re:compile("([\\d+])"),
	    {match, Match} = re:run(Chans, R, [{capture, [1], list}]),
	    wh_util:to_integer(lists:flatten(Match));
	_ ->
	    0
    end.

-spec convert_rows/2 :: (atom(), binary()) -> [proplist(),...] | [].
convert_rows(_, <<"\n0 total.\n">>) ->
    ?LOG("No channels up"),
    [];
convert_rows(Node, RowsBin) ->
    [_|Rows] = binary:split(RowsBin, <<"\n">>, [global]),
    return_rows(Node, Rows, []).

-spec return_rows/3 :: (atom(), [binary(),...] | [], [proplist(),...] | []) -> [proplist(),...] | [].
return_rows(Node, [<<>>|Rs], Acc) ->
    return_rows(Node, Rs, Acc);
return_rows(Node, [R|Rs], Acc) ->
    ?LOG("R: ~s", [R]),
    case binary:split(R, <<",">>) of
	[_Total] ->
	    ?LOG("Total: ~s", [_Total]),
	    return_rows(Node, Rs, Acc);
	[UUID|_] ->
	    ?LOG("UUID: ~s", [UUID]),
	    {ok, Dump} = freeswitch:api(Node, uuid_dump, wh_util:to_list(UUID)),
	    DumpProp = ecallmgr_util:eventstr_to_proplist(Dump),

	    %% Pull wanted data from the converted DUMP proplist
	    Prop = [{AMQPKey, props:get_value(FSKey, DumpProp)} || {FSKey, AMQPKey} <- ?CALL_STATUS_MAPPING],
	    return_rows(Node, Rs, [ Prop | Acc ])
    end;
return_rows(_Node, [], Acc) -> Acc.
