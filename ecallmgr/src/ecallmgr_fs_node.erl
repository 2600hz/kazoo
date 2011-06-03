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
-export([resource_consume/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("ecallmgr.hrl").

-record(state, {node = undefined :: atom()
	       ,stats = #node_stats{} :: #node_stats{}
	       ,options = [] :: proplist()
	       }).

-define(SERVER, ?MODULE).

-define(YR_TO_MICRO(Y), whistle_util:to_integer(Y)*365*24*3600*1000000).
-define(DAY_TO_MICRO(D), whistle_util:to_integer(D)*24*3600*1000000).
-define(HR_TO_MICRO(Hr), whistle_util:to_integer(Hr)*3600*1000000).
-define(MIN_TO_MICRO(Min), whistle_util:to_integer(Min)*60*1000000).
-define(SEC_TO_MICRO(Sec), whistle_util:to_integer(Sec)*1000000).
-define(MILLI_TO_MICRO(Mil), whistle_util:to_integer(Mil)*1000).

-define(FS_TIMEOUT, 5000).

-spec(resource_consume/3 :: (FsNodePid :: pid(), Route :: binary(), JObj :: json_object()) ->
				 tuple(resource_consumed, binary(), binary(), integer())
				     | tuple(resource_error, binary() | error)).
resource_consume(FsNodePid, Route, JObj) ->
    FsNodePid ! {resource_consume, self(), Route, JObj},
    receive Resp -> Resp
    after   10000 -> {resource_error, timeout}
    end.

-spec(start_link/1 :: (Node :: atom()) -> tuple(ok, pid()) | {error, term()}).
start_link(Node) ->
    gen_server:start_link(?SERVER, [Node, []], []).

-spec(start_link/2 :: (Node :: atom(), Options :: proplist()) -> tuple(ok, pid()) | {error, term()}).
start_link(Node, Options) ->
    gen_server:start_link(?SERVER, [Node, Options], []).

init([Node, Options]) ->
    logger:format_log(info, "FS_NODE(~p): Starting up~n", [self()]),
    Stats = #node_stats{started = erlang:now()},
    {ok, #state{node=Node, stats=Stats, options=Options}, 0}.

handle_call(_Req, _From, State) ->
    {reply, ok, State}.

handle_cast(_Req, State) ->
    {noreply, State}.

handle_info(timeout, #state{stats=Stats, node=Node}=State) ->
    erlang:monitor_node(Node, true),

    Type = register_event_handler,
    {foo, Node} ! Type,
    receive
	ok ->
	    NodeData = extract_node_data(Node),
	    {ok, Chans} = freeswitch:api(Node, show, "channels"),
	    {ok, R} = re:compile("([\\d+])"),
	    {match, Match} = re:run(Chans, R, [{capture, [1], list}]),
	    Active = whistle_util:to_integer(lists:flatten(Match)),

	    ok = freeswitch:event(Node, ['CHANNEL_CREATE', 'CHANNEL_DESTROY', 'HEARTBEAT', 'CHANNEL_HANGUP_COMPLETE'
					 ,'CUSTOM', 'sofia::register'
					]),

	    {noreply, State#state{stats=(Stats#node_stats{
					   created_channels = Active
					   ,fs_uptime = props:get_value(uptime, NodeData, 0)
					  })}};
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

handle_info({diagnostics, Pid}, #state{stats=Stats}=State) ->
    spawn(fun() -> diagnostics(Pid, Stats) end),
    {noreply, State};

handle_info({event, [undefined | Data]}, #state{stats=Stats}=State) ->
    EvtName = props:get_value(<<"Event-Name">>, Data),
    case EvtName of
	<<"HEARTBEAT">> ->
	    {noreply, State#state{stats=Stats#node_stats{last_heartbeat=erlang:now()}}};
	<<"CUSTOM">> ->
	    logger:format_log(info, "FS_NODE(~p): Evt: ~p~n", [self(), EvtName]),
	    spawn(fun() -> process_custom_data(Data, ?APP_VERSION) end),
	    {noreply, State};
	_ ->
	    logger:format_log(info, "FS_NODE(~p): Evt: ~p~n", [self(), EvtName]),
	    {noreply, State}
    end;

handle_info({event, [UUID | Data]}, #state{stats=#node_stats{created_channels=Cr, destroyed_channels=De}=Stats}=State) ->
    EvtName = props:get_value(<<"Event-Name">>, Data),
    logger:format_log(info, "FS_NODE(~p): Evt: ~p UUID: ~p~n", [self(), EvtName, UUID]),
    case EvtName of
	<<"CHANNEL_CREATE">> ->
	    {noreply, State#state{stats=Stats#node_stats{created_channels=Cr+1}}};
	<<"CHANNEL_DESTROY">> ->
	    ChanState = props:get_value(<<"Channel-State">>, Data),
	    case ChanState of
		<<"CS_NEW">> -> % ignore
		    {noreply, State};
		<<"CS_DESTROY">> ->
		    {noreply, State#state{stats=Stats#node_stats{destroyed_channels=De+1}}}
	    end;
	<<"CHANNEL_HANGUP_COMPLETE">> ->
	    {noreply, State};
	<<"CUSTOM">> ->
	    spawn(fun() -> process_custom_data(Data, ?APP_VERSION) end),
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
    {noreply, State#state{options=NewOptions}};

handle_info(_Msg, State) ->
    logger:format_log(info, "FS_NODE(~p): Recv unexpected ~p~n", [self(), _Msg]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


-spec(originate_channel/5 :: (Node :: atom(), Pid :: pid(), Route :: binary() | list(), AvailChan :: integer(), JObj :: json_object()) -> no_return()).
originate_channel(Node, Pid, Route, AvailChan, JObj) ->
    logger:format_log(info, "FS_NODE(~p): DS ~p~n", [self(), Route]),
    Action = get_originate_action(wh_json:get_value(<<"Application-Name">>, JObj), wh_json:get_value(<<"Application-Data">>, JObj)),

    OrigStr = binary_to_list(list_to_binary(["sofia/sipinterface_1/", Route, " &", Action])),
    logger:format_log(info, "FS_NODE(~p): Orig ~p~n", [self(), OrigStr]),
    case freeswitch:api(Node, originate, OrigStr, 9000) of
	{ok, X} ->
	    logger:format_log(info, "FS_NODE(~p): Originate to ~p resulted in ~p~n", [self(), Route, X]),
	    CallID = erlang:binary_part(X, {4, byte_size(X)-5}),
	    CtlQ = start_call_handling(Node, CallID),
	    Pid ! {resource_consumed, CallID, CtlQ, AvailChan-1};
	{error, Y} ->
	    ErrMsg = erlang:binary_part(Y, {5, byte_size(Y)-6}),
	    logger:format_log(info, "FS_NODE(~p): Failed to originate ~p: ~p~n", [self(), Route, ErrMsg]),
	    Pid ! {resource_error, ErrMsg};
	timeout ->
	    logger:format_log(info, "FS_NODE(~p): Originate to ~p timed out~n", [self(), Route]),
	    Pid ! {resource_error, timeout}
    end.

-spec(start_call_handling/2 :: (Node :: atom(), UUID :: binary()) -> CtlQueue :: binary()).
start_call_handling(Node, UUID) ->
    CtlQueue = amqp_util:new_callctl_queue(<<>>),
    amqp_util:bind_q_to_callctl(CtlQueue),

    {ok, CtlPid} = ecallmgr_call_sup:start_control_process(Node, UUID, CtlQueue),
    {ok, _} = ecallmgr_call_sup:start_event_process(Node, UUID, CtlPid),
    CtlQueue.

-spec(diagnostics/2 :: (Pid :: pid(), Stats :: tuple()) -> no_return()).
diagnostics(Pid, Stats) ->
    Resp = ecallmgr_diagnostics:get_diagnostics(Stats),
    Pid ! Resp.

channel_request(Pid, FSHandlerPid, AvailChan, Utilized, MinReq) ->
    logger:format_log(info, "FS_NODE(~p): Avail: ~p MinReq: ~p~n", [self(), AvailChan, MinReq]),
    case MinReq > AvailChan of
	true -> Pid ! {resource_response, FSHandlerPid, []};
	false -> Pid ! {resource_response, FSHandlerPid, [{node, FSHandlerPid}
							  ,{available_channels, AvailChan}
							  ,{percent_utilization, Utilized}
							 ]}
    end.

-spec(extract_node_data/1 :: (Node :: atom()) -> proplist()).
extract_node_data(Node) ->
    {ok, Status} = freeswitch:api(Node, status),
    Lines = string:tokens(whistle_util:to_list(Status), [$\n]),
    process_status(Lines).

-spec(process_status/1 :: (Lines :: list()) -> proplist()).
process_status(["UP " ++ Uptime, SessSince, Sess30, SessMax, CPU]) ->
    {match, [[Y],[D],[Hour],[Min],[Sec],[Milli],[Micro]]} = re:run(Uptime, "([\\d]+)", [{capture, [1], list}, global]),
    UpMicro = ?YR_TO_MICRO(Y) + ?DAY_TO_MICRO(D) + ?HR_TO_MICRO(Hour) + ?MIN_TO_MICRO(Min)
	+ ?SEC_TO_MICRO(Sec) + ?MILLI_TO_MICRO(Milli) + whistle_util:to_integer(Micro),
    {match, SessSinceNum} = re:run(SessSince, "([\\d]+)", [{capture, [1], list}]),
    {match, Sess30Num} = re:run(Sess30, "([\\d]+)", [{capture, [1], list}]),
    {match, SessMaxNum} = re:run(SessMax, "([\\d]+)", [{capture, [1], list}]),
    {match, CPUNum} = re:run(CPU, "([\\d\.]+)", [{capture, [1], list}]),

    [{uptime, UpMicro}
     ,{sessions_since_startup, whistle_util:to_integer(lists:flatten(SessSinceNum))}
     ,{sessions_per_thirty, whistle_util:to_integer(lists:flatten(Sess30Num))}
     ,{sessions_max, whistle_util:to_integer(lists:flatten(SessMaxNum))}
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
    DefProp = whistle_api:default_headers(<<>>, <<"directory">>, <<"reg_success">>, whistle_util:to_binary(?MODULE), AppVsn),
    ApiProp = lists:foldl(fun(K, Api) ->
				  case props:get_value(whistle_util:binary_to_lower(K), Data) of
				      undefined ->
					  case props:get_value(K, Data) of
					      undefined -> Api;
					      V -> [{K, V} | Api]
					  end;
				      V -> [{K, V} | Api]
				  end
			  end, [{<<"Event-Timestamp">>, round(calendar:datetime_to_gregorian_seconds(calendar:local_time()))} | DefProp], Keys),
    case whistle_api:reg_success(ApiProp) of
	{error, E} -> logger:format_log(error, "FS_NODE.pub_reg_event: Failed API message creation: ~p~n", [E]);
	{ok, JSON} ->
	    logger:format_log(info, "FS_NODE.p_reg_evt(~p): ~s~n", [self(), JSON]),
	    amqp_util:callmgr_publish(JSON, <<"application/json">>, ?KEY_REG_SUCCESS)
    end.

get_originate_action(<<"transfer">>, Data) ->
    case wh_json:get_value(<<"To-User">>, Data) of
	undefined -> <<"error">>;
	User ->
	    list_to_binary([ <<"loopback/">>, wh_util:to_e164(User), <<"@">>, wh_json:get_value(<<"To-Realm">>, Data) ])
    end;
get_originate_action(<<"bridge">>, Data) ->
    case ecallmgr_fs_xml:build_route(Data, wh_json:get_value(<<"Invite-Format">>, Data)) of
	{error, timeout} -> <<"error">>;
	EndPoint ->
	    CVs = ecallmgr_fs_xml:get_leg_vars(Data),
	    whistle_util:to_list(list_to_binary([CVs, EndPoint]))
    end;
get_originate_action(_, _) ->
    <<"park">>.
