%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2010, James Aimonetti
%%% @doc
%%% Manage a FreeSWITCH node and its resources
%%% @end
%%% Created : 11 Nov 2010 by James Aimonetti <james@2600hz.org>

-module(ecallmgr_fs_node).

%% API
-export([start_handler/3]).
-export([monitor_node/2]).

-import(props, [get_value/2, get_value/3]).
-import(logger, [log/2, format_log/3]).

-include("../include/amqp_client/include/amqp_client.hrl").
-include("freeswitch_xml.hrl").
-include("whistle_api.hrl").
-include("whistle_amqp.hrl").
-include("ecallmgr.hrl").

%% lookups = [{LookupPid, ID, erlang:now()}]
-record(handler_state, {fs_node = undefined :: atom()
		       ,amqp_host = "" :: string()
		       ,app_vsn = <<>> :: binary()
		       ,stats = #node_stats{} :: #node_stats{}
		       ,options = [] :: proplist()
		       }).

-define(YR_TO_MICRO(Y), whistle_util:to_integer(Y)*365*24*3600*1000000).
-define(DAY_TO_MICRO(D), whistle_util:to_integer(D)*24*3600*1000000).
-define(HR_TO_MICRO(Hr), whistle_util:to_integer(Hr)*3600*1000000).
-define(MIN_TO_MICRO(Min), whistle_util:to_integer(Min)*60*1000000).
-define(SEC_TO_MICRO(Sec), whistle_util:to_integer(Sec)*1000000).
-define(MILLI_TO_MICRO(Mil), whistle_util:to_integer(Mil)*1000).

-spec(start_handler/3 :: (Node :: atom(), Options :: proplist(), AmqpHost :: string()) -> pid() | {error, term()}).
start_handler(Node, Options, AmqpHost) ->
    NodeData = extract_node_data(Node),
    {ok, Chans} = freeswitch:api(Node, show, "channels"),
    {ok, R} = re:compile("([\\d+])"),
    {match, Match} = re:run(Chans, R, [{capture, [1], list}]),
    Active = whistle_util:to_integer(lists:flatten(Match)),

    {ok, Vsn} = application:get_key(ecallmgr, vsn),
    Stats = #node_stats{started = erlang:now()
			,created_channels=Active
			,fs_uptime=get_value(uptime, NodeData, 0)
		       },
    HState = #handler_state{fs_node=Node, amqp_host=AmqpHost, app_vsn=list_to_binary(Vsn), stats=Stats, options=Options},
    case freeswitch:start_event_handler(Node, ?MODULE, monitor_node, HState) of
	{ok, Pid} -> Pid;
	timeout -> {error, timeout};
	{error, _Err}=E -> E
    end.

-spec(monitor_node/2 :: (Node :: atom(), S :: tuple()) -> no_return()).
monitor_node(Node, #handler_state{}=S) ->
    %% do init
    erlang:monitor_node(Node, true),
    freeswitch:event(Node, ['CHANNEL_CREATE', 'CHANNEL_DESTROY', 'HEARTBEAT', 'CHANNEL_HANGUP_COMPLETE'
			    ,'CUSTOM', 'sofia::register', 'sofia::register_attempt']),
    monitor_loop(Node, S).

monitor_loop(Node, #handler_state{stats=#node_stats{created_channels=Cr, destroyed_channels=De}=Stats, options=Opts}=S) ->
    receive
	{diagnostics, Pid} ->
	    spawn(fun() -> diagnostics(Pid, Stats) end),
	    monitor_loop(Node, S);
	{event, [undefined | Data]} ->
	    EvtName = get_value(<<"Event-Name">>, Data),
	    format_log(info, "FS_NODE(~p): Evt: ~p~n", [self(), EvtName]),
	    case EvtName of
		<<"HEARTBEAT">> ->
		    monitor_loop(Node, S#handler_state{stats=Stats#node_stats{last_heartbeat=erlang:now()}});
		<<"CUSTOM">> ->
		    spawn(fun() -> process_custom_data(Data, S#handler_state.amqp_host, S#handler_state.app_vsn) end),
		    monitor_loop(Node, S);
		_ ->
		    monitor_loop(Node, S)
	    end;
	{event, [UUID | Data]} ->
	    EvtName = get_value(<<"Event-Name">>, Data),
	    format_log(info, "FS_NODE(~p): Evt: ~p UUID: ~p~n", [self(), EvtName, UUID]),
	    case EvtName of
		<<"CHANNEL_CREATE">> -> monitor_loop(Node, S#handler_state{stats=Stats#node_stats{created_channels=Cr+1}});
		<<"CHANNEL_DESTROY">> ->
		    ChanState = get_value(<<"Channel-State">>, Data),
		    case ChanState of
			<<"CS_NEW">> -> % ignore
			    monitor_loop(Node, S);
			<<"CS_DESTROY">> ->
			    monitor_loop(Node, S#handler_state{stats=Stats#node_stats{destroyed_channels=De+1}})
		    end;
		<<"CHANNEL_HANGUP_COMPLETE">> ->
		    monitor_loop(Node, S);
		_ -> monitor_loop(Node, S)
	    end;
	{resource_request, Pid, <<"audio">>, ChanOptions} ->
	    ActiveChan = Cr - De,
	    MaxChan = get_value(max_channels, Opts),
	    AvailChan =  MaxChan - ActiveChan,
	    Utilized =  round(ActiveChan / MaxChan * 100),

	    MinReq = get_value(min_channels_requested, ChanOptions),
	    FSHandlerPid = self(),
	    spawn(fun() ->channel_request(Pid, FSHandlerPid, AvailChan, Utilized, MinReq) end),
	    monitor_loop(Node, S);
	{resource_consume, Pid, Route} ->
	    ActiveChan = Cr - De,
	    MaxChan = get_value(max_channels, Opts, 1),
	    AvailChan =  MaxChan - ActiveChan,

	    spawn(fun() -> originate_channel(Node, S#handler_state.amqp_host, Pid, Route, AvailChan) end),
	    monitor_loop(Node, S);
	Msg ->
	    format_log(info, "FS_NODE(~p): Recv ~p~n", [self(), Msg]),
	    monitor_loop(Node, S)
    end.

-spec(originate_channel/5 :: (Node :: atom(), Host :: string(), Pid :: pid(), Route :: binary() | list(), AvailChan :: integer()) -> no_return()).
originate_channel(Node, Host, Pid, Route, AvailChan) ->
    DS = ecallmgr_util:route_to_dialstring(Route, ?DEFAULT_DOMAIN, Node), %% need to update this to be configurable
    format_log(info, "FS_NODE(~p): DS ~p~n", [self(), DS]),
    OrigStr = binary_to_list(list_to_binary(["sofia/sipinterface_1/", DS, " &park"])),
    format_log(info, "FS_NODE(~p): Orig ~p~n", [self(), OrigStr]),
    case freeswitch:api(Node, originate, OrigStr, 10000) of
	{ok, X} ->
	    format_log(info, "FS_NODE(~p): Originate to ~p resulted in ~p~n", [self(), DS, X]),
	    CallID = erlang:binary_part(X, {4, byte_size(X)-5}),
	    CtlQ = start_call_handling(Node, Host, CallID),
	    Pid ! {resource_consumed, CallID, CtlQ, AvailChan-1};
	{error, Y} ->
	    ErrMsg = erlang:binary_part(Y, {5, byte_size(Y)-6}),
	    format_log(info, "FS_NODE(~p): Failed to originate ~p: ~p~n", [self(), DS, ErrMsg]),
	    Pid ! {resource_error, ErrMsg};
	timeout ->
	    format_log(info, "FS_NODE(~p): Originate to ~p timed out~n", [self(), DS]),
	    Pid ! {resource_error, timeout}
    end.

-spec(start_call_handling/3 :: (Node :: atom(), Host :: string(), UUID :: binary()) -> CtlQueue :: binary()).
start_call_handling(Node, Host, UUID) ->
    CtlQueue = amqp_util:new_callctl_queue(Host, <<>>),
    amqp_util:bind_q_to_callctl(Host, CtlQueue),
    {ok, CtlPid} = ecallmgr_call_sup:start_control_process(Node, UUID, {Host, CtlQueue}),
    ecallmgr_call_sup:start_event_process(Node, UUID, Host, CtlPid),
    CtlQueue.

-spec(diagnostics/2 :: (Pid :: pid(), Stats :: tuple()) -> no_return()).
diagnostics(Pid, Stats) ->
    Resp = ecallmgr_diagnostics:get_diagnostics(Stats),
    Pid ! Resp.

channel_request(Pid, FSHandlerPid, AvailChan, Utilized, MinReq) ->
    format_log(info, "FS_NODE(~p): Avail: ~p MinReq: ~p~n", [self(), AvailChan, MinReq]),
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
process_status([[$U,$P, $  | Uptime], SessSince, Sess30, SessMax, CPU]) ->
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

process_custom_data(Data, Host, AppVsn) ->
    case get_value(<<"Event-Subclass">>, Data) of
	undefined -> ok;
	<<"sofia::register">> -> publish_register_event(Data, Host, AppVsn);
	_ -> ok
    end.

publish_register_event(Data, Host, AppVsn) ->
    Keys = ?OPTIONAL_REG_SUCCESS_HEADERS ++ ?REG_SUCCESS_HEADERS,
    DefProp = whistle_api:default_headers(<<>>, <<"directory">>, <<"reg_success">>, ?MODULE, AppVsn),
    ApiProp = lists:foldl(fun(K, Api) ->
				  Lk = binary_to_lower(K),
				  case props:get_value(Lk, Data) of
				      undefined -> Api;
				      V -> [{K, V} | Api]
				  end
			  end, DefProp, Keys),
    case whistle_api:reg_sucess(ApiProp) of
	{error, E} -> format_log(error, "FS_NODE.custom_data: Failed API message creation: ~p~n", [E]);
	{ok, JSON} ->
	    amqp_util:broadcast_publish(Host, JSON, <<"application/json">>)
    end.

-spec(binary_to_lower/1 :: (B :: binary()) -> binary()).
binary_to_lower(B) when is_binary(B) ->
    whistle_util:to_binary(string:to_lower(whistle_util:to_list(B))).
