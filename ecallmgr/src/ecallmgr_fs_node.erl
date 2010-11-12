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
		       ,app_vsn = [] :: binary()
		       ,stats = #node_stats{} :: tuple()
		       ,options = [] :: proplist()
		       }).

-spec(start_handler/3 :: (Node :: atom(), Options :: proplist(), AmqpHost :: string()) -> pid() | {error, term()}).
start_handler(Node, Options, AmqpHost) ->
    {ok, Vsn} = application:get_key(ecallmgr, vsn),
    Stats = #node_stats{started = erlang:now()},
    HState = #handler_state{fs_node=Node, amqp_host=AmqpHost, app_vsn=list_to_binary(Vsn), stats=Stats, options=Options},
    case freeswitch:start_event_handler(Node, ?MODULE, monitor_node, HState) of
	{ok, Pid} -> Pid;
	timeout -> {error, timeout};
	{error, _Err}=E -> E
    end.

monitor_node(Node, #handler_state{}=S) ->
    %% do init
    freeswitch:event(Node, ['CHANNEL_CREATE', 'CHANNEL_DESTROY', 'HEARTBEAT']),
    monitor_loop(Node, S).

monitor_loop(Node, #handler_state{stats=#node_stats{created_channels=Cr, destroyed_channels=De}=Stats}=S) ->
    receive
	{diagnostics, Pid} ->
	    Resp = ecallmgr_diagnostics:get_diagnostics(Stats),
	    Pid ! Resp,
	    monitor_loop(Node, S);
	{event, [undefined | Data]} ->
	    EvtName = get_value(<<"Event-Name">>, Data),
	    format_log(info, "FS_NODE(~p): Evt: ~p~n", [self(), EvtName]),
	    case EvtName of
		<<"HEARTBEAT">> -> monitor_loop(Node, S#handler_state{stats=Stats#node_stats{last_heartbeat=erlang:now()}});
		_ -> monitor_loop(Node, S)
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
		_ -> monitor_loop(Node, S)
	    end;
	Msg ->
	    format_log(info, "FS_NODE(~p): Recv ~p~n", [self(), Msg]),
	    monitor_loop(Node, S)
    end.
