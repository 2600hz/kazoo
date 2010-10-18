%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.com>
%%% @copyright (C) 2010, James Aimonetti
%%% @doc
%%% Receive dialplan bindings from FreeSWITCH, search for a match,
%%% and create call ctl and evt queues.
%%% @end
%%% Created : 24 Aug 2010 by James Aimonetti <james@2600hz.com>
%%%-------------------------------------------------------------------
-module(ecallmgr_fs_route).

%% API
-export([start_handler/2]).
-export([fetch_route/2, lookup_route/6]).

-import(props, [get_value/2, get_value/3]).
-import(logger, [log/2, format_log/3]).

-include("../include/amqp_client/include/amqp_client.hrl").
-include("freeswitch_xml.hrl").
-include("whistle_api.hrl").

-record(handler_stats, {lookups_success = 0 :: integer()
			,lookups_failed = 0 :: integer()
                        ,lookups_timeout = 0 :: integer()
                        ,lookups_requested = 0 :: integer()
		       }).

-record(handler_state, {fs_node :: atom()
			,amqp_host = "" :: string()
			,app_vsn :: binary()
			,stats = #handler_stats{} :: tuple()
			,lookups = [] :: list(tuple(pid(), binary(), tuple(integer(), integer(), integer())))
		       }).

-spec(start_handler/2 :: (Node :: atom(), Host :: string()) -> pid()).
start_handler(Node, Host) ->
    {ok, Vsn} = application:get_key(ecallmgr, vsn),
    HState = #handler_state{fs_node=Node, app_vsn=list_to_binary(Vsn), amqp_host=Host},
    {ok, RPid} = freeswitch:start_fetch_handler(Node, dialplan, ?MODULE, fetch_route, HState),
    RPid.

fetch_route(Node, #handler_state{lookups=LUs, stats=Stats, amqp_host=Host}=State) ->
    receive
	{fetch, dialplan, _Tag, _Key, _Value, ID, [UUID | Data]} ->
	    case get_value(<<"Event-Name">>, Data) of
		<<"REQUEST_PARAMS">> ->
		    Self = self(),
		    LookupPid = spawn_link(?MODULE, lookup_route, [Node, State, ID, UUID, Self, Data]),
		    LookupsReq = Stats#handler_stats.lookups_requested + 1,
		    format_log(info, "FETCH_ROUTE(~p): fetch route: Id: ~p UUID: ~p Lookup: ~p Req#: ~p~n"
			       ,[self(), ID, UUID, LookupPid, LookupsReq]),
		    ?MODULE:fetch_route(Node, State#handler_state{lookups=[{LookupPid, ID, erlang:now()}|LUs]
								  ,stats=Stats#handler_stats{lookups_requested=LookupsReq}});
		_Other ->
		    format_log(info, "FETCH_ROUTE(~p): Ignoring event ~p~n", [self(), _Other]),
		    ?MODULE:fetch_route(Node, State)
	    end;
	{fetch, _Section, _Something, _Key, _Value, ID, [undefined | _Data]} ->
	    format_log(info, "FETCH_ROUTE(~p): fetch unknown: Se: ~p So: ~p, K: ~p V: ~p ID: ~p~nD: ~p~n", [self(), _Section, _Something, _Key, _Value, ID, _Data]),
	    freeswitch:fetch_reply(Node, ID, ?EMPTYRESPONSE),
	    ?MODULE:fetch_route(Node, State);
	{nodedown, Node} ->
	    format_log(error, "FETCH_ROUTE(~p): Node ~p exited", [self(), Node]),
	    ok;
	{xml_response, ID, XML} ->
	    format_log(info, "FETCH_ROUTE(~p): Received XML for ID ~p~n", [self(), ID]),
	    freeswitch:fetch_reply(Node, ID, XML),
	    ?MODULE:fetch_route(Node, State);
	shutdown ->
	    lists:foreach(fun({Pid,_StartTime}) ->
				  case erlang:is_process_alive(Pid) of
				      true -> Pid ! shutdown;
				      false -> ok
				  end
			  end, LUs),
	    format_log(error, "FETCH_ROUTE(~p): shutting down~n", [self()]);
	{lookup_finished, LookupPid, EndResult} ->
	    close_lookup(LookupPid, Node, State, EndResult);
	%% send diagnostic info
	{diagnostics, Pid} ->
	    ActiveLUs = lists:map(fun({_LuPid, ID, Started}) -> [{fs_route_id, ID}, {started, Started}] end, LUs),
	    Resp = [{active_lookups, ActiveLUs}
		    ,{lookups_success, Stats#handler_stats.lookups_success}
		    ,{lookups_failed, Stats#handler_stats.lookups_failed}
		    ,{lookups_timeout, Stats#handler_stats.lookups_timeout}
		    ,{lookups_requested, Stats#handler_stats.lookups_requested}
		    ,{amqp_host, Host}
		   ],
	    Pid ! Resp,
	    ?MODULE:fetch_route(Node, State);
	Other ->
	    format_log(info, "FETCH_ROUTE(~p): got other response: ~p", [self(), Other]),
	    ?MODULE:fetch_route(Node, State)
    end.

close_lookup(LookupPid, Node, #handler_state{lookups=LUs, stats=Stats}=State, EndResult) ->
    case lists:keyfind(LookupPid, 1, LUs) of
	{LookupPid, ID, StartTime} ->
	    RunTime = timer:now_diff(erlang:now(), StartTime) div 1000,
	    format_log(info, "FETCH_ROUTE(~p): lookup (~p:~p) finished in ~p ms~n", [self(), LookupPid, ID, RunTime]),
	    Stats1 = case EndResult of
			 success -> Stats#handler_stats{lookups_success=Stats#handler_stats.lookups_success+1};
			 failed -> Stats#handler_stats{lookups_failed=Stats#handler_stats.lookups_failed+1};
			 timeout -> Stats#handler_stats{lookups_timeout=Stats#handler_stats.lookups_timeout+1}
		     end,
	    ?MODULE:fetch_route(Node, State#handler_state{lookups=lists:keydelete(LookupPid, 1, LUs), stats=Stats1});
	false ->
	    format_log(error, "FETCH_ROUTE(~p): unknown lookup ~p~n", [self(), LookupPid]),
	    ?MODULE:fetch_route(Node, State)
    end.

-spec(lookup_route/6 :: (Node :: atom(), HState :: tuple(), ID :: binary(), UUID :: binary(), FetchPid :: pid(), Data :: proplist()) ->
			     no_return()).
lookup_route(Node, #handler_state{amqp_host=Host, app_vsn=Vsn}=HState, ID, UUID, FetchPid, Data) ->
    Q = bind_q(Host, ID),
    {EvtQ, CtlQ} = bind_channel_qs(Host, UUID, Node),

    DefProp = [{<<"Msg-ID">>, ID}
	       ,{<<"Caller-ID-Name">>, get_value(<<"Caller-Caller-ID-Name">>, Data)}
	       ,{<<"Caller-ID-Number">>, get_value(<<"Caller-Caller-ID-Number">>, Data)}
	       ,{<<"To">>, ecallmgr_util:get_sip_to(Data)}
	       ,{<<"From">>, ecallmgr_util:get_sip_from(Data)}
	       ,{<<"Call-ID">>, UUID}
	       ,{<<"Event-Queue">>, EvtQ}
	       ,{<<"Custom-Channel-Vars">>, {struct, ecallmgr_util:custom_channel_vars(Data)}}
	       | whistle_api:default_headers(Q, <<"dialplan">>, <<"route_req">>, <<"ecallmgr.route">>, Vsn)],
    EndResult = case whistle_api:route_req(DefProp) of
		    {ok, JSON} ->
			format_log(info, "L/U.route(~p): Sending RouteReq JSON to Host(~p)~n", [self(), Host]),
			send_request(Host, JSON),
			Result = handle_response(ID, UUID, EvtQ, CtlQ, HState, FetchPid),
			amqp_util:queue_delete(Host, Q),
			Result;
		    {error, _Msg} ->
			format_log(error, "L/U.route(~p): Route Req API error ~p~n", [self(), _Msg]),
			failed
		end,
    FetchPid ! {lookup_finished, self(), EndResult}.

send_request(Host, JSON) ->
    amqp_util:broadcast_publish(Host, JSON, <<"application/json">>).

recv_response(ID) ->
    receive
	#'basic.consume_ok'{} ->
	    recv_response(ID);
	{_, #amqp_msg{props = Props, payload = Payload}} ->
	    format_log(info, "L/U.route(~p): Recv Content: ~p Payload: ~s~n", [self(), Props#'P_basic'.content_type, binary_to_list(Payload)]),
	    {struct, Prop} = mochijson2:decode(binary_to_list(Payload)),
	    case get_value(<<"Msg-ID">>, Prop) of
		ID ->
		    case whistle_api:route_resp_v(Prop) of
			true -> Prop;
			false ->
			    format_log(error, "L/U.route(~p): Invalid Route Resp~n~p~n", [self(), Prop]),
			    invalid_route_resp
		    end;
		_BadId ->
		    format_log(info, "L/U.route(~p): Recv MsgID ~p when expecting ~p~n", [self(), _BadId, ID]),
		    recv_response(ID)
	    end;
	shutdown -> shutdown;
	_Msg ->
	    format_log(info, "L/U.route(~p): Unexpected: received ~p off rabbit~n", [self(), _Msg]),
	    recv_response(ID)
    after 4000 ->
	    format_log(info, "L/U.route(~p): Failed to receive after 4000ms~n", [self()]),
	    timeout
    end.

bind_q(AmqpHost, ID) ->
    amqp_util:targeted_exchange(AmqpHost),
    amqp_util:broadcast_exchange(AmqpHost),
    Queue = amqp_util:new_targeted_queue(AmqpHost, ID),
    amqp_util:bind_q_to_targeted(AmqpHost, Queue),
    amqp_util:basic_consume(AmqpHost, Queue, self()),
    Queue.

%% creates the event and control queues for the call, spins up the event handler
%% to pump messages to the queue, and returns the control queue
bind_channel_qs(Host, UUID, Node) ->
    amqp_util:callevt_exchange(Host),
    amqp_util:callctl_exchange(Host),

    EvtQueue = amqp_util:new_callevt_queue(Host, UUID),
    CtlQueue = amqp_util:new_callctl_queue(Host, UUID),

    amqp_util:bind_q_to_callevt(Host, EvtQueue),
    amqp_util:bind_q_to_callctl(Host, CtlQueue),

    CtlPid = ecallmgr_call_control:start(Node, UUID, {Host, CtlQueue}),
    ecallmgr_call_events:start(Node, UUID, {Host, EvtQueue}, CtlPid),
    {EvtQueue, CtlQueue}.

send_control_queue(_Host, _Q, undefined) ->
    format_log(error, "L/U.route(~p): Cannot send control Q(~p) to undefined server-id~n", [self(), _Q]),
    failed;
send_control_queue(Host, CtlProp, AppQ) ->
    case whistle_api:route_win(CtlProp) of
	{ok, JSON} ->
	    amqp_util:targeted_publish(Host, AppQ, JSON, <<"application/json">>),
	    %% execute the publish command
	    format_log(info, "L/U.route(~p): Sending AppQ(~p) the control Q~n", [self(), AppQ]),
	    success;
	{error, _Msg} ->
	    format_log(error, "L/U.route(~p): Sending Ctl to AppQ(~p) failed: ~p~n", [self(), AppQ, _Msg]),
	    failed
    end.

%% Prop = Route Response
generate_xml(<<"bridge">>, Routes, _Prop) ->
    format_log(info, "L/U.route(~p): BRIDGEXML: Routes:~n~p~n", [self(), Routes]),
    %% format the Route based on protocol
    {_Idx, Extensions} = lists:foldl(fun({struct, RouteProp}, {Idx, Acc}) ->
					     Route = get_value(<<"Route">>, RouteProp), %% translate Route to FS-encoded URI
					     BypassMedia = case get_value(<<"Media">>, RouteProp) of
							       <<"bypass">> -> "true";
							       <<"process">> -> "false";
							       _ -> "true" %% auto?
							   end,
					     ChannelVars = get_channel_vars(RouteProp),
					     Ext = io_lib:format(?ROUTE_BRIDGE_EXT, [Idx, BypassMedia, ChannelVars, Route]),
					     {Idx+1, [Ext | Acc]}
				     end, {1, ""}, lists:reverse(Routes)),
    format_log(info, "L/U.route(~p): RoutesXML: ~s~n", [self(), Extensions]),
    lists:flatten(io_lib:format(?ROUTE_BRIDGE_RESPONSE, [Extensions]));
generate_xml(<<"park">>, _Routes, _Prop) ->
    ?ROUTE_PARK_RESPONSE;
generate_xml(<<"error">>, _Routes, Prop) ->
    ErrCode = get_value(<<"Route-Error-Code">>, Prop),
    ErrMsg = list_to_binary([" ", get_value(<<"Route-Error-Message">>, Prop, <<"">>)]),
    format_log(info, "L/U.route(~p): ErrorXML: ~s ~s~n", [self(), ErrCode, ErrMsg]),
    lists:flatten(io_lib:format(?ROUTE_ERROR_RESPONSE, [ErrCode, ErrMsg])).

get_channel_vars(Prop) ->
    Vars = lists:foldr(fun get_channel_vars/2, [], Prop),
    lists:flatten(["{", string:join(lists:map(fun binary_to_list/1, Vars), ","), "}"]).

get_channel_vars({<<"Auth-User">>, V}, Vars) ->
    [ list_to_binary(["sip_auth_username='", V, "'"]) | Vars];
get_channel_vars({<<"Auth-Password">>, V}, Vars) ->
    [ list_to_binary(["sip_auth_password='", V, "'"]) | Vars];
get_channel_vars({<<"Caller-ID-Name">>, V}, Vars) ->
    [ list_to_binary(["origination_caller_id_name='", V, "'"]) | Vars];
get_channel_vars({<<"Caller-ID-Number">>, V}, Vars) ->
    [ list_to_binary(["origination_caller_id_number='", V, "'"]) | Vars];
get_channel_vars({<<"Caller-ID-Type">>, <<"from">>}, Vars) ->
    [ <<"sip_cid_type=none">> | Vars];
get_channel_vars({<<"Caller-ID-Type">>, <<"rpid">>}, Vars) ->
    [ <<"sip_cid_type=rpid">> | Vars];
get_channel_vars({<<"Caller-ID-Type">>, <<"pid">>}, Vars) ->
    [ <<"sip_cid_type=pid">> | Vars];
get_channel_vars({<<"Codecs">>, Cs}, Vars) ->
    Codecs = lists:map(fun binary_to_list/1, Cs),
    CodecStr = string:join(Codecs, ","),
    [ list_to_binary(["absolute_codec_string='", CodecStr, "'"]) | Vars];
get_channel_vars({_K, _V}, Vars) ->
    format_log(info, "L/U.route(~p): Unknown channel var ~s::~s~n", [self(), _K, _V]),
    Vars.

handle_response(ID, UUID, EvtQ, CtlQ, #handler_state{amqp_host=Host, app_vsn=Vsn}, FetchPid) ->
    T1 = erlang:now(),
    case recv_response(ID) of
	shutdown ->
	    format_log(error, "L/U.route(~p): Shutting down for ID ~p~n", [self(), ID]),
	    failed;
	timeout ->
	    FetchPid ! {xml_response, ID, ?ROUTE_NOT_FOUND_RESPONSE},
	    timeout;
	invalid_route_resp ->
	    FetchPid ! {xml_response, ID, ?ROUTE_NOT_FOUND_RESPONSE},
	    failed;
	Prop ->
	    Xml = generate_xml(get_value(<<"Method">>, Prop), get_value(<<"Routes">>, Prop), Prop),
	    format_log(info, "L/U.route(~p): Sending XML to FS(~p) took ~pms ~n", [self(), ID, timer:now_diff(erlang:now(), T1) div 1000]),
	    FetchPid ! {xml_response, ID, Xml},

	    CtlProp = [{<<"Msg-ID">>, UUID}
		       ,{<<"Call-ID">>, UUID}
		       ,{<<"Event-Queue">>, EvtQ}
		       ,{<<"Control-Queue">>, CtlQ}
		       | whistle_api:default_headers(CtlQ, <<"dialplan">>, <<"route_win">>, <<"ecallmgr.route">>, Vsn)],
	    send_control_queue(Host, CtlProp
			       ,get_value(<<"Destination-Server">>, Prop, get_value(<<"Server-ID">>, Prop)))
    end.
