%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.com>
%%% @copyright (C) 2010, James Aimonetti
%%% @doc
%%% When a device tries to register with a known FS node, FS sends a
%%% fetch request to the corresponding fetch_handler (a process spawned
%%% during the call to add_fs_node/1). The fetch_handler checks to see
%%% that the fetch request is one the handler can, you know, handle
%%% and if so, spawns a call to lookup_user/4. The newly spawned process
%%% creates an Authentication Request API message, as well as a queue on
%%% the Targeted Exchange, and places the AuthReq onto the Broadcast
%%% Exchange. Lookup_user then waits, up to a threshold, for an AuthResp
%%% message. If a timeout occurs, an empty response is returned to the
%%% fetch_handler; otherwise, lookup_user tries to create the appropriate
%%% XML response to pass back to the fetch_handler.
%%% Upon receiving the XML, fetch_handler sends the XML to the FS node,
%%% and goes back to waiting for another fetch request. The lookup_user
%%% process ends after sending the XML and cleaning up.
%%%
%%%                 ---------------
%%%                 |ecallmgr_auth|
%%%                 ---------------
%%%                        |
%%%             -------------------------
%%%             |                       |
%%%     ---------------           ---------------
%%%     |fetch_handler|           |fetch_handler|
%%%     |(per FS Node)|           |(per FS Node)|
%%%     ---------------           ---------------
%%%            |                        |
%%%     ---------------          ---------------
%%%     |      |      |          |      |      |
%%%   -----  -----  -----      -----  -----  -----
%%%   |L/U|  |L/U|  |L/U|      |L/U|  |L/U|  |L/U|
%%%   -----  -----  -----      -----  -----  -----
%%%
%%% L/U = lookup_user per auth request
%%%
%%% @end
%%% Created : 17 Aug 2010 by James Aimonetti <james@2600hz.com>
%%%-------------------------------------------------------------------
-module(ecallmgr_auth).

%% API
-export([start_handler/1]).
-export([fetch_user/2]).

-import(props, [get_value/2, get_value/3]).
-import(logger, [log/2, format_log/3]).

-include("../include/amqp_client/include/amqp_client.hrl").
-include("freeswitch_xml.hrl").
-include("whistle_api.hrl").

%% lookups = [{LookupPid, ID, erlang:now()}]
-record(handler_state, {fs_node = undefined :: atom()
		       ,channel = undefined :: pid()
		       ,ticket = 0 :: integer()
		       ,app_vsn = [] :: list()
		       ,lookups = [] :: list(tuple(pid(), binary(), tuple(integer(), integer(), integer())))
		       }).

start_handler(Node) ->
    {ok, Vsn} = application:get_key(ecallmgr, vsn),
    HState = #handler_state{fs_node=Node, app_vsn=list_to_binary(Vsn)},
    {ok, APid} = freeswitch:start_fetch_handler(Node, directory, ?MODULE, fetch_user, HState),
    APid.

fetch_user(Node, #handler_state{channel=undefined}=State) ->
    {ok, Channel, Ticket} = amqp_manager:open_channel(self()),
    fetch_user(Node, State#handler_state{channel=Channel, ticket=Ticket});
fetch_user(Node, #handler_state{channel=Channel, lookups=LUs}=State) ->
    receive
	{fetch, directory, <<"domain">>, <<"name">>, _Value, ID, [undefined | Data]} ->
	    case get_value(<<"Event-Name">>, Data) of
		<<"REQUEST_PARAMS">> ->
		    Self = self(),
		    LookupPid = spawn(fun() -> lookup_user(State, ID, Self, Data) end),
		    link(LookupPid),
		    format_log(info, "Fetch_user(~p): fetch directory: Id: ~p Lookup ~p~n", [self(), ID, LookupPid]),
		    ?MODULE:fetch_user(Node, State#handler_state{lookups=[{LookupPid, ID, erlang:now()} | LUs]});
		_Other ->
		    format_log(info, "Fetch_user(~p): Ignoring event ~p~n", [self(), _Other]),
		    ?MODULE:fetch_user(Node, State)
	    end;
	{fetch, _Section, _Something, _Key, _Value, ID, [undefined | _Data]} ->
	    format_log(info, "Fetch_user(~p): fetch unknown: Se: ~p So: ~p, K: ~p V: ~p ID: ~p~n"
		       ,[self(), _Section, _Something, _Key, _Value, ID]),
	    freeswitch:fetch_reply(Node, ID, ?EMPTYRESPONSE),
	    ?MODULE:fetch_user(Node, State);
	{nodedown, Node} ->
	    format_log(error, "Fetch_user(~p): Node ~p exited", [self(), Node]),
	    ok;
	{xml_response, ID, XML} ->
	    format_log(info, "Fetch_user(~p): Received XML for ID ~p~n", [self(), ID]),
	    freeswitch:fetch_reply(Node, ID, XML),
	    ?MODULE:fetch_user(Node, State);
	{'EXIT', Channel, noconnection} ->
	    {ok, Channel1, Ticket1} = amqp_manager:open_channel(self()),
	    format_log(error, "Fetch_user(~p): Channel(~p) went down; replaced with ~p~n", [self(), Channel, Channel1]),
	    ?MODULE:fetch_user(Node, State#handler_state{channel=Channel1, ticket=Ticket1});
	shutdown ->
	    lists:foreach(fun({Pid,_StartTime}) ->
				  case erlang:is_process_alive(Pid) of
				      true -> Pid ! shutdown;
				      false -> ok
				  end
			  end, LUs),
	    format_log(error, "Fetch_user(~p): shutting down~n", [self()]);
	{lookup_finished, LookupPid} ->
	    close_lookup(LookupPid, Node, State);
	{diagnostics, Pid} ->
	    ActiveLUs = lists:map(fun({_LuPid, ID, Started}) -> [{fs_auth_id, ID}, {started, Started}] end, LUs),
	    Resp = [{active_lookups, ActiveLUs}],
	    Pid ! Resp,
	    ?MODULE:fetch_user(Node, State);
	Other ->
	    format_log(info, "Fetch_user(~p): got other response: ~p", [self(), Other]),
	    ?MODULE:fetch_user(Node, State)
    end.

close_lookup(LookupPid, Node, #handler_state{lookups=LUs}=State) ->
    case lists:keyfind(LookupPid, 1, LUs) of
	{LookupPid, ID, StartTime} ->
	    format_log(info, "Fetch_user(~p): lookup (~p:~p) finished in ~p ms~n"
		       ,[self(), LookupPid, ID, timer:now_diff(erlang:now(), StartTime) div 1000]),
	    ?MODULE:fetch_user(Node, State#handler_state{lookups=lists:keydelete(LookupPid, 1, LUs)});
	false ->
	    format_log(error, "Fetch_user(~p): unknown lookup ~p~n", [self(), LookupPid]),
	    ?MODULE:fetch_user(Node, State)
    end.

lookup_user(#handler_state{channel=Channel, ticket=Ticket, app_vsn=Vsn}, ID, FetchPid, Data) ->
    format_log(info, "L/U.user(~p): Starting up...~nC: ~p T: ~p ID: ~p FetchPid: ~p~n", [self(), Channel, Ticket, ID, FetchPid]),
    Q = bind_q(Channel, Ticket, ID),

    %% build req for rabbit
    Prop = [{<<"Msg-ID">>, ID}
	    ,{<<"To">>, ecallmgr_util:get_sip_to(Data)}
	    ,{<<"From">>, ecallmgr_util:get_sip_from(Data)}
	    ,{<<"Orig-IP">>, ecallmgr_util:get_orig_ip(Data)}
	    ,{<<"Auth-User">>, get_value(<<"user">>, Data, get_value(<<"Auth-User">>, Data))}
	    ,{<<"Auth-Domain">>, get_value(<<"domain">>, Data, get_value(<<"Auth-Domain">>, Data))}
	    | whistle_api:default_headers(Q, <<"directory">>, <<"auth_req">>, <<"ecallmgr.auth">>, Vsn)],
    case whistle_api:auth_req(Prop) of
	{ok, JSON} ->
	    format_log(info, "L/U(~p): Sending JSON over Channel(~p)~n~s~n", [self(), Channel, JSON]),
	    send_request(Channel, Ticket, JSON),
	    handle_response(ID, Data, FetchPid),
	    amqp_channel:cast(Channel, amqp_util:queue_delete(Ticket, Q));
	{error, _Msg} ->
	    format_log(error, "L/U(~p): Auth_Req API error ~p~n", [self(), _Msg])
    end,
    FetchPid ! {lookup_finished, self()}.

recv_response(ID) ->
    receive
	#'basic.consume_ok'{} ->
	    recv_response(ID);
	{_, #amqp_msg{props = Props, payload = Payload}} ->
	    {struct, Prop} = mochijson2:decode(binary_to_list(Payload)),
	    format_log(info, "L/U(~p): Recv Content: ~p EvtName: ~p~n"
		       ,[self(), Props#'P_basic'.content_type, get_value(<<"Event-Name">>, Prop)]),
	    case get_value(<<"Msg-ID">>, Prop) of
		ID ->
		    case whistle_api:auth_resp_v(Prop) of
			true -> Prop;
			false ->
			    format_log(error, "L/U.auth(~p): Invalid Auth Resp~n~p~n", [self(), Prop]),
			    invalid_auth_resp
		    end;
		_BadId ->
		    format_log(info, "L/U(~p): Recv Msg ~p when expecting ~p~n", [self(), _BadId, ID]),
		    recv_response(ID)
	    end;
	shutdown ->
	    shutdown;
	Msg ->
	    format_log(info, "L/U(~p): Received ~p off rabbit~n", [self(), Msg]),
	    recv_response(ID)
    after 4000 ->
	    format_log(info, "L/U(~p): Failed to receive after 4000ms~n", [self()]),
	    timeout
    end.

bind_q(Channel, Ticket, ID) ->
    amqp_channel:cast(Channel, amqp_util:targeted_exchange(Ticket)),
    #'queue.declare_ok'{queue = Queue} = amqp_channel:call(Channel, amqp_util:new_targeted_queue(Ticket, ID)),
    amqp_channel:cast(Channel, amqp_util:bind_q_to_targeted(Ticket, Queue, Queue)),
    amqp_channel:subscribe(Channel, amqp_util:basic_consume(Ticket, Queue), self()),
    Queue.

a1hash(User, Realm, Password) ->
    format_log(info, "AUTH(~p): a1hashing ~p:~p:~p~n", [self(), User, Realm, Password]),
    ecallmgr_util:to_hex(erlang:md5(list_to_binary([User,":",Realm,":",Password]))).

send_request(Channel, Ticket, JSON) ->
    {BP, AmqpMsg} = amqp_util:broadcast_publish(Ticket, JSON, <<"application/json">>),
    amqp_channel:cast(Channel, BP, AmqpMsg).

handle_response(ID, Data, FetchPid) ->
    T1 = erlang:now(),
    %% recv resp from rabbit
    case recv_response(ID) of
	shutdown ->
	    format_log(error, "L/U(~p): Shutting down for ID ~p~n", [self(), ID]),
	    shutdown;
	timeout ->
	    FetchPid ! {xml_response, ID, ?EMPTYRESPONSE};
	invalid_auth_resp ->
	    FetchPid ! {xml_response, ID, ?EMPTYRESPONSE};
	Prop ->
	    User = get_value(<<"user">>, Data),
	    Domain = get_value(<<"domain">>, Data),
	    case get_value(<<"Auth-Method">>, Prop) of
		<<"password">> ->
		    Hash = a1hash(User, Domain, get_value(<<"Auth-Password">>, Prop)),
		    ChannelParams = get_channel_params(Prop),
		    Resp = lists:flatten(io_lib:format(?REGISTER_HASH_RESPONSE, [Domain, User, Hash, ChannelParams])),
		    format_log(info, "LOOKUP_USER(~p): Sending pass resp (took ~pms)~n"
			       ,[self(), timer:now_diff(erlang:now(), T1) div 1000]),
		    FetchPid ! {xml_response, ID, Resp};
		<<"a1-hash">> ->
		    Hash = get_value(<<"Auth-Password">>, Prop),
		    ChannelParams = get_channel_params(Prop),
		    Resp = lists:flatten(
			     io_lib:format(?REGISTER_HASH_RESPONSE, [Domain, User, Hash, ChannelParams])
			    ),
		    format_log(info, "LOOKUP_USER(~p): Sending hashed resp (took ~pms)~n"
			       , [self(), timer:now_diff(erlang:now(), T1) div 1000]),
		    FetchPid ! {xml_response, ID, Resp};
		<<"ip">> ->
		    format_log(info, "LOOKUP_USER(~p): Unsupported auth by IP (took ~pms)~n"
			       , [self(), timer:now_diff(erlang:now(), T1) div 1000]),
		    FetchPid ! {xml_response, ID, ?EMPTYRESPONSE};
		<<"error">> ->
		    format_log(info, "LOOKUP_USER(~p): Auth by Error: ~p (took ~pms)~n"
			       ,[self(), get_value(<<"Auth-Password">>, Prop), timer:now_diff(erlang:now(), T1) div 1000]),
		    FetchPid ! {xml_response, ID, ?EMPTYRESPONSE}
	    end
    end.

get_channel_params(Prop) ->
    CV0 = case get_value(<<"Tenant-ID">>, Prop) of
	      undefined -> [];
	      TID -> [io_lib:format(?REGISTER_CHANNEL_PARAM
				    ,[list_to_binary([?CHANNEL_VAR_PREFIX, "Tenant-ID"]), TID])]
	  end,
    CV1 = case get_value(<<"Access-Group">>, Prop) of
    	      undefined -> CV0;
	      AG -> [io_lib:format(?REGISTER_CHANNEL_PARAM
				   ,[list_to_binary([?CHANNEL_VAR_PREFIX, "Access-Group"]), AG]) | CV0]
	  end,
    {struct, Custom} = get_value(<<"Custom-Channel-Vars">>, Prop, {struct, []}),
    lists:foldl(fun({K,V}, CV) ->
			[io_lib:format(?REGISTER_CHANNEL_PARAM
				       ,[list_to_binary([?CHANNEL_VAR_PREFIX, K]), V]) | CV]
		end, CV1, Custom).
