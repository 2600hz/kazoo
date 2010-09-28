%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.com>
%%% @copyright (C) 2010, James Aimonetti
%%% @doc
%%% Handles authentication requests on the FS instance by a device
%%% @end
%%% Created : 17 Aug 2010 by James Aimonetti <james@2600hz.com>
%%%-------------------------------------------------------------------
-module(ecallmgr_auth).

-behaviour(gen_server).

%% API
-export([start_link/0, add_fs_node/1, rm_fs_node/1]).
-export([fetch_user/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-import(proplists, [get_value/2, get_value/3]).
-import(logger, [log/2, format_log/3]).

-include("../include/amqp_client/include/amqp_client.hrl").
-include("freeswitch_xml.hrl").
-include("whistle_api.hrl").

-define(SERVER, ?MODULE). 

%% fs_nodes = [{FSNode, HandlerPid}]
-record(auth_state, {fs_nodes=[]}).
-record(handler_state, {fs_node, channel, ticket, app_vsn, lookups=[]}).

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

add_fs_node(Node) ->
    gen_server:call(?MODULE, {add_fs_node, Node}).

rm_fs_node(Node) ->
    gen_server:call(?MODULE, {rm_fs_node, Node}).

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
    process_flag(trap_exit, true),
    {ok, #auth_state{}}.

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
%% #auth_state{fs_nodes=[{FSNode, HandlerPid}]}
%%--------------------------------------------------------------------
handle_call({add_fs_node, Node}, _From, State) ->
    {Resp, State1} = add_fs_node(Node, State),
    {reply, Resp, State1};
handle_call({rm_fs_node, Node}, _From, State) ->
    {Resp, State1} = rm_fs_node(Node, State),
    {reply, Resp, State1};
handle_call(_Request, _From, State) ->
    format_log(error, "AUTH(~p): Unhandled call ~p~n", [self(), _Request]),
    {reply, {error, unhandled_request}, State}.

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
handle_info({'EXIT', HPid, Reason}, #auth_state{fs_nodes=Nodes}=State) ->
    case lists:keyfind(HPid, 2, Nodes) of
	{Node, HPid} ->
	    format_log(error, "AUTH(~p): Received EXIT(~p) for ~p, restarting...~n", [self(), Reason, HPid]),
	    {_, State1} = rm_fs_node(Node, State),
	    {_, State2} = add_fs_node(Node, State1),
	    {noreply, State2};
	false ->
	    format_log(error, "AUTH(~p): Received EXIT(~p) for unknown ~p~n", [self(), Reason, HPid]),
	    {noreply, State}
    end;
handle_info(_Info, State) ->
    format_log(info, "AUTH(~p): Unhandled Info: ~p~n", [self(), _Info]),
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
terminate(_Reason, #auth_state{fs_nodes=Nodes}) ->
    Self = self(),
    format_log(error, "AUTH(~p): terminating: ~p~n", [Self, _Reason]),
    lists:foreach(fun({_FSNode, HPid}) ->
			  format_log(error, "AUTH(~p): terminate handler: ~p(~p)~n", [Self, HPid, _FSNode]),
			  HPid ! shutdown
		  end, Nodes),
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
-spec(add_fs_node/2 :: (Node :: atom(), State :: tuple()) -> {ok, tuple()} | {{error, atom()}, tuple()}).
add_fs_node(Node, #auth_state{fs_nodes=Nodes}=State) ->
    case lists:keyfind(Node, 1, Nodes) of
	{Node, _HandlerPid} ->
	    format_log(info, "AUTH(~p): handler(~p) known for ~p~n", [self(), _HandlerPid, Node]),
	    {{error, node_is_known}, State};
	false ->
	    case net_adm:ping(Node) of
		pong ->
		    HPid = setup_fs_conn(Node),
		    link(HPid),
		    format_log(info, "AUTH(~p): Starting handler(~p) for ~p~n", [self(), HPid, Node]),
		    {ok, State#auth_state{fs_nodes=[{Node, HPid} | Nodes]}};
		pang ->
		    format_log(error, "AUTH(~p): ~p not responding~n", [self(), Node]),
		    {{error, no_connection}, State}
	    end
    end.

rm_fs_node(Node, #auth_state{fs_nodes=Nodes}=State) ->
    case lists:keyfind(Node, 1, Nodes) of
	{Node, HPid} ->
	    case erlang:is_process_alive(HPid) of
		true ->
		    HPid ! shutdown,
		    format_log(info, "AUTH(~p): Shutting down handler ~p for ~p~n", [self(), HPid, Node]),
		    {{ok, Node}, State#auth_state{fs_nodes=lists:keydelete(Node, 1, Nodes)}};
		false ->
		    format_log(error, "AUTH(~p): Handler ~p already down for ~p~n", [self(), HPid, Node]),
		    {{error, handler_down, Node}, State#auth_state{fs_nodes=lists:keydelete(Node, 1, Nodes)}}
	    end;
	false ->
	    format_log(error, "AUTH(~p): No handler for ~p~n", [self(), Node]),
	    {{error, no_node, Node}, State}
    end.

fetch_user(Node, #handler_state{channel=undefined, ticket=undefined}=State) ->
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
		    ?MODULE:fetch_user(Node, State#handler_state{lookups=[{LookupPid, erlang:now()} | LUs]});
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
	    case get_value(LookupPid, LUs) of
		StartTime when is_tuple(StartTime) ->
		    format_log(info, "Fetch_user(~p): lookup (~p) finished in ~p ms~n"
			       ,[self(), LookupPid, timer:now_diff(erlang:now(), StartTime) div 1000]),
		    ?MODULE:fetch_user(Node, State#handler_state{lookups=proplists:delete(LookupPid, LUs)});
		undefined ->
		    format_log(error, "Fetch_user(~p): unknown lookup ~p~n", [self(), LookupPid]),
		    ?MODULE:fetch_user(Node, State)
	    end;
	Other ->
	    format_log(info, "Fetch_user(~p): got other response: ~p", [self(), Other]),
	    ?MODULE:fetch_user(Node, State)
    end.

lookup_user(#handler_state{channel=Channel, ticket=Ticket, app_vsn=Vsn}, ID, FetchPid, Data) ->
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

%% setup a connection to mod_erlang_event for dialplan requests,
%% or setup a timer to query for the node and return the timer ref
setup_fs_conn(Node) ->
    {ok, Vsn} = application:get_key(ecallmgr, vsn),
    HState = #handler_state{fs_node=Node, app_vsn=list_to_binary(Vsn)},
    {ok, Pid} = freeswitch:start_fetch_handler(Node, directory, ?MODULE, fetch_user, HState),
    Pid.

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
