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
-export([start_link/0, lookup_user/2, send_fetch_response/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-import(proplists, [get_value/2, get_value/3]).

-include("../include/amqp_client/include/amqp_client.hrl").
-include("freeswitch_xml.hrl").

-define(SERVER, ?MODULE). 

-record(state, {fs_node, channel, ticket, app_vsn}).

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

send_fetch_response(ID, Response) ->
    gen_server:cast(?MODULE, {send_fetch_response, ID, Response}).

%% see lookup_user/2 after gen_server callbacks

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
    Node = list_to_atom(lists:concat(["freeswitch@", net_adm:localhost()])),
    {ok, Channel, Ticket} = amqp_manager:open_channel(self()),
    {ok, Vsn} = application:get_key(ecallmgr, vsn),
    State = #state{fs_node=Node, channel=Channel, ticket=Ticket, app_vsn=list_to_binary(Vsn)},
    case net_adm:ping(Node) of
	pong ->
	    {ok, Pid} = freeswitch:start_fetch_handler(Node, directory, ?MODULE, lookup_user, State),
	    link(Pid);
	_ ->
	    io:format("Unable to find ~p to talk to freeSWITCH~n", [Node])
    end,
    {ok, State}.

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
handle_cast({send_fetch_response, ID, Response}, #state{fs_node=Node}=State) ->
    freeswitch:fetch_reply(Node, ID, Response),
    {noreply, State};
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

lookup_user(Node, State) ->
    receive
	{fetch, directory, <<"domain">>, <<"name">>, _Value, ID, [undefined | Data]} ->
	    io:format("fetch directory: Id: ~p~nData: ~p~n", [ID, Data]),
	    spawn(fun() -> lookup_user(State, ID, Data) end),
	    ?MODULE:lookup_user(Node, State);
	{fetch, _Section, _Something, _Key, _Value, ID, [undefined | _Data]} ->
	    io:format("fetch unknown: Se: ~p So: ~p, K: ~p V: ~p ID: ~p, D: ~p~n", [_Section, _Something, _Key, _Value, ID, _Data]),
	    freeswitch:fetch_reply(Node, ID, ?EMPTYRESPONSE),
	    ?MODULE:lookup_user(Node, State);
	{nodedown, Node} ->
	    io:format("Node we were serving XML search requests to exited", []),
	    ok;
	Other ->
	    io:format("got other response: ~p", [Other]),
	    ?MODULE:lookup_user(Node, State)
    end.

lookup_user(#state{channel=Channel, ticket=Ticket, app_vsn=Vsn}, ID, Data) ->
    Q = bind_q(Channel, Ticket, ID),

    %% build req for rabbit
    Data0 = [{<<"Server-ID">>, Q}
	     ,{<<"Event-Category">>, <<"directory">>}
	     ,{<<"App-Name">>, <<"ecallmgr">>}
	     ,{<<"App-Version">>, Vsn}
	     ,{<<"Msg-ID">>, ID}
	     ,{<<"To">>, get_sip_to(Data)}
	     ,{<<"From">>, get_sip_from(Data)}
	     ,{<<"Orig-IP">>, get_value(<<"ip">>, Data)}
	     ,{<<"Auth-User">>, get_value(<<"user">>, Data)}
             ,{<<"Auth-Domain">>, get_value(<<"domain">>, Data)}
	     ,{<<"Auth-Pass">>, get_value(<<"password">>, Data, <<"">>)}
	     | Data],
    {ok, JSON} = whistle_api:auth_req(Data0),
    io:format("JSON REQ: ~s~n", [JSON]),

    %% put on wire to rabbit
    {BP, AmqpMsg} = amqp_util:broadcast_publish(Ticket, JSON, <<"application/json">>),
    amqp_channel:call(Channel, BP, AmqpMsg),

    %% recv resp from rabbit
    case recv_response(ID) of
	timeout ->
	    ?MODULE:send_fetch_response(ID, ?EMPTYRESPONSE);
	Prop ->
	    User = get_value(<<"user">>, Data),
	    Domain = get_value(<<"domain">>, Data),
	    case get_value(<<"Auth-Method">>, Prop) of
		<<"password">> ->
		    Hash = a1hash(User, Domain, get_value(<<"Auth-Pass">>, Prop)),
		    Resp = lists:flatten(io_lib:format(?REGISTERRESPONSE, [Domain, User, Hash])),
		    io:format("LOOKUP_USER(~p): Sending pass resp: ~p~n", [self(), Resp]),
		    ?MODULE:send_fetch_response(ID, Resp);
		<<"a1-hash">> ->
		    Resp = lists:flatten(
			     io_lib:format(?REGISTERRESPONSE, [Domain, User, get_value(<<"Auth-Pass">>, Prop)])
			    ),
		    io:format("LOOKUP_USER(~p): Sending hashed resp: ~p~n", [self(), Resp]),
		    ?MODULE:send_fetch_response(ID, Resp);
		<<"ip">> ->
		    io:format("LOOKUP_USER(~p): Unsupported auth by IP~n", [self()]),
		    ?MODULE:send_fetch_response(ID, ?EMPTYRESPONSE)
	    end
    end.

recv_response(ID) ->
    receive
	#'basic.consume_ok'{} ->
	    recv_response(ID);
	{_, #amqp_msg{props = Props, payload = Payload}} ->
	    io:format("Recv Content: ~p Payload: ~s~n", [Props#'P_basic'.content_type, binary_to_list(Payload)]),
	    {struct, Prop} = mochijson2:decode(binary_to_list(Payload)),
	    case get_value(<<"Msg-ID">>, Prop) of
		ID -> Prop;
		_BadId ->
		    io:format("Recv Msg ~p when expecting ~p~n", [_BadId, ID]),
		    recv_response(ID)
	    end;
	Msg ->
	    io:format("Received ~p off rabbit~n", [Msg])
    after 1000 ->
	    %% insert empty response
	    %% ?MODLULE_send_fetch_response(ID, ?EMPTY_RESPONSE),
	    io:format("Failed to receive after 1000ms~n", []),
	    timeout
    end.

get_sip_to(Data) ->
    list_to_binary([get_value(<<"sip_to_user">>, Data), "@", get_value(<<"sip_to_host">>, Data)]).
get_sip_from(Data) ->
    list_to_binary([get_value(<<"sip_from_user">>, Data), "@", get_value(<<"sip_from_host">>, Data)]).

bind_q(Channel, Ticket, ID) ->
    #'exchange.declare_ok'{} = amqp_channel:call(Channel, amqp_util:targeted_exchange(Ticket)),
    #'queue.declare_ok'{queue = Queue} = amqp_channel:call(Channel, amqp_util:new_targeted_queue(Ticket, ID)),
    #'queue.bind_ok'{} = amqp_channel:call(Channel, amqp_util:bind_q_to_targeted(Ticket, Queue, Queue)),
    #'basic.consume_ok'{} = amqp_channel:subscribe(Channel, amqp_util:basic_consume(Ticket, Queue), self()),
    Queue.

a1hash(User, Realm, Password) ->
    to_hex(erlang:md5(list_to_binary([User,":",Realm,":",Password]))).

to_hex(Bin) when is_binary(Bin) ->
    to_hex(binary_to_list(Bin));
to_hex(L) when is_list(L) ->
    string:to_lower(lists:flatten([io_lib:format("~2.16.0B", [H]) || H <- L])).
