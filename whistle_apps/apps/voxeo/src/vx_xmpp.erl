%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Manage the XMPP interactions
%%% @end
%%% Created : 11 Oct 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(vx_xmpp).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("voxeo.hrl").

-define(SERVER, ?MODULE).
-define(STARTUP_CONFIG, [code:priv_dir(voxeo), "/startup.config"]).

-record(state, {
	  xmpp_session = undefined :: pid() | 'undefined'
	  ,xmpp_server = "" :: string()
	  ,xmpp_port = ?DEFAULT_XMPP_PORT :: integer()
	  ,xmpp_use_ssl = 'true' :: boolean()
	  ,rayo_jid = #jid{} :: #jid{}
	  ,rayo_password = "" :: string()
	 }).

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
    case file:consult(?STARTUP_CONFIG) of
	{ok, Configs} ->
	    self() ! authenticate,
	    ?LOG("Starting up vx_xmpp"),
	    {ok, #state{
	       xmpp_session = exmpp_session:start_link()
	       ,rayo_jid = exmpp_jid:parse(wh_util:to_list(props:get_value(rayo_jid, Configs)))
	       ,rayo_password = wh_util:to_list(props:get_value(rayo_password, Configs))
	       ,xmpp_server = wh_util:to_list(props:get_value(xmpp_server, Configs))
	       ,xmpp_port = wh_util:to_integer(props:get_value(xmpp_port, Configs, ?DEFAULT_XMPP_PORT))
	      }};
	_ ->
	    ?LOG("Failed to load ~s", [?STARTUP_CONFIG]),
	    {stop, normal}
    end.

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

handle_info(#received_packet{}=Packet, #state{xmpp_session=Session}=State) ->
    spawn(fun() -> handle_packet(Session, Packet) end),
    {noreply, State};

handle_info(authenticate, #state{xmpp_session=Session, xmpp_server=Server, xmpp_port=Port
				 ,rayo_jid=JID, rayo_password=Pass}=State) ->
    try
	?LOG("Auth with session ~p", [Session]),
	?LOG("JID: ~p", [JID]),
	?LOG("Pass: ~p", [Pass]),

	ok = exmpp_session:auth(Session, JID, Pass, password),

	{ok, _StreamId} = exmpp_session:connect_TCP(Session, Server, Port),

	?LOG("Server: ~p:~p", [Server, Port]),
	?LOG("StreamID: ~p", [_StreamId]),

	{ok, _JID} = exmpp_session:login(Session),

	exmpp_session:send_packet(Session, exmpp_presence:set_status(exmpp_presence:available(), "VX Whapp Ready")),
	?LOG("Sent presence"),
	{noreply, State}
    catch
	_:{auth_error, 'not-authorized'} ->
	    ?LOG("Unauthorized login with the given credentials"),
	    {stop, normal, State};
	_T:_R ->
	    ?LOG("failed to login: ~p:~p", [_T, _R]),
	    [?LOG("ST: ~p", [ST]) || ST <- erlang:get_stacktrace()],
	    {stop, normal, State}
    end;

handle_info(_Info, State) ->
    ?LOG("Unhandled message: ~p", [_Info]),
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
terminate(_Reason, #state{xmpp_session=Session}) ->
    ?LOG("VX going down: ~p", [_Reason]),
    exmpp_session:stop(Session).

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
handle_packet(Session, #received_packet{packet_type='iq', type_attr=Type, raw_packet=IQ}) ->
    ?LOG("IQ packet of type ~p", [Type]),
    ?LOG("Sender: ~p", [exmpp_stanza:get_sender(IQ)]),
    ?LOG("Recv: ~p", [exmpp_stanza:get_recipient(IQ)]),
    process_iq(Session, Type, exmpp_xml:get_ns_as_atom(exmpp_iq:get_payload(IQ)), IQ);
handle_packet(Session, #received_packet{packet_type='presence', type_attr=Type, raw_packet=P}) ->
    ?LOG("Presense packet of type ~p", [Type]),
    ?LOG("Sender: ~p", [exmpp_stanza:get_sender(P)]),
    ?LOG("Recv: ~p", [exmpp_stanza:get_recipient(P)]),
    process_presence(Session, Type, P);
handle_packet(Session, Packet) ->
    ?LOG("Unhandled packet ~p", [Packet]).

process_presence(Session, Type, Presence) ->
    ?LOG("Type: ~p(~p)", [exmpp_presence:get_type(Presence), Type]),
    ?LOG("Show: ~p", [exmpp_presence:get_show(Presence)]),
    ?LOG("Status: ~p", [exmpp_presence:get_status(Presence)]),
    ?LOG("Priority: ~p", [exmpp_presence:get_priority(Presence)]).

process_iq(Session, "get", ?NS_TIME, IQ) ->
    ?LOG("NS_TIME from"),
    ?LOG("Type: ~p", [exmpp_iq:get_type(IQ)]),
    ?LOG("Kind: ~p", [exmpp_iq:get_kind(IQ)]),
    ?LOG("Req: ~p", [exmpp_iq:get_request(IQ)]),
    ?LOG("Res: ~p", [exmpp_iq:is_result(IQ) andalso exmpp_iq:get_result(IQ)]),
    ?LOG("Payload: ~p", [exmpp_iq:get_payload(IQ)]);

process_iq(Session, "get", ?NS_PING, IQ) ->
    ?LOG("NS_PING from ~s", [exmpp_stanza:get_sender(IQ)]),
    Reply = exmpp_xml:element(exmpp_xml:get_ns_as_atom(IQ), 'iq', [
								   exmpp_xml:attribute(<<"from">>, exmpp_stanza:get_recipient(IQ))
								   ,exmpp_xml:attribute(<<"to">>, exmpp_stanza:get_sender(IQ))
								   ,exmpp_xml:attribute(<<"id">>, exmpp_stanza:get_id(IQ))
								  ], []),
    exmpp_session:send_packet(Session, Reply);

process_iq(Session, _Type, NS, IQ) ->
    ?LOG("NS: ~p", [NS]),
    ?LOG("Type: ~p", [exmpp_iq:get_type(IQ)]),
    ?LOG("Kind: ~p", [exmpp_iq:get_kind(IQ)]),
    ?LOG("Req: ~p", [exmpp_iq:get_request(IQ)]),
    ?LOG("Res: ~p", [exmpp_iq:is_result(IQ) andalso exmpp_iq:get_result(IQ)]),
    ?LOG("Payload: ~p", [exmpp_iq:get_payload(IQ)]).
