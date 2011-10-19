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
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("voxeo.hrl").

-define(SERVER, ?MODULE).
-define(STARTUP_CONFIG, [code:priv_dir(voxeo), "/startup.config"]).

-record(state, {
	  my_q = 'undefined' :: 'undefined' | ne_binary()
	  ,xmpp_session = 'undefined' :: pid() | 'undefined'
	  ,xmpp_server = "" :: string()
	  ,xmpp_port = ?DEFAULT_XMPP_PORT :: integer()
	  ,xmpp_use_ssl = 'true' :: boolean()
	  ,xmpp_jid = #jid{} :: #jid{}
	  ,xmpp_password = "" :: string()
	  ,rayo_sip_user = 'undefined' :: ne_binary() | 'undefined'
          ,rayo_lang = "us-EN" :: string()
          ,stream_response = 'false' :: boolean()
	  ,aleg_callid = 'undefined' :: ne_binary() | 'undefined'
          ,aleg_ctl_q = 'undefined' :: ne_binary() | 'undefined'
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
start_link(AsrReq) ->
    gen_server:start_link(?MODULE, [AsrReq], []).

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
init([AsrReq]) ->
    self() ! authenticate,

    wh_util:put_callid(AsrReq),

    ?LOG("Starting up vx_xmpp"),

    JID = exmpp_jid:parse(wh_util:to_list(wh_json:get_value(<<"ASR-Account-ID">>, AsrReq))),

    ?LOG("AsrReq:"),
    [?LOG("~p", [KV]) || KV <- wh_json:to_proplist(AsrReq)],

    {ok, #state{
       xmpp_session = exmpp_session:start_link()
       ,xmpp_jid = JID
       ,xmpp_password = wh_util:to_list(wh_json:get_value(<<"ASR-Account-Password">>, AsrReq))
       ,xmpp_server = exmpp_jid:domain_as_list(JID)
       ,rayo_sip_user = wh_json:get_value(<<"ASR-Endpoint">>, AsrReq)
       ,rayo_lang = wh_util:to_list(wh_json:get_value(<<"Language">>, AsrReq, "us-EN"))
       ,stream_response = wh_json:is_true(<<"Stream-Response">>, AsrReq, false)
       ,aleg_callid = wh_json:get_value(<<"Call-ID">>, AsrReq)
       ,aleg_ctl_q = wh_json:get_value(<<"Control-Queue">>, AsrReq)
      }}.

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
				 ,xmpp_jid=JID, xmpp_password=Pass}=State) ->
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

	self() ! bridge,

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

handle_info(bridge, #state{my_q=Q
			   ,rayo_sip_user=SIP
			   ,rayo_lang=Lang
			   ,stream_response=StreamIt
			   ,aleg_callid=CallID
			   ,aleg_ctl_q=CtrlQ}=State) ->
    ?LOG("Bridge to ASR"),

    Command = [{<<"Application-Name">>, <<"bridge">>}
               ,{<<"Endpoints">>, make_endpoint(SIP)}
               ,{<<"Timeout">>, 1000}
               ,{<<"Call-ID">>, CallID}
               | wh_api:default_headers(Q, <<"call">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
	      ],
    {ok, Payload} = wapi_dialplan:bridge([ KV || {_, V}=KV <- Command, V =/= undefined ]),
    wapi_dialplan:publish_action(CtrlQ, Payload),

    {noreply, State};

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
-spec make_endpoint/1 :: (ne_binary()) -> json_objects().
make_endpoint(<<"sip:", _/binary>>=SIP) -> make_ep(SIP);
make_endpoint(EP) -> make_ep(<<"sip:", EP/binary>>).

-spec make_ep/1 :: (ne_binary()) -> json_objects().
make_ep(SIP) ->
    [wh_json:from_list([
			{<<"Invite-Format">>, <<"route">>}
			,{<<"Route">>, SIP}
		       ])
    ].

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
