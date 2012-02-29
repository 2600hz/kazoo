%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Manage the XMPP interactions
%%% @end
%%% Created : 11 Oct 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(vx_xmpp).

-behaviour(gen_listener).

%% API
-export([start_link/1, handle_amqp/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, handle_event/2
	 ,terminate/2, code_change/3]).

-include("voxeo.hrl").

-define(SERVER, ?MODULE).
-define(STARTUP_CONFIG, [code:priv_dir(voxeo), "/startup.config"]).

-define(RESPONDERS, [{{?MODULE, handle_amqp}, [{<<"*">>, <<"*">>}]}]).
-define(BINDINGS, [{self, []}]).

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
    gen_listener:start_link(?MODULE, [{responders, ?RESPONDERS}
				      ,{bindings, ?BINDINGS}
				     ], [AsrReq]).

-spec handle_amqp/2 :: (json_object(), proplist()) -> no_return().
handle_amqp(JObj, _Props) ->
    wh_util:put_callid(JObj).
    %% lager:debug("AMQP Recv: ~p", [wh_util:get_event_type(JObj)]),
    %% lager:debug("App: ~s", [wh_json:get_value(<<"Application-Name">>, JObj)]),
    %% lager:debug("Other UUID: ~s", [wh_json:get_value(<<"Other-Leg-Unique-ID">>, JObj)]).

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
    wh_util:put_callid(AsrReq),

    lager:debug("Starting up vx_xmpp"),

    JID = exmpp_jid:parse(wh_util:to_list(wh_json:get_value(<<"ASR-Account-ID">>, AsrReq))),
    CallID = wh_json:get_value(<<"Call-ID">>, AsrReq),

    gen_listener:add_binding(self(), call_events, [{callid, CallID}]),
    lager:debug("Adding binding for call events"),

    Self = self(),
    State = #state{
      xmpp_session = exmpp_session:start_link()
      ,xmpp_jid = JID
      ,xmpp_password = wh_util:to_list(wh_json:get_value(<<"ASR-Account-Password">>, AsrReq))
      ,xmpp_server = exmpp_jid:domain_as_list(JID)
      ,rayo_sip_user = wh_json:get_value(<<"ASR-Endpoint">>, AsrReq)
      ,rayo_lang = wh_util:to_list(wh_json:get_value(<<"Language">>, AsrReq, "us-EN"))
      ,stream_response = wh_json:is_true(<<"Stream-Response">>, AsrReq, false)
      ,aleg_callid = CallID
      ,aleg_ctl_q = wh_json:get_value(<<"Control-Queue">>, AsrReq)
     },

    spawn_monitor(fun() -> put(callid, CallID), authenticate(Self, State) end),
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
handle_info(#received_packet{}=Packet, #state{aleg_callid=CallID, xmpp_session=Session}=State) ->
    spawn(fun() -> put(callid, CallID), handle_packet(Session, Packet) end),
    {noreply, State};

handle_info({'DOWN', _Ref, process, _Pid, normal}, State) ->
    lager:debug("~p when down normally", [_Pid]),
    {noreply, State};

handle_info({'DOWN', _Ref, process, _Pid, Reason}, State) ->
    lager:debug("~p when down: ~p", [_Pid, Reason]),
    {noreply, State, 5000};

handle_info(timeout, State) ->
    {stop, timeout, State};

handle_info(_Info, State) ->
    lager:debug("Unhandled message: ~p", [_Info]),
    {noreply, State}.

handle_event(_JObj, _State) ->
    {reply, [{self, self()}]}.

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
    lager:debug("VX going down: ~p", [_Reason]),
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
    lager:debug("IQ packet of type ~p", [Type]),
    lager:debug("Sender: ~p", [exmpp_stanza:get_sender(IQ)]),
    lager:debug("Recv: ~p", [exmpp_stanza:get_recipient(IQ)]),
    process_iq(Session, Type, exmpp_xml:get_ns_as_atom(exmpp_iq:get_payload(IQ)), IQ);
handle_packet(Session, #received_packet{packet_type='presence', type_attr=Type, raw_packet=P}) ->
    lager:debug("Presence packet of type ~p", [Type]),
    lager:debug("Sender: ~p", [exmpp_stanza:get_sender(P)]),
    lager:debug("Recv: ~p", [exmpp_stanza:get_recipient(P)]),
    process_presence(Session, Type, P);
handle_packet(_Session, Packet) ->
    lager:debug("Unhandled packet ~p", [Packet]).

process_presence(Session, Type, Presence) ->
    case exmpp_xml:get_element(Presence, offer) of
	undefined ->
	    lager:debug("Ignoring");
	Offer ->
	    lager:debug("Is Offer"),
	    process_offer(Session, Presence, Offer)
    end.

process_offer(Session, Presence, Offer) ->
    Sender = exmpp_stanza:get_sender(Presence),
    Recipient = exmpp_stanza:get_recipient(Presence),

    [CallID] = [exmpp_xml:get_attribute(H, <<"value">>, undefined)
		|| H <- exmpp_xml:get_elements(Offer, header),
		   exmpp_xml:get_attribute(H, <<"name">>, undefined) =:= <<"Call-ID">>
	       ],
    lager:debug("B-LEG: ~s", [CallID]),

    answer(Session, Sender, Recipient).

answer(Session, From, To) ->
    IQ = exmpp_stanza:set_type(
	   exmpp_xml:element('undefined', iq, [
					       exmpp_xml:attribute(<<"from">>, To)
					       ,exmpp_xml:attribute(<<"to">>, From)
					      ]
			     ,[exmpp_xml:element('urn:xmpp:rayo:1', answer, [], [
										 exmpp_xml:element('urn:xmpp:rayo:1', header, [exmpp_xml:attribute(<<"value">>, <<"2600hz">>)
															       ,exmpp_xml:attribute(<<"name">>, <<"x-test-header">>)
															      ], [])])
			      ]
			    )
	   , set),
    lager:debug("IQ: ~s", [exmpp_stanza:to_binary(IQ)]),
    exmpp_session:send_packet(Session, IQ).

%% Create this XML structure next:
%% <iq type='set' to='9f00061@call.rayo.net/1' from='16577@app.rayo.net/1'>
%%   <input xmlns='urn:xmpp:rayo:input:1'
%%       mode='any|dtmf|speech'
%%       terminator='#'
%%       recognizer='en-US'
%%       initial-timeout='2000'
%%       inter-digit-timeout='2000'
%%       sensitivity='0.5'
%%       min-confidence='0.5'>
%%     <grammar url="http://grammarserver.com/digits?min=1&amp;max=10" />
%%     <grammar content-type='application/grammar+voxeo'>
%%       [4 DIGITS]
%%     </grammar>
%%   </input>
%% </iq>

process_iq(Session, "get", ?NS_TIME, IQ) ->
    lager:debug("NS_TIME from"),
    lager:debug("Type: ~p", [exmpp_iq:get_type(IQ)]),
    lager:debug("Kind: ~p", [exmpp_iq:get_kind(IQ)]),
    lager:debug("Req: ~p", [exmpp_iq:get_request(IQ)]),
    lager:debug("Res: ~p", [exmpp_iq:is_result(IQ) andalso exmpp_iq:get_result(IQ)]),
    lager:debug("Payload: ~p", [exmpp_iq:get_payload(IQ)]);

process_iq(Session, "get", ?NS_PING, IQ) ->
    lager:debug("NS_PING from ~s", [exmpp_stanza:get_sender(IQ)]),
    Reply = exmpp_xml:element(exmpp_xml:get_ns_as_atom(IQ), 'iq', [
								   exmpp_xml:attribute(<<"from">>, exmpp_stanza:get_recipient(IQ))
								   ,exmpp_xml:attribute(<<"to">>, exmpp_stanza:get_sender(IQ))
								   ,exmpp_xml:attribute(<<"id">>, exmpp_stanza:get_id(IQ))
								  ], []),
    exmpp_session:send_packet(Session, Reply);

process_iq(Session, _Type, NS, IQ) ->
    lager:debug("NS: ~p", [NS]),
    lager:debug("Type: ~p", [exmpp_iq:get_type(IQ)]),
    lager:debug("Kind: ~p", [exmpp_iq:get_kind(IQ)]),
    lager:debug("Req: ~p", [exmpp_iq:get_request(IQ)]),
    lager:debug("Res: ~p", [exmpp_iq:is_result(IQ) andalso exmpp_iq:get_result(IQ)]),
    lager:debug("Payload: ~p", [exmpp_iq:get_payload(IQ)]).


authenticate(Srv, #state{xmpp_session=Session, xmpp_server=Server, xmpp_port=Port
			 ,xmpp_jid=JID, xmpp_password=Pass}=State) ->
    try
	AmqpQ = gen_listener:queue_name(Srv),

	lager:debug("Auth with session ~p", [Session]),
	lager:debug("JID: ~p", [JID]),
	lager:debug("Pass: ~p", [Pass]),

	ok = exmpp_session:auth(Session, JID, Pass, password),

	{ok, _StreamId} = exmpp_session:connect_TCP(Session, Server, Port),

	lager:debug("Server: ~p:~p", [Server, Port]),
	lager:debug("StreamID: ~p", [_StreamId]),

	{ok, _JID} = exmpp_session:login(Session),

	exmpp_session:send_packet(Session, exmpp_presence:set_status(exmpp_presence:available(), "VX Whapp Ready")),
	lager:debug("Sent presence"),

	bridge(Srv, State#state{my_q=AmqpQ})
    catch
	_:{auth_error, 'not-authorized'} ->
	    lager:debug("Unauthorized login with the given credentials"),
	    exit(unauthed);
	_T:R ->
	    lager:debug("failed to login: ~p:~p", [_T, R]),
	    exit(R)
    end.

bridge(Srv, #state{my_q=Q
		   ,rayo_sip_user=SIP
		   ,rayo_lang=Lang
		   ,stream_response=StreamIt
		   ,aleg_callid=CallID
		   ,aleg_ctl_q=CtrlQ}=State) ->
    lager:debug("Bridge to ASR"),

    Command = [{<<"Application-Name">>, <<"bridge">>}
               ,{<<"Endpoints">>, make_endpoint(SIP)}
               ,{<<"Timeout">>, 1000}
               ,{<<"Call-ID">>, CallID}
               | wh_api:default_headers(Q, <<"call">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
	      ],

    {ok, Payload} = wapi_dialplan:bridge([ KV || {_, V}=KV <- Command, V =/= undefined ]),
    lager:debug("Sending ~s", [Payload]),
    wapi_dialplan:publish_action(CtrlQ, Payload),
    gen_server:cast(Srv, bridge_sent).
    
