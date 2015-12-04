%% Copyright ProcessOne 2006-2010. All Rights Reserved.
%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.

%% @author Ery Lee<ery.lee@gmail.com>

%% @doc
%% The module <strong>{@module}</strong> implements xep-0114 component.
%%
%% <p>
%% This module is intended to be used directly by client developers.
%% </p>
%%

-module(exmpp_component).
-behaviour(gen_fsm).

%% XMPP Component API:
-export([start/0, start_link/0, start_debug/0, stop/1]).

-export([auth/3, 
     connect/3,
     handshake/1,
     send_packet/2,
	 set_controlling_process/2]).

%% gen_fsm callbacks
-export([init/1,
	 code_change/4,
	 handle_info/3,
	 handle_event/3,
	 handle_sync_event/4,
	 terminate/3]).

%% States
-export([setup/3, 
	 wait_for_stream/2, wait_for_stream/3,
	 stream_opened/2, stream_opened/3,
	 stream_error/2, stream_error/3,
	 stream_closed/2, stream_closed/3,
	 wait_for_handshake_result/2,
	 session_established/2, session_established/3
	]).

-include("exmpp.hrl").
-include("exmpp_client.hrl").

-record(state, {
      domain,
      auth_method,
	  client_pid,
	  connection = exmpp_socket,
	  connection_ref,
	  stream_ref,
      stream_id = false,
	  stream_error,
	  receiver_ref,
	  from_pid           %% Use by gen_fsm to handle postponed replies
	 }).

%% This timeout should match the connect timeout
-define(TIMEOUT, 5000).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> ok,Pid} | ignore | {error,Error}
%% Description:Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this function
%% does not return until Module:init/1 has returned.
%%--------------------------------------------------------------------
%% Start the Component (used to get a reference):
start() ->
    case gen_fsm:start(?MODULE, [self()], []) of
	{ok, PID} -> PID;
	{error, Reason} -> erlang:error({error, Reason})
    end.
%% Start the Component (used to get a reference):
start_link() ->
    case gen_fsm:start_link(?MODULE, [self()], []) of
	{ok, PID} -> PID;
	{error, Reason} -> erlang:error({error, Reason})
    end.

%% Start the Component in debug mode
%% (trace events)
start_debug() ->
    case gen_fsm:start(?MODULE, [self()], [{debug,[trace]}]) of
	{ok, PID} -> PID;
	{error, Reason} -> erlang:error({error, Reason})
    end.

%% Close Component and disconnect
stop(Component) ->
    catch gen_fsm:sync_send_all_state_event(Component, stop),
    ok.

%% Set authentication mode to basic (password)
auth(Component, Domain, Password)
  when is_pid(Component),
       is_list(Domain),
       is_list(Password) ->
    gen_fsm:sync_send_event(Component, {set_auth, Domain, Password}).

%% Initiate standard TCP XMPP server connection
%% If the domain is not passed we expect to find it in the authentication
%% method. It should thus be set before.
%% Returns StreamId (String)
connect(Component, Server, Port)
  when is_pid(Component),
       is_list(Server),
       is_integer(Port) ->
    case gen_fsm:sync_send_event(Component, {connect_tcp, Server, Port},
				 ?TIMEOUT) of
	Error when is_tuple(Error) -> erlang:throw(Error);
	StreamId -> StreamId
    end.

%% Handshake
%% Returns ok
handshake(Component) when is_pid(Component) ->
    case gen_fsm:sync_send_event(Component, {handshake}) of
	ok -> ok;
	Error when is_tuple(Error) -> erlang:throw(Error)
    end.

%% Send any exmpp formatted packet
send_packet(Component, Packet) when is_pid(Component) ->
    case gen_fsm:sync_send_event(Component, {send_packet, Packet}) of
	Error when is_tuple(Error) -> erlang:throw(Error);
        Id -> Id
    end.

set_controlling_process(Component,Client) when is_pid(Component), is_pid(Client) ->
    case gen_fsm:sync_send_all_state_event(Component, {set_controlling_process, Client}) of
	Error when is_tuple(Error) -> erlang:throw(Error);
        Id -> Id
    end.

%%====================================================================
%% gen_fsm callbacks
%%====================================================================
init([Pid]) ->
    inets:start(),
    exmpp_stringprep:start(),
    {A1,A2,A3} = now(),
    random:seed(A1, A2, A3),
    {ok, setup, #state{client_pid=Pid}}.

handle_event(tcp_closed, _StateName, State) ->
    {stop, tcp_closed, State};

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(stop, _From, _StateName, State) ->
    Reply = ok,
    {stop, normal, Reply, State};
handle_sync_event({set_controlling_process,Client}, _From, StateName, State) ->
    Reply = ok,
    {reply,Reply,StateName,State#state{client_pid=Client}};
handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.


terminate(Reason, _StateName, #state{connection_ref = undefined,
				     stream_ref = undefined,
				     from_pid=From}) ->
    reply(Reason, From),
    ok;
terminate(Reason, _StateName, #state{connection_ref = undefined,
				     stream_ref = StreamRef,
				     from_pid=From}) ->
    exmpp_xmlstream:stop(StreamRef),
    exmpp_xml:stop_parser(exmpp_xmlstream:get_parser(StreamRef)),
    reply(Reason, From),
    ok;
terminate(Reason, _StateName, #state{connection_ref = ConnRef,
				     connection = Module,
				     stream_ref = undefined,
				     from_pid=From}) ->
    Module:close(ConnRef),
    reply(Reason, From),
    ok;
terminate(Reason, _StateName, #state{connection_ref = ConnRef,
				     connection = Module,
				     stream_ref = StreamRef,
				     receiver_ref = ReceiverRef,
				     from_pid=From}) ->
    exmpp_xmlstream:stop(StreamRef),
    exmpp_xml:stop_parser(exmpp_xmlstream:get_parser(StreamRef)),
    Module:close(ConnRef, ReceiverRef),
    reply(Reason, From),
    ok.

%% Send gen_fsm reply if needed
reply(_Reply, undefined) ->
    ok;
reply(Reply, {P, _} = From) when is_pid(P) ->
    gen_fsm:reply(From, Reply);
reply(_, _) ->
    ok.

%%--------------------------------------------------------------------
%% Function:
%% code_change(OldVsn, StateName, State, Extra) -> {ok, StateName, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%====================================================================
%% States
%%====================================================================
%% ---------------------------
%% Setup state: Configuration

%% Define JID and authentication method
setup({set_auth, Domain, Password}, _From, State) ->
    {reply, ok, setup, State#state{auth_method ={Domain, Password}}};
setup({connect_tcp, Host, Port}, From, State) ->
    case State#state.auth_method of
	undefined ->
	    {reply, {connect_error,
		     authentication_or_domain_undefined}, setup, State};
	_Other ->
	    connect(exmpp_socket, {Host, Port, []}, From, State)
    end.

%% ---------------------------
%% Stream negociation:

%% Standard opening stream:
-define(stream,
	#xmlstreamstart{element=#xmlel{
			  ns='http://etherx.jabber.org/streams',
			  name=stream}}).
%% Standard stream error:
-define(streamerror,
	#xmlstreamelement{element=#xmlel{
			    ns='http://etherx.jabber.org/streams',
			    name=error,
			    children=[#xmlel{name=Reason} | _MoreReasons]}}).

%% Special stream error: disconnected
-define(streamdisconnected,
        #xmlstreamelement{element=#xmlel{
			    ns='http://etherx.jabber.org/streams',
			    name=error,
			    children=[#xmlcdata{cdata=  <<"Disconnected">> }]}}).

%% Standard end of stream:
-define(streamend,
        #xmlstreamend{endtag=#xmlendtag{
			ns='http://etherx.jabber.org/streams',
			name=stream}}).

%% We cannot receive API call in this state
wait_for_stream(_Event, _From, State) ->
    {reply, {error, busy_connecting_to_server}, wait_for_stream, State}.
%% TODO: Check that we receive a client stream. Need change in the
%% parsing library.
wait_for_stream(Start = ?stream, State = #state{connection = _Module,
						connection_ref = _ConnRef,
						auth_method = _Auth,
						from_pid = From}) ->
    %% Get StreamID
    StreamId = exmpp_xml:get_attribute_as_list(Start#xmlstreamstart.element, <<"id">>, ""),
    gen_fsm:reply(From, StreamId),
    {next_state, stream_opened, State#state{from_pid=undefined,
					    stream_id = StreamId}}.

%% ---------------------------
%% Between stream opening and Component opening

%% Supported user commands at this stage:
%% handshake and register
stream_opened({handshake}, _From,State=#state{auth_method=undefined}) ->
    {reply, {error, auth_method_undefined}, stream_opened, State};
stream_opened({handshake}, From, State=#state{connection = Module,
					  connection_ref = ConnRef,
                      stream_id = StreamId, auth_method = {_Domain, Password}}) ->
    %% Retrieve supported authentication methods:
    %% TODO: Do different thing if we use basic or SASL auth
    %% For now, we consider everything is legacy (basic)
    Digest = exmpp_client_legacy_auth:hex(exmpp_client_legacy_auth:digest(StreamId, Password)),
    Handshake = #xmlel{name=handshake, children= [?XMLCDATA(Digest)]},
    Module:send(ConnRef, Handshake),
    {next_state, wait_for_handshake_result, State#state{from_pid=From}};

%% We can define update handshake informations after we are connected to
%% the XMPP server:
%% Define JID and authentication method
stream_opened({set_auth, Domain, Password}, _From, State) ->
    {reply, ok, stream_opened, State#state{auth_method= {Domain, Password}}};
stream_opened({presence, _Status, _Show}, _From, State) ->
    {reply, {error, not_session_established}, setup, State};
%% We allow to send packet here to give control to the developer on all packet
%% send to the server. The developer can implements his own login management
%% code.
%% If the packet is an iq set or get:
%% We check that there is a valid id and return it to match the reply
stream_opened({send_packet, Packet}, _From,
	      State = #state{connection = Module,
			     connection_ref = ConnRef}) ->
    Id = send_packet(Packet, Module, ConnRef),
    {reply, Id, stream_opened, State}.

%% Process incoming
%% Dispatch incoming messages
stream_opened(#xmlstreamelement{element=#xmlel{name=message, attrs=Attrs}=MessageElement}, State = #state{connection = _Module, connection_ref = _ConnRef}) ->
    process_message(State#state.client_pid, Attrs, MessageElement),
    {next_state, stream_opened, State};
%% Dispach IQs from server
stream_opened(#xmlstreamelement{element=#xmlel{name=iq, attrs=Attrs}=IQElement}, State) ->
    process_iq(State#state.client_pid, Attrs, IQElement),
    {next_state, stream_opened, State};
%% Handle stream error: We keep the process alive to be able
%%                      return errors
stream_opened(?streamerror, State) ->
    {next_state, stream_error, State#state{stream_error=Reason}};
%% Handle end of stream
stream_opened(?streamend, State) ->
    {next_state, stream_closed, State}.

stream_error(_Signal, _From, State) ->
    {reply, {stream_error, State#state.stream_error}, stream_error, State}.
stream_error(?streamend, State) ->
    {next_state, stream_closed, State};
stream_error(_Signal, State) ->
    {next_state, stream_error, State}.

stream_closed(_Signal, _From, State = #state{stream_error = undefined}) ->
    {reply, {stream_closed, undefined}, stream_closed, State};
stream_closed(_Signal, _From, State) ->
    {reply, {stream_error, State#state.stream_error}, stream_closed, State}.
stream_closed(_Signal, State) ->
    {next_state, stream_closed, State}.

wait_for_handshake_result(#xmlstreamelement{element=#xmlel{name=handshake}} , State=#state{from_pid = From}) ->
    case From of
        undefined -> ok;
        From -> gen_fsm:reply(From, ok)
    end,
	{next_state, session_established, State};

%% Reason comes from streamerror macro
wait_for_handshake_result(?streamerror, State) ->
    {stop, {error, Reason}, State}.

%% ---
%% Send packets
%% If the packet is an iq set or get:
%% We check that there is a valid id and return it to match the reply
session_established({send_packet, Packet}, _From,
	  State = #state{connection = Module,
			 connection_ref = ConnRef}) ->
    Id = send_packet(Packet, Module, ConnRef),
    {reply, Id, session_established, State}.

%% ---
%% Receive packets
%% When logged in we dispatch the event we receive
%% Dispatch incoming presence packets
session_established(#xmlstreamelement{element=#xmlel{name=presence, attrs=Attrs}=PresenceElement},
	  State = #state{connection = _Module, connection_ref = _ConnRef}) ->
    process_presence(State#state.client_pid, Attrs, PresenceElement),
    {next_state, session_established, State};
%% Dispatch incoming messages
session_established(#xmlstreamelement{element=#xmlel{name=message, attrs=Attrs}=MessageElement}, State = #state{connection = _Module, connection_ref = _ConnRef}) ->
    process_message(State#state.client_pid, Attrs, MessageElement),
    {next_state, session_established, State};
%% Dispach IQs from server
session_established(#xmlstreamelement{element=#xmlel{name=iq, attrs=Attrs}=IQElement}, State) ->
    process_iq(State#state.client_pid, Attrs, IQElement),
    {next_state, session_established, State};
%% Process unexpected packet
session_established(_Packet, State) ->
    %% log it or do something better
    %io:format("!!!ALERT!!! Unknown packet:~p~p~n", [_Packet, State]),
    {next_state, session_established, State}.

%% TODO:
%% Handle disconnections
%% Connection replaced.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

%% Connect to server
connect(Module, Params, From, State=#state{auth_method = {Domain, _P}}) ->
    connect(Module, Params, Domain, From, State).
connect(Module, Params, Domain, From, #state{client_pid=_ClientPid} = State) ->
    try start_parser() of
	StreamRef ->
	    try Module:connect(self(), StreamRef, Params) of
		{ConnRef, ReceiverRef} ->
		    %% basic (legacy) authent: we do not use version
		    %% 1.0 in stream:
		    ok = Module:send(ConnRef,
				     exmpp_stream:opening(Domain,
							  ?NS_COMPONENT_ACCEPT,
							  {0,0})),
		    %% TODO: Add timeout on wait_for_stream to return
		    %% meaningfull error.
		    {next_state, wait_for_stream,
		     State#state{domain = Domain,
				 connection = Module,
				 connection_ref = ConnRef,
				 stream_ref = StreamRef,
				 receiver_ref = ReceiverRef,
				 from_pid = From}}
	    catch
		Error ->
		    exmpp_xmlstream:stop(StreamRef),
		    %% We do not stop here, because the developer
		    %% might want to start a connection using another
		    %% transport
		    {reply, Error, setup,
		     State#state{stream_ref = undefined,
				 from_pid = From}}
	    end
    catch
	Error ->
	    {reply, Error, setup, State}
    end.

%% Define parser options
%% No compatibility mode: We use all the nice optimisation of exmpp:
-define(PARSER_OPTIONS,
	[
	 {names_as_atom, true},
	 {check_nss, xmpp},
	 {check_elems, xmpp},
	 {emit_endtag, false},
	 {root_depth, 0},
	 {max_size, infinity}]).

%% Start parser and return stream reference
start_parser() ->
    exmpp_xmlstream:start({gen_fsm, self()},
                          exmpp_xml:start_parser(?PARSER_OPTIONS),
                          [{xmlstreamstart,new}]).

%% Packet processing functions
parse_and_deliver(ClientPid, Attrs, Packet, F) ->
	try
		F(ClientPid, Attrs, Packet)
	catch
		_:_ ->
			%%Some error, deliver only the raw packet
			ClientPid ! #received_packet{packet_type = undefined,
                                 type_attr = undefined,
                                 from = undefined,
                                 id = undefined,
                                 raw_packet = Packet}
	end.

process_presence(ClientPid, Attrs, Packet) ->
	parse_and_deliver(ClientPid, Attrs, Packet, fun do_process_presence/3).
process_message(ClientPid, Attrs, Packet) ->
	parse_and_deliver(ClientPid, Attrs, Packet, fun do_process_message/3).
process_iq(ClientPid, Attrs, Packet) ->
	parse_and_deliver(ClientPid, Attrs, Packet, fun do_process_iq/3).

do_process_presence(ClientPid, Attrs, Packet) ->
    Type = get_attribute_value(Attrs, <<"type">>, "available"),
    Who = exmpp_jid:to_lower(get_attribute_value(Attrs, <<"from">>, "")),
    Id = get_attribute_value(Attrs, <<"id">>, ""),
    ClientPid ! #received_packet{packet_type = presence,
                                 type_attr = Type,
                                 from = Who,
                                 id = Id,
                                 raw_packet = Packet}.

do_process_message(ClientPid, Attrs, Packet) ->
    Type = get_attribute_value(Attrs, <<"type">>, "normal"),
    Who = exmpp_jid:to_lower(get_attribute_value(Attrs, <<"from">>, "")),
    Id = get_attribute_value(Attrs, <<"id">>, ""),
    ClientPid ! #received_packet{packet_type = message,
                                 type_attr = Type,
                                 from = Who,
                                 id = Id,
                                 raw_packet = Packet}.

do_process_iq(ClientPid, Attrs, Packet) ->
    Type = get_attribute_value(Attrs, <<"type">>, ""),
    Who = exmpp_jid:to_lower(get_attribute_value(Attrs, <<"from">>, "")),
    Id = get_attribute_value(Attrs, <<"id">>, ""),
    NS = exmpp_iq:get_payload_ns_as_atom(Packet),
    ClientPid ! #received_packet{packet_type = iq,
                                 type_attr = Type,
                                 from = Who,
                                 id = Id,
                                 queryns = NS,
                                 raw_packet = Packet}.

%% Add a packet ID is needed:
%% Check that the attribute list has defined an ID.
%% If no ID has been defined, add a packet id to the list of attributes
%% This function uses {@link random:uniform/1}. It's up to the caller to
%% seed the generator.
check_id(Attrs) ->
    case exmpp_xml:get_attribute_from_list_as_binary(Attrs, <<"id">>, <<>>) of
	<<>> ->
	    Id = exmpp_utils:random_id("Component"),
	    {exmpp_xml:set_attribute_in_list(Attrs, <<"id">>, Id), Id};
        Id -> {Attrs, Id}
    end.

%% Try getting a given atribute from a list of xmlattr records
%% Return default value if attribute is not found
get_attribute_value(Attrs, Attr, Default) ->
    exmpp_xml:get_attribute_from_list_as_list(Attrs, Attr, Default).

%% Internal operations
%% send_packet: actually format and send the packet:
send_packet(#xmlel{name=iq, attrs=Attrs}=IQElement, Module, ConnRef) ->
    Type = exmpp_xml:get_attribute_from_list_as_binary(Attrs, <<"type">>, undefined),
    case Type of
	<<"error">> ->
	    {Attrs2, PacketId} = check_id(Attrs),
	    Module:send(ConnRef, IQElement#xmlel{attrs=Attrs2}),
	    PacketId;
	<<"result">> ->
	    {Attrs2, PacketId} = check_id(Attrs),
	    Module:send(ConnRef, IQElement#xmlel{attrs=Attrs2}),
	    PacketId;
	<<"set">> ->
	    {Attrs2, PacketId} = check_id(Attrs),
	    Module:send(ConnRef, IQElement#xmlel{attrs=Attrs2}),
	    PacketId;
	<<"get">> ->
	    {Attrs2, PacketId} = check_id(Attrs),
	    Module:send(ConnRef, IQElement#xmlel{attrs=Attrs2}),
	    PacketId
    end;
send_packet(#xmlel{attrs=Attrs}=Element, Module, ConnRef) ->
    {Attrs2, Id} = check_id(Attrs),
    Module:send(ConnRef, Element#xmlel{attrs=Attrs2}),
    Id.

