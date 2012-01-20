%%%-------------------------------------------------------------------
%%% @author James Aimonetti <>
%%% @copyright (C) 2012, James Aimonetti
%%% @doc
%%%
%%% @end
%%% Created : 12 Jan 2012 by James Aimonetti <>
%%%-------------------------------------------------------------------
-module(dg_game).

-behaviour(gen_listener).

%% API
-export([start_link/3, handle_req/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, handle_event/2
         ,terminate/2, code_change/3]).

-include("datinggame.hrl").

-define(SERVER, ?MODULE). 

-define(RESPONDERS, [{?MODULE, [{<<"*">>, <<"*">>}]}]).
-define(QUEUE_NAME, <<>>).
-define(QUEUE_OPTIONS, []).
-define(CONSUME_OPTIONS, []).

-record(state, {
          agent = #dg_agent{} :: #dg_agent{}
         ,customer = #dg_customer{} :: #dg_customer{}
         ,recording_name = <<>> :: binary()
         ,server_pid = undefined :: undefined | pid()
         ,store_sent = false :: boolean()
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
start_link(Srv, #dg_agent{call_id=ACallID}=Agent, #dg_customer{call_id=CCallID}=Customer) ->
    Bindings = [{call, [{callid, ACallID}, {restrict_to, [events]}]}
                ,{self, []}
                ,{call, [{callid, CCallID}, {restrict_to, [events]}]}
               ],
    gen_listener:start_link(?MODULE
                            ,[{responders, ?RESPONDERS}
                              ,{bindings, Bindings}
                              ,{queue_name, ?QUEUE_NAME}
                              ,{queue_options, ?QUEUE_OPTIONS}
                              ,{consume_options, ?CONSUME_OPTIONS}
                             ]
                            ,[Srv, Agent, Customer]).

handle_req(JObj, Props) ->
    gen_listener:cast(props:get_value(server, Props), {event, JObj}).

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
init([Srv, #dg_agent{call_id=CallID}=Agent, Customer]) ->
    put(callid, CallID),

    ?LOG("the game is afoot"),
    gen_listener:cast(self(), connect_call),

    {ok, #state{
       server_pid = Srv
       ,agent = Agent
       ,customer = Customer
       ,recording_name = new_recording_name()
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
handle_cast({event, JObj}, #state{agent=Agent
                                  ,customer=(#dg_customer{call_id=CCID})=Customer
                                  ,server_pid=Srv
                                  ,recording_name=MediaName
                                  ,store_sent=StoreSent
                                 }=State) ->
    EvtType = wh_util:get_event_type(JObj),
    ?LOG("recv evt ~p", [EvtType]),

    case process_event(EvtType, JObj) of
        ignore ->
            ?LOG("ignoring event"),
            {noreply, State};
        {connected, _CallID} ->
            ?LOG("bridge on ~s", [_CallID]),
            dg_util:start_recording(Agent, MediaName),
            ?LOG("beginning recording to ~s", [MediaName]),
            {noreply, State};
        {unbridge, CallID} ->
            ?LOG(CallID, "leg has been unbridged", []),
            case CallID =/= CCID of
                true ->
                    ?LOG(CallID, "customer leg ~s unbridged, freeing agent", [CCID]),
                    datinggame_listener:free_agent(Srv, Agent),

                    dg_util:stop_recording(Agent, MediaName),
                    ?LOG(CallID, "stopping recording", []),
                    store_recording(Agent, Customer, MediaName, StoreSent),
                    {stop, normal, State};
                false ->
                    ?LOG(CCID, "agent leg ~s unbridged, that's odd", [Agent#dg_agent.call_id]),

                    dg_util:stop_recording(Agent, MediaName),
                    store_recording(Agent, Customer, MediaName, StoreSent),
                    dg_util:hangup(Customer),

                    datinggame_listener:rm_agent(Srv, Agent),
                    {stop, normal, State}
            end;
        {hangup, CallID} ->
            %% see who hung up
            ?LOG("call-id ~s hungup", [CallID]),
            case CallID =:= CCID of
                true ->
                    ?LOG("customer hungup, freeing agent"),

                    datinggame_listener:free_agent(Srv, Agent),

                    dg_util:stop_recording(Agent, MediaName),
                    store_recording(Agent, Customer, MediaName, StoreSent),
                    {stop, normal, State};
                false ->
                    ?LOG("agent hungup or disconnected"),
                    dg_util:stop_recording(Agent, MediaName),
                    store_recording(Agent, Customer, MediaName, StoreSent),
                    dg_util:hangup(Customer),
                    dg_util:hangup(Agent),
                    datinggame_listener:rm_agent(Srv, Agent),
                    {stop, normal, State}
            end;
        {channel_status, JObj} ->
            gen_listener:cast(self(), connect_call),
            ?LOG("channel status resp recv"),
            {noreply, State#state{customer=update_customer(Customer, JObj)}}
    end;

handle_cast(connect_call, #state{agent=Agent, customer=Customer}=State) ->
    ok = connect_agent(Agent, Customer),
    ?LOG("sent connection request"),
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

handle_event(_, _) ->
    {reply, []}.

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
new_recording_name() ->
    <<(list_to_binary(wh_util:to_hex(crypto:rand_bytes(16))))/binary, ".mp3">>.

-spec connect_agent/2 :: (#dg_agent{}, #dg_customer{}) -> 'ok'.
connect_agent(_Agent, #dg_customer{switch_hostname = <<>>}=Customer) ->
    ?LOG("no switch hostname known for customer, let's ask"),
    Self = self(),
    _ = spawn(fun() ->
                      Queue = gen_listener:queue_name(Self),
                      dg_util:channel_status(Customer, Queue),
                      ?LOG("sent request for customer channel_status")
              end),
    ok;
connect_agent(#dg_agent{switch_hostname=AgentHost}=Agent, #dg_customer{switch_hostname=CustomerHost}=Customer) ->
    case AgentHost of
        CustomerHost ->
            ?LOG("both agent and customer are on media switch ~s", [AgentHost]),
            dg_util:pickup_call(Agent, Customer);
        _ ->
            ?LOG("agent is on media switch ~s, customer on ~s", [AgentHost, CustomerHost]),
            IP = dg_util:get_node_ip(CustomerHost),
            #dg_customer{user=ToUser, realm=ToRealm} = Customer,

            Contact = <<"sip:", ToUser/binary, "@", ToRealm/binary>>,
            Server = <<"sip:", IP/binary, ":5060">>,

            ?LOG("redirect using contact: ~s and server: ~s", [Contact, Server]),
            dg_util:redirect(Contact, Server, Agent)
    end.

-spec process_event/2 :: ({ne_binary(), ne_binary()}, json_object()) -> 
                                 {'connected', ne_binary()} |
                                 {'hangup', ne_binary()} |
                                 {'channel_status', json_object()} |
                                 {'unbridge', ne_binary()} |
                                 'ignore'.
process_event({<<"call_event">>, <<"CHANNEL_BRIDGE">>}, JObj) ->
    CallID = wh_json:get_value(<<"Call-ID">>, JObj),
    ?LOG(CallID, "bridge event received", []),
    ?LOG(CallID, "bridge other leg id: ~s", [wh_json:get_value(<<"Other-Leg-Unique-ID">>, JObj)]),
    {connected, wh_json:get_value(<<"Call-ID">>, JObj)};
process_event({<<"call_event">>, <<"CHANNEL_UNBRIDGE">>}, JObj) ->
    CallID = wh_json:get_value(<<"Call-ID">>, JObj),
    ?LOG(CallID, "unbridge event received", []),
    ?LOG(CallID, "unbridge code: ~s", [wh_json:get_value(<<"Hangup-Code">>, JObj)]),
    ?LOG(CallID, "unbridge cause: ~s", [wh_json:get_value(<<"Hangup-Cause">>, JObj)]),
    {unbridge, CallID};
process_event({<<"call_event">>, <<"CHANNEL_HANGUP">>}, JObj) ->
    CallID = wh_json:get_value(<<"Call-ID">>, JObj),
    ?LOG(CallID, "hangup event received", []),
    ?LOG(CallID, "hangup code: ~s", [wh_json:get_value(<<"Hangup-Code">>, JObj)]),
    ?LOG(CallID, "hangup cause: ~s", [wh_json:get_value(<<"Hangup-Cause">>, JObj)]),
    {hangup, CallID};
process_event({<<"call_event">>, <<"channel_status_resp">>}, JObj) ->
    CallID = wh_json:get_value(<<"Call-ID">>, JObj),
    ?LOG(CallID, "channel_status_resp received", []),

    case wh_json:get_value(<<"Status">>, JObj) of
        <<"active">> -> {channel_status, JObj};
        _S ->
            ?LOG(CallID, "channel status is ~s", [_S]),
            {hangup, CallID}
    end;
process_event({_EvtCat, _EvtName}, _JObj) ->
    _CallID = wh_json:get_value(<<"Call-ID">>, _JObj),
    ?LOG(_CallID, "ignoring evt ~s:~s", [_EvtCat, _EvtName]),
    ?LOG(_CallID, "media app name: ~s", [wh_json:get_value(<<"Application-Name">>, _JObj)]),
    ?LOG(_CallID, "media app response: ~s", [wh_json:get_value(<<"Application-Response">>, _JObj)]),
    ignore.

-spec update_customer/2 :: (#dg_customer{}, json_object()) -> #dg_customer{}.
update_customer(Customer, JObj) ->
    CallID = wh_json:get_value(<<"Call-ID">>, JObj),

    Hostname = wh_json:get_ne_value(<<"Switch-Hostname">>, JObj),

    ?LOG(CallID, "update customer:", []),
    ?LOG(CallID, "switch hostname: ~s", [Hostname]),

    Customer#dg_customer{
      switch_hostname=Hostname
     }.

-spec new_session_doc/3 :: (#dg_agent{}, #dg_customer{}, ne_binary()) -> ne_binary().
new_session_doc(#dg_agent{id=ID, call_id=ACallID, account_db=DB}, #dg_customer{call_id=CCallID}, MediaName) ->
    MediaID = list_to_binary([ACallID, "-", CCallID]),
    JObj = wh_json:from_list([{<<"_id">>, MediaID}
                              ,{<<"agent_id">>, ID}
                              ,{<<"agent_callid">>, ACallID}
                              ,{<<"customer_callid">>, CCallID}
                              ,{<<"pvt_type">>, <<"acd_recording">>}
                              ,{<<"app_name">>, ?APP_NAME}
                              ,{<<"app_version">>, ?APP_VERSION}
                              ,{<<"media_name">>, MediaName}
                             ]),
    {ok, JObj1} = couch_mgr:save_doc(DB, JObj),
    get_new_attachment_url(DB, MediaID, MediaName, wh_json:get_value(<<"_rev">>, JObj1)).

-spec get_new_attachment_url/4 :: (ne_binary(), ne_binary(), ne_binary(), 'undefined' | ne_binary()) -> ne_binary().
get_new_attachment_url(DB, MediaID, MediaName, undefined) ->
    list_to_binary([couch_mgr:get_url(), DB, "/", MediaID, "/", MediaName]);
get_new_attachment_url(DB, MediaID, MediaName, Rev) ->
    list_to_binary([couch_mgr:get_url(), DB, "/", MediaID, "/", MediaName, <<"?rev=">>, Rev]).

store_recording(#dg_agent{call_id=CallID}=Agent, Customer, MediaName, false) ->
    RecordingURL = new_session_doc(Agent, Customer, MediaName),
    ?LOG(CallID, "storing recording ~s to ~s", [MediaName, RecordingURL]),
    dg_util:store_recording(Agent, MediaName, RecordingURL);
store_recording(#dg_agent{call_id=_CallID}, _Customer, _MediaName, true) ->
    ?LOG(_CallID, "recording of ~s already sent a store command", [_MediaName]).
