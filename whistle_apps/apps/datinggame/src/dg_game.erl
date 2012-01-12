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
-export([start_link/3, handle_req/2, send_command/3]).

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
         ,waiting_for = connect :: 'connect' | 'customer_hangup'
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
start_link(Srv, #dg_agent{call_id=CallID}=Agent, #dg_customer{call_id=CCallID}=Customer) ->
    Bindings = [{call, [{callid, CallID}]}
                ,{call, [{callid, CCallID}]}
                ,{self, []}
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
    Srv = props:get_value(server, Props),
    ?LOG("sending event to ~p", [Srv]),
    gen_listener:cast(Srv, {event, JObj}).

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
init([Srv, Agent, Customer]) ->
    gen_listener:cast(self(), connect_call),
    ?LOG("the game has started"),
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
handle_cast({event, JObj}, #state{waiting_for=Waiting}=State) ->
    EvtType = wh_util:get_event_type(JObj),
    ?LOG("recv evt ~p", [EvtType]),

    case process_event(EvtType, JObj, Waiting) of
        ignore ->
            ?LOG("ignoring event"),
            {noreply, State};
        {hangup, CallID} ->
            % see who hung up
            ?LOG("call-id ~s hungup", [CallID]),
            {stop, normal, State}
    end;

handle_cast(connect, #state{agent=Agent, customer=Customer}=State) ->
    connect_agent(Agent, Customer),
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

-spec connect_agent/2 :: (#dg_agent{}, #dg_customer{}) -> 'ok' | {'error', _}.
connect_agent(#dg_agent{call_id=ACallID, control_queue=CtlQ}=Agent, #dg_customer{call_id=CCallID}=Customer) ->
    connect(CtlQ, ACallID, CCallID).

connect(CtlQ, ACallID, CCallID) ->
    Command = [{<<"Application-Name">>, <<"call_pickup">>}
               ,{<<"Target-Call-ID">>, CCallID}
              ],
    send_command(Command, ACallID, CtlQ).

send_command(Command, CallID, CtrlQ) ->
    Prop = Command ++ [{<<"Call-ID">>, CallID}
                       | wh_api:default_headers(<<>>, <<"call">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
                      ],
    wapi_dialplan:publish_command(CtrlQ, Prop).


-spec process_event/3 :: ({ne_binary(), ne_binary()}, json_object(), 'connect' | 'customer_hangup') -> {'hangup', ne_binary()} |
                                                                                                       'ignore'.
process_event({<<"call_event">>, <<"CHANNEL_HANGUP">> }, JObj, _) ->
    {hangup, wh_json:get_value(<<"Call-ID">>, JObj)};
process_event({_EvtCat, _EvtName}, _JObj, _WaitFor) ->
    ?LOG("ignoring evt ~s:~s in state ~s", [_EvtCat, _EvtName, _WaitFor]),
    ignore.
