%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% Worker with a dedicated targeted queue
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(ecallmgr_amqp_worker).

-behaviour(gen_listener).

%% API
-export([start_link/1, handle_resp/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, handle_event/2
         ,terminate/2, code_change/3]).

-include("ecallmgr.hrl").

-record(state, {
          current_msg_id :: ne_binary()
         ,client_pid :: pid()
         ,client_ref :: reference()
         ,client_from :: {pid(), reference()}
         ,req_timeout_ref :: reference()
         ,req_start_time :: wh_now()
         ,callid :: ne_binary()
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
start_link(Args) ->
    gen_listener:start_link(?MODULE
                            ,[{bindings, [{self, []}]}
                              ,{responders, [{{?MODULE, handle_resp}, [{<<"*">>, <<"*">>}]}]}
                             ]
                            ,[Args]).

handle_resp(JObj, Props) ->
    gen_listener:cast(props:get_value(server, Props), {event, wh_json:get_value(<<"Msg-ID">>, JObj), JObj}).

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
init([_Args]) ->
    put(callid, ?LOG_SYSTEM_ID),
    lager:debug("starting amqp worker"),

    {ok, #state{}}.

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
handle_call({request, ReqProp, PublishFun, CallID, Timeout}, {ClientPid, _}=From, State) ->
    put(callid, CallID),
    lager:debug("starting AMQP request for ~p", [ClientPid]),

    Self = self(),

    ClientRef = erlang:monitor(process, ClientPid),
    ReqRef = erlang:start_timer(Timeout, Self, req_timeout),

    spawn(fun() ->
                  put(callid, CallID),
                  Prop = [{<<"Server-ID">>, gen_listener:queue_name(Self)}
                          ,{<<"Call-ID">>, CallID}
                          | ReqProp],
                  PublishFun(Prop)
          end),

    MsgID = props:get_value(<<"Msg-ID">>, ReqProp),

    lager:debug("published request with msg id ~s", [MsgID]),

    {noreply, State#state{
                client_pid = ClientPid
                ,client_ref = ClientRef
                ,client_from = From
                ,current_msg_id = MsgID
                ,req_timeout_ref = ReqRef
                ,req_start_time = erlang:now()
                ,callid = CallID
               }}.

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
handle_cast({event, MsgId, JObj}, #state{current_msg_id = MsgId
                                         ,client_from = From
                                         ,client_ref = ClientRef
                                         ,req_timeout_ref = ReqRef
                                         ,req_start_time = StartTime
                                         ,callid = CallID
                                        }) ->
    put(callid, CallID),
    lager:debug("recv response with msg id ~s", [MsgId]),

    erlang:demonitor(ClientRef, [flush]),

    gen_server:reply(From, {ok, JObj}),

    lager:debug("response took ~b micro to return", [timer:now_diff(erlang:now(), StartTime)]),

    erlang:cancel_timer(ReqRef),

    {noreply, #state{}};
handle_cast({event, _MsgId, JObj}, State) ->
    wh_util:put_callid(JObj),
    lager:debug("received message with old/expired message id: ~s", [_MsgId]),
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
handle_info({'DOWN', ClientRef, process, _Pid, _Reason}, #state{current_msg_id = _MsgID
                                                                ,client_ref = ClientRef
                                                                ,req_timeout_ref = ReqRef
                                                                ,callid = CallID
                                                              }) ->
    put(callid, CallID),
    lager:debug("client ~p down with msg id ~s", [_Pid, _MsgID]),

    erlang:demonitor(ClientRef, [flush]),
    erlang:cancel_timer(ReqRef),

    poolboy:checkin(self()),

    {noreply, #state{}};
handle_info({timeout, ReqRef, req_timeout}, #state{current_msg_id = _MsgID
                                                   ,client_ref = ClientRef
                                                   ,req_timeout_ref = ReqRef
                                                   ,client_from = From
                                                   ,callid = CallID
                                                  }) ->
    put(callid, CallID),
    lager:debug("request timeout exceeded for msg id: ~s", [_MsgID]),

    erlang:demonitor(ClientRef, [flush]),

    gen_server:reply(From, {error, timeout}),

    erlang:cancel_timer(ReqRef),

    {noreply, #state{}};

handle_info(_Info, State) ->
    put(callid, ?LOG_SYSTEM_ID),
    lager:debug("unhandled message: ~p", [_Info]),
    {noreply, State}.

handle_event(_JObj, _State) ->
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
    lager:debug("amqp worker terminating: ~p", [_Reason]).

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
