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
         ,client_vfun :: fun((api_terms()) -> boolean())
         ,neg_resp :: wh_json:json_object()
         ,neg_resp_count = 0 :: non_neg_integer()
         ,neg_resp_threshold = 2 :: pos_integer()
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
                            ,[Args]
                           ).

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
    process_flag(trap_exit, true),
    put(callid, ?LOG_SYSTEM_ID),
    lager:debug("starting amqp worker"),
    Self = self(),
    spawn(fun() ->
                  timer:sleep(1000),
                  NegThreshold = wh_util:to_integer(ecallmgr_config:get(<<"negative_response_threshold">>, <<"2">>)),
                  gen_server:cast(Self, {set_negative_threshold, NegThreshold})
          end),
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
handle_call({request, ReqProp, PublishFun, VFun, Timeout}, {ClientPid, _}=From, State) ->
    _ = wh_util:put_callid(ReqProp),
    CallID = get(callid),

    lager:debug("starting AMQP request for ~p", [ClientPid]),

    Self = self(),

    ClientRef = erlang:monitor(process, ClientPid),
    ReqRef = erlang:start_timer(Timeout, Self, req_timeout),

    {ReqProp1, MsgID} = case props:get_value(<<"Msg-ID">>, ReqProp) of
                            undefined ->
                                M = wh_util:rand_hex_binary(8),
                                {[{<<"Msg-ID">>, M} | ReqProp], M};
                            M -> {ReqProp, M}
                        end,

    spawn(fun() ->
                  put(callid, CallID),
                  Prop = [{<<"Server-ID">>, gen_listener:queue_name(Self)}
                          ,{<<"Call-ID">>, CallID}
                          | ReqProp1],
                  PublishFun(Prop)
          end),

    lager:debug("published request with msg id ~s", [MsgID]),

    {noreply, State#state{
                client_pid = ClientPid
                ,client_ref = ClientRef
                ,client_from = From
                ,client_vfun = VFun
                ,neg_resp_count = 0
                ,current_msg_id = MsgID
                ,req_timeout_ref = ReqRef
                ,req_start_time = erlang:now()
                ,callid = CallID
               }};
handle_call(_Request, _From, State) ->
    {reply, {error, not_implemented}, State}.

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
handle_cast({set_negative_threshold, NegThreshold}, State) ->
    lager:debug("set negative threshold to ~p", [NegThreshold]),
    {noreply, State#state{neg_resp_threshold = NegThreshold}};
handle_cast({event, MsgId, JObj}, #state{current_msg_id = MsgId
                                         ,client_from = From
                                         ,client_ref = ClientRef
                                         ,client_vfun = VFun
                                         ,req_timeout_ref = ReqRef
                                         ,req_start_time = StartTime
                                         ,neg_resp_count = NegCount
                                         ,neg_resp_threshold = NegThreshold
                                        }=State) when NegCount < NegThreshold ->
    _ = wh_util:put_callid(JObj),
    lager:debug("recv response for msg id ~s", [MsgId]),
    lager:debug("response took ~b micro to return", [timer:now_diff(erlang:now(), StartTime)]),

    case VFun(JObj) of
        true ->
            erlang:demonitor(ClientRef, [flush]),

            gen_server:reply(From, {ok, JObj}),
            _ = erlang:cancel_timer(ReqRef),

            put(callid, ?LOG_SYSTEM_ID),
            {noreply, reset(State)};
        false ->
            lager:debug("response failed validator, waiting for more responses"),
            {noreply, State#state{neg_resp_count = NegCount + 1, neg_resp=JObj}, 0}
    end;
handle_cast({event, _MsgId, JObj}, #state{current_msg_id=_CurrMsgId}=State) ->
    _ = wh_util:put_callid(JObj),
    lager:debug("received unexpected message with old/expired message id: ~s, waiting for ~s", [_MsgId, _CurrMsgId]),
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
handle_info(timeout, #state{neg_resp=JObj, neg_resp_count=Thresh, neg_resp_threshold=Thresh
                            ,client_ref=ClientRef, client_from=From, req_timeout_ref=ReqRef
                           }=State) ->
    lager:debug("negative resp threshold reached"),
    erlang:demonitor(ClientRef, [flush]),

    gen_server:reply(From, {error, JObj}),
    _ = erlang:cancel_timer(ReqRef),

    put(callid, ?LOG_SYSTEM_ID),
    {noreply, reset(State)};
handle_info(timeout, State) ->
    {noreply, State};
handle_info({'DOWN', ClientRef, process, _Pid, _Reason}, #state{current_msg_id = _MsgID
                                                                ,client_ref = ClientRef
                                                                ,req_timeout_ref = ReqRef
                                                                ,callid = CallID
                                                               }=State) ->
    put(callid, CallID),
    lager:debug("client ~p down with msg id ~s", [_Pid, _MsgID]),

    erlang:demonitor(ClientRef, [flush]),
    _ = erlang:cancel_timer(ReqRef),

    put(callid, ?LOG_SYSTEM_ID),
    {noreply, reset(State)};
handle_info({timeout, ReqRef, req_timeout}, #state{current_msg_id = _MsgID
                                                   ,client_ref = ClientRef
                                                   ,req_timeout_ref = ReqRef
                                                   ,client_from = From
                                                   ,callid = CallID
                                                  }=State) ->
    put(callid, CallID),
    lager:debug("request timeout exceeded for msg id: ~s", [_MsgID]),

    erlang:demonitor(ClientRef, [flush]),

    gen_server:reply(From, {error, timeout}),

    _ = erlang:cancel_timer(ReqRef),

    put(callid, ?LOG_SYSTEM_ID),
    {noreply, reset(State)};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Allows listener to pass options to handlers
%%
%% @spec handle_event(JObj, State) -> {reply, Options}
%% @end
%%--------------------------------------------------------------------
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
-spec reset/1 :: (#state{}) -> #state{}.
reset(State) ->
    State#state{client_pid = undefined
                ,client_ref = undefined
                ,client_from = undefined
                ,client_vfun = undefined
                ,neg_resp = undefined
                ,neg_resp_count = 0
                ,current_msg_id = undefined
                ,req_timeout_ref = undefined
                ,req_start_time = undefined
                ,callid = undefined
               }.
