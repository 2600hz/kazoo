%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2014, 2600Hz
%%% @doc
%%% Worker with a dedicated targeted queue.
%%%
%%% Inserts Queue Name as the Server-ID and proxies the AMQP request
%%% (expects responses to the request)
%%%
%%% Two primary interactions, call and call_collect
%%%   call: The semantics of call are similar to gen_server's call: send a
%%%     request, expect a response back (or timeout)
%%%   call_collect: uses the timeout to collect responses (successful or not)
%%%     and returns the resulting list of responses
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(wh_amqp_worker).

-behaviour(gen_listener).

%% API
-export([start_link/1

         ,call/3, call/4
         ,call_collect/2, call_collect/3, call_collect/4

         ,call_custom/4, call_custom/5

         ,cast/2

         ,default_timeout/0
         ,collect_until_timeout/0
         ,collect_from_whapp/1
         ,collect_from_whapp_or_validate/2
         ,handle_resp/2
         ,send_request/4
        ]).

%% gen_listener callbacks
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,handle_event/2
         ,terminate/2
         ,code_change/3
        ]).

-include("amqp_util.hrl").

-define(FUDGE, 2600).
-define(BINDINGS, [{'self', []}]).
-define(RESPONDERS, [{{?MODULE, 'handle_resp'}
                      ,[{<<"*">>, <<"*">>}]}
                    ]).
-define(QUEUE_NAME, <<>>).
-define(QUEUE_OPTIONS, []).
-define(CONSUME_OPTIONS, []).

-type publish_fun() :: fun((api_terms()) -> _).
-type validate_fun() :: fun((api_terms()) -> boolean()).
-type collect_until_fun() :: fun((wh_json:objects()) -> boolean()).

-type whapp() :: atom() | ne_binary().

-type collect_until() :: collect_until_fun() |
                         whapp() |
                         {whapp(), validate_fun() | boolean()} |
                         {whapp(), validate_fun(), boolean()}.
-type timeout_or_until() :: wh_timeout() | collect_until().

-export_type([publish_fun/0
              ,validate_fun/0
              ,collect_until/0
              ,timeout_or_until/0
             ]).

-record(state, {current_msg_id :: ne_binary()
                ,client_pid :: pid()
                ,client_ref :: reference()
                ,client_from :: {pid(), reference()}
                ,client_vfun :: validate_fun()
                ,client_cfun = collect_until_timeout() :: collect_until_fun()
                ,responses :: wh_json:objects()
                ,neg_resp :: wh_json:object()
                ,neg_resp_count = 0 :: non_neg_integer()
                ,neg_resp_threshold = 1 :: pos_integer()
                ,req_timeout_ref :: reference()
                ,req_start_time :: wh_now()
                ,callid :: ne_binary()
                ,pool_ref :: server_ref()
                ,defer_response :: api_object()
                ,queue :: api_binary()
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
    gen_listener:start_link(?MODULE, [{'bindings', ?BINDINGS}
                                      ,{'responders', ?RESPONDERS}
                                      ,{'queue_name', ?QUEUE_NAME}
                                      ,{'queue_options', ?QUEUE_OPTIONS}
                                      ,{'consume_options', ?CONSUME_OPTIONS}
                                     ], [Args]).

-spec default_timeout() -> 2000.
default_timeout() -> 2000.

-type request_return() :: {'ok', wh_json:object() | wh_json:objects()} |
                          {'returned', wh_json:object(), wh_json:object()} |
                          {'timeout', wh_json:objects()} |
                          {'error', _}.
-spec call(api_terms(), publish_fun(), validate_fun()) ->
                  request_return().
-spec call(api_terms(), publish_fun(), validate_fun(), wh_timeout()) ->
                  request_return().
-spec call(api_terms(), publish_fun(), validate_fun(), wh_timeout(), pid()) ->
                  request_return().
call(Req, PubFun, VFun) ->
    call(Req, PubFun, VFun, default_timeout()).

call(Req, PubFun, VFun, Timeout) ->
    case next_worker() of
        {'error', _}=E -> E;
        Worker -> call(Req, PubFun, VFun, Timeout, Worker)
    end.

call(Req, PubFun, VFun, Timeout, Worker) ->
    Prop = maybe_convert_to_proplist(Req),
    try gen_listener:call(Worker
                          ,{'request', Prop, PubFun, VFun, Timeout}
                          ,fudge_timeout(Timeout)
                         )
    of
        Reply -> Reply
    catch
        _E:R ->
            lager:warning("request failed: ~s: ~p", [_E, R]),
            {'error', R}
    after
        checkin_worker(Worker)
    end.

-spec next_worker() -> pid() |
                       {'error', _}.
next_worker() ->
    Pool = wh_amqp_sup:pool_name(),
    try poolboy:checkout(Pool, 'false', default_timeout()) of
        'full' -> {'error', 'pool_full'};
        Worker -> Worker
    catch
        _E:_R ->
            lager:warning("poolboy exception: ~s: ~p", [_E, _R]),
            {'error', 'poolboy_fault'}
    end.

-spec checkin_worker(pid()) -> 'ok'.
checkin_worker(Worker) ->
    Pool = wh_amqp_sup:pool_name(),
    poolboy:checkin(Pool, Worker).

-spec call_custom(api_terms(), publish_fun(), validate_fun(), gen_listener:binding()) ->
                         request_return().
-spec call_custom(api_terms(), publish_fun(), validate_fun(), wh_timeout(), gen_listener:binding()) ->
                         request_return().
-spec call_custom(api_terms(), publish_fun(), validate_fun(), wh_timeout(), gen_listener:binding(), pid()) ->
                         request_return().
call_custom(Req, PubFun, VFun, Bind) ->
    call_custom(Req, PubFun, VFun, default_timeout(), Bind).
call_custom(Req, PubFun, VFun, Timeout, Bind) ->
    case next_worker() of
        {'error', _}=E -> E;
        Worker -> call_custom(Req, PubFun, VFun, Timeout, Bind, Worker)
    end.

call_custom(Req, PubFun, VFun, Timeout, Bind, Worker) ->
    Prop = maybe_convert_to_proplist(Req),
    gen_listener:add_binding(Worker, Bind),
    try gen_listener:call(Worker
                          ,{'request', Prop, PubFun, VFun, Timeout}
                          ,fudge_timeout(Timeout)
                         )
    of
        Reply -> Reply
    catch
        _E:R ->
            lager:debug("request failed: ~s: ~p", [_E, R]),
            {'error', R}
    after
        gen_listener:rm_binding(Worker, Bind),
        checkin_worker(Worker)
    end.

-spec call_collect(api_terms(), publish_fun()) ->
                          request_return().
-spec call_collect(api_terms(), publish_fun(), timeout_or_until()) ->
                          request_return().
-spec call_collect(api_terms(), publish_fun(), collect_until(), wh_timeout()) ->
                          request_return().
-spec call_collect(api_terms(), publish_fun(), collect_until(), wh_timeout(), pid()) ->
                          request_return().
call_collect(Req, PubFun) ->
    call_collect(Req, PubFun, default_timeout()).

call_collect(Req, PubFun, UntilFun) when is_function(UntilFun) ->
    call_collect(Req, PubFun, UntilFun, default_timeout());
call_collect(Req, PubFun, Whapp) when is_atom(Whapp); is_binary(Whapp) ->
    call_collect(Req, PubFun, Whapp, default_timeout());
call_collect(Req, PubFun, {_, _}=Until) ->
    call_collect(Req, PubFun, Until, default_timeout());
call_collect(Req, PubFun, {_, _, _}=Until) ->
    call_collect(Req, PubFun, Until, default_timeout());
call_collect(Req, PubFun, Timeout) ->
    call_collect(Req, PubFun, collect_until_timeout(), Timeout).

call_collect(Req, PubFun, {Whapp, IncludeFederated}, Timeout)
  when (is_atom(Whapp) orelse is_binary(Whapp))
       andalso is_boolean(IncludeFederated) ->
    CollectFromWhapp = collect_from_whapp(Whapp, IncludeFederated),
    call_collect(Req, PubFun, CollectFromWhapp, Timeout);
call_collect(Req, PubFun, {Whapp, VFun}, Timeout)
  when (is_atom(Whapp) orelse is_binary(Whapp))
       andalso is_function(VFun) ->
    CollectFromWhapp = collect_from_whapp_or_validate(Whapp, VFun),
    call_collect(Req, PubFun, CollectFromWhapp, Timeout);
call_collect(Req, PubFun, {Whapp, VFun, IncludeFederated}, Timeout)
  when (is_atom(Whapp) orelse is_binary(Whapp))
       andalso is_function(VFun)
       andalso is_boolean(IncludeFederated) ->
    CollectFromWhapp = collect_from_whapp_or_validate(Whapp, VFun, IncludeFederated),
    call_collect(Req, PubFun, CollectFromWhapp, Timeout);
call_collect(Req, PubFun, Whapp, Timeout)
  when is_atom(Whapp) orelse is_binary(Whapp) ->
    call_collect(Req, PubFun, collect_from_whapp(Whapp), Timeout);
call_collect(Req, PubFun, UntilFun, Timeout)
  when is_integer(Timeout), Timeout >= 0 ->
    case next_worker() of
        {'error', _}=E -> E;
        Worker -> call_collect(Req, PubFun, UntilFun, Timeout, Worker)
    end.

call_collect(Req, PubFun, UntilFun, Timeout, Worker) ->
    Prop = maybe_convert_to_proplist(Req),
    try gen_listener:call(Worker
                          ,{'call_collect', Prop, PubFun, UntilFun, Timeout}
                          ,fudge_timeout(Timeout)
                         )
    of
        Reply -> Reply
    catch
        _E:R ->
            lager:debug("request failed: ~s: ~p", [_E, R]),
            {'error', R}
    after
        checkin_worker(Worker)
    end.

-spec cast(api_terms(), publish_fun()) -> 'ok' | {'error', _}.
-spec cast(api_terms(), publish_fun(), pid()) -> 'ok' | {'error', _}.
cast(Req, PubFun) ->
    case next_worker() of
        {'error', _}=E -> E;
        Worker -> cast(Req, PubFun, Worker)
    end.

cast(Req, PubFun, Worker) ->
    Prop = maybe_convert_to_proplist(Req),
    try gen_listener:call(Worker, {'publish', Prop, PubFun}) of
        Reply -> Reply
    catch
        _E:R ->
            lager:debug("request failed: ~s: ~p", [_E, R]),
            {'error', R}
    after
        checkin_worker(Worker)
    end.

-spec collect_until_timeout() -> collect_until_fun().
collect_until_timeout() -> fun wh_util:always_false/1.

-spec collect_from_whapp(text()) -> collect_until_fun().
collect_from_whapp(Whapp) ->
    collect_from_whapp(Whapp, 'false').

-spec collect_from_whapp(text(), boolean()) -> collect_until_fun().
collect_from_whapp(Whapp, IncludeFederated) ->
    Count = wh_nodes:whapp_count(Whapp, IncludeFederated),
    lager:debug("attempting to collect ~p responses from ~s", [Count, Whapp]),
    fun(Responses) -> length(Responses) >= Count end.

-spec collect_from_whapp_or_validate(text(), validate_fun()) -> collect_until_fun().
collect_from_whapp_or_validate(Whapp, VFun) ->
    collect_from_whapp_or_validate(Whapp, VFun, 'false').

-spec collect_from_whapp_or_validate(text(),validate_fun(), boolean()) -> collect_until_fun().
collect_from_whapp_or_validate(Whapp, VFun, IncludeFederated) ->
    Count = wh_nodes:whapp_count(Whapp, IncludeFederated),
    lager:debug("attempting to collect ~p responses from ~s or the first valid", [Count, Whapp]),
    fun([Response|_]=Responses) ->
            length(Responses) >= Count
                orelse VFun(Response)
    end.

-spec handle_resp(wh_json:object(), wh_proplist()) -> 'ok'.
handle_resp(JObj, Props) ->
    gen_listener:cast(props:get_value('server', Props)
                      ,{'event', wh_json:get_value(<<"Msg-ID">>, JObj), JObj}
                     ).

-spec send_request(ne_binary(), ne_binary(), publish_fun(), wh_proplist()) ->
                          'ok' | {'error', _}.
send_request(CallID, Self, PublishFun, ReqProp) when is_function(PublishFun, 1) ->
    put('callid', CallID),
    Prop = [{<<"Server-ID">>, Self}
            ,{<<"Call-ID">>, CallID}
            | ReqProp
           ],
    try PublishFun(Prop) of
        'ok' -> 'ok'
    catch
        _:E -> {'error', E}
    end.

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
init([Args]) ->
    put('callid', ?LOG_SYSTEM_ID),
    lager:debug("starting amqp worker"),
    NegThreshold = props:get_value('neg_resp_threshold', Args, 1),
    Pool = props:get_value('name', Args, 'undefined'),
    {'ok', #state{neg_resp_threshold=NegThreshold
                  ,pool_ref=Pool
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
handle_call({'request', ReqProp, _, _, _}, _, #state{queue='undefined'}=State) ->
    lager:debug("unable to publish request prior to queue creation: ~p", [ReqProp]),
    {'reply', {'error', 'timeout'}, reset(State)};
handle_call({'request', ReqProp, PublishFun, VFun, Timeout}
            ,{ClientPid, _}=From
            ,#state{queue=Q}=State
           ) ->
    _ = wh_util:put_callid(ReqProp),
    CallID = get('callid'),
    {ReqProp1, MsgID} = case props:get_value(<<"Msg-ID">>, ReqProp) of
                            'undefined' ->
                                M = wh_util:rand_hex_binary(8),
                                {[{<<"Msg-ID">>, M} | ReqProp], M};
                            M -> {ReqProp, M}
                        end,
    case ?MODULE:send_request(CallID, Q, PublishFun, ReqProp1) of
        'ok' ->
            lager:debug("published request with msg id ~s for ~p", [MsgID, ClientPid]),
            {'noreply'
             ,State#state{
                client_pid = ClientPid
                ,client_ref = erlang:monitor('process', ClientPid)
                ,client_from = From
                ,client_vfun = VFun
                ,responses = 'undefined' % how we know not to collect many responses
                ,neg_resp_count = 0
                ,current_msg_id = MsgID
                ,req_timeout_ref = start_req_timeout(Timeout)
                ,req_start_time = os:timestamp()
                ,callid = CallID
               }
             ,'hibernate'
            };
        {'error', Err} ->
            lager:debug("failed to send request: ~p", [Err]),
            {'reply', {'error', Err}, reset(State), 'hibernate'}
    end;
handle_call({'call_collect', ReqProp, _, _, _}, _, #state{queue='undefined'}=State) ->
    lager:debug("unable to publish collect request prior to queue creation: ~p", [ReqProp]),
    {'reply', {'error', 'timeout'}, reset(State)};
handle_call({'call_collect', ReqProp, PublishFun, UntilFun, Timeout}
            ,{ClientPid, _}=From
            ,#state{queue=Q}=State
           ) ->
    _ = wh_util:put_callid(ReqProp),
    CallID = get('callid'),
    {ReqProp1, MsgID} = case props:get_value(<<"Msg-ID">>, ReqProp) of
                            'undefined' ->
                                M = wh_util:rand_hex_binary(8),
                                {[{<<"Msg-ID">>, M} | ReqProp], M};
                            M -> {ReqProp, M}
                        end,
    case ?MODULE:send_request(CallID, Q, PublishFun, ReqProp1) of
        'ok' ->
            lager:debug("published request with msg id ~s for ~p", [MsgID, ClientPid]),
            {'noreply'
             ,State#state{
                client_pid = ClientPid
                ,client_ref = erlang:monitor('process', ClientPid)
                ,client_from = From
                ,client_cfun = UntilFun
                ,responses = [] % how we know to collect all responses
                ,neg_resp_count = 0
                ,current_msg_id = MsgID
                ,req_timeout_ref = start_req_timeout(Timeout)
                ,req_start_time = os:timestamp()
                ,callid = CallID
               }
             ,'hibernate'
            };
        {'error', Err} ->
            lager:debug("failed to send request: ~p", [Err]),
            {'reply', {'error', Err}, reset(State), 'hibernate'}
    end;
handle_call({'publish', ReqProp, _}, _From, #state{queue='undefined'}=State) ->
    lager:debug("unable to publish message prior to queue creation: ~p", [ReqProp]),
    {'reply', {'error', 'not_ready'}, reset(State), 'hibernate'};
handle_call({'publish', ReqProp, PublishFun}, _From, State) ->
    Resp =
        try PublishFun(ReqProp) of
            'ok' -> 'ok';
            Other ->
                lager:debug("publisher fun returned ~p instead of 'ok'", [Other]),
                {'error', Other}
        catch
            'error':'badarg' ->
                ST = erlang:get_stacktrace(),
                lager:debug("badarg error when publishing:"),
                wh_util:log_stacktrace(ST),
                {'error', 'badarg'};
            'error':'function_clause' ->
                ST = erlang:get_stacktrace(),
                lager:debug("function clause error when publishing:"),
                wh_util:log_stacktrace(ST),
                {'error', 'function_clause'};
            _E:R ->
                lager:debug("failed to publish request: ~s:~p", [_E, R]),
                {'error', R}
        end,
    {'reply', Resp, reset(State)};
handle_call(_Request, _From, State) ->
    {'reply', {'error', 'not_implemented'}, State, 'hibernate'}.

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
handle_cast({'gen_listener', {'created_queue', Q}}, State) ->
    {'noreply', State#state{queue=Q}, 'hibernate'};
handle_cast({'set_negative_threshold', NegThreshold}, State) ->
    lager:debug("set negative threshold to ~p", [NegThreshold]),
    {'noreply', State#state{neg_resp_threshold = NegThreshold}, 'hibernate'};
handle_cast({'gen_listener', {'return', JObj, BasicReturn}}
            ,#state{current_msg_id = MsgId
                    ,client_from = From
                   }=State) ->
    _ = wh_util:put_callid(JObj),
    case wh_json:get_value(<<"Msg-ID">>, JObj) of
        MsgId ->
            lager:debug("published message was returned from the broker"),
            gen_server:reply(From, {'returned', JObj, BasicReturn}),
            {'noreply', reset(State), 'hibernate'};
        _MsgId ->
            lager:debug("ignoring published message was returned from the broker"),
            lager:debug("payload: ~p", [JObj]),
            lager:debug("return: ~p", [BasicReturn]),
            {'noreply', State, 'hibernate'}
    end;
handle_cast({'event', MsgId, JObj}, #state{current_msg_id = MsgId
                                           ,client_from = From
                                           ,client_vfun = VFun
                                           ,responses = 'undefined'
                                           ,req_start_time = StartTime
                                           ,neg_resp_count = NegCount
                                           ,neg_resp_threshold = NegThreshold
                                          }=State) when NegCount < NegThreshold ->
    _ = wh_util:put_callid(JObj),
    case VFun(JObj) of
        'true' ->
            case wh_json:is_true(<<"Defer-Response">>, JObj) of
                'false' ->
                    lager:debug("response for msg id ~s took ~b micro to return", [MsgId, timer:now_diff(os:timestamp(), StartTime)]),
                    gen_server:reply(From, {'ok', JObj}),
                    {'noreply', reset(State), 'hibernate'};
                'true' ->
                    lager:debug("defered response for msg id ~s, waiting for primary response", [MsgId]),
                    {'noreply', State#state{defer_response=JObj}, 'hibernate'}
            end;
        'false' ->
            case wh_json:is_true(<<"Defer-Response">>, JObj) of
                'true' ->
                    lager:debug("ignoring invalid resp as it was deferred"),
                    {'noreply', State};
                'false' ->
                    lager:debug("response failed validator, waiting for more responses"),
                    {'noreply', State#state{neg_resp_count = NegCount + 1
                                            ,neg_resp=JObj
                                           }, 0}
            end
    end;
handle_cast({'event', MsgId, JObj}, #state{current_msg_id = MsgId
                                           ,client_from = From
                                           ,client_cfun = UntilFun
                                           ,responses = Resps
                                           ,req_start_time = StartTime
                                          }=State) when is_list(Resps) ->
    _ = wh_util:put_callid(JObj),
    lager:debug("recv a response"),
    Responses = [JObj | Resps],
    case UntilFun(Responses) of
        'true' ->
            lager:debug("responses have apparently met the criteria for the client, returning", []),
            lager:debug("response for msg id ~s took ~b micro to return", [MsgId, timer:now_diff(os:timestamp(), StartTime)]),
            gen_server:reply(From, {'ok', Responses}),
            {'noreply', reset(State), 'hibernate'};
        'false' ->
            {'noreply', State#state{responses=Responses}, 'hibernate'}
    end;
handle_cast({'event', _MsgId, JObj}, #state{current_msg_id=_CurrMsgId}=State) ->
    _ = wh_util:put_callid(JObj),
    lager:debug("received unexpected message with old/expired message id: ~s, waiting for ~s", [_MsgId, _CurrMsgId]),
    {'noreply', State};
handle_cast({'gen_listener',{'is_consuming',_IsConsuming}}, State) ->
    {'noreply', State};
handle_cast(_Msg, State) ->
    lager:debug("unhandled cast: ~p", [_Msg]),
    {'noreply', State, 'hibernate'}.

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
handle_info({'DOWN', ClientRef, 'process', _Pid, _Reason}, #state{current_msg_id = _MsgID
                                                                  ,client_ref = ClientRef
                                                                  ,callid = CallID
                                                                 }=State) ->
    put('callid', CallID),
    lager:debug("requestor processes ~p  died while waiting for msg id ~s", [_Pid, _MsgID]),
    {'noreply', reset(State), 'hibernate'};
handle_info('timeout', #state{neg_resp=ErrorJObj
                              ,neg_resp_count=Thresh
                              ,neg_resp_threshold=Thresh
                              ,client_from={_Pid, _}=From
                              ,responses='undefined'
                              ,defer_response=ReservedJObj
                             }=State) ->
    case wh_util:is_empty(ReservedJObj) of
        'true' ->
            lager:debug("negative response threshold reached, returning last negative message to ~p", [_Pid]),
            gen_server:reply(From, {'error', ErrorJObj});
        'false' ->
            lager:debug("negative response threshold reached, returning defered response to ~p", [_Pid]),
            gen_server:reply(From, {'ok', ReservedJObj})
    end,
    {'noreply', reset(State), 'hibernate'};
handle_info('timeout', #state{responses=Resps
                              ,client_from=From
                             }=State) when is_list(Resps) ->
    lager:debug("timeout reached, returning responses"),
    gen_server:reply(From, {'error', Resps}),
    {'noreply', reset(State), 'hibernate'};
handle_info('timeout', State) ->
    {'noreply', State};
handle_info({'timeout', ReqRef, 'req_timeout'}, #state{current_msg_id= _MsgID
                                                       ,req_timeout_ref=ReqRef
                                                       ,callid=CallID
                                                       ,responses='undefined'
                                                       ,client_from=From
                                                       ,defer_response=ReservedJObj
                                                      }=State) ->
    put('callid', CallID),
    case wh_util:is_empty(ReservedJObj) of
        'true' ->
            lager:debug("request timeout exceeded for msg id: ~s", [_MsgID]),
            gen_server:reply(From, {'error', 'timeout'});
        'false' ->
            lager:debug("only received defered response for msg id: ~s", [_MsgID]),
            gen_server:reply(From, {'ok', ReservedJObj})
    end,
    {'noreply', reset(State), 'hibernate'};
handle_info({'timeout', ReqRef, 'req_timeout'}, #state{responses=Resps
                                                       ,req_timeout_ref=ReqRef
                                                       ,client_from=From
                                                       ,callid=CallId
                                                      }=State) ->
    put('callid', CallId),
    lager:debug("req timeout for call_collect"),
    gen_server:reply(From, {'timeout', Resps}),
    {'noreply', reset(State), 'hibernate'};
handle_info(_Info, State) ->
    lager:debug("unhandled message: ~p", [_Info]),
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Allows listener to pass options to handlers
%%
%% @spec handle_event(JObj, State) -> {reply, Options}
%% @end
%%--------------------------------------------------------------------
handle_event(_JObj, _State) ->
    {'reply', []}.

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
    {'ok', State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec reset(#state{}) -> #state{}.
reset(#state{req_timeout_ref = ReqRef
             ,client_ref = ClientRef
            }=State) ->
    put('callid', ?LOG_SYSTEM_ID),
    _ = case is_reference(ReqRef) of
            'true' -> erlang:cancel_timer(ReqRef);
            'false' -> 'ok'
        end,
    _ = case is_reference(ClientRef) of
            'true' -> erlang:demonitor(ClientRef, ['flush']);
            'false' -> 'ok'
        end,
    State#state{client_pid = 'undefined'
                ,client_ref = 'undefined'
                ,client_from = 'undefined'
                ,client_vfun = 'undefined'
                ,client_cfun = collect_until_timeout()
                ,neg_resp = 'undefined'
                ,neg_resp_count = 0
                ,current_msg_id = 'undefined'
                ,req_timeout_ref = 'undefined'
                ,req_start_time = 'undefined'
                ,callid = 'undefined'
                ,defer_response = 'undefined'
                ,responses = 'undefined'
               }.

-spec fudge_timeout(wh_timeout()) -> wh_timeout().
fudge_timeout('infinity'=T) -> T;
fudge_timeout(T) -> T + ?FUDGE.

-spec start_req_timeout(wh_timeout()) -> reference().
start_req_timeout('infinity') -> make_ref();
start_req_timeout(Timeout) ->
    erlang:start_timer(Timeout, self(), 'req_timeout').

-spec maybe_convert_to_proplist(wh_proplist() | wh_json:object()) -> wh_proplist().
maybe_convert_to_proplist(Req) ->
    case wh_json:is_json_object(Req) of
        'true' -> wh_json:to_proplist(Req);
        'false' -> Req
    end.
