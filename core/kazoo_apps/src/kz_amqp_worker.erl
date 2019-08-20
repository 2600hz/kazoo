%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc Worker with a dedicated targeted queue.
%%% Inserts Queue Name as the `Server-ID' and proxies the AMQP request
%%% (expects responses to the request).
%%%
%%% There are two primary interactions, `call' and `call_collect':
%%% <dl>
%%%   <dt>`call'</dt><dd>The semantics of call are similar to `gen_server''s call: send a
%%%   request, expect a response back (or timeout)</dd>
%%%   <dt>`call_collect'</dt><dd>uses the timeout to collect responses (successful or not)
%%%   and returns the resulting list of responses</dd>
%%% </dl>
%%%
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_amqp_worker).
-behaviour(gen_listener).

%% API
-export([start_link/1

        ,call/2, call/3, call/4, call/5
        ,call_collect/2, call_collect/3, call_collect/4

        ,call_custom/4, call_custom/5

        ,cast/2, cast/3

        ,relay_to/2, stop_relay/2

        ,default_timeout/0
        ,collect_until_timeout/0
        ,collect_from_whapp/1
        ,collect_from_whapp_or_validate/2
        ,send_request/4
        ,checkout_worker/0, checkout_worker/1
        ,checkin_worker/1, checkin_worker/2
        ,worker_pool/0, worker_pool/1
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

-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_stdlib/include/kz_log.hrl").
-include_lib("kazoo_amqp/include/kz_api.hrl").
-include_lib("kazoo_amqp/include/kz_amqp.hrl").

-define(SERVER, ?MODULE).

-define(FUDGE, 2600).
-define(BINDINGS, [{'self', []}]).
-define(QUEUE_NAME, <<>>).
-define(QUEUE_OPTIONS, []).
-define(CONSUME_OPTIONS, []).

-type publish_fun() :: fun((kz_term:api_terms()) -> any()).
-type validate_fun() :: fun((kz_term:api_terms()) -> boolean()).

-type collect_until_acc() :: any().

-type collect_until_acc_fun() :: fun((kz_json:objects(), collect_until_acc()) -> boolean() | {boolean(), collect_until_acc()}).
-type collect_until_fun() :: fun((kz_json:objects()) -> boolean()) |
                             collect_until_acc_fun() |
                             {collect_until_acc_fun(), collect_until_acc()}.

-type whapp() :: atom() | kz_term:ne_binary().

-type collect_until() :: collect_until_fun() |
                         whapp() |
                         {whapp(), validate_fun() | boolean()} | %% {Whapp, VFun | IncludeFederated}
                         {whapp(), validate_fun(), boolean()} |  %% {Whapp, VFun, IncludeFederated}
                         {whapp(), boolean(), boolean()} |       %% {Whapp, IncludeFederated, IsShared}
                         {whapp(), validate_fun(), boolean(), boolean()}. %% {Whapp, VFun, IncludeFederated, IsShared}
-type timeout_or_until() :: timeout() | collect_until().

%% case {IsFederated, IsShared} of
%%  {'true', 'true'} -> Get from {0,1} whapp instance per zone
%%  {'false', 'true'} -> %% Get from {0,1} whapp instance in the local zone
%%  _Otherwise -> Get from all instances, either local or federated

-export_type([publish_fun/0
             ,validate_fun/0
             ,collect_until/0
             ,timeout_or_until/0
             ,request_return/0
             ,cast_return/0
             ]).

-record(state, {current_msg_id :: kz_term:api_binary()
               ,client_pid :: kz_term:api_pid()
               ,client_ref :: kz_term:api_reference()
               ,client_from :: kz_term:api_pid_ref() | 'relay'
               ,client_vfun :: validate_fun() | 'undefined'
               ,client_cfun = collect_until_timeout() :: collect_until_fun()
               ,responses :: kz_term:api_objects()
               ,neg_resp :: kz_term:api_object()
               ,neg_resp_count = 0 :: non_neg_integer()
               ,neg_resp_threshold = 1 :: pos_integer()
               ,req_timeout_ref :: kz_term:api_reference()
               ,req_start_time :: kz_time:now() | 'undefined'
               ,callid :: kz_term:api_binary()
               ,pool_ref :: kz_types:server_ref()
               ,defer_response :: kz_term:api_object()
               ,queue :: kz_term:api_binary()
               ,confirms = 'false' :: boolean()
               ,flow = 'undefined' :: kz_term:api_boolean()
               ,acc = 'undefined' :: any()
               ,defer = 'undefined' :: 'undefined' | {any(), {pid(), reference()}}
               ,confirm_timeout_ref :: kz_term:api_reference()
               ,confirm_start_time :: kz_time:now() | 'undefined'
               ,timeout :: non_neg_integer() | 'undefined'
               ,method :: atom()
               ,reply_to :: kz_term:api_pid_ref()
               }).
-type state() :: #state{}.

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the server.
%% @end
%%------------------------------------------------------------------------------
-spec start_link(kz_term:proplist()) -> kz_types:startlink_ret().
start_link(Args) ->
    gen_listener:start_link(?SERVER, [{'bindings', maybe_bindings(Args)}
                                     ,{'queue_name', maybe_queuename(Args)}
                                     ,{'queue_options', ?QUEUE_OPTIONS}
                                     ,{'consume_options', ?CONSUME_OPTIONS}
                                      | maybe_broker(Args)
                                      ++ maybe_exchanges(Args)
                                      ++ maybe_server_confirms(Args)
                                     ], [Args]).

-spec maybe_broker(kz_term:proplist()) -> kz_term:proplist().
maybe_broker(Args) ->
    case props:get_value('amqp_broker', Args) of
        'undefined' -> [];
        Broker -> [{'broker', Broker}]
    end.

-spec maybe_queuename(kz_term:proplist()) -> binary().
maybe_queuename(Args) ->
    case props:get_value('amqp_queuename_start', Args) of
        'undefined' -> ?QUEUE_NAME;
        QueueStart -> <<(kz_term:to_binary(QueueStart))/binary, "_", (kz_binary:rand_hex(4))/binary>>
    end.

-spec maybe_bindings(kz_term:proplist()) -> kz_term:proplist().
maybe_bindings(Args) ->
    case props:get_value('amqp_bindings', Args) of
        'undefined' -> ?BINDINGS;
        Bindings -> Bindings
    end.

-spec maybe_exchanges(kz_term:proplist()) -> kz_term:proplist().
maybe_exchanges(Args) ->
    case props:get_value('amqp_exchanges', Args) of
        'undefined' -> [];
        Exchanges -> [{'declare_exchanges', Exchanges}]
    end.

-spec maybe_server_confirms(kz_term:proplist()) -> kz_term:proplist().
maybe_server_confirms(Args) ->
    case props:get_value('amqp_server_confirms', Args) of
        'undefined' -> [];
        Confirms -> [{'server_confirms', Confirms}]
    end.

-spec default_timeout() -> 2000.
default_timeout() -> 2 * ?MILLISECONDS_IN_SECOND.

-type request_return() :: {'ok', kz_json:object() | kz_json:objects()} |
                          {'returned', kz_json:object(), kz_json:object()} |
                          {'timeout', kz_json:objects()} |
                          {'error', any()}.

-spec call(kz_term:api_terms(), publish_fun()) -> request_return().
call(Req, PubFun) ->
    call(Req, PubFun, fun kz_term:always_true/1).

-spec call(kz_term:api_terms(), publish_fun(), validate_fun()) ->
                  request_return().
call(Req, PubFun, VFun) ->
    call(Req, PubFun, VFun, default_timeout()).

-spec call(kz_term:api_terms(), publish_fun(), validate_fun(), timeout()) ->
                  request_return().
call(Req, PubFun, VFun, Timeout) ->
    case next_worker() of
        {'error', _}=E -> E;
        Worker -> call(Req, PubFun, VFun, Timeout, Worker)
    end.

-spec call(kz_term:api_terms(), publish_fun(), validate_fun(), timeout(), pid()  | atom()) ->
                  request_return().
call(Req, PubFun, VFun, Timeout, Pool) when is_atom(Pool) ->
    case next_worker(Pool) of
        {'error', _}=E -> E;
        Worker -> call(Req, PubFun, VFun, Timeout, Worker)
    end;
call(Req, PubFun, VFun, Timeout, Worker) when is_pid(Worker) ->
    Prop = maybe_convert_to_proplist(Req),
    try gen_listener:call(Worker
                         ,{'request', Prop, PubFun, VFun, Timeout}
                         ,fudge_timeout(Timeout)
                         )
    catch
        'exit':{timeout, _} ->
            lager:warning("request timeout"),
            {error, timeout};
        _E:R ->
            lager:warning("request failed: ~s: ~p", [_E, R]),
            {'error', R}
    after
        checkin_worker(Worker)
    end.

-type pool_error() :: 'pool_full' | 'poolboy_fault'.

-spec next_worker() -> pid() | {'error', pool_error()}.
next_worker() ->
    next_worker(worker_pool()).

-spec next_worker(atom()) -> pid() | {'error', pool_error()}.
next_worker(Pool) ->
    next_worker(Pool, whereis(Pool)).

next_worker(_Pool, 'undefined') ->
    lager:warning("pool ~s not available yet for application ~s"
                 ,[_Pool
                  ,kapps_util:get_application()
                  ]
                 ),
    {'error', 'poolboy_fault'};
next_worker(Pool, Pid) when is_pid(Pid) ->
    try poolboy:checkout(Pool, 'false', default_timeout()) of
        'full' ->
            {'error', 'pool_full'};
        Worker ->
            lager:debug("application ~s checked out worker ~p from pool ~s"
                       ,[kapps_util:get_application()
                        ,Worker
                        ,Pool
                        ]
                       ),
            Worker
    catch
        _E:_R ->
            lager:warning("poolboy exception: ~s: ~p", [_E, _R]),
            {'error', 'poolboy_fault'}
    end.

-spec checkout_worker() -> {'ok', pid()} | {'error', pool_error()}.
checkout_worker() ->
    checkout_worker(worker_pool()).

-spec checkout_worker(atom()) -> {'ok', pid()} | {'error', pool_error()}.
checkout_worker(Pool) ->
    try poolboy:checkout(Pool, 'false', default_timeout()) of
        'full' ->
            {'error', 'pool_full'};
        Worker ->
            lager:debug("application ~s checked out worker ~p from pool ~s"
                       ,[kapps_util:get_application()
                        ,Worker
                        ,Pool
                        ]
                       ),
            {'ok', Worker}
    catch
        _E:_R ->
            lager:warning("poolboy ~s exception ~s: ~p"
                         ,[Pool, _E, _R]
                         ),
            {'error', 'poolboy_fault'}
    end.

-spec checkin_worker(pid()) -> 'ok'.
checkin_worker(Worker) ->
    checkin_worker(Worker, worker_pool()).

-spec checkin_worker(pid(), atom()) -> 'ok'.
checkin_worker(Worker, Pool) ->
    poolboy:checkin(Pool, Worker).

-spec call_custom(kz_term:api_terms(), publish_fun(), validate_fun(), gen_listener:binding()) ->
                         request_return().
call_custom(Req, PubFun, VFun, Bind) ->
    call_custom(Req, PubFun, VFun, default_timeout(), Bind).

-spec call_custom(kz_term:api_terms(), publish_fun(), validate_fun(), timeout(), gen_listener:binding()) ->
                         request_return().
call_custom(Req, PubFun, VFun, Timeout, Bind) ->
    case next_worker() of
        {'error', _}=E -> E;
        Worker -> call_custom(Req, PubFun, VFun, Timeout, Bind, Worker)
    end.

-spec call_custom(kz_term:api_terms(), publish_fun(), validate_fun(), timeout(), gen_listener:binding(), pid()) ->
                         request_return().
call_custom(Req, PubFun, VFun, Timeout, Bind, Worker) ->
    Prop = maybe_convert_to_proplist(Req),
    gen_listener:add_binding(Worker, Bind),
    try gen_listener:call(Worker
                         ,{'request', Prop, PubFun, VFun, Timeout}
                         ,fudge_timeout(Timeout)
                         )
    catch
        _E:R ->
            lager:debug("request failed: ~s: ~p", [_E, R]),
            {'error', R}
    after
        gen_listener:rm_binding(Worker, Bind),
        checkin_worker(Worker)
    end.

-spec call_collect(kz_term:api_terms(), publish_fun()) ->
                          request_return().
call_collect(Req, PubFun) ->
    call_collect(Req, PubFun, default_timeout()).

-spec call_collect(kz_term:api_terms(), publish_fun(), timeout_or_until()) ->
                          request_return().
call_collect(Req, PubFun, UntilFun) when is_function(UntilFun) ->
    call_collect(Req, PubFun, UntilFun, default_timeout());
call_collect(Req, PubFun, Whapp) when is_atom(Whapp); is_binary(Whapp) ->
    call_collect(Req, PubFun, Whapp, default_timeout());
call_collect(Req, PubFun, {_, _}=Until) ->
    call_collect(Req, PubFun, Until, default_timeout());
call_collect(Req, PubFun, {_, _, _}=Until) ->
    call_collect(Req, PubFun, Until, default_timeout());
call_collect(Req, PubFun, {_, _, _, _}=Until) ->
    call_collect(Req, PubFun, Until, default_timeout());
call_collect(Req, PubFun, Timeout) ->
    call_collect(Req, PubFun, collect_until_timeout(), Timeout).

-spec call_collect(kz_term:api_terms(), publish_fun(), collect_until(), timeout()) ->
                          request_return().
call_collect(_Req, _PubFun, 'undefined', _Timeout) ->
    lager:debug("no VFun, no responses"),
    {'ok', []};
call_collect(Req, PubFun, {Whapp, IncludeFederated}, Timeout)
  when (is_atom(Whapp)
        orelse is_binary(Whapp)
       )
       andalso is_boolean(IncludeFederated) ->
    CollectFromWhapp = collect_from_whapp(Whapp, IncludeFederated),
    call_collect(Req, PubFun, CollectFromWhapp, Timeout);
call_collect(Req, PubFun, {Whapp, VFun}, Timeout)
  when (is_atom(Whapp)
        orelse is_binary(Whapp)
       )
       andalso is_function(VFun) ->
    CollectFromWhapp = collect_from_whapp_or_validate(Whapp, VFun),
    call_collect(Req, PubFun, CollectFromWhapp, Timeout);
call_collect(Req, PubFun, {Whapp, IncludeFederated, IsShared}, Timeout)
  when (is_atom(Whapp)
        orelse is_binary(Whapp)
       )
       andalso is_boolean(IncludeFederated)
       andalso is_boolean(IsShared) ->
    CollectFromWhapp = collect_from_whapp(Whapp, IncludeFederated, IsShared),
    call_collect(Req, PubFun, CollectFromWhapp, Timeout);
call_collect(Req, PubFun, {Whapp, VFun, IncludeFederated}, Timeout)
  when (is_atom(Whapp)
        orelse is_binary(Whapp)
       )
       andalso is_function(VFun)
       andalso is_boolean(IncludeFederated) ->
    CollectFromWhapp = collect_from_whapp_or_validate(Whapp, VFun, IncludeFederated),
    call_collect(Req, PubFun, CollectFromWhapp, Timeout);
call_collect(Req, PubFun, {Whapp, VFun, IncludeFederated, IsShared}, Timeout)
  when (is_atom(Whapp)
        orelse is_binary(Whapp)
       )
       andalso is_function(VFun)
       andalso is_boolean(IncludeFederated)
       andalso is_boolean(IsShared) ->
    CollectFromWhapp = collect_from_whapp_or_validate(Whapp, VFun, IncludeFederated, IsShared),
    call_collect(Req, PubFun, CollectFromWhapp, Timeout);
call_collect(Req, PubFun, Whapp, Timeout)
  when is_atom(Whapp)
       orelse is_binary(Whapp) ->
    call_collect(Req, PubFun, collect_from_whapp(Whapp), Timeout);
call_collect(Req, PubFun, UntilFun, Timeout)
  when is_integer(Timeout)
       andalso Timeout >= 0 ->
    case next_worker() of
        {'error', _}=E ->
            lager:debug("failed to get next worker: ~p", [E]),
            E;
        Worker ->
            call_collect(Req, PubFun, UntilFun, Timeout, Worker)
    end.

-spec call_collect(kz_term:api_terms(), publish_fun(), collect_until(), timeout(), pid()) ->
                          request_return().
call_collect(Req, PubFun, {UntilFun, Acc}, Timeout, Worker)
  when is_function(UntilFun, 2) ->
    call_collect(Req, PubFun, UntilFun, Timeout, Acc, Worker);
call_collect(Req, PubFun, UntilFun, Timeout, Worker) ->
    call_collect(Req, PubFun, UntilFun, Timeout, 'undefined', Worker).

call_collect(Req, PubFun, UntilFun, Timeout, Acc, Worker) ->
    Prop = maybe_convert_to_proplist(Req),
    try gen_listener:call(Worker
                         ,{'call_collect', Prop, PubFun, UntilFun, Timeout, Acc}
                         ,fudge_timeout(Timeout)
                         )
    catch
        _E:R ->
            lager:debug("request failed: ~s: ~p", [_E, R]),
            {'error', R}
    after
        checkin_worker(Worker)
    end.

-type cast_return() :: 'ok' |
                       {'error', any()} |
                       {'returned', kz_json:object(), kz_json:object()}.

-spec cast(kz_term:api_terms(), publish_fun()) -> cast_return().
cast(Req, PubFun) ->
    cast(Req, PubFun, worker_pool()).

-spec cast(kz_term:api_terms(), publish_fun(), pid() | atom()) -> cast_return().
cast(Req, PubFun, Pool) when is_atom(Pool) ->
    case next_worker(Pool) of
        {'error', _}=E -> E;
        Worker ->
            Resp = cast(Req, PubFun, Worker),
            checkin_worker(Worker, Pool),
            Resp
    end;
cast(Req, PubFun, Worker) when is_pid(Worker) ->
    Prop = maybe_convert_to_proplist(Req),
    try gen_listener:call(Worker, {'publish', Prop, PubFun})
    catch
        _E:R ->
            lager:debug("request failed: ~s: ~p", [_E, R]),
            {'error', R}
    end.

-spec relay_to(pid() | atom(), pid()) -> 'ok'.
relay_to(Worker, RelayPid) ->
    gen_listener:call(Worker, {'relay_to', RelayPid}).

-spec stop_relay(pid() | atom(), pid()) -> 'ok'.
stop_relay(Worker, RelayPid) ->
    gen_listener:call(Worker, {'stop_relay', RelayPid}).

-spec collect_until_timeout() -> collect_until_fun().
collect_until_timeout() -> fun kz_term:always_false/1.

-spec collect_from_whapp(kz_term:text()) -> 'undefined' | collect_until_fun().
collect_from_whapp(Whapp) ->
    collect_from_whapp(Whapp, 'false').

-spec collect_from_whapp(kz_term:text(), boolean()) ->
                                'undefined' | collect_until_fun().
collect_from_whapp(Whapp, IncludeFederated) ->
    collect_from_whapp(Whapp, IncludeFederated, 'false').

-spec collect_from_whapp(kz_term:text(), boolean(), boolean()) ->
                                'undefined' | collect_until_fun().
collect_from_whapp(Whapp, IncludeFederated, IsShared) ->
    Count = case {IncludeFederated, IsShared} of
                {'true', 'true'} -> kz_nodes:whapp_zone_count(Whapp); %% Get from {0,1} whapp instance per zone
                {'false', 'true'} -> 1; %% Get from one whapp instance
                _ -> kz_nodes:whapp_count(Whapp, IncludeFederated) %% Get from all instances, either local or federated
            end,
    lager:debug("attempting to collect ~p responses from ~s", [Count, Whapp]),
    count_fun(Count).

-spec count_fun(non_neg_integer()) -> 'undefined' | collect_until_fun().
count_fun(0) -> 'undefined';
count_fun(Count) ->
    fun(Responses) -> length(Responses) >= Count end.

-spec collect_from_whapp_or_validate(kz_term:text(), validate_fun()) -> collect_until_fun().
collect_from_whapp_or_validate(Whapp, VFun) ->
    collect_from_whapp_or_validate(Whapp, VFun, 'false').

-spec collect_from_whapp_or_validate(kz_term:text(),validate_fun(), boolean()) -> collect_until_fun().
collect_from_whapp_or_validate(Whapp, VFun, IncludeFederated) ->
    collect_from_whapp_or_validate(Whapp, VFun, IncludeFederated, 'false').

-spec collect_from_whapp_or_validate(kz_term:text(),validate_fun(), boolean(), boolean()) -> collect_until_fun().
collect_from_whapp_or_validate(Whapp, VFun, 'true', 'true') ->
    Count = kz_nodes:whapp_zone_count(Whapp),
    lager:debug("attempting to collect ~p responses from ~s or the first valid", [Count, Whapp]),
    collect_or_validate_fun(VFun, Count);
collect_from_whapp_or_validate(Whapp, VFun, 'false', 'true') ->
    Count = 1,
    lager:debug("attempting to collect ~p responses from ~s or the first valid", [Count, Whapp]),
    collect_or_validate_fun(VFun, Count);
collect_from_whapp_or_validate(Whapp, VFun, IncludeFederated, 'false') ->
    Count = kz_nodes:whapp_count(Whapp, IncludeFederated),
    lager:debug("attempting to collect ~p responses from ~s or the first valid", [Count, Whapp]),
    collect_or_validate_fun(VFun, Count).

-spec collect_or_validate_fun(validate_fun(), pos_integer()) -> collect_until_fun().
collect_or_validate_fun(VFun, 0) ->
    fun([Response|_]) -> VFun(Response) end;
collect_or_validate_fun(VFun, Count) ->
    fun([Response|_]=Responses) ->
            length(Responses) >= Count
                orelse VFun(Response)
    end.

-spec send_request(kz_term:ne_binary(), kz_term:ne_binary(), publish_fun(), kz_term:proplist()) ->
                          'ok' | {'error', any()}.
send_request(CallId, Self, PublishFun, ReqProps)
  when is_function(PublishFun, 1) ->
    kz_util:put_callid(CallId),
    FilteredProps = request_filter(ReqProps),
    Props = request_filter(props:set_values([{?KEY_SERVER_ID, Self}
                                            ,{?KEY_QUEUE_ID, props:get_value(?KEY_SERVER_ID, FilteredProps)}
                                            ,{?KEY_LOG_ID, CallId}
                                            ]
                                           ,FilteredProps
                                           )),
    try PublishFun(Props) of
        'ok' -> 'ok'
    catch
        _R:E ->
            lager:debug("failed to publish: ~s: ~p", [_R, E]),
            kz_util:log_stacktrace(),
            {'error', E}
    end.

-spec request_filter(kz_term:proplist()) -> kz_term:proplist().
request_filter(Props) ->
    props:filter(fun request_proplist_filter/1, Props).

-spec request_proplist_filter({kz_term:proplist_key(), kz_term:proplist_value()}) -> boolean().
request_proplist_filter({<<"Server-ID">>, Value}) ->
    not kz_term:is_empty(Value);
request_proplist_filter({_, 'undefined'}) -> 'false';
request_proplist_filter(_) -> 'true'.

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server.
%% @end
%%------------------------------------------------------------------------------
-spec init(list()) -> {'ok', state()}.
init([Args]) ->
    kz_util:put_callid(?DEFAULT_LOG_SYSTEM_ID),
    lager:debug("starting amqp worker"),
    NegThreshold = props:get_value('neg_resp_threshold', Args, 1),
    Pool = props:get_value('name', Args, 'undefined'),
    {'ok', #state{neg_resp_threshold=NegThreshold
                 ,pool_ref=Pool
                 }}.

%%------------------------------------------------------------------------------
%% @doc Handling call messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call(Call, From, #state{queue='undefined'}=State)
  when is_tuple(Call) ->
    kz_util:put_callid(element(2, Call)),
    lager:debug("unable to publish message prior to queue creation - deferring"),
    {'noreply', State#state{defer={Call,From}}};
handle_call(_, _, #state{flow='false'}=State) ->
    lager:debug("flow control is active and server put us in waiting"),
    {'reply', {'error', 'flow_control'}, reset(State)};
handle_call({'relay_to', RelayPid}, _From, State) ->
    {'reply', 'ok', State#state{client_pid=RelayPid
                               ,client_ref=erlang:monitor('process', RelayPid)
                               ,client_from='relay'
                               }
    };
handle_call({'stop_relay', RelayPid}, _From, #state{client_pid=RelayPid
                                                   ,client_ref=Ref
                                                   ,client_from='relay'
                                                   }=State) ->
    erlang:demonitor(Ref, ['flush']),
    lager:debug("stopping relay to ~p", [RelayPid]),
    {'reply', 'ok', reset(State)};
handle_call({'request', ReqProp, PublishFun, VFun, Timeout}
           ,{ClientPid, _}=From
           ,#state{queue=Q, confirms=Confirm}=State
           ) ->
    _ = kz_util:put_callid(ReqProp),
    CallId = get('callid'),
    MsgId = kz_api:msg_reply_id(ReqProp),

    case send_request(CallId, Q, PublishFun, ReqProp) of
        'ok' when Confirm =:= 'true' ->
            lager:debug("published request with msg id ~s for ~p waiting for confirmation", [MsgId, ClientPid]),
            {'noreply'
            ,State#state{client_pid = ClientPid
                        ,client_ref = erlang:monitor('process', ClientPid)
                        ,client_from = From
                        ,client_vfun = VFun
                        ,responses = 'undefined' % how we know not to collect any responses
                        ,neg_resp_count = 0
                        ,current_msg_id = MsgId

                        ,confirm_timeout_ref = start_confirm_timeout(Timeout)
                        ,confirm_start_time = os:timestamp()
                        ,timeout = Timeout
                        ,method = 'request'
                        ,callid = CallId
                        }
            };
        'ok' ->
            lager:debug("published request with msg id ~s for ~p", [MsgId, ClientPid]),
            {'noreply'
            ,State#state{client_pid = ClientPid
                        ,client_ref = erlang:monitor('process', ClientPid)
                        ,client_from = From
                        ,client_vfun = VFun
                        ,responses = 'undefined' % how we know not to collect any responses
                        ,neg_resp_count = 0
                        ,current_msg_id = MsgId
                        ,req_timeout_ref = start_req_timeout(Timeout)
                        ,req_start_time = os:timestamp()
                        ,callid = CallId
                        }
            };
        {'error', Err}=Error ->
            lager:debug("failed to send request: ~p", [Err]),
            {'reply', Error, reset(State)}
    end;
handle_call({'call_collect', ReqProp, PublishFun, UntilFun, Timeout, Acc}
           ,{ClientPid, _}=From
           ,#state{queue=Q, confirms=Confirm}=State
           ) ->
    _ = kz_util:put_callid(ReqProp),
    CallId = get('callid'),
    MsgId = kz_api:msg_reply_id(ReqProp),

    case send_request(CallId, Q, PublishFun, ReqProp) of
        'ok' when Confirm =:= 'true' ->
            lager:debug("published request with msg id ~s for ~p waiting for confirmation", [MsgId, ClientPid]),
            {'noreply'
            ,State#state{client_pid = ClientPid
                        ,client_ref = erlang:monitor('process', ClientPid)
                        ,client_from = From
                        ,client_cfun = UntilFun
                        ,acc = Acc
                        ,responses = [] % how we know to collect all responses
                        ,neg_resp_count = 0
                        ,current_msg_id = MsgId

                        ,confirm_timeout_ref = start_confirm_timeout(Timeout)
                        ,confirm_start_time = os:timestamp()
                        ,timeout = Timeout
                        ,method = 'call_collect'

                        ,callid = CallId
                        }
            };
        'ok' ->
            lager:debug("published request with msg id ~s for ~p", [MsgId, ClientPid]),
            {'noreply'
            ,State#state{client_pid = ClientPid
                        ,client_ref = erlang:monitor('process', ClientPid)
                        ,client_from = From
                        ,client_cfun = UntilFun
                        ,acc = Acc
                        ,responses = [] % how we know to collect all responses
                        ,neg_resp_count = 0
                        ,current_msg_id = MsgId
                        ,req_timeout_ref = start_req_timeout(Timeout)
                        ,req_start_time = os:timestamp()
                        ,callid = CallId
                        }
            };
        {'error', Err}=Error ->
            lager:debug("failed to send request: ~p", [Err]),
            {'reply', Error, reset(State)}
    end;

handle_call({'publish', ReqProp0, PublishFun}
           ,{_Pid, _}=From
           ,#state{client_from='relay'
                  ,queue=Queue
                  ,confirms=Confirm
                  }=State
           ) ->
    ReqProp = props:insert_value(?KEY_SERVER_ID, Queue, ReqProp0),
    case publish_api(PublishFun, ReqProp) of
        'ok' when Confirm =:= 'true' ->
            {'noreply', State#state{confirm_timeout_ref = start_confirm_timeout(default_timeout())
                                   ,confirm_start_time = os:timestamp()
                                   ,method = 'publish'
                                   ,reply_to=From
                                   }};
        'ok' ->
            lager:debug("published message ~s for ~p", [kz_api:msg_id(ReqProp), _Pid]),
            {'reply', 'ok', State};
        {'error', _E}=Err ->
            lager:error("failed to publish message ~s for ~p: ~p", [kz_api:msg_id(ReqProp), _Pid, _E]),
            {'reply', Err, State}
    end;
handle_call({'publish', ReqProp, PublishFun}
           ,{Pid, _}=From
           ,#state{confirms=Confirm}=State
           ) ->
    _ = kz_util:put_callid(ReqProp),
    case publish_api(PublishFun, ReqProp) of
        'ok' when Confirm =:= 'true' ->
            lager:debug("published message ~s for ~p", [kz_api:msg_id(ReqProp), Pid]),
            {'noreply'
            ,State#state{client_pid = Pid
                        ,client_ref = erlang:monitor('process', Pid)
                        ,client_from = From

                        ,confirm_timeout_ref = start_confirm_timeout(default_timeout())
                        ,confirm_start_time = os:timestamp()
                        ,method = 'publish'

                        ,req_start_time = os:timestamp()
                        }
            };
        'ok' ->
            lager:debug("published message ~s for ~p", [kz_api:msg_id(ReqProp), Pid]),
            {'reply', 'ok', reset(State)};
        {'error', _E}=Err ->
            lager:error("failed to publish message ~s for ~p: ~p", [kz_api:msg_id(ReqProp), Pid, _E]),
            {'reply', Err, reset(State)}
    end;
handle_call(_Request, _From, State) ->
    {'reply', {'error', 'not_implemented'}, State}.

%%------------------------------------------------------------------------------
%% @doc Handling cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast('hibernate', State) ->
    {'noreply', State, 'hibernate'};
handle_cast({'gen_listener', {'created_queue', Q}}, #state{defer='undefined'}=State) ->
    {'noreply', State#state{queue=Q}};
handle_cast({'gen_listener', {'created_queue', Q}}, #state{defer={Call,From}}=State) ->
    kz_util:put_callid(element(2, Call)),
    lager:debug("resuming deferred call"),
    case handle_call(Call, From, State#state{queue=Q}) of
        {'reply', Reply, NewState} ->
            gen_server:reply(From, Reply),
            {'noreply', NewState};
        {'noreply', _NewState}=NoReply ->
            NoReply
    end;
handle_cast({'set_negative_threshold', NegThreshold}, State) ->
    lager:debug("set negative threshold to ~p", [NegThreshold]),
    {'noreply', State#state{neg_resp_threshold = NegThreshold}, 'hibernate'};
handle_cast({'gen_listener', {'return', JObj, BasicReturn}}
           ,#state{current_msg_id = MsgId
                  ,client_from = From
                  ,confirms=Confirms
                  }=State) ->
    _ = kz_util:put_callid(JObj),
    case kz_api:msg_id(JObj) of
        MsgId ->
            lager:debug("published message was returned from the broker"),
            gen_server:reply(From, {'returned', JObj, BasicReturn}),
            {'noreply', reset(State), 'hibernate'};
        _MsgId when Confirms =:= 'true' ->
            lager:debug("published message was returned from the broker"),
            gen_server:reply(From, {'returned', JObj, BasicReturn}),
            {'noreply', reset(State), 'hibernate'};
        _MsgId ->
            lager:debug("ignoring published message was returned from the broker"),
            lager:debug("payload: ~p", [JObj]),
            lager:debug("return: ~p", [BasicReturn]),
            {'noreply', State, 'hibernate'}
    end;


handle_cast({'gen_listener', {'confirm', Msg}}, State) ->
    handle_confirm(Msg, State);

handle_cast({'gen_listener',{'is_consuming',_IsConsuming}}, State) ->
    {'noreply', State};
handle_cast({'gen_listener',{'server_confirms',ServerConfirms}}, State) ->
    lager:debug("Server confirms status : ~p", [ServerConfirms]),
    {'noreply', State#state{confirms=ServerConfirms}};
handle_cast({'gen_listener',{'channel_flow', 'true'}}, State) ->
    lager:debug("channel flow enabled"),
    {'noreply', State#state{flow='true'}};
handle_cast({'gen_listener',{'channel_flow', 'false'}}, State) ->
    lager:debug("channel flow disabled"),
    {'noreply', State#state{flow='undefined'}};
handle_cast({'gen_listener',{'channel_flow_control', Active}}, State) ->
    lager:debug("channel flow is ~p", [Active]),
    {'noreply', State#state{flow=Active}};
handle_cast(_Msg, State) ->
    lager:debug("unhandled cast: ~p", [_Msg]),
    {'noreply', State, 'hibernate'}.

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info({'DOWN', ClientRef, 'process', _Pid, _Reason}
           ,#state{current_msg_id = _MsgId
                  ,client_ref = ClientRef
                  ,callid = CallId
                  }=State
           ) ->
    kz_util:put_callid(CallId),
    lager:debug("requestor processes ~p  died while waiting for msg id ~s", [_Pid, _MsgId]),
    {'noreply', reset(State), 'hibernate'};
handle_info('timeout'
           ,#state{neg_resp=ErrorJObj
                  ,neg_resp_count=Thresh
                  ,neg_resp_threshold=Thresh
                  ,client_from={_Pid, _}=From
                  ,responses='undefined'
                  ,defer_response=ReservedJObj
                  }=State
           ) ->
    case kz_term:is_empty(ReservedJObj) of
        'true' ->
            lager:debug("negative response threshold reached, returning last negative message to ~p", [_Pid]),
            gen_server:reply(From, {'error', ErrorJObj});
        'false' ->
            lager:debug("negative response threshold reached, returning deferred response to ~p", [_Pid]),
            gen_server:reply(From, {'ok', ReservedJObj})
    end,
    {'noreply', reset(State), 'hibernate'};
handle_info('timeout'
           ,#state{responses=Resps
                  ,client_from=From
                  }=State
           ) when is_list(Resps) ->
    lager:debug("timeout reached, returning responses"),
    gen_server:reply(From, {'error', Resps}),
    {'noreply', reset(State), 'hibernate'};
handle_info('timeout', State) ->
    {'noreply', State};
handle_info({'timeout', ReqRef, 'req_timeout'}
           ,#state{current_msg_id= _MsgId
                  ,req_timeout_ref=ReqRef
                  ,callid=CallId
                  ,responses='undefined'
                  ,client_from={_Pid, _}=From
                  ,defer_response=ReservedJObj
                  }=State
           ) ->
    kz_util:put_callid(CallId),
    case kz_term:is_empty(ReservedJObj) of
        'true' ->
            lager:debug("request timeout exceeded for msg id: ~s and client: ~p", [_MsgId, _Pid]),
            gen_server:reply(From, {'error', 'timeout'});
        'false' ->
            lager:debug("only received deferred response for msg id: ~s and client: ~p", [_MsgId, _Pid]),
            gen_server:reply(From, {'ok', ReservedJObj})
    end,
    {'noreply', reset(State), 'hibernate'};
handle_info({'timeout', ReqRef, 'req_timeout'}
           ,#state{responses=Resps
                  ,req_timeout_ref=ReqRef
                  ,client_from=From
                  ,callid=CallId
                  }=State
           ) ->
    kz_util:put_callid(CallId),
    lager:debug("req timeout for call_collect"),
    gen_server:reply(From, {'timeout', Resps}),
    {'noreply', reset(State), 'hibernate'};
handle_info({'timeout', ReqRef, 'confirm_timeout'}
           ,#state{confirm_timeout_ref=ReqRef}=State
           ) ->
    handle_publish_timeout(State);
handle_info(_Info, State) ->
    lager:debug("unhandled message: ~p", [_Info]),
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Allows listener to pass options to handlers.
%% @end
%%------------------------------------------------------------------------------
-spec handle_event(kz_json:object(), state()) -> gen_listener:handle_event_return().
handle_event(JObj, #state{client_from='relay'
                         ,client_pid=Pid
                         }) ->
    relay_event(Pid, JObj),
    lager:debug("relayed event to ~p", [Pid]),
    'ignore';
handle_event(JObj, State) ->
    case handle_payload(kz_api:msg_id(JObj), JObj, State) of
        {'noreply', NewState} ->  {'ignore', NewState};
        {'noreply', NewState, 'hibernate'} ->
            gen_listener:cast(self(), 'hibernate'),
            {'ignore', NewState}
    end.

%%------------------------------------------------------------------------------
%% @doc This function is called by a `gen_server' when it is about to
%% terminate. It should be the opposite of `Module:init/1' and do any
%% necessary cleaning up. When it returns, the `gen_server' terminates
%% with Reason. The return value is ignored.
%%
%% @end
%%------------------------------------------------------------------------------
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, _State) ->
    lager:debug("amqp worker terminating: ~p", [_Reason]).

%%------------------------------------------------------------------------------
%% @doc Convert process state when code is changed.
%% @end
%%------------------------------------------------------------------------------
-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec reset(#state{}) -> #state{}.
reset(#state{req_timeout_ref = ReqRef
            ,client_ref = ClientRef
            ,confirm_timeout_ref = ConfirmRef
            }=State) ->
    kz_util:put_callid(?DEFAULT_LOG_SYSTEM_ID),
    _ = case is_reference(ReqRef) of
            'true' -> erlang:cancel_timer(ReqRef);
            'false' -> 'ok'
        end,
    _ = case is_reference(ClientRef) of
            'true' -> erlang:demonitor(ClientRef, ['flush']);
            'false' -> 'ok'
        end,
    _ = case is_reference(ConfirmRef) of
            'true' -> erlang:cancel_timer(ConfirmRef);
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
               ,confirm_timeout_ref = 'undefined'
               ,confirm_start_time = 'undefined'
               ,method = 'undefined'
               }.

-spec fudge_timeout(timeout()) -> timeout().
fudge_timeout('infinity'=T) -> T;
fudge_timeout(T) -> T + ?FUDGE.

-spec start_req_timeout(timeout()) -> reference().
start_req_timeout('infinity') -> make_ref();
start_req_timeout(Timeout) ->
    erlang:start_timer(Timeout, self(), 'req_timeout').

-spec start_confirm_timeout(timeout()) -> reference().
start_confirm_timeout('infinity') -> make_ref();
start_confirm_timeout(Timeout) ->
    erlang:start_timer(Timeout, self(), 'confirm_timeout').

-spec maybe_convert_to_proplist(kz_term:proplist() | kz_json:object()) -> kz_term:proplist().
maybe_convert_to_proplist(Req) ->
    case kz_json:is_json_object(Req) of
        'true' -> maybe_set_msg_id(kz_json:to_proplist(Req));
        'false' -> maybe_set_msg_id(Req)
    end.

-spec maybe_set_msg_id(kz_term:proplist()) -> kz_term:proplist().
maybe_set_msg_id(Props) ->
    case kz_api:msg_id(Props) of
        'undefined' ->
            props:set_value(<<"Msg-ID">>, kz_binary:rand_hex(8), Props);
        _MsgId ->
            Props
    end.

-spec publish_api(fun(), kz_term:api_terms()) -> 'ok' | {'error', any()}.
publish_api(PublishFun, ReqProps) ->
    try PublishFun(ReqProps) of
        'ok' -> 'ok';
        {'error', _E}=Err -> Err;
        Other ->
            lager:error("publisher fun returned ~p instead of 'ok'", [Other]),
            {'error', Other}
    catch
        'error':'badarg' ->
            ST = erlang:get_stacktrace(),
            lager:error("badarg error when publishing:"),
            kz_util:log_stacktrace(ST),
            {'error', 'badarg'};
        'error':'function_clause' ->
            ST = erlang:get_stacktrace(),
            lager:error("function clause error when publishing:"),
            kz_util:log_stacktrace(ST),
            lager:error("pub fun: ~p", [PublishFun]),
            {'error', 'function_clause'};
        _E:R ->
            lager:error("error when publishing: ~s:~p", [_E, R]),
            {'error', R}
    end.

-type relay_fun() :: fun((pid() | atom(), any()) -> any()).

-spec relay_event(pid(), kz_json:object()) -> any().
relay_event(Pid, JObj) ->
    relay_event(Pid, JObj, fun erlang:send/2).

-spec relay_event(pid(), kz_json:object(), relay_fun()) -> any().
relay_event(Pid, JObj, RelayFun) ->
    RelayFun(Pid, {'amqp_msg', JObj}).

-spec worker_pool(atom()) -> atom().
worker_pool(Pool) ->
    put('$kz_amqp_worker_pool', Pool).

-spec worker_pool() -> atom().
worker_pool() ->
    case get('$kz_amqp_worker_pool') of
        'undefined' -> kz_amqp_sup:pool_name();
        Pool -> Pool
    end.

-spec handle_publish_timeout(state()) -> kz_types:handle_info_ret_state(state()).
handle_publish_timeout(#state{client_from='relay'
                             ,reply_to=From
                             }=State) ->
    lager:debug("timeout waiting for server confirmation"),
    gen_server:reply(From, {'error', <<"timeout receiving server confirmation">>}),
    {'noreply', State};
handle_publish_timeout(#state{client_from=From}=State) ->
    lager:debug("timeout waiting for server confirmation"),
    gen_server:reply(From, {'error', <<"timeout receiving server confirmation">>}),
    {'noreply', reset(State), 'hibernate'}.


-spec handle_confirm(#'basic.ack'{} | #'basic.nack'{}, state()) -> kz_types:handle_cast_ret_state(state()).
handle_confirm(_Msg, #state{client_from='undefined'}=State) ->
    lager:debug("confirm message was returned from the broker but it was too late : ~p",[_Msg]),
    {'noreply', reset(State), 'hibernate'};
handle_confirm(Msg, #state{method=Method
                          ,confirm_timeout_ref = ConfirmRef
                          }=State) ->
    _ = erlang:cancel_timer(ConfirmRef),
    handle_method_confirm(Method, Msg, State#state{confirm_timeout_ref = 'undefined'}).

-spec handle_method_confirm(atom(), #'basic.ack'{} | #'basic.nack'{}, state()) -> kz_types:handle_cast_ret_state(state()).
handle_method_confirm('publish', #'basic.ack'{}, #state{client_from='relay'
                                                       ,reply_to=From
                                                       }=State) ->
    lager:debug("published message was confirmed by the broker"),
    gen_server:reply(From, 'ok'),
    {'noreply', State};
handle_method_confirm('publish', #'basic.ack'{}, #state{client_from=From}=State) ->
    lager:debug("published message was confirmed by the broker"),
    gen_server:reply(From, 'ok'),
    {'noreply', reset(State), 'hibernate'};

handle_method_confirm('publish', #'basic.nack'{}, #state{client_from='relay'
                                                        ,reply_to=From
                                                        }=State) ->
    lager:debug("published message was declined by the broker"),
    gen_server:reply(From, {'error', <<"server declined message">>}),
    {'noreply', State};
handle_method_confirm('publish', #'basic.nack'{}, #state{client_from=From}=State) ->
    lager:debug("published message was declined by the broker"),
    gen_server:reply(From, {'error', <<"server declined message">>}),
    {'noreply', reset(State), 'hibernate'};

handle_method_confirm('request', #'basic.ack'{}, #state{client_pid=ClientPid
                                                       ,current_msg_id = MsgId
                                                       ,timeout=Timeout
                                                       }=State) ->
    lager:debug("request for msg id ~s for ~p was confirmed by broker", [MsgId, ClientPid]),
    NewState = State#state{req_timeout_ref = start_req_timeout(Timeout)
                          ,req_start_time = os:timestamp()
                          },
    {'noreply', NewState};

handle_method_confirm('request', #'basic.nack'{}, #state{client_pid=ClientPid
                                                        ,current_msg_id = MsgId
                                                        ,client_from=From
                                                        }=State) ->
    lager:debug("request for msg id ~s for ~p was declined by broker", [MsgId, ClientPid]),
    gen_server:reply(From, {'error', <<"server declined message">>}),
    {'noreply', reset(State), 'hibernate'};

handle_method_confirm('call_collect', #'basic.ack'{}, #state{client_pid=ClientPid
                                                            ,current_msg_id = MsgId
                                                            ,timeout=Timeout
                                                            }=State) ->
    lager:debug("call collect for msg id ~s for ~p was confirmed by broker", [MsgId, ClientPid]),
    NewState = State#state{req_timeout_ref = start_req_timeout(Timeout)
                          ,req_start_time = os:timestamp()
                          },
    {'noreply', NewState};

handle_method_confirm('call_collect', #'basic.nack'{}, #state{client_pid=ClientPid
                                                             ,current_msg_id = MsgId
                                                             ,client_from=From
                                                             }=State) ->
    lager:debug("call collect for msg id ~s for ~p was declined by broker", [MsgId, ClientPid]),
    gen_server:reply(From, {'error', <<"server nack">>}),
    {'noreply', reset(State), 'hibernate'}.


-spec handle_payload(kz_term:ne_binary(), kz_json:object(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_payload(MsgId, JObj
              ,#state{current_msg_id = MsgId
                     ,client_from = From
                     ,client_vfun = VFun
                     ,responses = 'undefined'
                     ,req_start_time = StartTime
                     ,neg_resp_count = NegCount
                     ,neg_resp_threshold = NegThreshold
                     }=State) when NegCount < NegThreshold ->
    _ = kz_util:put_callid(JObj),

    case VFun(JObj) of
        'true' ->
            case kz_json:is_true(<<"Defer-Response">>, JObj) of
                'false' ->
                    lager:debug("response for msg id ~s took ~b micro to return", [MsgId, timer:now_diff(os:timestamp(), StartTime)]),
                    gen_server:reply(From, {'ok', JObj}),
                    {'noreply', reset(State), 'hibernate'};
                'true' ->
                    lager:debug("deferred response for msg id ~s, waiting for primary response", [MsgId]),
                    {'noreply', State#state{defer_response=JObj}, 'hibernate'}
            end;
        'false' ->
            case kz_json:is_true(<<"Defer-Response">>, JObj) of
                'true' ->
                    lager:debug("ignoring invalid resp as it was deferred"),
                    {'noreply', State};
                'false' ->
                    lager:debug("response failed validator, waiting for more responses"),
                    {'noreply', State#state{neg_resp_count = NegCount + 1
                                           ,neg_resp=JObj
                                           }}
            end
    end;
handle_payload(MsgId, JObj
              ,#state{current_msg_id = MsgId
                     ,client_from = From
                     ,client_cfun = UntilFun
                     ,responses = Resps
                     ,acc = Acc
                     ,req_start_time = StartTime
                     }=State)
  when is_list(Resps)
       andalso is_function(UntilFun, 2) ->
    _ = kz_util:put_callid(JObj),
    lager:debug("recv message ~s", [MsgId]),
    Responses = [JObj | Resps],
    try UntilFun(Responses, Acc) of
        'true' ->
            lager:debug("responses have apparently met the criteria for the client, returning"),
            lager:debug("response for msg id ~s took ~bμs to return"
                       ,[MsgId, kz_time:elapsed_us(StartTime)]
                       ),
            gen_server:reply(From, {'ok', Responses}),
            {'noreply', reset(State), 'hibernate'};
        'false' ->
            {'noreply', State#state{responses=Responses}, 'hibernate'};
        {'false', Acc0} ->
            {'noreply', State#state{responses=Responses, acc=Acc0}, 'hibernate'}
    catch
        _E:_R ->
            lager:warning("supplied until_fun crashed: ~s: ~p", [_E, _R]),
            lager:debug("pretending like until_fun returned false"),
            {'noreply', State#state{responses=Responses}, 'hibernate'}
    end;
handle_payload(MsgId, JObj
              ,#state{current_msg_id = MsgId
                     ,client_from = From
                     ,client_cfun = UntilFun
                     ,responses = Resps
                     ,req_start_time = StartTime
                     }=State) when is_list(Resps) ->
    _ = kz_util:put_callid(JObj),
    lager:debug("recv message ~s", [MsgId]),
    Responses = [JObj | Resps],
    try UntilFun(Responses) of
        'true' ->
            lager:debug("responses have apparently met the criteria for the client, returning"),
            lager:debug("response for msg id ~s took ~bus to return"
                       ,[MsgId, kz_time:elapsed_us(StartTime)]
                       ),
            gen_server:reply(From, {'ok', Responses}),
            {'noreply', reset(State), 'hibernate'};
        'false' ->
            {'noreply', State#state{responses=Responses}, 'hibernate'}
    catch
        _E:_R ->
            lager:warning("supplied until_fun crashed: ~s: ~p", [_E, _R]),
            lager:debug("pretending like until_fun returned false"),
            {'noreply', State#state{responses=Responses}, 'hibernate'}
    end;
handle_payload(_MsgId, JObj
              ,#state{current_msg_id=_CurrMsgId}=State) ->
    _ = kz_util:put_callid(JObj),
    lager:debug("received unexpected message with old/expired message id: ~s, waiting for ~s", [_MsgId, _CurrMsgId]),
    {'noreply', State}.
