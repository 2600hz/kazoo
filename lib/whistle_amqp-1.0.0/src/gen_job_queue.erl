%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% The problem:
%%%   We have a queue of jobs that need processing, and a list of resources
%%%   available to process those jobs. If there are no available resources
%%%   to process the next job, nothing progresses until an appropriate
%%%   resource is freed.
%%%
%%%   Two approaches:
%%%     Two queues: A Jobs queue and a Resource queue
%%%       The Jobs queue has all the jobs to be processed, which we bind to but
%%%       fetch from manually after receiving the Resource state payload
%%%       The Resource queue has at most one payload, representing the state of
%%%       all resources. We bind to and consume from the queue, so that when
%%%       we receive the state payload, we fetch a job and try to pair the job
%%%       with an available resource.
%%%       If we fail to match a job, we nack the job, and republish the resource
%%%       state unchanged. If we match a job, we ack the job and update the
%%%       resource state, publishing the new state, and processing the job.
%%%
%%%     Three queues: A Jobs queue, a coordination queue, and an anonymous queue
%%%       The Jobs queue is identical to the two-queue approach
%%%       The Coordination queue only has a token, no state, but serves to sync
%%%       workers. Only with the token can a worker fetch a job and match it to
%%%       the worker's internal state of resources. Similar to the above Resource
%%%       queue, but state is kept inside the module rather than in the token.
%%%       The anonymous queue is bound to a resource routing key, and as each worker
%%%       uses/releases resources, they publish to this routing key, so each worker
%%%       gets updates as they happen.
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(gen_job_queue).

-behaviour(gen_server).

-include_lib("amqp_client/include/amqp_client.hrl").
-include_lib("whistle/include/wh_amqp.hrl").
-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_log.hrl").

%% API
-export([behaviour_info/1]).

-export([start_link/3, start_link/4, stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% gen_server API
-export([call/2, call/3, cast/2, reply/2]).

behaviour_info(callbacks) ->
    [{init, 1}
     ,{handle_job, 3} % exec Mod:handle_job(Job, ResourceState, ModState)
     ,{handle_call, 3}
     ,{handle_cast, 2}
     ,{handle_info, 2}
     ,{terminate, 2}
    ];
behaviour_info(_) ->
    undefined.

-record(state, {
          jobs_queue = <<>> :: binary()
         ,resource_queue = <<>> :: binary()
         ,is_consuming = false :: boolean()
         ,module :: atom()
         ,module_state :: term()
         ,module_timeout_ref :: reference()
         }).

-define(TIMEOUT_RETRY_CONN, 1000).
-define(CALLBACK_TIMEOUT_MSG, callback_timeout).

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
-spec start_link/3 :: (atom(), ne_binary(), ne_binary()) -> {'ok', pid()} |
                                                            {'error', term()}.
-spec start_link/4 :: (atom(), ne_binary(), ne_binary(), wh_proplist()) -> {'ok', pid()} |
                                                                           {'error', term()}.
start_link(Module, JobsQueue, ResourceQueue) ->
    start_link(Module, JobsQueue, ResourceQueue, []).
start_link(Module, <<_/binary>> = JobsQueue, <<_/binary>> = ResourceQueue, InitArgs) when is_atom(Module),
                                                           is_list(InitArgs) ->
    gen_server:start_link(?MODULE, [Module, JobsQueue, ResourceQueue, InitArgs], []).

-spec stop/1 :: (pid()) -> 'ok'.
stop(Srv) when is_pid(Srv) ->
    gen_server:cast(Srv, stop).

%% API functions that mirror gen_server:call,cast,reply
-spec call/2 :: (pid(), term()) -> term().
call(Name, Request) ->
    gen_server:call(Name, Request).

-spec call/3 :: (pid(), term(), 'infinity' | non_neg_integer()) -> term().
call(Name, Request, Timeout) ->
    gen_server:call(Name, Request, Timeout).

-spec cast/2 :: (pid(), term()) -> 'ok'.
cast(Name, Request) ->
    gen_server:cast(Name, Request).

-spec reply/2 :: ({pid(), reference()}, term()) -> no_return().
reply(From, Msg) ->
    gen_server:reply(From, Msg).

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
init([Module, JobsQueue, ResourceQueue, InitArgs]) ->
    process_flag(trap_exit, true),

    ?LOG_START("new gen_job_queue proc: ~s", [Module]),

    {ModState, TimeoutRef} = case erlang:function_exported(Module, init, 1) andalso Module:init(InitArgs) of
                                 {ok, MS} ->
                                     {MS, undefined};
                                 {ok, MS, hibernate} ->
                                     {MS, undefined};
                                 {ok, MS, Timeout} ->
                                     {MS, start_timer(Timeout)};
                                 Err ->
                                     throw(Err)
                             end,

    ok = start_jobs_queue(JobsQueue),
    ok = start_resource_queue(ResourceQueue),

    _ = erlang:send_after(?TIMEOUT_RETRY_CONN, self(), is_consuming),

    {ok, #state{jobs_queue=JobsQueue
                ,resource_queue=ResourceQueue
                ,module=Module
                ,module_state=ModState
                ,module_timeout_ref=TimeoutRef
                }, hibernate}.

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
handle_call(Request, From, #state{module=Module, module_state=ModState, module_timeout_ref=OldRef}=State) ->
    _ = stop_timer(OldRef),
    case catch Module:handle_call(Request, From, ModState) of
        {reply, Reply, ModState1} ->
            {reply, Reply, State#state{module_state=ModState1, module_timeout_ref=undefined}, hibernate};
        {reply, Reply, ModState1, Timeout} when is_integer(Timeout) andalso Timeout >= 0 ->
            Ref = start_timer(Timeout),
            {reply, Reply, State#state{module_state=ModState1, module_timeout_ref=Ref}, hibernate};
        {noreply, ModState1} ->
            {noreply, State#state{module_state=ModState1}, hibernate};
        {noreply, ModState1, Timeout} ->
            Ref = start_timer(Timeout),
            {noreply, State#state{module_state=ModState1, module_timeout_ref=Ref}, hibernate};
        {stop, Reason, ModState1} ->
            {stop, Reason, State#state{module_state=ModState1}};
        {stop, Reason, Reply, ModState1} ->
            {stop, Reason, Reply, State#state{module_state=ModState1}};
        {'EXIT', Why} ->
            ?LOG(alert, "exception: ~p", [Why]),
            {stop, Why, State}
    end.

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
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(Message, #state{module=Module, module_state=ModState, module_timeout_ref=OldRef}=State) ->
    _ = stop_timer(OldRef),
    case catch Module:handle_cast(Message, ModState) of
        {noreply, ModState1} ->
            {noreply, State#state{module_state=ModState1}, hibernate};
        {noreply, ModState1, Timeout} ->
            Ref = start_timer(Timeout),
            {noreply, State#state{module_state=ModState1, module_timeout_ref=Ref}, hibernate};
        {stop, Reason, ModState1} ->
            {stop, Reason, State#state{module_state=ModState1}};
        {'EXIT', {Reason, ST}} ->
            ?LOG("exception: ~p", [Reason]),
            ?LOG_STACKTRACE(ST),
            {stop, Reason, State}
    end.

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
handle_info({#'basic.deliver'{exchange=_Ex, routing_key=_Rk, delivery_tag=_Dt}, #amqp_msg{props = #'P_basic'{content_type=CT}, payload = Payload}}, #state{}=State) ->
    ?LOG("received from ~s: ~s", [_Ex, _Rk]),
    ?LOG("delivery tag: ~p", [_Dt]),
    ?LOG("content type: ~s", [CT]),
    ?LOG("amqp payload: ~s", [Payload]),
    {noreply, State};

handle_info({amqp_host_down, _H}=Down, #state{jobs_queue=JobsQueue
                                              ,resource_queue=ResourceQueue
                                             }=State) ->
    ?LOG(alert, "amqp host down msg: ~p", [_H]),
    case amqp_util:is_host_available() of
        true ->
            ?LOG("host is available, let's try wiring up"),
            ok = start_jobs_queue(JobsQueue),
            ok = start_resource_queue(ResourceQueue),
            _ = erlang:send_after(?TIMEOUT_RETRY_CONN, self(), is_consuming),
            {noreply, State#state{is_consuming=false}, hibernate};
        false ->
            ?LOG("no AMQP host ready, waiting another ~b", [?TIMEOUT_RETRY_CONN]),
            erlang:send_after(?TIMEOUT_RETRY_CONN, self(), Down),
            {noreply, State#state{is_consuming=false}, hibernate}
    end;

handle_info({amqp_lost_channel, no_connection}, State) ->
    ?LOG(alert, "lost our channel, checking every second for a host to come back up"),
    _Ref = erlang:send_after(?TIMEOUT_RETRY_CONN, self(), {amqp_host_down, ok}),
    {noreply, State#state{is_consuming=false}, hibernate};

handle_info(#'basic.consume_ok'{}, S) ->
    ?LOG("consuming from a queue"),
    {noreply, S#state{is_consuming=true}, hibernate};

handle_info(is_consuming, #state{is_consuming=false}=State) ->
    ?LOG("huh, we're not consuming, consider AMQP down"),
    _Ref = erlang:send_after(?TIMEOUT_RETRY_CONN, self(), {amqp_host_down, ok}),
    {noreply, State};

handle_info(is_consuming, State) ->
    {noreply, State};

handle_info(callback_timeout, State) ->
    handle_callback_info(timeout, State);

handle_info(Message, State) ->
    handle_callback_info(Message, State).

handle_callback_info(Message, #state{module=Module, module_state=ModState, module_timeout_ref=OldRef}=State) ->
    _ = stop_timer(OldRef),
    case catch Module:handle_info(Message, ModState) of
        {noreply, ModState1} ->
            {noreply, State#state{module_state=ModState1}, hibernate};
        {noreply, ModState1, Timeout} ->
            Ref = start_timer(Timeout),
            {noreply, State#state{module_state=ModState1, module_timeout_ref=Ref}, hibernate};
        {stop, Reason, ModState1} ->
            {stop, Reason, State#state{module_state=ModState1}};
        {'EXIT', Why} ->
            ?LOG(alert, "exception: ~p", [Why]),
            {stop, Why, State}
    end.

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
terminate(Reason, #state{module=Module, module_state=ModState}) ->
    Module:terminate(Reason, ModState),
    ?LOG_END("~s terminated cleanly, going down", [Module]).

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
-spec stop_timer/1 :: ('undefined' | reference()) -> non_neg_integer() | 'false'.
stop_timer(undefined) ->
    false;
stop_timer(Ref) when is_reference(Ref) ->
    erlang:cancel_timer(Ref).

-spec start_timer/1 :: (term()) -> reference() | 'undefined'.
start_timer(Timeout) when is_integer(Timeout) andalso Timeout >= 0 ->
    erlang:send_after(Timeout, self(), ?CALLBACK_TIMEOUT_MSG);
start_timer(_) -> 'undefined'.

-spec start_jobs_queue/1 :: (ne_binary()) -> 'ok' | {'error', term()}.
start_jobs_queue(JobsQueue) ->
    amqp_util:new_queue(JobsQueue),
    amqp_util:bind_q_to_targeted(JobsQueue).

-spec start_resource_queue/1 :: (ne_binary()) -> 'ok' | {'error', term()}.
start_resource_queue(ResourceQueue) ->
    amqp_util:new_queue(ResourceQueue),
    amqp_util:bind_q_to_targeted(ResourceQueue),
    ConsumeOpts = [{no_ack, false}
                   ,{exclusive, false}
                  ],
    amqp_util:basic_consume(ResourceQueue, ConsumeOpts),
    amqp_util:basic_qos(1).
