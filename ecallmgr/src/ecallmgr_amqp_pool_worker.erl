%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Send/receive AMQP API calls
%%% @end
%%% Created : 19 Sep 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(ecallmgr_amqp_pool_worker).

-behaviour(gen_listener).

%% API
-export([start_link/0, stop/1, start_req/7, handle_req/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, handle_event/2,
         terminate/2, code_change/3]).

-include("ecallmgr.hrl").

-define(SERVER, ?MODULE).
-define(RESPONDERS, [
                     {?MODULE, [{<<"*">>, <<"*">>}]}
                    ]).
-define(BINDINGS, [{self, []}]).

-record(state, {
          status = 'free' :: 'free' | 'busy'
         ,from = 'undefined' :: 'undefined' | {pid(), reference()}
         ,ref = 'undefined' :: 'undefined' | reference()
         ,req_ref = 'undefined' :: 'undefined' | reference()
         ,parent = 'undefined' :: 'undefined' | pid() | atom()
         ,start = 'undefined' :: 'undefined' | wh_now()
         }).

%%%===================================================================
%%% API
%%%===================================================================
handle_req(JObj, Props) ->
    gen_listener:cast(props:get_value(server, Props), {response_recv, JObj}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_listener:start_link(?MODULE, [{responders, ?RESPONDERS}
                                      ,{bindings, ?BINDINGS}
                                     ], []).

stop(Srv) ->
    gen_listener:stop(Srv).

-spec start_req/7 :: (pid(), proplist(), fun(), ne_binary(), {pid(), reference()}, pid() | atom(), non_neg_integer()) -> 'ok'.
start_req(Srv, Prop, ApiFun, CallId, From, Parent, Timeout) when is_integer(Timeout) ->
    JObj = case wh_json:is_json_object(Prop) of
               true -> Prop;
               false -> wh_json:from_list(Prop)
           end,
    JObj1 = wh_json:set_value(<<"Server-ID">>, gen_listener:queue_name(Srv)
                              ,wh_json:set_value(<<"Call-ID">>, CallId, JObj)),
    PubFun = fun() -> put(callid, CallId), ApiFun(JObj1) end,
    gen_listener:cast(Srv, {employed, From, Parent, PubFun, CallId, Timeout}).

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
init([]) ->
    ?LOG("AMQP pool worker started"),
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
handle_cast({employed, {Pid, _}=From, Parent, PubFun, CallId, Timeout}, #state{status=free}=State) ->
    ?LOG(CallId, "Employed by ~p via ~p", [Pid, Parent]),
    Ref = erlang:monitor(process, Pid),
    ReqRef = erlang:start_timer(Timeout, self(), req_timeout),
    try
        PubFun(),
        {noreply, State#state{status=busy, from=From, ref=Ref
                              ,parent=Parent, start=erlang:now()
                              ,req_ref=ReqRef
                             }}
    catch
        E:R ->
            ?LOG("publish fun ~s: ~p", [E,R]),
            erlang:cancel_timer(ReqRef),
            ecallmgr_amqp_pool:worker_free(Parent, self(), 0),
            {noreply, State}
    end;
handle_cast({response_recv, JObj}, #state{status=busy, from=From, parent=Parent, ref=Ref
                                          ,start=Start, req_ref=ReqRef}) ->
    Elapsed = timer:now_diff(erlang:now(), Start),
    ?LOG("received response after ~b ms, returning to pool ~p", [Elapsed div 1000, Parent]),

    erlang:demonitor(Ref, [flush]),
    erlang:cancel_timer(ReqRef),
    gen_server:reply(From, {ok, JObj}),

    ecallmgr_amqp_pool:worker_free(Parent, self(), Elapsed),
    {noreply, #state{}};
handle_cast({response_recv, JObj}, State) ->
    ?LOG("WTF, I'm free, yet receiving a response?"),
    ?LOG("EvtCat: ~s", [wh_json:get_value(<<"Event-Category">>, JObj)]),
    ?LOG("EvtName: ~s", [wh_json:get_value(<<"Event-Name">>, JObj)]),
    ?LOG("SrvID: ~s", [wh_json:get_value(<<"Server-ID">>, JObj)]),
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
handle_info({'DOWN', Ref, process, Pid, _Info}, #state{status=busy, ref=Ref, parent=Parent}) ->
    ?LOG_END("requestor (~w) down, giving up on task", [Pid]),
    erlang:demonitor(Ref, [flush]),
    ecallmgr_amqp_pool:worker_free(Parent, self(), 0),
    {noreply, #state{}};

handle_info(req_timeout, #state{status=busy, from=From, parent=Parent, ref=Ref
                                ,start=Start, req_ref=ReqRef
                               }) ->
    ?LOG("request took too long, timing out caller"),
    Elapsed = timer:now_diff(erlang:now(), Start),
    ?LOG("received response after ~b ms, returning to pool ~p", [Elapsed div 1000, Parent]),

    erlang:demonitor(Ref, [flush]),
    erlang:cancel_timer(ReqRef),

    gen_server:reply(From, {error, timeout}),

    ecallmgr_amqp_pool:worker_free(Parent, self(), Elapsed),
    {noreply, #state{}};

handle_info(_Info, State) ->
    ?LOG("Unhandled message: ~p", [_Info]),
    {noreply, State}.

handle_event(_JObj, _State) ->
    {reply, [{server, self()}]}.

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
