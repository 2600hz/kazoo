%%%-------------------------------------------------------------------
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Listener for route requests that can be fulfilled by callflows
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(cf_listener).

-behaviour(gen_listener).

%% API
-export([start_link/0]).
-export([stop/0]).
-export([pause/0]).
-export([resume/0]).
-export([handle_call_status_resp/2]).

%% gen_listener callbacks
-export([init/1, handle_call/3, handle_cast/2
         ,handle_info/2, handle_event/2, terminate/2
         ,code_change/3
        ]).

-include("callflow.hrl").

-define(SERVER, ?MODULE).

-define(RESPONDERS, [{cf_route_req, [{<<"dialplan">>, <<"route_req">>}]}
                     ,{cf_route_win, [{<<"dialplan">>, <<"route_win">>}]}
                     ,{{cf_util, presence_probe}, [{<<"notification">>, <<"presence_probe">>}]}
                     ,{{cf_util, presence_mwi_query}, [{<<"notification">>, <<"mwi_query">>}]}
                     ,{{?MODULE, handle_call_status_resp}, [{<<"call_event">>, <<"channel_status_resp">>}]}
                    ]).
-define(BINDINGS, [{route, []}
                   ,{self, []}
                  ]).
-define(QUEUE_NAME, <<"">>).
-define(QUEUE_OPTIONS, []).
-define(CONSUME_OPTIONS, []).

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
start_link() ->
    gen_listener:start_link(?MODULE, [{responders, ?RESPONDERS}
                                      ,{bindings, ?BINDINGS}
                                      ,{queue_name, ?QUEUE_NAME}
                                      ,{queue_options, ?QUEUE_OPTIONS}
                                      ,{consume_options, ?CONSUME_OPTIONS}
                                     ], []).

-spec pause/0 :: () -> 'ok'.
pause() ->
    {ok, Srv} = callflow_sup:listener_proc(),
    gen_listener:rm_responder(Srv, cf_route_req).

-spec resume/0 :: () -> 'ok'.
resume() ->
    {ok, Srv} = callflow_sup:listener_proc(),
    gen_listener:add_responder(Srv, cf_route_req, [{<<"dialplan">>, <<"route_req">>}]).

-spec stop/0 :: () -> 'ok'.
stop() ->
    {ok, Srv} = callflow_sup:listener_proc(),
    gen_listener:stop(Srv).

-spec handle_call_status_resp/2 :: (wh_json:json_object(), proplist()) -> any().
handle_call_status_resp(JObj, Props) ->
    Consumers = props:get_value(consumers, Props),
    StatusCallId = wh_json:get_value(<<"Call-ID">>, JObj),
    [Consumer ! {call_status_resp, JObj}
     || {CallId, Consumer, _} <- Consumers
            ,CallId =:= StatusCallId
    ].

%%%===================================================================
%%% gen_listener callbacks
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
    process_flag(trap_exit, true),
    lager:debug("starting new callflow listener"),
    Self = self(),
    spawn(fun() -> 
                  QueueName = <<"callflow_presence_probe">>,
                  Options = [{queue_options, [{exclusive, false}]}
                             ,{consume_options, [{exclusive, false}]}
                             ,{basic_qos, 1}
                            ],
                  Bindings= [{notifications, [{restrict_to, [presence_probe, mwi_query]}]}],
                  gen_listener:add_queue(Self, QueueName, Options, Bindings)
          end),
    {ok, []}.

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
handle_call(_Msg, _From, Consumers) ->
    {noreply, Consumers}.

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
handle_cast({add_consumer, CallId, Consumer}, Consumers) ->
    MRef = erlang:monitor(process, Consumer),
    lager:debug("added call status response consumer (~p) for ~s", [Consumer, CallId]),
    {noreply, [{CallId, Consumer, MRef}|Consumers]};
handle_cast({remove_consumer, Consumer}, Consumers) ->
    {noreply, lists:filter(fun({_, C, MRef}) when C =:= Consumer -> 
                                   lager:debug("removed call status response consumer (~p): response sent", [Consumer]),
                                   erlang:demonitor(MRef, [flush]),
                                   false; 
                              (_) -> true 
                           end, Consumers)};
handle_cast(_Msg, Consumers) ->
    {noreply, Consumers}.

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
handle_info({'DOWN', _, _, Consumer, _R}, Consumers) ->
    {noreply, lists:filter(fun({_, C, MRef}) when C =:= Consumer -> 
                                   lager:debug("removed call status response consumer (~p): ~p", [Consumer, _R]),
                                   erlang:demonitor(MRef, [flush]),
                                   false; 
                              (_) -> true 
                           end, Consumers)};
handle_info(_Info, Consumers) ->
    lager:debug("unhandled message: ~p", [_Info]),
    {noreply, Consumers}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling AMQP event objects
%%
%% @spec handle_event(JObj, State) -> {reply, Props}
%% @end
%%--------------------------------------------------------------------
handle_event(_JObj, Consumers) ->
    {reply, [{consumers, Consumers}]}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_listener when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_listener terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec terminate/2 :: (term(), term()) -> 'ok'.
terminate(_Reason, _) ->
    lager:debug("callflow listner ~p termination", [_Reason]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, Consumers, _Extra) ->
    {ok, Consumers}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
