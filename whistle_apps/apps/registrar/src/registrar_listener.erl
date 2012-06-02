%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2012, VoIP INC
%%% @doc
%%% Listener for authn_req, reg_success, and reg_query AMQP requests
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(registrar_listener).

-behaviour(gen_listener).

%% API
-export([start_link/0, stop/1]).
-export([handle_reg_query_resp/2]).

%% gen_listener callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, handle_event/2
         ,terminate/2, code_change/3]).

-include("reg.hrl").

-define(RESPONDERS, [{reg_authn_req, [{<<"directory">>, <<"authn_req">>}]}
                     ,{reg_success, [{<<"directory">>, <<"reg_success">>}]}
                     ,{{reg_query, req_query_req}, [{<<"directory">>, <<"reg_query">>}]}
                     ,{{?MODULE, handle_reg_query_resp}, [{<<"directory">>, <<"reg_query_resp">>}]}
                     ,{{reg_query, presence_probe}, [{<<"notification">>, <<"presence_probe">>}]}
                     ,{{reg_route_req, handle_route_req}, [{<<"dialplan">>, <<"route_req">>}]}
                    ]).
-define(BINDINGS, [{authn, []}
                   ,{registration, []}
                   ,{route, []}
                   ,{self, []}
                  ]).

-define(SERVER, ?MODULE).
-define(REG_QUEUE_NAME, <<"">>).
-define(REG_QUEUE_OPTIONS, []).
-define(REG_CONSUME_OPTIONS, []).

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
    gen_listener:start_link({local, ?SERVER}, ?MODULE, [{responders, ?RESPONDERS}
                                                        ,{bindings, ?BINDINGS}
                                                        ,{queue_name, ?REG_QUEUE_NAME}
                                                        ,{queue_options, ?REG_QUEUE_OPTIONS}
                                                        ,{consume_options, ?REG_CONSUME_OPTIONS}
                                                       ], []).

-spec stop/1 :: (pid()) -> 'ok'.
stop(Srv) ->
    gen_listener:stop(Srv).

-spec handle_reg_query_resp/2 :: (wh_json:json_object(), proplist()) -> any().
handle_reg_query_resp(JObj, Props) ->
    Reg = wh_json:get_value(<<"Fields">>, JObj),
    Username =  wh_json:get_value(<<"Username">>, Reg),
    Realm =  wh_json:get_value(<<"Realm">>, Reg),
    Consumers = props:get_value(consumers, Props),
    _ = [Consumer ! {reg_query_resp, Reg}
         || {User, Consumer, _} <- Consumers,
            User =:= {Username, Realm}
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
    lager:debug("starting new registrar server"),
    Self = self(),
    spawn(fun() -> 
                  QueueName = <<"registrar_presence_probe">>,
                  Options = [{queue_options, [{exclusive, false}]}
                             ,{consume_options, [{exclusive, false}]}
                             ,{basic_qos, 1}
                            ],
                  Bindings= [{notifications, [{restrict_to, [presence_probe]}]}],
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
handle_cast({add_consumer, User, Realm, Consumer}, Consumers) ->
    MRef = erlang:monitor(process, Consumer),
    lager:debug("added req query response consumer (~p) for ~s@~s", [Consumer, User, Realm]),
    {noreply, [{{User, Realm}, Consumer, MRef}|Consumers]};
handle_cast({remove_consumer, Consumer}, Consumers) ->
    {noreply, lists:filter(fun({_, C, MRef}) when C =:= Consumer -> 
                                   lager:debug("removed req query response consumer (~p): response sent", [Consumer]),
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
                                   lager:debug("removed req query response consumer (~p): ~p", [Consumer, _R]),
                                   erlang:demonitor(MRef, [flush]),
                                   false; 
                              (_) -> true 
                           end, Consumers)};
handle_info(_Info, Consumers) ->
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
    lager:debug("registrar server ~p termination", [_Reason]).

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
