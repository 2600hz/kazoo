%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% Manages queue processes:
%%%   starting when a queue is created
%%%   stopping when a queue is deleted
%%%   collecting stats from queues
%%%   and more!!!
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(acdc_queue_manager).

-behaviour(gen_listener).

%% API
-export([start_link/0
         ,handle_member_call/2
         ,handle_member_call_cancel/2
         ,should_ignore_member_call/2
         ,handle_channel_destroy/2
        ]).

%% gen_server callbacks
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,handle_event/2
         ,terminate/2
         ,code_change/3
        ]).

-include("acdc.hrl").

-define(SERVER, ?MODULE).

-record(state, {ignored_member_calls = dict:new() :: dict()}).

-define(BINDINGS, [{conf, [{doc_type, <<"queue">>}]}
                   ,{acdc_queue, [{restrict_to, [stats_req]}
                                  ,{account_id, <<"*">>}
                                  ,{queue_id, <<"*">>}
                                 ]}
                   ,{notifications, [{restrict_to, [presence_probe]}]}
                  ]).
-define(RESPONDERS, [{{acdc_queue_handler, handle_config_change}
                      ,[{<<"configuration">>, <<"*">>}]
                     }
                     ,{{acdc_queue_handler, handle_stats_req}
                       ,[{<<"queue">>, <<"stats_req">>}]
                      }
                     ,{{acdc_queue_handler, handle_presence_probe}
                       ,[{<<"notification">>, <<"presence_probe">>}]
                      }
                     ,{{acdc_queue_manager, handle_member_call}
                       ,[{<<"member">>, <<"call">>}]
                      }
                     ,{{acdc_queue_manager, handle_member_call_cancel}
                       ,[{<<"member">>, <<"call_cancel">>}]
                      }
                     ,{{acdc_queue_manager, handle_channel_destroy}
                       ,[{<<"call_event">>, <<"CHANNEL_DESTROY">>}]
                      }
                    ]).

-define(SECONDARY_BINDINGS, [{acdc_queue, [{restrict_to, [member_call]}
                                           ,{account_id, <<"*">>}
                                           ,{queue_id, <<"*">>}
                                          ]}
                            ]).
-define(SECONDARY_QUEUE_NAME, <<"acdc.queue.manager">>).
-define(SECONDARY_QUEUE_OPTIONS, [{exclusive, false}]).
-define(SECONDARY_CONSUME_OPTIONS, [{exclusive, false}]).

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
    gen_listener:start_link({local, ?SERVER}, ?MODULE
                            ,[{bindings, ?BINDINGS}
                              ,{responders, ?RESPONDERS}
                             ]
                            ,[]
                           ).

handle_channel_destroy(JObj, _Props) ->
    true = wapi_call:event_v(JObj),
    acdc_stats:call_finished(wh_json:get_value(<<"Call-ID">>, JObj)).

handle_member_call(JObj, Props) ->
    true = wapi_acdc_queue:member_call_v(JObj),

    Call = whapps_call:from_json(wh_json:get_value(<<"Call">>, JObj)),

    AcctId = wh_json:get_value(<<"Account-ID">>, JObj),
    QueueId = wh_json:get_value(<<"Queue-ID">>, JObj),

    lager:debug("member call for ~s:~s: ~s", [AcctId, QueueId, whapps_call:call_id(Call)]),

    acdc_stats:call_waiting(AcctId, QueueId, whapps_call:call_id(Call)),

    case acdc_queues_sup:find_queue_supervisor(AcctId, QueueId) of
        P when is_pid(P) ->
            acdc_queue:put_member_on_hold(acdc_queue_sup:queue(P), Call);
        undefined ->
            whapps_call_command:answer(Call),
            whapps_call_command:hold(Call)
    end,
    wapi_acdc_queue:publish_shared_member_call(AcctId, QueueId, JObj),

    gen_listener:cast(props:get_value(server, Props), {monitor_call, Call}),

    acdc_util:queue_presence_update(AcctId, QueueId, ?PRESENCE_RED_FLASH).

handle_member_call_cancel(JObj, _Props) ->
    true = wapi_acdc_queue:member_call_cancel_v(JObj),
    K = make_ignore_key(wh_json:get_value(<<"Account-ID">>, JObj)
                        ,wh_json:get_value(<<"Queue-ID">>, JObj)
                        ,wh_json:get_value(<<"Call-ID">>, JObj)
                       ),
    gen_listener:cast(?SERVER, {member_call_cancel, K}).

should_ignore_member_call(Call, CallJObj) ->
    K = make_ignore_key(wh_json:get_value(<<"Account-ID">>, CallJObj)
                        ,wh_json:get_value(<<"Queue-ID">>, CallJObj)
                        ,whapps_call:call_id(Call)
                       ),
    gen_listener:call(?SERVER, {should_ignore_member_call, K}).

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
    _ = start_secondary_queue(),
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
handle_call({should_ignore_member_call, K}, _, #state{ignored_member_calls=Dict}=State) ->
    case catch dict:fetch(K, Dict) of
        {'EXIT', _} -> {reply, false, State};
        _Res -> {reply, true, State#state{ignored_member_calls=dict:erase(K, Dict)}}
    end;
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
handle_cast({member_call_cancel, K}, #state{ignored_member_calls=Dict}=State) ->
    {noreply, State#state{
                ignored_member_calls=dict:store(K, true, Dict)
               }};
handle_cast({monitor_call, Call}, State) ->
    gen_listener:add_binding(self(), call, [{callid, whapps_call:call_id(Call)}
                                            ,{restrict_to, [events]}
                                           ]),
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
    lager:debug("queue manager terminating: ~p", [_Reason]).

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
start_secondary_queue() ->
    Self = self(),
    spawn(fun() -> gen_listener:add_queue(Self
                                          ,?SECONDARY_QUEUE_NAME
                                          ,[{queue_options, ?SECONDARY_QUEUE_OPTIONS}
                                            ,{consume_options, ?SECONDARY_CONSUME_OPTIONS}
                                            ,{basic_qos, 1}
                                           ]
                                          ,?SECONDARY_BINDINGS
                                         )
          end).

make_ignore_key(AcctId, QueueId, CallId) ->
    {AcctId, QueueId, CallId}.
