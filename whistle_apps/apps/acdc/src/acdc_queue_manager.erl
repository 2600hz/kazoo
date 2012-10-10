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

-define(SERVER, ?MODULE).

-define(BINDINGS, [{conf, [{doc_type, <<"queue">>}]}
                   ,{acdc_queue, [{restrict_to, [stats_req, member_call]}
                                  ,{account_id, <<"*">>}
                                  ,{queue_id, <<"*">>}
                                 ]}
                  ]).
-define(RESPONDERS, [{{acdc_queue_handler, handle_config_change}
                      ,[{<<"configuration">>, <<"*">>}]
                     }
                     ,{{acdc_queue_handler, handle_stats_req}
                       ,[{<<"queue">>, <<"stats_req">>}]
                      }
                     ,{{acdc_queue_manager, handle_member_call}
                       ,[{<<"member">>, <<"call">>}]
                      }
                    ]).

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

handle_member_call(JObj, _Props) ->
    Call = whapps_call:from_json(wh_json:get_value(<<"Call">>, JObj)),

    AcctId = wh_json:get_value(<<"Account-ID">>, JObj),
    QueueId = wh_json:get_value(<<"Queue-ID">>, JObj),

    lager:debug("member call for ~s: ~s", [AcctId, QueueId]),

    acdc_stats:call_waiting(AcctId, QueueId, whapps_call:call_id(Call)),

    case acdc_queues_sup:find_queue_supervisor(AcctId, QueueId) of
        P when is_pid(P) ->
            acdc_queue:put_member_on_hold(acdc_queue_sup:queue(P), Call);
        undefined ->
            whapps_call_command:answer(Call),
            whapps_call_command:hold(Call)
    end,
    wapi_acdc_queue:publish_shared_member_call(AcctId, QueueId, JObj).

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
    {ok, ok}.

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
