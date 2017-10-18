%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(kz_notify_resend).
-behaviour(gen_server).

-export([start_link/0, stop/0]).
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3
        ]).

-export([running/0
        ,send_single/1
        ]).

-include("tasks.hrl").

-record(state, {running = [] :: kz_json:objects()
               }).
-type state() :: #state{}.

-define(NAME, ?MODULE).
-define(SERVER, {'via', 'kz_globals', ?NAME}).

-define(MOD_CONFIG_CAT, <<(?CONFIG_CAT)/binary, ".notify_resend">>).
-define(DEFAULT_TIMEOUT, 10 * ?MILLISECONDS_IN_SECOND).

%% notify resned crawler settungs
-define(NOTIFY_RESEND_ENABLED,
        kapps_config:get_is_true(?MOD_CONFIG_CAT, <<"notify_resend_enabled">>, 'true')).
-define(TIME_BETWEEN_CYCLE,
        kapps_config:get_integer(?MOD_CONFIG_CAT, <<"cycle_delay_time_ms">>, 5 * ?MILLISECONDS_IN_MINUTE)).
-define(READ_LIMIT,
        kapps_config:get_integer(?MOD_CONFIG_CAT, <<"max_doc_read">>, 20)).
-define(PUBLISH_TIMEOUT,
        kapps_config:get_pos_integer(?MOD_CONFIG_CAT, <<"publish_timeout_ms">>, ?DEFAULT_TIMEOUT)).

%% default reschedule rules
-define(DEFAULT_RETRY_COUNT,
        kapps_config:get_integer(?MOD_CONFIG_CAT, <<"max_retries">>, 3)).
-define(DEFAULT_RETRY_PERIOD,
        kapps_config:get_integer(?MOD_CONFIG_CAT, <<"retry_after_fudge_s">>, 10 * ?SECONDS_IN_MINUTE)).

-define(VOICEMAIL_RESCHEDULE_RULES,
        kz_json:from_list(
          [{<<"rules">>
           ,kz_json:from_list(
              [{<<"after_15_mins">>
               ,kz_json:from_list([{<<"attempt">>, 1}
                                  ,{<<"retries">>, 4}
                                  ,{<<"retry_after_s">>, 15 * ?SECONDS_IN_MINUTE}
                                  ])
               }
              ,{<<"after_45_mins">>
               ,kz_json:from_list([{<<"attempt">>, 2}
                                  ,{<<"retries">>, 4}
                                  ,{<<"retry_after_s">>, 45 * ?SECONDS_IN_MINUTE}
                                  ])
               }
              ,{<<"after_two_hours">>
               ,kz_json:from_list([{<<"attempt">>, 3}
                                  ,{<<"retries">>, 4}
                                  ,{<<"retry_after_s">>, 2 * ?SECONDS_IN_HOUR}
                                  ])
               }
              ,{<<"after_one_day">>
               ,kz_json:from_list([{<<"attempt">>, 4}
                                  ,{<<"retries">>, 4}
                                  ,{<<"retry_after_s">>, 1 * ?SECONDS_IN_DAY}
                                  ])
               }
              ])
           }
          ])).

-define(DEFAULT_RESCHEDULE_RULES,
        kz_json:from_list(
          [{<<"voicemail_new">>, ?VOICEMAIL_RESCHEDULE_RULES}
          ])).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts the server
%%--------------------------------------------------------------------
-spec start_link() -> startlink_ret().
start_link() ->
    case gen_server:start_link(?SERVER, ?MODULE, [], []) of
        {'error', {'already_started', Pid}}
          when is_pid(Pid)->
            erlang:link(Pid),
            {'ok', Pid};
        Other -> Other
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
-spec init([]) -> {'ok', state(), kz_timeout()}.
init([]) ->
    kz_util:put_callid(?NAME),
    lager:debug("~s has been started", [?NAME]),
    {'ok', #state{}, ?TIME_BETWEEN_CYCLE}.

-spec stop() -> ok.
stop() ->
    gen_server:cast(?SERVER, stop).

-spec running() -> kz_json:objects().
running() ->
    case kz_globals:where_is(?NAME) of
        'undefined' -> [];
        _ -> gen_server:call(?SERVER, 'running')
    end.

-spec send_single(ne_binary()) -> {'ok' | 'failed', kz_json:object()} | {'error', any()}.
send_single(Id) ->
    case kz_datamgr:open_doc(?KZ_PENDING_NOTIFY_DB, Id) of
        {'ok', JObj} ->
            kz_util:put_callid(kz_json:get_value(<<"payload">>, JObj)),
            process_single(JObj);
        {'error', _}=Error -> Error
    end.

%% @private
-spec next() -> 'ok'.
next() ->
    gen_server:cast(?SERVER, 'next_cycle').

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
-spec handle_call(any(), pid_ref(), state()) -> handle_call_ret_state(state()).
handle_call('running', _From, #state{running=Running}=State) ->
    {'reply', Running, State};
handle_call(_Request, _From, State) ->
    {'reply', {'error', 'not_implemented'}, State}.

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
-spec handle_cast(any(), state()) -> handle_cast_ret_state(state()).
handle_cast('next_cycle', State) ->
    {'noreply', State#state{running=[]}, ?TIME_BETWEEN_CYCLE};
handle_cast(stop, State) ->
    lager:debug("notify resender has been stopped"),
    {stop, normal, State};
handle_cast(_Msg, State) ->
    lager:debug("unhandled cast: ~p", [_Msg]),
    {'noreply', State}.

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
-spec handle_info(any(), state()) -> handle_info_ret_state(state()).
handle_info('timeout', State) ->
    ViewOptions = [{'startkey', 0}
                  ,{'endkey', kz_time:now_s()}
                  ,{'limit', ?READ_LIMIT}
                  ,'include_docs'
                  ],
    case kz_datamgr:get_results(?KZ_PENDING_NOTIFY_DB, <<"pending_notify/list_by_modified">>, ViewOptions) of
        {'ok', []} -> {'noreply', State, ?TIME_BETWEEN_CYCLE};
        {'ok', Pendings} ->
            _ = kz_util:spawn(fun () -> process_then_next_cycle([kz_json:get_value(<<"doc">>, J) || J <- Pendings]) end),
            {'noreply', State#state{running=Pendings}};
        {'error', 'not_found'} ->
            kapps_maintenance:refresh(?KZ_PENDING_NOTIFY_DB),
            {'noreply', State, ?TIME_BETWEEN_CYCLE};
        {'error', _Reason} ->
            lager:debug("failed to pending notifications jobs: ~p", [_Reason]),
            {'noreply', State, ?TIME_BETWEEN_CYCLE}
    end;
handle_info(_Info, State) ->
    lager:debug("unhandled message: ~p", [_Info]),
    {'noreply', State}.

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
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, _State) ->
    lager:debug("terminating: ~p", [_Reason]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec process_single(kz_json:object()) -> {'ok' | 'failed', kz_json:object()}.
process_single(JObj) ->
    API = kz_json:get_value(<<"payload">>, JObj, kz_json:new()),
    NotifyType = kz_json:get_ne_binary_value(<<"notification_type">>, JObj),
    PublishFun = map_to_publish_fun(NotifyType),

    case handle_result(call_collect(API, PublishFun)) of
        'true' -> {'ok', JObj};
        'false' -> {'failed', JObj}
    end.

-spec process_then_next_cycle(kz_json:objects()) -> 'ok'.
process_then_next_cycle(Pendings) ->
    save_result(
      lists:foldl(fun(JObj, #{ko := KO}=Map) ->
                          try send_notification(JObj, Map)
                          catch
                              _T:_E ->
                                  Map#{ko := [JObj|KO]}
                          end
                  end
                 ,new_results_map()
                 ,Pendings
                 )
     ),
    next().

-spec send_notification(kz_json:object(), map()) -> map().
send_notification(JObj, #{ok := OK}=Map) ->
    API = kz_json:get_value(<<"payload">>, JObj, kz_json:new()),
    NotifyType = kz_json:get_ne_binary_value(<<"notification_type">>, JObj),
    PublishFun = map_to_publish_fun(NotifyType),

    case handle_result(call_collect(API, PublishFun)) of
        'true' -> Map#{ok := [JObj|OK]};
        'false' -> maybe_reschedule(NotifyType, JObj, Map)
    end.

-spec call_collect(api_terms(), api_atom()) -> kz_amqp_worker:request_return().
call_collect(_API, undefined) -> 'ok';
call_collect(undefined, _) -> 'ok';
call_collect(API, PublishFun) ->
    kz_amqp_worker:call_collect(kz_json:recursive_to_proplist(API)
                               ,fun kapi_notifications:PublishFun/1
                               ,fun collecting/1
                               ,?PUBLISH_TIMEOUT
                               ).

-spec save_result(map()) -> 'ok'.
save_result(Map) ->
    delete_successful_publish(Map),
    save_reschedules_publish(Map).

-spec delete_successful_publish(map()) -> 'ok'.
delete_successful_publish(#{ok := OK}) ->
    case kz_datamgr:del_docs(?KZ_PENDING_NOTIFY_DB, OK) of
        {'ok', Js} ->
            {_Saved, _Failed} = lists:partition(fun db_bulk_result/1, Js),
            lager:debug("successfully deleted ~b jobs", [length(_Saved)]);
        {'error', _R} ->
            lager:error("failed to delete successfully published notifications: ~p", [_R])
    end.

-spec save_reschedules_publish(map()) -> 'ok'.
save_reschedules_publish(#{ko := KO}) ->
    case kz_datamgr:save_docs(?KZ_PENDING_NOTIFY_DB, KO) of
        {'ok', Js} ->
            {_Saved, _Failed} = lists:partition(fun db_bulk_result/1, Js),
            lager:debug("~b notifications was rescheduled", [length(_Saved)]);
        {'error', _R} ->
            lager:error("failed to delete rescheduled notifications: ~p", [_R])
    end.

-spec db_bulk_result(kz_json:object()) -> boolean().
db_bulk_result(JObj) ->
    case kz_json:get_value(<<"error">>, JObj) of
        'undefined' -> 'true';
        Error ->
            lager:debug("failed to save/delete ~s: ~p", [kz_doc:id(JObj), Error]),
            'false'
    end.

-spec handle_result(kz_amqp_worker:request_return()) -> boolean().
handle_result({'ok', Resp}) -> is_completed(Resp);
handle_result({'error', [Error|_]=List}) ->
    case kz_json:is_json_object(Error) of
        'true' -> is_completed(List);
        _ -> 'false'
    end;
handle_result({'error', _Reason}) -> 'false';
handle_result({'returned', _, Resp}) -> is_completed(Resp);
handle_result({'timeout', Resp}) -> is_completed(Resp).

-spec maybe_reschedule(ne_binary(), kz_json:object(), map()) -> map().
maybe_reschedule(NotifyType, JObj, #{ko := KO}=Map) ->
    J = apply_reschedule_logic(NotifyType, JObj),
    Attempts = kz_json:get_integer_value(<<"attempts">>, J, 0),
    Retries = kz_json:get_integer_value(<<"retries">>, J, ?DEFAULT_RETRY_COUNT),

    case Retries - Attempts >= 1 of
        'true' ->
            lager:debug("notification is rescheduled"),
            Map#{ko := [kz_json:set_value(<<"attempts">>, Attempts + 1, J)|KO]};
        'false' ->
            lager:debug("max retires reached"),
            Map#{ko := [kz_json:set_value(<<"max_retried">>, 'true', J)|KO]} %% attempts ++ 1
    end.

-spec apply_reschedule_logic(ne_binary(), kz_json:object()) -> kz_json:object().
apply_reschedule_logic(NotifyType, JObj) ->
    Attempts = kz_json:get_integer_value(<<"attempts">>, JObj, 0),

    Rules = kapps_config:get_json(?MOD_CONFIG_CAT, <<"reschedule_rules">>, ?DEFAULT_RESCHEDULE_RULES),

    NotifySpecific = kz_json:get_value(NotifyType, Rules, kz_json:new()),
    NotifyRules = kz_json:get_value(<<"rules">>, NotifySpecific, kz_json:new()),

    case apply_reschedule_rules(NotifyType, kz_json:get_values(NotifyRules), set_default_update_fields(JObj, Attempts)) of
        {'no_rules', JObj2} ->
            lager:debug("default rules applied in notification reschedule logic"),
            JObj2;
        {'ok', JObj2} ->
            lager:debug("rule '~s' applied in notification reschedule logic"
                       ,[kz_json:get_value(<<"reschedule_rule">>, JObj2)]
                       ),
            JObj2
    end.

-spec apply_reschedule_rules(ne_binary(),{kz_json:objects(), kz_json:path()}, kz_json:object()) ->
                                    {'ok', kz_json:object()} |
                                    {'no_rules', kz_json:object()}.
apply_reschedule_rules(_NotifyType, {[], _}, JObj) -> {'no_rules', JObj};
apply_reschedule_rules(NotifyType, {[Rule | Rules], [Key | Keys]}, JObj) ->
    Attempts = kz_json:get_integer_value(<<"attempts">>, JObj, 0),
    RuleAttempt = kz_json:get_integer_value(<<"attempt">>, Rule, Attempts),
    Retries = kz_json:get_integer_value(<<"retries">>, Rule, ?DEFAULT_RETRY_COUNT),

    RetryAfter = kz_json:get_integer_value(<<"retry_after_s">>, Rule, ?DEFAULT_RETRY_PERIOD),

    case RuleAttempt =:= Attempts of
        'true' ->
            RuleName = <<NotifyType/binary, ".", Key/binary>>,
            NewJObj = kz_json:set_values([{<<"retry_after">>, RetryAfter}
                                         ,{<<"reschedule_rule">>, RuleName}
                                         ,{<<"retries">>, Retries}
                                         ]
                                        ,JObj
                                        ),
            {'ok', NewJObj};
        'false' ->
            apply_reschedule_rules(NotifyType, {Rules, Keys}, JObj)
    end.

-spec set_default_update_fields(kz_json:object(), integer()) -> kz_json:object().
set_default_update_fields(JObj, Attempts) ->
    kz_json:set_values([{<<"pvt_modified">>, kz_time:now_s()}
                       ,{<<"retry_after">>, fudge_retry_after(Attempts)}
                       ]
                      ,JObj
                      ).

-spec fudge_retry_after(integer()) -> integer().
fudge_retry_after(0) -> ?DEFAULT_RETRY_PERIOD;
fudge_retry_after(Attempts) -> Attempts * ?DEFAULT_RETRY_PERIOD.

-spec map_to_publish_fun(api_binary()) -> api_atom().
map_to_publish_fun(undefined) -> undefined;
map_to_publish_fun(Type) ->
    kz_term:to_atom(<<"publish_", Type/binary>>, 'true').

-spec new_results_map() -> map().
new_results_map() ->
    #{ok => []
     ,ko => []
     }.

-spec collecting(kz_json:objects()) -> boolean().
collecting([JObj|_]) ->
    case kapi_notifications:notify_update_v(JObj)
        andalso kz_json:get_value(<<"Status">>, JObj)
    of
        <<"completed">> -> 'true';
        <<"failed">> -> 'true';
        _ -> 'false'
    end.

-spec is_completed(kz_json:object() | kz_json:objects()) -> boolean().
is_completed([]) -> 'false';
is_completed([JObj|_]) ->
    case kapi_notifications:notify_update_v(JObj)
        andalso kz_json:get_value(<<"Status">>, JObj)
    of
        <<"completed">> -> 'true';
        %% FIXME: Is pending enough to consider publish was successful? at least teletype received the notification!
        %% <<"pending">> -> 'true';
        _ -> 'false'
    end;
is_completed(JObj) -> is_completed([JObj]).
