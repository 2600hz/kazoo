%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2019, 2600Hz
%%% @doc Helpers for cli commands
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(acdc_maintenance).

-export([current_calls/1, current_calls/2
        ,current_statuses/1
        ,current_queues/1
        ,current_agents/1
        ,logout_agents/1, logout_agent/2
        ,agent_presence_id/2
        ,migrate_to_acdc_db/0, migrate/0

        ,refresh/0, refresh_account/1
        ,register_views/0

        ,flush_call_stat/1
        ,queues_summary/0, queues_summary/1, queue_summary/2
        ,queues_detail/0, queues_detail/1, queue_detail/2
        ,queues_restart/1, queue_restart/2

        ,agents_summary/0, agents_summary/1, agent_summary/2
        ,agents_detail/0, agents_detail/1, agent_detail/2
        ,agent_login/2
        ,agent_logout/2
        ,agent_pause/2, agent_pause/3
        ,agent_resume/2
        ,agent_queue_login/3
        ,agent_queue_logout/3
        ]).

-include("acdc.hrl").

-spec logout_agents(kz_term:ne_binary()) -> 'ok'.
logout_agents(AccountId) ->
    ?PRINT("Sending notices to logout agents for ~s", [AccountId]),
    AccountDb = kz_util:format_account_id(AccountId, 'encoded'),
    {'ok', AgentView} = kz_datamgr:get_all_results(AccountDb, <<"agents/crossbar_listing">>),
    _ = [logout_agent(AccountId, kz_doc:id(Agent)) || Agent <- AgentView],
    'ok'.

-spec logout_agent(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
logout_agent(AccountId, AgentId) ->
    io:format("Sending notice to log out agent ~s (~s)~n", [AgentId, AccountId]),
    Update = props:filter_undefined(
               [{<<"Account-ID">>, AccountId}
               ,{<<"Agent-ID">>, AgentId}
                | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
               ]),
    kz_amqp_worker:cast(Update, fun kapi_acdc_agent:publish_logout/1).

-define(KEYS, [<<"Waiting">>, <<"Handled">>, <<"Processed">>, <<"Abandoned">>]).

-spec current_statuses(kz_term:text()) -> 'ok'.
current_statuses(AccountId) ->
    {'ok', Agents} = acdc_agent_util:most_recent_statuses(AccountId),
    case kz_json:values(Agents) of
        [] -> io:format("No agent statuses found for ~s~n", [AccountId]);
        As ->
            io:format("Agent Statuses for ~s~n", [AccountId]),
            io:format("~4s | ~35s | ~12s | ~20s |~n"
                     ,[<<>>, <<"Agent-ID">>, <<"Status">>, <<"Timestamp">>]),
            log_current_statuses(As, 1)
    end.

log_current_statuses([], _) -> 'ok';
log_current_statuses([A|As], N) ->
    log_current_status(A, N),
    log_current_statuses(As, N+1).

log_current_status(A, N) ->
    TS = kz_json:get_integer_value(<<"timestamp">>, A),
    io:format("~4b | ~35s | ~12s | ~20s |~n", [N, kz_json:get_value(<<"agent_id">>, A)
                                              ,kz_json:get_value(<<"status">>, A)
                                              ,kz_time:pretty_print_datetime(TS)
                                              ]).

-spec current_queues(kz_term:ne_binary()) -> 'ok'.
current_queues(AccountId) ->
    case acdc_agents_sup:find_acct_supervisors(AccountId) of
        [] -> io:format("no agent processes found for ~s~n", [AccountId]);
        Agents ->
            io:format("Agent Queue Assignments for Account ~s~n", [AccountId]),
            log_current_queues(Agents)
    end.

log_current_queues(Agents) ->
    io:format(" ~35s | ~s~n", [<<"Agent ID">>, <<"Queue IDs">>]),
    lists:foreach(fun log_current_queue/1, Agents).
log_current_queue(AgentSup) ->
    AgentL = acdc_agent_sup:listener(AgentSup),
    io:format(" ~35s | ~s~n", [acdc_agent_listener:id(AgentL)
                              ,kz_binary:join(acdc_agent_listener:queues(AgentL))
                              ]).

-spec current_agents(kz_term:ne_binary()) -> 'ok'.
current_agents(AccountId) ->
    case acdc_queues_sup:find_acct_supervisors(AccountId) of
        [] -> io:format("no queue processes found for ~s~n", [AccountId]);
        Queues ->
            io:format("Queue Agent Assignments for Account ~s~n", [AccountId]),
            log_current_agents(Queues)
    end.
log_current_agents(Queues) ->
    io:format(" ~35s | ~s~n", [<<"Queue ID">>, <<"Agent IDs">>]),
    lists:foreach(fun log_current_agent/1, Queues).
log_current_agent(QueueSup) ->
    QueueM = acdc_queue_sup:manager(QueueSup),
    {_AccountId, QueueId} = acdc_queue_manager:config(QueueM),
    io:format(" ~35s | ~s~n", [QueueId
                              ,kz_binary:join(acdc_queue_manager:current_agents(QueueM))
                              ]).

-spec current_calls(kz_term:ne_binary()) -> 'ok'.
current_calls(AccountId) ->
    Req = [{<<"Account-ID">>, AccountId}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    get_and_show(AccountId, <<"all">>, Req).

-spec current_calls(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
current_calls(AccountId, QueueId) when is_binary(QueueId) ->
    Req = [{<<"Account-ID">>, AccountId}
          ,{<<"Queue-ID">>, QueueId}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    get_and_show(AccountId, QueueId, Req);
current_calls(AccountId, Props) ->
    Req = [{<<"Account-ID">>, AccountId}
           | Props ++ kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    get_and_show(AccountId, <<"custom">>, Req).

get_and_show(AccountId, QueueId, Req) ->
    kz_log:put_callid(<<"acdc_maint.", AccountId/binary, ".", QueueId/binary>>),
    case kz_amqp_worker:call_collect(Req
                                    ,fun kapi_acdc_stats:publish_current_calls_req/1
                                    ,'acdc'
                                    )
    of
        {_, []} ->
            io:format("no call stats returned for account ~s (queue ~s)~n", [AccountId, QueueId]);
        {'ok', JObjs} ->
            io:format("call stats for account ~s (queue ~s)~n", [AccountId, QueueId]),
            show_call_stats(JObjs, ?KEYS);
        {'timeout', JObjs} ->
            io:format("call stats for account ~s (queue ~s)~n", [AccountId, QueueId]),
            show_call_stats(JObjs, ?KEYS);
        {'error', _E} ->
            io:format("failed to lookup call stats for account ~s (queue ~s): ~p~n", [AccountId, QueueId, _E])
    end.

show_call_stats([], _) -> 'ok';
show_call_stats([Resp|Resps], Ks) ->
    kz_log:put_callid(?MODULE),
    show_call_stat_cat(Ks, Resp),
    show_call_stats(Resps, Ks).

show_call_stat_cat([], _) -> 'ok';
show_call_stat_cat([K|Ks], Resp) ->
    case kz_json:get_value(K, Resp) of
        'undefined' -> show_call_stat_cat(Ks, Resp);
        V ->
            io:format("call stats in ~s~n", [K]),
            show_stats(V),
            show_call_stat_cat(Ks, Resp),
            io:format("~n~n", [])
    end.

show_stats([]) -> 'ok';
show_stats([S|Ss]) ->
    _ = [io:format("~s: ~p~n", [K, V])
         || {K, V} <- kz_json:to_proplist(kz_doc:public_fields(S))
        ],
    show_stats(Ss).

-spec refresh() -> 'ok'.
refresh() ->
    case kz_datamgr:get_all_results(?KZ_ACDC_DB, <<"acdc/accounts_listing">>) of
        {'ok', []} ->
            lager:debug("no accounts configured for acdc");
        {'ok', Accounts} ->
            _ = [refresh_account(kz_json:get_value(<<"key">>, Acct)) || Acct <- Accounts],
            lager:debug("refreshed accounts");
        {'error', 'not_found'} ->
            lager:debug("acdc db not found"),
            lager:debug("consider running ~s:migrate() to enable acdc for already-configured accounts", [?MODULE]);
        {'error', _E} ->
            lager:debug("failed to query acdc db: ~p", [_E])
    end.

-spec refresh_account(kz_term:ne_binary()) -> 'ok'.
refresh_account(Account) ->
    MODB = acdc_stats_util:db_name(Account),
    refresh_account(MODB, kazoo_modb:maybe_create(MODB)),
    lager:debug("refreshed: ~s", [MODB]).

refresh_account(MODB, 'true') ->
    lager:debug("created ~s", [MODB]),
    _ = kapps_maintenance:refresh(MODB),
    'ok';
refresh_account(MODB, 'false') ->
    case kz_datamgr:db_exists(MODB) of
        'true' ->
            lager:debug("exists ~s", [MODB]),
            _ = kapps_maintenance:refresh(MODB),
            'ok';
        'false' ->
            lager:debug("modb ~s was not created", [MODB])
    end.

-spec register_views() -> 'ok'.
register_views() ->
    kz_datamgr:register_views_from_folder('acdc').

-spec migrate() -> 'ok'.
migrate() ->
    migrate_to_acdc_db().

-spec migrate_to_acdc_db() -> 'ok'.
migrate_to_acdc_db() ->
    {'ok', Accounts} = kz_datamgr:all_docs(?KZ_ACDC_DB),
    _ = [maybe_remove_acdc_account(kz_doc:id(Account)) || Account <- Accounts],
    io:format("removed any missing accounts from ~s~n", [?KZ_ACDC_DB]),
    lists:foreach(fun migrate_to_acdc_db/1, kapps_util:get_all_accounts('raw')),
    io:format("migration complete~n").

-spec maybe_remove_acdc_account(kz_term:ne_binary()) -> 'ok'.
maybe_remove_acdc_account(<<"_design/", _/binary>>) -> 'ok';
maybe_remove_acdc_account(AccountId) ->
    case kzd_accounts:fetch(AccountId) of
        {'ok', _} -> 'ok';
        {'error', 'not_found'} ->
            {'ok', JObj} = kz_datamgr:open_cache_doc(?KZ_ACDC_DB, AccountId),
            {'ok', _Del} = kz_datamgr:del_doc(?KZ_ACDC_DB, JObj),
            io:format("account ~p not found in ~s, removing from ~s~n", [AccountId, ?KZ_ACCOUNTS_DB, ?KZ_ACDC_DB])
    end.

-spec migrate_to_acdc_db(kz_term:ne_binary()) -> 'ok'.
migrate_to_acdc_db(AccountId) ->
    migrate_to_acdc_db(AccountId, 3).

-spec migrate_to_acdc_db(kz_term:ne_binary(), non_neg_integer()) -> 'ok'.
migrate_to_acdc_db(AccountId, 0) ->
    io:format("retries exceeded, skipping account ~s~n", [AccountId]);
migrate_to_acdc_db(AccountId, Retries) ->
    case kz_datamgr:get_results(?KZ_ACDC_DB
                               ,<<"acdc/accounts_listing">>
                               ,[{'key', AccountId}]
                               )
    of
        {'ok', []} ->
            maybe_migrate(AccountId);
        {'ok', _} -> 'ok';
        {'error', 'not_found'} ->
            io:format("acdc db not found (or view is missing, restoring then trying again)~n", []),
            acdc_init:init_db(),
            timer:sleep(250),
            migrate_to_acdc_db(AccountId, Retries-1);
        {'error', _E} ->
            io:format("failed to check acdc db for account ~s: ~p~n", [AccountId, _E]),
            timer:sleep(250),
            migrate_to_acdc_db(AccountId, Retries-1)
    end.

-spec maybe_migrate(kz_term:ne_binary()) -> 'ok'.
maybe_migrate(AccountId) ->
    AccountDb = kz_util:format_account_id(AccountId, 'encoded'),
    case kz_datamgr:get_results(AccountDb, <<"queues/crossbar_listing">>, [{'limit', 1}]) of
        {'ok', []} -> 'ok';
        {'ok', [_|_]} ->
            io:format("account ~s has queues, adding to acdc db~n", [AccountId]),
            Doc = kz_doc:update_pvt_parameters(kz_json:from_list([{<<"_id">>, AccountId}])
                                              ,?KZ_ACDC_DB
                                              ,[{'account_id', AccountId}
                                               ,{'type', <<"acdc_activation">>}
                                               ]),
            _ = kz_datamgr:ensure_saved(?KZ_ACDC_DB, Doc),
            io:format("saved account ~s to db~n", [AccountId]);
        {'error', _E} ->
            io:format("failed to query queue listing for account ~s: ~p~n", [AccountId, _E])
    end.

-spec agent_presence_id(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
agent_presence_id(AccountId, AgentId) ->
    case acdc_agents_sup:find_agent_supervisor(AccountId, AgentId) of
        'undefined' ->
            io:format("agent ~s(~s) not logged in or not found~n", [AgentId, AccountId]);
        SupPid ->
            PresenceId = acdc_agent_listener:presence_id(acdc_agent_sup:listener(SupPid)),
            io:format("agent ~s(~s) is using presence ID ~s~n", [AgentId, AccountId, PresenceId])
    end.

-spec flush_call_stat(kz_term:ne_binary()) -> 'ok'.
flush_call_stat(CallId) ->
    case acdc_stats:find_call(CallId) of
        'undefined' -> io:format("nothing found for call ~s~n", [CallId]);
        Call ->
            acdc_stats:call_abandoned(kz_json:get_value(<<"Account-ID">>, Call)
                                     ,kz_json:get_value(<<"Queue-ID">>, Call)
                                     ,CallId
                                     ,'INTERNAL_ERROR'
                                     ),
            io:format("setting call to 'abandoned'~n", [])
    end.

-spec queues_summary() -> 'ok'.
queues_summary() ->
    kz_log:put_callid(?MODULE),
    show_queues_summary(acdc_queues_sup:queues_running()).

-spec queues_summary(kz_term:ne_binary()) -> 'ok'.
queues_summary(AcctId) ->
    kz_log:put_callid(?MODULE),
    show_queues_summary(
      [Q || {_, {QAcctId, _}} = Q <- acdc_queues_sup:queues_running(),
            QAcctId =:= AcctId
      ]).

-spec queue_summary(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
queue_summary(AcctId, QueueId) ->
    kz_log:put_callid(?MODULE),
    show_queues_summary(
      [Q || {_, {QAcctId, QQueueId}} = Q <- acdc_queues_sup:queues_running(),
            QAcctId =:= AcctId,
            QQueueId =:= QueueId
      ]).

-spec show_queues_summary([{pid(), {kz_term:ne_binary(), kz_term:ne_binary()}}]) -> 'ok'.
show_queues_summary([]) -> 'ok';
show_queues_summary([{P, {AcctId, QueueId}}|Qs]) ->
    ?PRINT("  Supervisor: ~p Acct: ~s Queue: ~s~n", [P, AcctId, QueueId]),
    show_queues_summary(Qs).

-spec queues_detail() -> 'ok'.
queues_detail() ->
    acdc_queues_sup:status().

-spec queues_detail(kz_term:ne_binary()) -> 'ok'.
queues_detail(AcctId) ->
    kz_log:put_callid(?MODULE),
    Supervisors = acdc_queues_sup:find_acct_supervisors(AcctId),
    lists:foreach(fun acdc_queue_sup:status/1, Supervisors).

-spec queue_detail(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
queue_detail(AcctId, QueueId) ->
    case acdc_queues_sup:find_queue_supervisor(AcctId, QueueId) of
        'undefined' -> lager:info("no queue ~s in account ~s", [QueueId, AcctId]);
        Pid -> acdc_queue_sup:status(Pid)
    end.

-spec queues_restart(kz_term:ne_binary()) -> 'ok'.
queues_restart(AcctId) ->
    kz_log:put_callid(?MODULE),
    case acdc_queues_sup:find_acct_supervisors(AcctId) of
        [] -> lager:info("there are no running queues in ~s", [AcctId]);
        Pids ->
            F = fun (Pid) -> maybe_stop_then_start_queue(AcctId, Pid) end,
            lists:foreach(F, Pids)
    end.

-spec queue_restart(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
queue_restart(AcctId, QueueId) ->
    kz_log:put_callid(?MODULE),
    case acdc_queues_sup:find_queue_supervisor(AcctId, QueueId) of
        'undefined' ->
            lager:info("queue ~s in account ~s not running", [QueueId, AcctId]);
        Pid ->
            maybe_stop_then_start_queue(AcctId, QueueId, Pid)
    end.

-spec maybe_stop_then_start_queue(kz_term:ne_binary(), pid()) -> 'ok'.
maybe_stop_then_start_queue(AcctId, Pid) ->
    {AcctId, QueueId} = acdc_queue_manager:config(acdc_queue_sup:manager(Pid)),
    maybe_stop_then_start_queue(AcctId, QueueId, Pid).

-spec maybe_stop_then_start_queue(kz_term:ne_binary(), kz_term:ne_binary(), pid()) -> 'ok'.
maybe_stop_then_start_queue(AcctId, QueueId, Pid) ->
    case supervisor:terminate_child('acdc_queues_sup', Pid) of
        'ok' ->
            lager:info("stopped queue supervisor ~p", [Pid]),
            maybe_start_queue(AcctId, QueueId);
        {'error', 'not_found'} ->
            lager:info("queue supervisor ~p not found", [Pid]);
        {'error', _E} ->
            lager:info("failed to terminate queue supervisor ~p: ~p", [_E])
    end.

maybe_start_queue(AcctId, QueueId) ->
    case acdc_queues_sup:new(AcctId, QueueId) of
        {'ok', 'undefined'} ->
            lager:info("tried to start queue but it asked to be ignored");
        {'ok', Pid} ->
            lager:info("started queue back up in ~p", [Pid]);
        {'error', 'already_present'} ->
            lager:info("queue is already present (but not running)");
        {'error', {'already_running', Pid}} ->
            lager:info("queue is already running in ~p", [Pid]);
        {'error', _E} ->
            lager:info("failed to start queue: ~p", [_E])
    end.

-spec agents_summary() -> 'ok'.
agents_summary() ->
    kz_log:put_callid(?MODULE),
    show_agents_summary(acdc_agents_sup:agents_running()).

-spec agents_summary(kz_term:ne_binary()) -> 'ok'.
agents_summary(AcctId) ->
    kz_log:put_callid(?MODULE),
    show_agents_summary(
      [A || {_, {AAcctId, _, _}} = A <- acdc_agents_sup:agents_running(),
            AAcctId =:= AcctId
      ]).

-spec agent_summary(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
agent_summary(AcctId, AgentId) ->
    kz_log:put_callid(?MODULE),
    show_agents_summary(
      [Q || {_, {AAcctId, AAgentId, _}} = Q <- acdc_agents_sup:agents_running(),
            AAcctId =:= AcctId,
            AAgentId =:= AgentId
      ]).

-spec show_agents_summary([{pid(), acdc_agent_listener:config()}]) -> 'ok'.
show_agents_summary([]) -> 'ok';
show_agents_summary([{P, {AcctId, QueueId, _AMQPQueue}}|Qs]) ->
    lager:info("  Supervisor: ~p Acct: ~s Agent: ~s", [P, AcctId, QueueId]),
    show_queues_summary(Qs).

-spec agents_detail() -> 'ok'.
agents_detail() ->
    kz_log:put_callid(?MODULE),
    acdc_agents_sup:status().

-spec agents_detail(kz_term:ne_binary()) -> 'ok'.
agents_detail(AcctId) ->
    kz_log:put_callid(?MODULE),
    Supervisors = acdc_agents_sup:find_acct_supervisors(AcctId),
    lists:foreach(fun acdc_agent_sup:status/1, Supervisors).

-spec agent_detail(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
agent_detail(AcctId, AgentId) ->
    kz_log:put_callid(?MODULE),
    case acdc_agents_sup:find_agent_supervisor(AcctId, AgentId) of
        'undefined' -> lager:info("no agent ~s in account ~s", [AgentId, AcctId]);
        Pid -> acdc_agent_sup:status(Pid)
    end.

-spec agent_login(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
agent_login(AcctId, AgentId) ->
    kz_log:put_callid(?MODULE),
    Update = props:filter_undefined(
               [{<<"Account-ID">>, AcctId}
               ,{<<"Agent-ID">>, AgentId}
                |  kz_api:default_headers(?APP_NAME, ?APP_VERSION)
               ]),
    _ = kz_amqp_worker:cast(Update, fun kapi_acdc_agent:publish_login/1),
    lager:info("published login update for agent").

-spec agent_logout(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
agent_logout(AcctId, AgentId) ->
    kz_log:put_callid(?MODULE),
    Update = props:filter_undefined(
               [{<<"Account-ID">>, AcctId}
               ,{<<"Agent-ID">>, AgentId}
                |  kz_api:default_headers(?APP_NAME, ?APP_VERSION)
               ]),
    _ = kz_amqp_worker:cast(Update, fun kapi_acdc_agent:publish_logout/1),
    lager:info("published logout update for agent").

-spec agent_pause(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
agent_pause(AcctId, AgentId) ->
    Timeout = kapps_config:get_integer(?CONFIG_CAT, <<"default_agent_pause_timeout">>, 600),
    agent_pause(AcctId, AgentId, Timeout).

-spec agent_pause(kz_term:ne_binary(), kz_term:ne_binary(), pos_integer()) -> 'ok'.
agent_pause(AcctId, AgentId, Timeout) ->
    kz_log:put_callid(?MODULE),
    Update = props:filter_undefined(
               [{<<"Account-ID">>, AcctId}
               ,{<<"Agent-ID">>, AgentId}
               ,{<<"Timeout">>, kz_term:to_integer(Timeout)}
                | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
               ]),
    _ = kz_amqp_worker:cast(Update, fun kapi_acdc_agent:publish_pause/1),
    lager:info("published pause for agent").

-spec agent_resume(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
agent_resume(AcctId, AgentId) ->
    kz_log:put_callid(?MODULE),
    Update = props:filter_undefined(
               [{<<"Account-ID">>, AcctId}
               ,{<<"Agent-ID">>, AgentId}
                |  kz_api:default_headers(?APP_NAME, ?APP_VERSION)
               ]),
    _ = kz_amqp_worker:cast(Update, fun kapi_acdc_agent:publish_resume/1),
    lager:info("published resume for agent").

-spec agent_queue_login(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
agent_queue_login(AcctId, AgentId, QueueId) ->
    kz_log:put_callid(?MODULE),
    Update = props:filter_undefined(
               [{<<"Account-ID">>, AcctId}
               ,{<<"Agent-ID">>, AgentId}
               ,{<<"Queue-ID">>, QueueId}
                |  kz_api:default_headers(?APP_NAME, ?APP_VERSION)
               ]),
    _ = kz_amqp_worker:cast(Update, fun kapi_acdc_agent:publish_login_queue/1),
    lager:info("published login update for agent").

-spec agent_queue_logout(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
agent_queue_logout(AcctId, AgentId, QueueId) ->
    kz_log:put_callid(?MODULE),
    Update = props:filter_undefined(
               [{<<"Account-ID">>, AcctId}
               ,{<<"Agent-ID">>, AgentId}
               ,{<<"Queue-ID">>, QueueId}
                |  kz_api:default_headers(?APP_NAME, ?APP_VERSION)
               ]),
    _ = kz_amqp_worker:cast(Update, fun kapi_acdc_agent:publish_logout_queue/1),
    lager:info("published logout update for agent").
