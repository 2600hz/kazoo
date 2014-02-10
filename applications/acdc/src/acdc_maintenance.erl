%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2014, 2600Hz
%%% @doc
%%% Helpers for cli commands
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(acdc_maintenance).

-export([current_calls/1, current_calls/2
         ,current_statuses/1
         ,current_queues/1
         ,current_agents/1
         ,logout_agent/2
         ,agent_presence_id/2
         ,migrate_to_acdc_db/0, migrate/0
         ,refresh/0, refresh_account/1
         ,flush_call_stat/1
        ]).

-include("acdc.hrl").

logout_agent(AccountId, AgentId) ->
    io:format("Sending notice to log out agent ~s (~s)~n", [AgentId, AccountId]),
    Update = props:filter_undefined(
               [{<<"Account-ID">>, AccountId}
                ,{<<"Agent-ID">>, AgentId}
                | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
               ]),
    whapps_util:amqp_pool_send(Update, fun wapi_acdc_agent:publish_logout/1).

-define(KEYS, [<<"Waiting">>, <<"Handled">>, <<"Processed">>, <<"Abandoned">>]).

-spec current_statuses(text()) -> 'ok'.
current_statuses(AccountId) ->
    {'ok', Agents} = acdc_agent_util:most_recent_statuses(AccountId),
    case wh_json:get_values(Agents) of
        {[], []} ->
            io:format("No agent statuses found for ~s~n", [AccountId]);
        {As, _} ->
            io:format("Agent Statuses for ~s~n", [AccountId]),
            io:format("~4s | ~35s | ~12s | ~20s |~n", [<<>>, <<"Agent-ID">>, <<"Status">>, <<"Timestamp">>]),
            log_current_statuses(As, 1)
    end,
    'ok'.

log_current_statuses([], _) -> 'ok';
log_current_statuses([A|As], N) ->
    log_current_status(A, N),
    log_current_statuses(As, N+1).

log_current_status(A, N) ->
    io:format("~4b | ~35s | ~12s | ~20s |~n", [N, wh_json:get_value(<<"agent_id">>, A)
                                               ,wh_json:get_value(<<"status">>, A)
                                               ,wh_util:pretty_print_datetime(wh_json:get_integer_value(<<"timestamp">>, A))
                                              ]).

current_queues(AccountId) ->
    case acdc_agents_sup:find_acct_supervisors(AccountId) of
        [] -> io:format("no agent processes found for ~s~n", [AccountId]);
        Agents ->
            io:format("Agent Queue Assignments for Account ~s~n", [AccountId]),
            log_current_queues(Agents)
    end.

log_current_queues(Agents) ->
    io:format(" ~35s | ~s~n", [<<"Agent ID">>, <<"Queue IDs">>]),
    [log_current_queue(Agent) || Agent <- Agents],
    'ok'.
log_current_queue(AgentSup) ->
    AgentL = acdc_agent_sup:listener(AgentSup),
    io:format(" ~35s | ~s~n", [acdc_agent_listener:id(AgentL)
                               ,wh_util:join_binary(acdc_agent_listener:queues(AgentL))
                              ]).

current_agents(AccountId) ->
    case acdc_queues_sup:find_acct_supervisors(AccountId) of
        [] -> io:format("no queue processes found for ~s~n", [AccountId]);
        Queues ->
            io:format("Queue Agent Assignments for Account ~s~n", [AccountId]),
            log_current_agents(Queues)
    end.
log_current_agents(Queues) ->
    io:format(" ~35s | ~s~n", [<<"Queue ID">>, <<"Agent IDs">>]),
    [log_current_agent(Queue) || Queue <- Queues],
    'ok'.
log_current_agent(QueueSup) ->
    QueueM = acdc_queue_sup:manager(QueueSup),
    {_AccountId, QueueId} = acdc_queue_manager:config(QueueM),
    io:format(" ~35s | ~s~n", [QueueId
                               ,wh_util:join_binary(acdc_queue_manager:current_agents(QueueM))
                              ]).

current_calls(AccountId) ->
    Req = [{<<"Account-ID">>, AccountId}
           | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    get_and_show(AccountId, <<"all">>, Req).

current_calls(AccountId, QueueId) when is_binary(QueueId) ->
    Req = [{<<"Account-ID">>, AccountId}
           ,{<<"Queue-ID">>, QueueId}
           | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    get_and_show(AccountId, QueueId, Req);
current_calls(AccountId, Props) ->
    Req = [{<<"Account-ID">>, AccountId}
           | Props ++ wh_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    get_and_show(AccountId, <<"custom">>, Req).

get_and_show(AccountId, QueueId, Req) ->
    put('callid', <<"acdc_maint.", AccountId/binary, ".", QueueId/binary>>),
    case whapps_util:amqp_pool_collect(Req
                                       ,fun wapi_acdc_stats:publish_current_calls_req/1
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
    show_call_stat_cat(Ks, Resp),
    show_call_stats(Resps, Ks).

show_call_stat_cat([], _) -> 'ok';
show_call_stat_cat([K|Ks], Resp) ->
    case wh_json:get_value(K, Resp) of
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
         || {K, V} <- wh_json:to_proplist(wh_doc:public_fields(S))
        ],
    show_stats(Ss).

-spec refresh() -> 'ok'.
refresh() ->
    case couch_mgr:get_all_results(?KZ_ACDC_DB, <<"acdc/accounts_listing">>) of
        {'ok', []} ->
            lager:debug("no accounts configured for acdc");
        {'ok', Accounts} ->
            [refresh_account(wh_json:get_value(<<"key">>, Acct)) || Acct <- Accounts],
            lager:debug("refreshed accounts");
        {'error', 'not_found'} ->
            lager:debug("acdc db not found"),
            lager:debug("consider running acdc_maintenance:migrate() to enable acdc for already-configured accounts");
        {'error', _E} ->
            lager:debug("failed to query acdc db: ~p", [_E])
    end.

-spec refresh_account(ne_binary()) -> 'ok'.
refresh_account(Acct) ->
    MoDB = acdc_stats_util:db_name(Acct),
    refresh_account(MoDB, couch_mgr:db_create(MoDB)),
    lager:debug("refreshed: ~s", [MoDB]).

refresh_account(MoDB, 'true') ->
    lager:debug("created ~s", [MoDB]),
    couch_mgr:revise_views_from_folder(MoDB, 'acdc');
refresh_account(MoDB, 'false') ->
    lager:debug("exists ~s", [MoDB]),
    couch_mgr:revise_views_from_folder(MoDB, 'acdc').

-spec migrate() -> 'ok'.
migrate() ->
    migrate_to_acdc_db().
migrate_to_acdc_db() ->
    [migrate_to_acdc_db(Acct) || Acct <- whapps_util:get_all_accounts('raw')],
    'ok'.

-spec migrate_to_acdc_db(ne_binary()) -> 'ok'.
-spec migrate_to_acdc_db(ne_binary(), non_neg_integer()) -> 'ok'.
migrate_to_acdc_db(AccountId) ->
    migrate_to_acdc_db(AccountId, 3).

migrate_to_acdc_db(AccountId, 0) ->
    io:format("retries exceeded, skipping account ~s~n", [AccountId]);
migrate_to_acdc_db(AccountId, Retries) ->
    case couch_mgr:get_results(?KZ_ACDC_DB
                               ,<<"acdc/accounts_listing">>
                               ,[{'key', AccountId}]
                              )
    of
        {'ok', []} ->
            maybe_migrate(AccountId);
        {'ok', [_|_]} ->
            io:format("account ~s already in acdc db~n", [AccountId]);
        {'error', 'not_found'} ->
            io:format("acdc db not found (or view is missing, restoring then trying again~n", []),
            acdc_init:init_db(),
            timer:sleep(250),
            migrate_to_acdc_db(AccountId, Retries-1);
        {'error', _E} ->
            io:format("failed to check acdc db for account: ~p~n", [_E]),
            timer:sleep(250),
            migrate_to_acdc_db(AccountId, Retries-1)
    end.

-spec maybe_migrate(ne_binary()) -> 'ok'.
maybe_migrate(AccountId) ->
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    case couch_mgr:get_results(AccountDb, <<"queues/crossbar_listing">>, [{'limit', 1}]) of
        {'ok', []} ->
            io:format("account ~s has no queues, skipping~n", [AccountId]);
        {'ok', [_|_]} ->
            io:format("account ~s has queues, adding to acdc db~n", [AccountId]),
            Doc = wh_doc:update_pvt_parameters(wh_json:new()
                                               ,?KZ_ACDC_DB
                                               ,[{'account_id', AccountId}
                                                 ,{'type', <<"acdc_activation">>}
                                                ]),
            couch_mgr:ensure_saved(?KZ_ACDC_DB, Doc),
            io:format("saved account ~s to db~n", [AccountId]);
        {'error', _E} ->
            io:format("failed to query queue listing for account ~s: ~p~n", [AccountId, _E])
    end.

-spec agent_presence_id(ne_binary(), ne_binary()) -> 'ok'.
agent_presence_id(AccountId, AgentId) ->
    case acdc_agents_sup:find_agent_supervisor(AccountId, AgentId) of
        'undefined' ->
            io:format("agent ~s(~s) not logged in or not found~n", [AgentId, AccountId]);
        SupPid ->
            PresenceId = acdc_agent_listener:presence_id(acdc_agent_sup:listener(SupPid)),
            io:format("agent ~s(~s) is using presence ID ~s~n", [AgentId, AccountId, PresenceId])
    end.

flush_call_stat(CallId) ->
    case acdc_stats:find_call(CallId) of
        'undefined' -> io:format("nothing found for call ~s~n", [CallId]);
        Call ->
            acdc_stats:call_abandoned(wh_json:get_value(<<"Account-ID">>, Call)
                                      ,wh_json:get_value(<<"Queue-ID">>, Call)
                                      ,CallId
                                      ,<<"INTERNAL_ERROR">>
                                     ),
            io:format("setting call to 'abandoned'~n", [])
    end.

