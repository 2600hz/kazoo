%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2019, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(acdc_agent_util).

-export([update_status/3, update_status/4

        ,most_recent_status/2
        ,most_recent_statuses/1, most_recent_statuses/2, most_recent_statuses/3

        ,most_recent_ets_status/2
        ,most_recent_db_status/2

        ,most_recent_ets_statuses/1, most_recent_ets_statuses/2, most_recent_ets_statuses/3
        ,most_recent_db_statuses/1, most_recent_db_statuses/2, most_recent_db_statuses/3

        ,changed/2, find_most_recent_fold/3

        ,status_should_auto_start/1
        ]).

-include("acdc.hrl").

-spec update_status(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
update_status(AccountId, AgentId, Status) ->
    update_status(AccountId, AgentId, Status, []).

-spec update_status(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
update_status(?NE_BINARY = AccountId, AgentId, Status, Options) ->
    API = [{<<"Account-ID">>, AccountId}
          ,{<<"Agent-ID">>, AgentId}
          ,{<<"Status">>, Status}
          ,{<<"Timestamp">>, kz_time:now_s()}
           | Options ++ kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    kz_amqp_worker:cast(API, fun kapi_acdc_stats:publish_status_update/1).

-spec most_recent_status(kz_term:ne_binary(), kz_term:ne_binary()) ->
                                {'ok', kz_term:ne_binary()}.
most_recent_status(AccountId, AgentId) ->
    case most_recent_ets_status(AccountId, AgentId) of
        {'ok', _}=OK -> OK;
        {'error', _ErrJObj} ->
            case kz_json:is_valid_json_object(_ErrJObj) of
                true  -> lager:debug("failed to get ETS stats: ~p", [kz_json:get_value(<<"Error-Reason">>, _ErrJObj)]);
                false -> lager:debug("failed to get ETS stats: ~p", [_ErrJObj])
            end,
            most_recent_db_status(AccountId, AgentId)
    end.

-spec most_recent_ets_status(kz_term:ne_binary(), kz_term:ne_binary()) ->
                                    {'ok', kz_term:ne_binary()} |
                                    {'error', any()}.
most_recent_ets_status(AccountId, AgentId) ->
    case most_recent_ets_statuses(AccountId, AgentId) of
        {'error', _}=E -> E;
        {'ok', Statuses} ->
            most_recent_ets_agent_status(kz_json:get_json_value(AgentId, Statuses))
    end.

-spec most_recent_ets_agent_status(kz_term:api_object()) ->
                                          {'ok', kz_term:ne_binary()} |
                                          {'error', 'not_found'}.
most_recent_ets_agent_status('undefined') -> {'error', 'not_found'};
most_recent_ets_agent_status(Stats) ->
    {_, StatusJObj} = kz_json:foldl(fun find_most_recent_fold/3, {0, kz_json:new()}, Stats),
    {'ok', kz_json:get_value(<<"status">>, StatusJObj)}.

-spec most_recent_db_status(kz_term:ne_binary(), kz_term:ne_binary()) ->
                                   {'ok', kz_term:ne_binary()}.
most_recent_db_status(AccountId, AgentId) ->
    Opts = [{'startkey', [AgentId, kz_time:now_s()]}
           ,{'endkey', [AgentId, 0]}
           ,{'limit', 1}
           ,'descending'
           ],
    case kz_datamgr:get_results(acdc_stats_util:db_name(AccountId), <<"agent_stats/status_log">>, Opts) of
        {'ok', [StatusJObj]} ->
            {'ok', kz_json:get_value(<<"value">>, StatusJObj)};
        {'ok', []} ->
            lager:debug("could not find a recent status for agent ~s, checking previous modb", [AgentId]),
            prev_month_recent_db_status(AccountId, AgentId);
        {'error', 'not_found'} ->
            acdc_maintenance:refresh_account(AccountId),
            timer:sleep(150),
            most_recent_db_status(AccountId, AgentId);
        {'error', _E} ->
            lager:debug("error querying view: ~p", [_E]),
            {'ok', <<"unknown">>}
    end.

-spec prev_month_recent_db_status(kz_term:ne_binary(), kz_term:ne_binary()) ->
                                         {'ok', kz_term:ne_binary()}.
prev_month_recent_db_status(AccountId, AgentId) ->
    Opts = [{'startkey', [AgentId, kz_time:now_s()]}
           ,{'endkey', [AgentId, 0]}
           ,{'limit', 1}
           ,'descending'
           ],
    Db = kazoo_modb_util:prev_year_month_mod(acdc_stats_util:db_name(AccountId)),
    case kz_datamgr:get_results(Db, <<"agent_stats/status_log">>, Opts) of
        {'ok', [StatusJObj]} ->
            {'ok', kz_json:get_value(<<"value">>, StatusJObj)};
        {'ok', []} ->
            {'ok', <<"unknown">>};
        {'error', 'not_found'} ->
            lager:debug("no previous modb found, returning unknown status"),
            {'ok', <<"unknown">>};
        {'error', _E} ->
            lager:debug("error querying view: ~p", [_E]),
            {'ok', <<"unknown">>}
    end.

-type statuses_return() :: {'ok', kz_json:object()}.

-spec most_recent_statuses(kz_term:ne_binary()) ->
                                  statuses_return().
most_recent_statuses(AccountId) ->
    most_recent_statuses(AccountId, 'undefined', []).

-spec most_recent_statuses(kz_term:ne_binary(), kz_term:api_binary() | kz_term:proplist()) ->
                                  statuses_return().
most_recent_statuses(AccountId, 'undefined') ->
    most_recent_statuses(AccountId, 'undefined', []);
most_recent_statuses(AccountId, ?NE_BINARY = AgentId) ->
    most_recent_statuses(AccountId, AgentId, []);
most_recent_statuses(AccountId, Options) when is_list(Options) ->
    most_recent_statuses(AccountId, props:get_value(<<"Agent-ID">>, Options), Options).

-spec most_recent_statuses(kz_term:ne_binary(), kz_term:api_binary(), kz_term:proplist()) ->
                                  statuses_return().
most_recent_statuses(AccountId, AgentId, Options) ->
    ETSStatuses = case most_recent_ets_statuses(AccountId, AgentId, Options) of
                      {'ok', Statuses} -> Statuses;
                      {'error', _} -> kz_json:new()
                  end,
    DBStatuses = case fetch_db_statuses(AccountId, AgentId) of
                     {'ok', Statuses2} -> Statuses2;
                     {'error', _} -> kz_json:new()
                 end,
    {'ok', kz_json:merge(DBStatuses, ETSStatuses)}.

fetch_db_statuses(AccountId, AgentId) ->
    case kz_cache:fetch_local(?CACHE_NAME, db_fetch_key(AccountId)) of
        {'ok', Statuses} -> {'ok', filter_agent_statuses(Statuses, AgentId)};
        {'error', 'not_found'} -> maybe_db_lookup(AccountId, AgentId)
    end.

-spec maybe_db_lookup(kz_term:ne_binary(), kz_term:ne_binary()) ->
                             statuses_return() | {'error', any()}.
maybe_db_lookup(AccountId, AgentId) ->
    case most_recent_db_statuses(AccountId) of
        {'ok', Statuses} ->
            kz_cache:store_local(?CACHE_NAME, db_fetch_key(AccountId), Statuses),
            {'ok', filter_agent_statuses(Statuses, AgentId)};
        {'error', _}=E -> E
    end.

-spec db_fetch_key(AccountId) -> {'async_most_recent_db_statuses', AccountId}.
db_fetch_key(AccountId) -> {'async_most_recent_db_statuses', AccountId}.

filter_agent_statuses(Statuses, 'undefined') -> Statuses;
filter_agent_statuses(Statuses, KeepAgentId) ->
    kz_json:filter(fun({AgentId, _}) -> AgentId =:= KeepAgentId end, Statuses).

-spec most_recent_ets_statuses(kz_term:ne_binary()) ->
                                      statuses_return() |
                                      {'error', any()}.
most_recent_ets_statuses(AccountId) ->
    most_recent_ets_statuses(AccountId, 'undefined', []).

-spec most_recent_ets_statuses(kz_term:ne_binary(), kz_term:api_binary()) ->
                                      statuses_return() |
                                      {'error', any()}.
most_recent_ets_statuses(AccountId, ?NE_BINARY = AgentId) ->
    most_recent_ets_statuses(AccountId, AgentId, []);
most_recent_ets_statuses(AccountId, Options) when is_list(Options) ->
    most_recent_ets_statuses(AccountId, 'undefined', Options).

-spec most_recent_ets_statuses(kz_term:ne_binary(), kz_term:api_binary(), kz_term:proplist()) ->
                                      statuses_return() |
                                      {'error', any()}.
most_recent_ets_statuses(AccountId, AgentId, Options) ->
    API = props:filter_undefined(
            [{<<"Account-ID">>, AccountId}
            ,{<<"Agent-ID">>, AgentId}
             | kz_api:default_headers(?APP_NAME, ?APP_VERSION) ++ Options
            ]),
    case kz_amqp_worker:call_collect(API
                                    ,fun kapi_acdc_stats:publish_status_req/1
                                    ,'acdc'
                                    ,3 * ?MILLISECONDS_IN_SECOND
                                    )
    of
        {'error', _}=E -> E;
        {Result, Resps} when Result =:= 'ok'
                             orelse Result =:= 'timeout' ->
            OKResps = lists:filter(fun kapi_acdc_stats:status_resp_v/1, Resps),
            Statuses = lists:foldl(fun(Resp, AccJObj) ->
                                           AgentsStatuses = kz_json:get_json_value(<<"Agents">>, Resp),
                                           kz_json:merge(AgentsStatuses, AccJObj)
                                   end, kz_json:new(), OKResps),
            {'ok', Statuses}
    end.

-spec most_recent_db_statuses(kz_term:ne_binary()) ->
                                     statuses_return() |
                                     {'error', any()}.
most_recent_db_statuses(AccountId) ->
    most_recent_db_statuses(AccountId, 'undefined', []).

-spec most_recent_db_statuses(kz_term:ne_binary(), kz_term:api_binary() | kz_term:proplist()) ->
                                     statuses_return() |
                                     {'error', any()}.
most_recent_db_statuses(AccountId, ?NE_BINARY = AgentId) ->
    most_recent_db_statuses(AccountId, AgentId, []);
most_recent_db_statuses(AccountId, Options) when is_list(Options) ->
    most_recent_db_statuses(AccountId, 'undefined', Options).

-spec most_recent_db_statuses(kz_term:ne_binary(), kz_term:api_binary(), kz_term:proplist()) ->
                                     statuses_return() |
                                     {'error', any()}.
most_recent_db_statuses(AccountId, 'undefined', ReqOptions) ->
    case props:get_value(<<"Agent-ID">>, ReqOptions) of
        'undefined' -> most_recent_db_statuses_by_timestamp(AccountId, ReqOptions);
        AgentId -> most_recent_db_statuses_by_agent(AccountId, AgentId, ReqOptions)
    end;
most_recent_db_statuses(AccountId, AgentId, ReqOptions) ->
    most_recent_db_statuses_by_agent(AccountId, AgentId, ReqOptions).

most_recent_db_statuses_by_agent(AccountId, AgentId, ReqOptions) ->
    ViewOptions = build_agent_view_options(AgentId, ReqOptions),
    case kz_datamgr:get_results(acdc_stats_util:db_name(AccountId)
                               ,<<"agent_stats/most_recent_by_agent">>
                               ,ViewOptions
                               )
    of
        {'error', _}=E -> E;
        {'ok', Stats} ->
            {'ok', cleanup_db_statuses(Stats, [{<<"Agent-ID">>, AgentId} | ReqOptions])}
    end.

most_recent_db_statuses_by_timestamp(AccountId, ReqOptions) ->
    ViewOptions = build_timestamp_view_options(ReqOptions),
    case kz_datamgr:get_results(acdc_stats_util:db_name(AccountId)
                               ,<<"agent_stats/most_recent_by_timestamp">>
                               ,ViewOptions
                               )
    of
        {'error', _}=E -> E;
        {'ok', Stats} -> {'ok', cleanup_db_statuses(Stats, ReqOptions)}
    end.

build_timestamp_view_options(ReqOptions) ->
    build_timestamp_view_options(ReqOptions, ['descending'
                                             ,'include_docs'
                                             ,{'reduce', 'false'}
                                             ]).

build_timestamp_view_options([], ViewOptions) -> ViewOptions;
build_timestamp_view_options([{<<"Start-Range">>, T}|ReqOptions], ViewOptions) ->
    build_timestamp_view_options(ReqOptions, [{'endkey', [T, kz_json:new()]}|ViewOptions]);
build_timestamp_view_options([{<<"End-Range">>, T}|ReqOptions], ViewOptions) ->
    build_timestamp_view_options(ReqOptions, [{'startkey', [T, 0]}|ViewOptions]);
build_timestamp_view_options([_|ReqOptions], ViewOptions) ->
    build_timestamp_view_options(ReqOptions, ViewOptions).

build_agent_view_options(AgentId, ReqOptions) ->
    ViewOptions = build_agent_view_options(AgentId, ReqOptions, ['descending'
                                                                ,'include_docs'
                                                                ,{'reduce', 'false'}
                                                                ]),
    constrain_agent_view_options(AgentId, ViewOptions).

constrain_agent_view_options(AgentId, ViewOptions) ->
    Window = ?CLEANUP_WINDOW,
    case {props:get_value('startkey', ViewOptions), props:get_value('endkey', ViewOptions)} of
        {'undefined', 'undefined'} ->
            %% No time constraints, limit it to most recent time period
            Now = kz_time:now_s(),
            Past = Now - Window,
            [{'startkey', [AgentId, Now]}
            ,{'endkey', [AgentId, Past]}
             | ViewOptions
            ];
        {'undefined', [AgentId, Past]} ->
            %% constrain how far forward we look
            Present = Past + Window,
            [{'startkey', [AgentId, Present]} | ViewOptions];
        {[AgentId, Present], 'undefined'} ->
            %% constrain how far back we look
            Past = Present - Window,
            [{'endkey', [AgentId, Past]} | ViewOptions];
        {[AgentId, Present], [AgentId, Past]} ->
            case (Present - Past) > Window of
                'true' ->
                    %% Gap is too large, constrain against Present
                    ConstrainedPast = Present - Window,
                    lager:debug("using constrained past ~b instead of ~b", [ConstrainedPast, Past]),
                    [{'endkey', [AgentId, ConstrainedPast]} | props:delete('endkey', ViewOptions)];
                'false' ->
                    ViewOptions
            end
    end.

build_agent_view_options(_AgentId, [], ViewOptions) -> ViewOptions;
build_agent_view_options(AgentId, [{<<"Start-Range">>, T} | ReqOptions], ViewOptions) ->
    build_agent_view_options(AgentId, ReqOptions, [{'endkey', [AgentId, T]} | ViewOptions]);
build_agent_view_options(AgentId, [{<<"End-Range">>, T} | ReqOptions], ViewOptions) ->
    build_agent_view_options(AgentId, ReqOptions, [{'startkey', [AgentId, T]} | ViewOptions]);
build_agent_view_options(AgentId, [_| ReqOptions], ViewOptions) ->
    build_agent_view_options(AgentId, ReqOptions, ViewOptions).

-spec find_most_recent_fold(integer() | kz_term:ne_binary(), kz_json:object(), {integer(), kz_json:object()}) ->
                                   {integer(), kz_json:object()}.
find_most_recent_fold(K, V, {T, _V}=Acc) ->
    try kz_term:to_integer(K) of
        N when N > T ->
            {N, kz_doc:public_fields(V)};
        _ -> Acc
    catch
        _E:_R ->
            lager:debug("key ~p not an int", [K]),
            Acc
    end.

cleanup_db_statuses(Stats, ReqOpts) ->
    Filters = [case props:get_value(<<"Status">>, ReqOpts) of
                   'undefined' -> fun always_true/1;
                   S -> fun(Stat) -> kz_json:get_value([<<"doc">>, <<"status">>], Stat) =:= S end
               end
              ,case props:get_value(<<"Agent-ID">>, ReqOpts) of
                   'undefined' -> fun always_true/1;
                   A -> fun(Stat) -> kz_json:get_value([<<"doc">>, <<"agent_id">>], Stat) =:= A end
               end
              ,case props:get_value(<<"Start-Range">>, ReqOpts) of
                   'undefined' -> fun always_true/1;
                   T -> fun(Stat) ->
                                kz_json:get_integer_value([<<"doc">>, <<"timestamp">>], Stat) >= T
                        end
               end
              ,case props:get_value(<<"End-Range">>, ReqOpts) of
                   'undefined' -> fun always_true/1;
                   T -> fun(Stat) ->
                                kz_json:get_integer_value([<<"doc">>, <<"timestamp">>], Stat) =< T
                        end
               end
              ],

    {Key1, Key2} = {[<<"doc">>, <<"agent_id">>], [<<"doc">>, <<"timestamp">>]},

    lists:foldl(fun(S, Acc) ->
                        kz_json:set_value([kz_json:get_binary_value(Key1, S), kz_json:get_binary_value(Key2, S)]
                                         ,kz_doc:public_fields(kz_json:get_value(<<"doc">>, S))
                                         ,Acc
                                         )
                end, kz_json:new()
               ,[S || S <- Stats, lists:all(fun(Filter) -> Filter(S) end, Filters)]
               ).

always_true(_) -> 'true'.

-spec changed(list(), list()) -> {list(), list()}.
changed([], To) -> {To, []};
changed(From, []) -> {[], From};
changed(From, To) -> changed(From, To, [], []).

changed([], To, Add, Rm) -> {To ++ Add, Rm};
changed(From, [], Add, Rm) -> {Add, From ++ Rm};
changed([F|From], To, Add, Rm) ->
    case lists:member(F, To) of
        'true' -> changed(From, lists:delete(F, To), Add, Rm);
        'false' -> changed(From, To, Add, [F|Rm])
    end.

-spec status_should_auto_start(kz_term:ne_binary()) -> boolean().
status_should_auto_start(<<"logged_out">>) -> 'false';
status_should_auto_start(<<"unknown">>) -> 'false';
status_should_auto_start(_) -> 'true'.
