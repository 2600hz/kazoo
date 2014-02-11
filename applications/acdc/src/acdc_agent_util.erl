%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2014, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(acdc_agent_util).

-export([update_status/3, update_status/4

         ,most_recent_status/2
         ,most_recent_statuses/1, most_recent_statuses/2, most_recent_statuses/3

         ,most_recent_ets_status/2
         ,most_recent_db_status/2

         ,most_recent_ets_statuses/1, most_recent_ets_statuses/2, most_recent_ets_statuses/3
         ,most_recent_db_statuses/1, most_recent_db_statuses/2, most_recent_db_statuses/3

         ,changed/2
        ]).

-export([async_most_recent_ets_statuses/4
         ,async_most_recent_db_statuses/4
        ]).

-include("acdc.hrl").

-spec update_status(ne_binary(), ne_binary(), ne_binary()) -> 'ok'.
-spec update_status(ne_binary(), ne_binary(), ne_binary(), wh_proplist()) -> 'ok'.
update_status(AccountId, AgentId, Status) ->
    update_status(AccountId, AgentId, Status, []).
update_status(?NE_BINARY = AccountId, AgentId, Status, Options) ->
    API = [{<<"Account-ID">>, AccountId}
           ,{<<"Agent-ID">>, AgentId}
           ,{<<"Status">>, Status}
           ,{<<"Timestamp">>, wh_util:current_tstamp()}
           | Options ++ wh_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    wapi_acdc_stats:publish_status_update(API).

-spec most_recent_status(ne_binary(), ne_binary()) ->
                                {'ok', ne_binary()} |
                                {'error', any()}.
most_recent_status(AccountId, AgentId) ->
    case most_recent_ets_status(AccountId, AgentId) of
        {'ok', _}=OK -> OK;
        {'error', _ErrJObj} ->
            lager:debug("failed to get ETS stats: ~p", [wh_json:get_value(<<"Error-Reason">>, _ErrJObj)]),
            most_recent_db_status(AccountId, AgentId)
    end.

-spec most_recent_ets_status(ne_binary(), ne_binary()) ->
                                    {'ok', ne_binary()} |
                                    {'error', any()}.
most_recent_ets_status(AccountId, AgentId) ->
    API = [{<<"Account-ID">>, AccountId}
           ,{<<"Agent-ID">>, AgentId}
           | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    case whapps_util:amqp_pool_request(API
                                       ,fun wapi_acdc_stats:publish_status_req/1
                                       ,fun wapi_acdc_stats:status_resp_v/1
                                      )
    of
        {'error', _E}=E -> E;
        {'ok', Resp} ->
            Stats = wh_json:get_value([<<"Agents">>, AgentId], Resp),
            {_, StatusJObj} = wh_json:foldl(fun find_most_recent_fold/3, {0, wh_json:new()}, Stats),
            {'ok', wh_json:get_value(<<"status">>, StatusJObj)}
    end.

-spec most_recent_db_status(ne_binary(), ne_binary()) -> {'ok', ne_binary()}.
most_recent_db_status(AccountId, AgentId) ->
    Opts = [{'startkey', [AgentId, wh_util:current_tstamp()]}
            ,{'limit', 1}
            ,'descending'
           ],
    case couch_mgr:get_results(acdc_stats_util:db_name(AccountId), <<"agent_stats/status_log">>, Opts) of
        {'ok', [StatusJObj]} -> {'ok', wh_json:get_value(<<"value">>, StatusJObj)};
        {'ok', []} -> {'ok', <<"unknown">>};
        {'error', 'not_found'} ->
            acdc_maintenance:refresh_account(AccountId);
        {'error', _E} ->
            lager:debug("error querying view: ~p", [_E]),
            {'ok', <<"unknown">>}
    end.

-type statuses_return() :: {'ok', wh_json:object()}.
-spec most_recent_statuses(ne_binary()) ->
                                  statuses_return().
-spec most_recent_statuses(ne_binary(), api_binary() | wh_proplist()) ->
                                  statuses_return().
-spec most_recent_statuses(ne_binary(), api_binary(), wh_proplist()) ->
                                  statuses_return().

most_recent_statuses(AccountId) ->
    most_recent_statuses(AccountId, 'undefined', []).

most_recent_statuses(AccountId, 'undefined') ->
    most_recent_statuses(AccountId, 'undefined', []);
most_recent_statuses(AccountId, ?NE_BINARY = AgentId) ->
    most_recent_statuses(AccountId, AgentId, []);
most_recent_statuses(AccountId, Options) when is_list(Options) ->
    most_recent_statuses(AccountId, props:get_value(<<"Agent-ID">>, Options), Options).

most_recent_statuses(AccountId, AgentId, Options) ->
    Self = self(),

    ETS = spawn_monitor(?MODULE, 'async_most_recent_ets_statuses', [AccountId, AgentId, Options, Self]),

    DB = maybe_start_db_lookup('async_most_recent_db_statuses', AccountId, AgentId, Options, Self),

    maybe_reduce_statuses(AgentId, receive_statuses([ETS, DB])).

-spec maybe_start_db_lookup(atom(), ne_binary(), api_binary(), list(), pid()) ->
                                   {pid(), reference()} | 'undefined'.
maybe_start_db_lookup(F, AccountId, AgentId, Options, Self) ->
    case wh_cache:fetch_local(?ACDC_CACHE, db_fetch_key(F, AccountId, AgentId)) of
        {'ok', _} -> 'undefined';
        {'error', 'not_found'} ->
            spawn_monitor(?MODULE, F, [AccountId, AgentId, Options, Self])
    end.
db_fetch_key(F, AccountId, AgentId) -> {F, AccountId, AgentId}.

-spec maybe_reduce_statuses(api_binary(), wh_json:object()) ->
                                   {'ok', wh_json:object()}.
maybe_reduce_statuses('undefined', Statuses) ->
    {'ok', wh_json:map(fun map_reduce_agent_statuses/2, Statuses)};
maybe_reduce_statuses(_, Statuses) -> {'ok', Statuses}.

map_reduce_agent_statuses(AgentId, Statuses) ->
    {_, S} = wh_json:foldl(fun reduce_agent_statuses/3, {0, wh_json:new()}, Statuses),
    {AgentId, S}.

reduce_agent_statuses(_, Data, {T, _}=Acc) ->
    StatT = wh_json:get_value(<<"timestamp">>, Data),
    try wh_util:to_integer(StatT) of
        Timestamp when Timestamp > T ->
            {Timestamp, Data};
        _ -> Acc
    catch
        _:_ -> Acc
    end.

-type receive_info() :: [{pid(), reference()} | 'undefined',...] | [].
-spec receive_statuses(receive_info()) ->
                              wh_json:object().
-spec receive_statuses(receive_info(), wh_json:object()) ->
                              wh_json:object().
receive_statuses(Reqs) -> receive_statuses(Reqs, wh_json:new()).

receive_statuses([], AccJObj) -> AccJObj;
receive_statuses(['undefined' | Reqs], AccJObj) ->
    receive_statuses(Reqs, AccJObj);
receive_statuses([{Pid, Ref} | Reqs], AccJObj) ->
    receive
        {'statuses', Statuses, Pid} ->
            clear_monitor(Ref),
            receive_statuses(Reqs, wh_json:merge_recursive(Statuses, AccJObj));
        {'DOWN', Ref, 'process', Pid, _R} ->
            lager:debug("req in ~p died: ~p", [Pid, _R]),
            clear_monitor(Ref),
            receive_statuses(Reqs, AccJObj)
    after 3000 ->
            lager:debug("timed out waiting for ~p to respond", [Pid]),
            receive_statuses(Reqs, AccJObj)
    end.

-spec clear_monitor(reference()) -> 'ok'.
clear_monitor(Ref) ->
    erlang:demonitor(Ref, ['flush']),
    receive
        {'DOWN', Ref, 'process', _, _} -> clear_monitor(Ref)
    after 0 -> 'ok'
    end.

-spec async_most_recent_ets_statuses(ne_binary(), api_binary(), wh_proplist(), pid()) ->
                                            'ok'.
async_most_recent_ets_statuses(AccountId, AgentId, Options, Pid) ->
    case most_recent_ets_statuses(AccountId, AgentId, Options) of
        {'ok', Statuses} ->
            Pid ! {'statuses', Statuses, self()},
            'ok';
        {'error', _E} ->
            Pid ! {'statuses', wh_json:new(), self()},
            'ok'
    end.

-spec async_most_recent_db_statuses(ne_binary(), api_binary(), wh_proplist(), pid()) ->
                                           'ok'.
async_most_recent_db_statuses(AccountId, AgentId, Options, Pid) ->
    case most_recent_db_statuses(AccountId, AgentId, Options) of
        {'ok', Statuses} ->
            Pid ! {'statuses', Statuses, self()},
            wh_cache:store_local(?ACDC_CACHE, db_fetch_key('async_most_recent_db_statuses', AccountId, AgentId), 'true'),
            'ok';
        {'error', _E} ->
            Pid ! {'statuses', wh_json:new(), self()},
            'ok'
    end.

-spec most_recent_ets_statuses(ne_binary()) ->
                                      statuses_return() |
                                      {'error', _}.
-spec most_recent_ets_statuses(ne_binary(), api_binary(), wh_proplist()) ->
                                      statuses_return() |
                                      {'error', _}.
most_recent_ets_statuses(AccountId) ->
    most_recent_ets_statuses(AccountId, 'undefined', []).

most_recent_ets_statuses(AccountId, ?NE_BINARY = AgentId) ->
    most_recent_ets_statuses(AccountId, AgentId, []);
most_recent_ets_statuses(AccountId, Options) when is_list(Options) ->
    most_recent_ets_statuses(AccountId, 'undefined', Options).

most_recent_ets_statuses(AccountId, AgentId, Options) ->
    API = props:filter_undefined(
            [{<<"Account-ID">>, AccountId}
             ,{<<"Agent-ID">>, AgentId}
             | wh_api:default_headers(?APP_NAME, ?APP_VERSION) ++ Options
            ]),
    case whapps_util:amqp_pool_request(API
                                       ,fun wapi_acdc_stats:publish_status_req/1
                                       ,fun wapi_acdc_stats:status_resp_v/1
                                      )
    of
        {'error', _}=E -> E;
        {'ok', Resp} ->
            {'ok', wh_json:get_value([<<"Agents">>], Resp, wh_json:new())}
    end.

-spec most_recent_db_statuses(ne_binary()) ->
                                      statuses_return() |
                                      {'error', _}.
-spec most_recent_db_statuses(ne_binary(), api_binary(), wh_proplist()) ->
                                      statuses_return() |
                                      {'error', _}.
most_recent_db_statuses(AccountId) ->
    most_recent_db_statuses(AccountId, 'undefined', []).
most_recent_db_statuses(AccountId, ?NE_BINARY = AgentId) ->
    most_recent_db_statuses(AccountId, AgentId, []);
most_recent_db_statuses(AccountId, Options) when is_list(Options) ->
    most_recent_db_statuses(AccountId, 'undefined', Options).

most_recent_db_statuses(AccountId, 'undefined', ReqOptions) ->
    case props:get_value(<<"Agent-ID">>, ReqOptions) of
        'undefined' -> most_recent_db_statuses_by_timestamp(AccountId, ReqOptions);
        AgentId -> most_recent_db_statuses_by_agent(AccountId, AgentId, ReqOptions)
    end;
most_recent_db_statuses(AccountId, AgentId, ReqOptions) ->
    most_recent_db_statuses_by_agent(AccountId, AgentId, ReqOptions).

most_recent_db_statuses_by_agent(AccountId, AgentId, ReqOptions) ->
    ViewOptions = build_agent_view_options(AgentId, ReqOptions),
    case couch_mgr:get_results(acdc_stats_util:db_name(AccountId)
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
    case couch_mgr:get_results(acdc_stats_util:db_name(AccountId)
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
    build_timestamp_view_options(ReqOptions, [{'endkey', [T, wh_json:new()]}|ViewOptions]);
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
            Now = wh_util:current_tstamp(),
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

-spec find_most_recent_fold(integer() | ne_binary(), wh_json:object(), {integer(), wh_json:object()}) ->
                                   {integer(), wh_json:object()}.
find_most_recent_fold(K, V, {T, _V}=Acc) ->
    try wh_util:to_integer(K) of
        N when N > T ->
            {N, wh_doc:public_fields(V)};
        _ -> Acc
    catch
        _E:_R ->
            lager:debug("key ~p not an int", [K]),
            Acc
    end.

cleanup_db_statuses(Stats, ReqOpts) ->
    Filters = [case props:get_value(<<"Status">>, ReqOpts) of
                   'undefined' -> fun always_true/1;
                   S -> fun(Stat) -> wh_json:get_value([<<"doc">>, <<"status">>], Stat) =:= S end
               end
               ,case props:get_value(<<"Agent-ID">>, ReqOpts) of
                    'undefined' -> fun always_true/1;
                    A -> fun(Stat) -> wh_json:get_value([<<"doc">>, <<"agent_id">>], Stat) =:= A end
                end
               ,case props:get_value(<<"Start-Range">>, ReqOpts) of
                    'undefined' -> fun always_true/1;
                    T -> fun(Stat) ->
                                 wh_json:get_integer_value([<<"doc">>, <<"timestamp">>], Stat) >= T
                         end
                end
               ,case props:get_value(<<"End-Range">>, ReqOpts) of
                    'undefined' -> fun always_true/1;
                    T -> fun(Stat) ->
                                 wh_json:get_integer_value([<<"doc">>, <<"timestamp">>], Stat) =< T
                         end
                end
              ],

    {Key1, Key2} = {[<<"doc">>, <<"agent_id">>], [<<"doc">>, <<"timestamp">>]},

    lists:foldl(fun(S, Acc) ->
                        wh_json:set_value([wh_json:get_binary_value(Key1, S), wh_json:get_binary_value(Key2, S)]
                                          ,wh_doc:public_fields(wh_json:get_value(<<"doc">>, S))
                                          ,Acc
                                         )
                end, wh_json:new()
                ,[S || S <- Stats, lists:all(fun(Filter) -> Filter(S) end, Filters)]
               ).

always_true(_) -> 'true'.

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


