%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2013, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(crossbar_cleanup).

-behaviour(gen_server).

%% API
-export([start_link/0
         ,cleanup_heard_voicemail/1
         ,cleanup_soft_deletes/1
         ,start_cleanup_pass/0
         ,binding_account/0
         ,binding_account_mod/0
         ,binding_system/0
         ,binding_other/0
         ,binding_all_dbs/0
         ,binding_minute/0
         ,binding_hour/0
         ,binding_day/0
        ]).

%% gen_server callbacks
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,terminate/2
         ,code_change/3
        ]).

-include("crossbar.hrl").

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
    gen_server:start_link({'local', ?MODULE}, ?MODULE, [], []).

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
    put('callid', ?LOG_SYSTEM_ID),

    _ = crossbar_bindings:bind(binding_account(), ?MODULE, 'cleanup_heard_voicemail'),
    _ = crossbar_bindings:bind(binding_all_dbs(), ?MODULE, 'cleanup_soft_deletes'),

    start_timers(),
    lager:debug("started ~s", [?MODULE]),

    {'ok', 'ok'}.

-define(BINDING_PREFIX, "v2_resource.cleanup.").

binding_account() ->
    <<?BINDING_PREFIX, "account">>.
binding_account_mod() ->
    <<?BINDING_PREFIX, "account_mod">>.
binding_system() ->
    <<?BINDING_PREFIX, "system">>.
binding_other() ->
    <<?BINDING_PREFIX, "other">>.
binding_minute() ->
    <<?BINDING_PREFIX, "minute">>.
binding_hour() ->
    <<?BINDING_PREFIX, "hour">>.
binding_day() ->
    <<?BINDING_PREFIX, "day">>.
binding_all_dbs() ->
    [binding_account()
     ,binding_account_mod()
     ,binding_system()
     ,binding_other()
    ].


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
handle_info('cleanup', 'ok') ->
    _Pid = spawn(?MODULE, 'start_cleanup_pass', []),
    lager:debug("cleaning up in ~p", [_Pid]),
    start_cleanup_timer(),
    {'noreply', 'ok'};
handle_info('minute_cleanup', 'ok') ->
    _Pid = spawn('crossbar_bindings', 'map', [binding_minute(), []]),
    lager:debug("minute cleanup binding fired"),
    start_minute_timer(),
    {'noreply', 'ok'};
handle_info('hour_cleanup', 'ok') ->
    _Pid = spawn('crossbar_bindings', 'map', [binding_hour(), []]),
    lager:debug("hour cleanup binding fired"),
    start_hour_timer(),
    {'noreply', 'ok'};
handle_info('day_cleanup', 'ok') ->
    _Pid = spawn('crossbar_bindings', 'map', [binding_day(), []]),
    lager:debug("day cleanup binding fired"),
    start_day_timer(),
    {'noreply', 'ok'};
handle_info(_Msg, 'ok') ->
    lager:debug("unhandled msg: ~p", [_Msg]),
    {'noreply', 'ok'}.

-spec start_cleanup_pass() -> 'ok'.
start_cleanup_pass() ->
    {'ok', Dbs} = couch_mgr:db_info(),
    lager:debug("starting cleanup pass of databases"),

    _ = [crossbar_bindings:map(db_routing_key(Db), Db)
         || Db <- Dbs
        ],
    lager:debug("pass completed").

db_routing_key(Db) ->
    Classifiers = [{fun whapps_util:is_account_db/1, fun binding_account/0}
                   ,{fun whapps_util:is_account_mod/1, fun binding_account_mod/0}
                   ,{fun wh_util:is_system_db/1, fun binding_system/0}
                  ],
    db_routing_key(Db, Classifiers).
db_routing_key(_Db, []) ->
    binding_other();
db_routing_key(Db, [{Classifier, BindingFun} | Classifiers]) ->
    case Classifier(Db) of
        'true' -> BindingFun();
        'false' -> db_routing_key(Db, Classifiers)
    end.

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
    lager:debug("~s terminating: ~p", [?MODULE, _Reason]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec start_timers() -> 'ok'.
start_timers() ->
    _ = start_cleanup_timer(),
    _ = start_minute_timer(),
    _ = start_hour_timer(),
    _ = start_day_timer(),
    'ok'.

-spec start_timer(pos_integer(), any()) -> reference().
start_timer(Expiry, Msg) ->
    erlang:send_after(Expiry, self(), Msg).

-spec start_cleanup_timer() -> reference().
start_cleanup_timer() ->
    Expiry = whapps_config:get_integer(?CONFIG_CAT, <<"cleanup_timer">>, ?SECONDS_IN_DAY),
    lager:debug("starting cleanup timer for ~b s", [Expiry]),
    start_timer(Expiry*1000, 'cleanup').

start_minute_timer() ->
    start_timer(?MILLISECONDS_IN_MINUTE, 'minute_cleanup').
start_hour_timer() ->
    start_timer(?MILLISECONDS_IN_HOUR, 'hour_cleanup').
start_day_timer() ->
    start_timer(?MILLISECONDS_IN_DAY, 'day_cleanup').

-spec cleanup_soft_deletes(ne_binary()) -> any().
cleanup_soft_deletes(AcctDb) ->
    case couch_mgr:get_results(AcctDb, <<"maintenance/soft_deleted">>, [{'limit', 1000}]) of
        {'ok', []} -> 'ok';
        {'ok', L} ->
            lager:debug("removing ~b soft-deleted docs from ~s", [length(L), AcctDb]),
            couch_mgr:del_docs(AcctDb, prepare_docs_for_deletion(L));
        {'error', 'not_found'} ->
            lager:debug("db ~s or view 'maintenance/soft_deleted' not found", [AcctDb]);
        {'error', _E} ->
            lager:debug("failed to lookup soft-deleted tokens: ~p", [_E])
    end.

-spec cleanup_heard_voicemail(ne_binary()) -> any().
-spec cleanup_heard_voicemail(ne_binary(), pos_integer()) -> any().
-spec cleanup_heard_voicemail(ne_binary(), pos_integer(), wh_proplist()) -> any().
cleanup_heard_voicemail(AcctDb) ->
    case whapps_account_config:get(AcctDb
                                   ,<<"callflow">>
                                   ,[<<"voicemail">>, <<"message_retention_duration">>]
                                  )
    of
        'undefined' -> lager:debug("no limit to voicemail retention");
        Duration ->
            lager:debug("retaining messages for ~p days, delete those older", [Duration]),
            cleanup_heard_voicemail(AcctDb, wh_util:to_integer(Duration))
    end.

cleanup_heard_voicemail(AcctDb, Duration) ->
    Today = wh_util:current_tstamp(),
    DurationS = Duration * 86400, % duration in seconds
    case couch_mgr:get_results(AcctDb, <<"vmboxes/crossbar_listing">>, ['include_docs']) of
        {'ok', []} -> lager:debug("no voicemail boxes in ~s", [AcctDb]);
        {'ok', View} ->
            lager:debug("cleaning up ~b voicemail boxes in ~s", [length(View), AcctDb]),
            cleanup_heard_voicemail(AcctDb
                                    ,Today - DurationS
                                    ,[{wh_json:get_value(<<"doc">>, V)
                                       ,wh_json:get_value([<<"doc">>, <<"messages">>], V)
                                      }
                                      || V <- View
                                     ]
                                   );
        {'error', _E} ->
            lager:debug("failed to get voicemail boxes in ~s: ~p", [AcctDb, _E])
    end.
cleanup_heard_voicemail(AcctDb, Timestamp, Boxes) ->
    [cleanup_voicemail_box(AcctDb, Timestamp, Box) || Box <- Boxes].

-spec cleanup_voicemail_box(ne_binary(), pos_integer(), {wh_json:object(), wh_json:objects()}) -> any().
cleanup_voicemail_box(AcctDb, Timestamp, {Box, Msgs}) ->
    case lists:partition(fun(Msg) ->
                                 %% must be old enough, and not in the NEW folder
                                 wh_json:get_integer_value(<<"timestamp">>, Msg) < Timestamp
                                     andalso wh_json:get_value(<<"folder">>, Msg) =/= <<"new">>
                         end, Msgs)
    of
        {[], _} -> lager:debug("there are no old messages to remove from ~s", [wh_json:get_value(<<"_id">>, Box)]);
        {Older, Newer} ->
            lager:debug("there are ~b old messages to remove", [length(Older)]),

            _ = [catch delete_media(AcctDb, wh_json:get_value(<<"media_id">>, Msg)) || Msg <- Older],
            lager:debug("soft-deleted old messages"),

            Box1 = wh_json:set_value(<<"messages">>, Newer, Box),
            {'ok', Box2} = couch_mgr:save_doc(AcctDb, Box1),
            lager:debug("updated messages in voicemail box ~s", [wh_json:get_value(<<"_id">>, Box2)])
    end.

delete_media(AcctDb, MediaId) ->
    {'ok', JObj} = couch_mgr:open_doc(AcctDb, MediaId),
    couch_mgr:ensure_saved(AcctDb, wh_json:set_value(<<"pvt_deleted">>, 'true', JObj)).

-spec prepare_docs_for_deletion(wh_json:objects()) -> wh_json:objects().
-spec prepare_doc_for_deletion(wh_json:object()) -> wh_json:object().
prepare_docs_for_deletion(L) ->
    [prepare_doc_for_deletion(D) || D <- L].
prepare_doc_for_deletion(D) ->
    wh_json:from_list([{<<"_id">>, wh_json:get_value(<<"id">>, D)}
                       ,{<<"_rev">>, wh_json:get_value(<<"value">>, D)}
                      ]).
