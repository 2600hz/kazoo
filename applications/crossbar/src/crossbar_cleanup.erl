%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2014, 2600Hz INC
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

         ,status/0
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

-record(state, {cleanup_timer_ref=start_cleanup_timer()
                ,minute_timer_ref=start_minute_timer()
                ,hour_timer_ref=start_hour_timer()
                ,day_timer_ref=start_day_timer()
               }).
-type state() :: #state{}.

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

-spec status() -> wh_proplist().
status() ->
    gen_server:call(?MODULE, 'status').

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
    put('callid', ?MODULE),

    _ = crossbar_bindings:bind(binding_all_dbs(), ?MODULE, 'cleanup_soft_deletes'),

    State = start_timers(),
    lager:debug("started ~s", [?MODULE]),

    {'ok', State}.

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
handle_call('status', _From, #state{cleanup_timer_ref=Cleanup
                                    ,minute_timer_ref=Minute
                                    ,hour_timer_ref=Hour
                                    ,day_timer_ref=Day
                                   }=State) ->
    {'reply', [{'cleanup', erlang:read_timer(Cleanup)}
               ,{'minute', erlang:read_timer(Minute)}
               ,{'hour', erlang:read_timer(Hour)}
               ,{'day', erlang:read_timer(Day)}
              ]
     , State};
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
handle_info('cleanup', #state{cleanup_timer_ref=Ref}=State) ->
    _Pid = spawn(?MODULE, 'start_cleanup_pass', []),
    lager:debug("cleaning up in ~p", [_Pid]),
    _ = stop_timer(Ref),
    {'noreply', State#state{cleanup_timer_ref=start_cleanup_timer()}};
handle_info('minute_cleanup', #state{minute_timer_ref=Ref}=State) ->
    _Pid = spawn('crossbar_bindings', 'map', [binding_minute(), []]),
    _ = stop_timer(Ref),
    {'noreply', State#state{minute_timer_ref=start_minute_timer()}};
handle_info('hour_cleanup', #state{hour_timer_ref=Ref}=State) ->
    _Pid = spawn('crossbar_bindings', 'map', [binding_hour(), []]),
    _ = stop_timer(Ref),
    {'noreply', State#state{hour_timer_ref=start_hour_timer()}};
handle_info('day_cleanup', #state{day_timer_ref=Ref}=State) ->
    _Pid = spawn('crossbar_bindings', 'map', [binding_day(), []]),
    _ = stop_timer(Ref),
    {'noreply', State#state{day_timer_ref=start_day_timer()}};
handle_info(_Msg, State) ->
    lager:debug("unhandled msg: ~p", [_Msg]),
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

-spec start_timers() -> state().
start_timers() ->
    #state{cleanup_timer_ref=start_cleanup_timer()
           ,minute_timer_ref=start_minute_timer()
           ,hour_timer_ref=start_hour_timer()
           ,day_timer_ref=start_day_timer()
          }.

-spec stop_timer(any()) -> any().
stop_timer(Ref) when is_reference(Ref) ->
    erlang:cancel_timer(Ref);
stop_timer(_) -> 'ok'.

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
cleanup_soft_deletes(Account) ->
    AccountDb = wh_util:format_account_id(Account, 'encoded'),
    case couch_mgr:get_results(AccountDb, <<"maintenance/soft_deletes">>, [{'limit', 1000}]) of
        {'ok', []} -> 'ok';
        {'ok', L} ->
            lager:debug("removing ~b soft-deleted docs from ~s", [length(L), AccountDb]),
            couch_mgr:del_docs(AccountDb, L);
        {'error', 'not_found'} ->
            lager:debug("db ~s or view 'maintenance/soft_deletes' not found", [AccountDb]),
            try whapps_maintenance:refresh(AccountDb) of
                OK -> lager:debug("maintenance refresh: ~p", [OK])
            catch
                _E:_R -> lager:debug("maintenance refresh died: ~s: ~p", [_E, _R])
            end;
        {'error', _E} ->
            lager:debug("failed to lookup soft-deleted tokens: ~p", [_E])
    end.
