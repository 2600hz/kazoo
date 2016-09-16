%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2016, 2600Hz INC
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
        ,start_cleanup_pass/1
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

-define(SERVER, ?MODULE).

-record(state, {cleanup_ref = cleanup_timer() :: reference()
               }).
-type state() :: #state{}.

-define(BINDING_PREFIX, "v2_resource.cleanup").

%% How long to pause before attempting to delete the next chunk of soft-deleted docs
-define(SOFT_DELETE_PAUSE,
        kapps_config:get(?CONFIG_CAT, <<"soft_delete_pause_ms">>, 10 * ?MILLISECONDS_IN_SECOND)).

-define(CLEANUP_TIMER,
        kapps_config:get_integer(?CONFIG_CAT, <<"cleanup_timer">>, ?SECONDS_IN_DAY)).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts the server
%%--------------------------------------------------------------------
-spec start_link() -> startlink_ret().
start_link() ->
    gen_server:start_link({'local', ?SERVER}, ?MODULE, [], []).

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
-spec init([]) -> {ok, state()}.
init([]) ->
    kz_util:put_callid(?MODULE),
    _ = crossbar_bindings:bind(binding_all_dbs(), ?MODULE, 'cleanup_soft_deletes'),
    lager:debug("started ~s", [?MODULE]),
    {'ok', #state{}}.

-spec binding_account() -> ne_binary().
binding_account() ->
    <<?BINDING_PREFIX, ".account">>.

-spec binding_account_mod() -> ne_binary().
binding_account_mod() ->
    <<?BINDING_PREFIX, ".account_mod">>.

-spec binding_system() -> ne_binary().
binding_system() ->
    <<?BINDING_PREFIX, ".system">>.

-spec binding_other() -> ne_binary().
binding_other() ->
    <<?BINDING_PREFIX, ".other">>.

-spec binding_minute() -> ne_binary().
binding_minute() ->
    <<?BINDING_PREFIX, ".minute">>.

-spec binding_hour() -> ne_binary().
binding_hour() ->
    <<?BINDING_PREFIX, ".hour">>.

-spec binding_day() -> ne_binary().
binding_day() ->
    <<?BINDING_PREFIX, ".day">>.

-spec binding_all_dbs() -> ne_binaries().
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
%% @end
%%--------------------------------------------------------------------
-spec handle_call(any(), pid_ref(), state()) -> handle_call_ret_state(state()).
handle_call(_Request, _From, State) ->
    {'reply', {'error', 'not_implemented'}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(any(), state()) -> handle_cast_ret_state(state()).
handle_cast({'cleanup_finished', Ref}, #state{cleanup_ref=Ref}=State) ->
    lager:debug("cleanup finished for ~p, starting timer", [Ref]),
    {'noreply', State#state{cleanup_ref=cleanup_timer()}, 'hibernate'};

handle_cast(_Msg, State) ->
    lager:debug("unhandled cast: ~p", [_Msg]),
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_info(any(), state()) -> handle_info_ret_state(state()).
handle_info({timeout, Ref, _Msg}, #state{cleanup_ref=Ref}=State) ->
    _Pid = kz_util:spawn(fun start_cleanup_pass/1, [Ref]),
    lager:debug("cleaning up in ~p(~p)", [_Pid, Ref]),
    {'noreply', State};

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
%% @end
%%--------------------------------------------------------------------
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, _State) ->
    lager:debug("~s terminating: ~p", [?MODULE, _Reason]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec start_cleanup_pass(reference()) -> 'ok'.
start_cleanup_pass(Ref) ->
    kz_util:put_callid(<<"cleanup_pass_", (kz_util:rand_hex_binary(4))/binary>>),
    {'ok', Dbs} = kz_datamgr:db_info(),
    lager:debug("starting cleanup pass of databases"),
    lists:foreach(fun cleanup_pass/1, Dbs),
    lager:debug("pass completed for ~p", [Ref]),
    gen_server:cast(?SERVER, {'cleanup_finished', Ref}).

cleanup_pass(Db) ->
    crossbar_bindings:map(db_routing_key(Db), Db),
    erlang:garbage_collect(self()).

-spec db_routing_key(ne_binary()) -> ne_binary().
db_routing_key(Db) ->
    Classifiers = [{fun kapps_util:is_account_db/1, binding_account()}
                  ,{fun kapps_util:is_account_mod/1, binding_account_mod()}
                  ,{fun kz_util:is_system_db/1, binding_system()}
                  ],
    db_routing_key(Db, Classifiers).
db_routing_key(_Db, []) ->
    binding_other();
db_routing_key(Db, [{Classifier, Binding} | Classifiers]) ->
    case Classifier(Db) of
        'true' -> Binding;
        'false' -> db_routing_key(Db, Classifiers)
    end.

-spec cleanup_timer() -> reference().
cleanup_timer() ->
    Expiry = ?CLEANUP_TIMER,
    lager:debug("starting cleanup timer for ~b s", [Expiry]),
    erlang:start_timer(?CLEANUP_TIMER, self(), ok).

-spec cleanup_soft_deletes(ne_binary()) -> any().
cleanup_soft_deletes(Account) ->
    kz_datamgr:suppress_change_notice(),
    case kapps_util:is_account_db(Account) of
        'true' -> cleanup_account_soft_deletes(Account);
        'false' -> 'ok' % no longer checking other dbs for soft deletes
    end.

-spec cleanup_account_soft_deletes(ne_binary()) -> 'ok'.
cleanup_account_soft_deletes(Account) ->
    AccountDb = kz_util:format_account_id(Account, 'encoded'),
    do_cleanup(AccountDb).

-spec do_cleanup(ne_binary()) -> 'ok'.
do_cleanup(Db) ->
    View = <<"maintenance/soft_deletes">>,
    ViewOptions = [{'limit', kz_datamgr:max_bulk_insert()}],
    case kz_datamgr:get_results(Db, View, ViewOptions) of
        {'ok', []} -> 'ok';
        {'ok', L} ->
            lager:debug("removing ~b soft-deleted docs from ~s", [length(L), Db]),
            _ = kz_datamgr:del_docs(Db, L),
            'ok' = timer:sleep(?SOFT_DELETE_PAUSE),
            do_cleanup(Db);
        {'error', 'not_found'} ->
            lager:warning("db ~s or view '~s' not found", [Db, View]);
        {'error', _E} ->
            lager:debug("failed to lookup soft-deleted tokens: ~p", [_E])
    end.
