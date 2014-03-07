%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(caller10_contests).

-behaviour(gen_server).

%% API
-export([start_link/0
         ,created/1
         ,updated/1
         ,deleted/1

         ,handle_contest_handler_request/1
         ,handle_contest_handler_response/1
        ]).

%% gen_server callbacks
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,terminate/2
         ,code_change/3
        ]).

%% ETS related
-export([table_id/0
         ,table_options/0
         ,find_me_function/0
         ,gift_data/0

         ,load_contests/0
         ,add_contest_handlers/1
        ]).

-include("caller10.hrl").

-define(SERVER, ?MODULE).

-define(LOAD_WINDOW, whapps_config:get_integer(<<"caller10">>, <<"load_window_size_s">>, 3600)).
-define(CUTOFF_WINDOW, whapps_config:get_integer(<<"caller10">>, <<"cutoff_window_size_s">>, 360)).

-define(REFRESH_WINDOW_MSG, 'refresh_window').

-define(HANDLER_START_TIME, whapps_config:get_integer(<<"caller10">>, <<"handler_delayed_start_s">>, 360)).
-define(HANDLER_START_MSG(ContestId), {'handler_delayed_start', ContestId}).

-record(state, {is_writable = 'false' :: boolean()
                ,refresh_ref :: reference()
               }).
-type state() :: #state{}.

-record(contest, {id :: ne_binary() | '_'
                  ,cutoff_time :: pos_integer() | '_'
                  ,begin_time :: pos_integer() | '$1' | '_'
                  ,prior_time :: pos_integer() | 'undefined' | '_'
                  ,start_time :: pos_integer() | '_'
                  ,end_time :: pos_integer() | 'undefined' | '_'
                  ,after_offset :: pos_integer() | 'undefined' | '_'
                  ,doc :: api_object() | '_'
                  ,handling_app :: api_binary() | '_'
                  ,handling_vote :: 0..100 | '_'
                  ,numbers :: ne_binaries() | '_'
                  ,account_id :: ne_binary() | '_'
                  ,sup :: api_pid() | '_'
                  ,listener_pid :: api_pid() | '_'
                  ,fsm_pid :: api_pid() | '_'
                 }).
-type contest() :: #contest{}.
-type contests() :: [contest(),...] | [].

-export_type([contest/0
              ,contests/0
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
-spec start_link() -> startlink_ret().
start_link() ->
    gen_server:start_link({'local', ?SERVER}, ?MODULE, [], []).

-spec created(wh_json:object()) -> 'ok'.
created(JObj) ->
    case is_contest_relevant(JObj) of
        'true' ->
            gen_server:cast(?MODULE, {'load_contest', jobj_to_record(JObj)});
        'false' ->
            lager:debug("contest was outside the window, ignoring")
    end.

-spec updated(wh_json:object()) -> 'ok'.
updated(JObj) ->
    case is_contest_relevant(JObj) of
        'true' ->
            gen_server:cast(?MODULE, {'update_contest', jobj_to_record(JObj)});
        'false' ->
            lager:debug("contest was outside the window, ignoring")
    end.

-spec deleted(ne_binary()) -> 'ok'.
deleted(DocId) ->
    gen_server:cast(?MODULE, {'delete_contest', DocId}).

-spec handle_contest_handler_request(wh_json:object()) -> 'ok'.
handle_contest_handler_request(JObj) ->
    ContestId = wapi_caller10:contest_id(JObj),
    case lookup_by_id(ContestId) of
        'undefined' -> lager:debug("no contest ~s in our ETS", [ContestId]);
        #contest{}=Contest -> publish_contest_handler_response(Contest)
    end.

-spec handle_contest_handler_response(wh_json:object()) -> 'ok'.
handle_contest_handler_response(JObj) ->
    ContestId = wapi_caller10:contest_id(JObj),
    case lookup_by_id(ContestId) of
        'undefined' -> lager:debug("no contest ~s in our ETS", [ContestId]);
        #contest{}=Contest -> maybe_update_contest_from_response(JObj, Contest)
    end.

-spec lookup_by_id(ne_binary() | contest() | wh_json:object()) -> contest() | 'undefined'.
lookup_by_id(Id) when is_binary(Id) ->
    case ets:lookup(table_id(), Id) of
        [] -> 'undefined';
        [#contest{}=Contest] -> Contest
    end;
lookup_by_id(#contest{id=Id}) ->
    lookup_by_id(Id);
lookup_by_id(JObj) ->
    lookup_by_id(wh_json:get_first_defined([<<"Contest-ID">>, <<"_id">>], JObj)).

-spec table_id() -> ?MODULE.
table_id() -> ?MODULE. %% Any atom will do

-spec table_options() -> list().
table_options() ->
    ['set'
     ,'protected'
     ,{'keypos', #contest.id}
     ,'named_table'
    ].

-spec find_me_function() -> api_pid().
find_me_function() -> whereis(?MODULE).

-spec gift_data() -> 'ok'.
gift_data() -> 'ok'.

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
-spec init([]) -> {'ok', state()}.
init([]) ->
    wh_util:put_callid(?MODULE),
    {'ok', #state{refresh_ref=start_refresh_timer()}}.

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
-spec handle_call(term(), pid_ref(), state()) -> {'reply', _, state()}.
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
-spec handle_cast(term(), state()) -> {'noreply', state()} |
                                      {'noreply', state(), 'hibernate'}.
handle_cast({'load_contest', #contest{id=Id}=Contest}
            ,#state{is_writable='true'}=State
           ) ->
    case ets:insert_new(table_id(), Contest) of
        'false' -> lager:debug("contest ~s already loaded", [Id]);
        'true' -> lager:debug("loaded contest ~s", [Id])
    end,
    {'noreply', State};
handle_cast({'update_contest', #contest{id=Id}=Contest}
            ,#state{is_writable='true'}=State
           ) ->
    'true' = ets:insert(table_id(), Contest),
    lager:debug("updated contest ~s", [Id]),
    {'noreply', State};
handle_cast({'delete_contest', Id}
            ,#state{is_writable='true'}=State
           ) ->
    'true' = ets:delete(table_id(), Id),
    lager:debug("maybe deleted contest ~s", [Id]),
    {'noreply', State};
handle_cast({'handler_update', ContestId, ApiApp, ApiVote}, State) ->
    case lookup_by_id(ContestId) of
        'undefined' -> lager:debug("failed to find contest ~s for handler_update", [ContestId]);
        #contest{handling_app=App
                 ,handling_vote=Vote
                }=Contest ->
            case should_update_app({App, Vote}, {ApiApp, ApiVote}) of
                'true' ->
                    ets:insert(table_id(), Contest#contest{handling_app=ApiApp
                                                           ,handling_vote=ApiVote
                                                          }),
                    maybe_start_handler_start_timer(ApiApp, my_app(), ContestId);
                'false' ->
                    lager:debug("API handler update for ~s no longer valid, sticking with ~s(~b)"
                                ,[ContestId, App, Vote]
                               )
            end
    end,
    {'noreply', State, 'hibernate'};
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
-spec handle_info(term(), state()) -> {'noreply', state()} |
                                      {'noreply', state(), 'hibernate'}.
handle_info({'ETS-TRANSFER', _TableId, _From, _GiftData}, State) ->
    lager:debug("recv ETS table ~p from ~p", [_TableId, _From]),
    LoadPid = spawn(?MODULE, 'load_contests', []),
    _AddPid = spawn(?MODULE, 'add_contest_handlers', [LoadPid]),
    lager:debug("loading contests in ~p and adding handlers in ~p", [LoadPid, _AddPid]),
    {'noreply', State#state{is_writable='true'}};
handle_info(?REFRESH_WINDOW_MSG, State) ->
    lager:debug("time to refresh the ETS window"),
    LoadPid = spawn(?MODULE, 'load_contests', []),
    _AddPid = spawn(?MODULE, 'add_contest_handlers', [LoadPid]),
    lager:debug("loading contests in ~p and adding handlers in ~p", [LoadPid, _AddPid]),
    {'noreply', State#state{refresh_ref=start_refresh_timer()}, 'hibernate'};
handle_info(?HANDLER_START_MSG(ContestId), State) ->
    case lookup_by_id(ContestId) of
        'undefined' -> lager:debug("ignoring start_handler msg for contest ~s", [ContestId]);
        #contest{handling_app=App}=Contest ->
            SupPid = case maybe_start_contest_handler(Contest, App, my_app()) of
                         'ok' -> 'undefined';
                         {'ok', Sup} -> Sup
                     end,
            lager:debug("updating contest ~s with supervisor ~p", [ContestId, SupPid]),
            ets:insert(table_id(), Contest#contest{sup=SupPid})
    end,
    {'noreply', State};
handle_info(_Info, State) ->
    lager:debug("unhandled msg: ~p", [_Info]),
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
-spec terminate(term(), state()) -> 'ok'.
terminate(_Reason, _State) ->
    lager:debug("caller10_contests terminating: ~p", [_Reason]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec code_change(term(), state(), term()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec load_contests() -> 'ok'.
load_contests() ->
    wh_util:put_callid(?MODULE),
    case couch_mgr:get_results(<<"contests">>, <<"contests/accounts_listing">>, [{'reduce', 'false'}]) of
        {'ok', []} ->
            lager:debug("no accounts found in the aggregate");
        {'ok', Accounts} ->
            load_contests(Accounts),
            lager:debug("loaded ~p contest accounts", [length(Accounts)]);
        {'error', 'not_found'} ->
            lager:debug("aggregate DB not found or view is missing"),
            caller10_maintenance:refresh_contests_db(),
            load_contests();
        {'error', _E} ->
            lager:debug("unable to load contest accounts: ~p", [_E])
    end.

-spec load_contests(wh_json:objects()) -> 'ok'.
load_contests(Accounts) ->
    [load_account_contests(wh_json:get_value(<<"key">>, Account)) || Account <- Accounts],
    'ok'.

-spec load_account_contests(ne_binary()) -> 'ok'.
load_account_contests(AccountId) ->
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    Now = wh_util:current_tstamp(),
    Options = ['include_docs'
               ,{'startkey', Now}
               ,{'endkey', Now + ?LOAD_WINDOW}
              ],
    case couch_mgr:get_results(AccountDb, <<"contests/listing_by_begin_time">>, Options) of
        {'ok', []} ->
            lager:debug("account ~s is in aggregate, but has no contests coming in the future (after ~b but before ~b)"
                        ,[AccountId, Now, Now + ?LOAD_WINDOW]);
        {'ok', Contests} ->
            _ = [load_account_contest(wh_json:get_value(<<"doc">>, Contest))
                 || Contest <- Contests
                ],
            lager:debug("account ~s: found ~b contests starting after ~b but before ~b"
                        ,[AccountId, length(Contests), Now, Now + ?LOAD_WINDOW]
                       );
        {'error', _E} ->
            lager:debug("failed to load contests from account ~s: ~p", [AccountId, _E])
    end.

-spec load_account_contest(wh_json:object()) -> 'ok'.
load_account_contest(ContestJObj) ->
    gen_server:cast(?MODULE, {'load_contest', jobj_to_record(ContestJObj)}).

-spec jobj_to_record(wh_json:object()) -> contest().
jobj_to_record(JObj) ->
    BeginTime = begin_time(JObj),
    #contest{id = wh_json:get_value(<<"_id">>, JObj)
             ,prior_time = wh_json:get_integer_value(<<"prior_time">>, JObj)
             ,start_time = wh_json:get_integer_value(<<"start_time">>, JObj)
             ,begin_time = BeginTime
             ,cutoff_time = BeginTime - ?CUTOFF_WINDOW
             ,end_time = wh_json:get_integer_value(<<"end_time">>, JObj)
             ,after_offset = wh_json:get_integer_value(<<"after_offset">>, JObj)
             ,doc = JObj
             ,handling_app = 'undefined'
             ,handling_vote = 0
             ,numbers = []
             ,account_id = wh_json:get_value(<<"pvt_account_id">>, JObj)
            }.

-spec start_refresh_timer() -> reference().
start_refresh_timer() ->
    SendAfter = trunc(?LOAD_WINDOW * 1000 * 0.9),
    erlang:send_after(SendAfter, self(), ?REFRESH_WINDOW_MSG).

-spec is_contest_relevant(wh_json:object()) -> boolean().
is_contest_relevant(JObj) ->
    Now = wh_util:current_tstamp(),
    From = Now - ?LOAD_WINDOW,
    To = Now + ?LOAD_WINDOW,

    BeginTime = begin_time(JObj),

    case BeginTime > From orelse BeginTime < To of
        'true' -> 'true';
        'false' ->
            (EndTime = wh_json:get_integer_value(<<"end_time">>, JObj)) =/= 'undefined' andalso EndTime < To
    end.

-spec begin_time(wh_json:object() | contest()) -> pos_integer().
begin_time(#contest{prior_time='undefined'
                    ,begin_time='undefined'
                    ,start_time=StartTime
                   }) ->
    StartTime;
begin_time(#contest{prior_time=PriorTime
                    ,begin_time='undefined'
                   }) ->
    PriorTime;
begin_time(#contest{begin_time=BeginTime}) ->
    BeginTime;
begin_time(JObj) ->
    case wh_json:get_integer_value(<<"prior_time">>, JObj) of
        'undefined' -> wh_json:get_integer_value(<<"start_time">>, JObj);
        PriorTime -> PriorTime
    end.

-spec add_contest_handlers(pid()) -> 'ok'.
add_contest_handlers(LoadPid) ->
    _ = wait_for_loader(LoadPid),
    case contests_needing_handlers() of
        [] -> lager:debug("no contests missing handlers");
        Contests ->
            [publish_contest_handler_request(Contest) || Contest <- Contests],
            lager:debug("published handler requests for ~b contests", [length(Contests)])
    end.

-spec publish_contest_handler_request(contest()) -> 'ok'.
publish_contest_handler_request(#contest{id=Id
                                         ,account_id=AccountId
                                        }) ->
    API = [{<<"Contest-ID">>, Id}
           ,{<<"Account-ID">>, AccountId}
           | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    whapps_util:amqp_pool_send(API, fun wapi_caller10:publish_contest_handler_req/1).

-spec publish_contest_handler_response(contest()) -> 'ok'.
publish_contest_handler_response(#contest{id=Id
                                          ,account_id=AccountId
                                          ,handling_app='undefined'
                                         }) ->
    Vote = random:uniform(100),
    lager:debug("contest ~s has no handling_app, sending ourselves with vote ~b", [Id, Vote]),
    API = [{<<"Contest-ID">>, Id}
           ,{<<"Account-ID">>, AccountId}
           ,{<<"Handling-App">>, my_app()}
           ,{<<"Handling-Vote">>, Vote}
           | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    wapi_caller10:publish_contest_handler_response(API);
publish_contest_handler_response(#contest{id=Id
                                          ,account_id=AccountId
                                          ,handling_app=HandlingApp
                                          ,handling_vote=Vote
                                         }) ->
    Vote = random:uniform(100),
    lager:debug("contest ~s has a handling_app(~s) and vote(~b)", [Id, HandlingApp, Vote]),
    API = [{<<"Contest-ID">>, Id}
           ,{<<"Account-ID">>, AccountId}
           ,{<<"Handling-App">>, HandlingApp}
           ,{<<"Handling-Vote">>, Vote}
           | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    wapi_caller10:publish_contest_handler_response(API).

-spec contests_needing_handlers() -> contests().
contests_needing_handlers() ->
    Now = wh_util:current_tstamp(),
    Match = [{#contest{begin_time='$1'
                       ,handling_app='undefined'
                       ,_='_'
                       }
              ,[{'>', '$1', Now}]
              ,['$$']
             }],
    ets:select(table_id(), Match).

-spec wait_for_loader(pid()) -> 'ok' | 'false'.
wait_for_loader(LoadPid) ->
    Ref = erlang:monitor('process', LoadPid),
    erlang:is_process_alive(LoadPid) andalso
        receive
            {'DOWN', Ref, 'process', LoadPid, _Reason} -> 'ok'
        after 5000 -> wait_for_loader(LoadPid)
        end.

-spec maybe_update_contest_from_response(wh_json:object(), contest()) -> 'ok'.
maybe_update_contest_from_response(JObj, #contest{id=Id
                                                  ,handling_app='undefined'
                                                 }) ->
    lager:debug("we have no handling app for ~s, update contest and try to update it", [Id]),
    gen_server:cast(?MODULE, {'handler_update', Id
                              ,wapi_caller10:handling_app(JObj)
                              ,wapi_caller10:vote(JObj)
                             });
maybe_update_contest_from_response(JObj, #contest{id=Id
                                                  ,handling_app=App
                                                  ,handling_vote=Vote
                                                  ,cutoff_time=Cutoff
                                                 }) ->
    ApiApp = wapi_caller10:handling_app(JObj),
    ApiVote = wapi_caller10:vote(JObj),

    PastCutoff = Cutoff > wh_util:current_tstamp(),

    case should_update_app({App, Vote}, {ApiApp, ApiVote})
        andalso PastCutoff
    of
        'true' ->
            gen_server:cast(?MODULE, {'handler_update', Id, ApiApp, ApiVote});
        'false' ->
            lager:debug("sticking with contest ~s's current app/vote ~s / ~b (past cutoff: ~s)"
                        ,[Id, App, Vote, PastCutoff]
                       )
    end.

-spec should_update_app({ne_binary(), pos_integer()}, {ne_binary(), pos_integer()}) ->
                               boolean().
should_update_app({_App, Vote}, {_ApiApp, ApiVote}) when ApiVote > Vote -> 'true';
should_update_app(_Current, _Api) -> 'false'.

-spec my_app() -> ne_binary().
my_app() ->
    wh_util:to_binary(node()).

-spec maybe_start_handler_start_timer(ne_binary(), ne_binary(), ne_binary()) -> reference().
maybe_start_handler_start_timer(App, App, ContestId) ->
    erlang:start_timer(?HANDLER_START_TIME, self(), ?HANDLER_START_MSG(ContestId)).

-spec maybe_start_contest_handler(contest(), ne_binary(), ne_binary()) ->
                                         'ok' |
                                         {'ok', pid()}.
maybe_start_contest_handler(#contest{id=_Id
                                     ,sup='undefined'
                                     ,doc=JObj
                                    }, App, App) ->
    caller10_contests_sup:new(JObj);
maybe_start_contest_handler(_Contest, _App, _MyApp) ->
    'ok'.
