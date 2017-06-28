%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2017, 2600Hz
%%% @doc
%%% Manage the ETS table separate from the main process to use the ETS table
%%% Protects against the main writer dying
%%%
%%% Inspired by: http://steve.vinoski.net/blog/2011/03/23/dont-lose-your-ets-tables/
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(kazoo_etsmgr_srv).
-behaviour(gen_server).

%% API
-export([start_link/1, start_link/2
        ,default_table_options/0
        ]).

%% gen_server callbacks
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3
        ]).

%% Internal
-export([find_me/2]).

-include_lib("kazoo_types/include/kz_types.hrl").
-include_lib("kazoo/include/kz_log.hrl").

-define(SERVER, ?MODULE).

-define(TABLE_DATA, 0).

-type start_arg() :: {'table_id', atom()} |
                     {'table_options', list()} |
                     {'gift_data', any()} |
                     {'find_me_function', find_me_fun()}.
-type start_args() :: [start_arg()].

-type find_me_fun() :: fun(() -> pid()).
-export_type([start_arg/0, start_args/0
             ,find_me_fun/0
             ]).

-record(state, {table_id :: atom()
               ,give_away_pid :: pid()
               ,find_me_fun :: find_me_fun()
               ,find_me_pid_ref :: {pid(), reference()}
               ,gift_data :: any()
               }).
-type state() :: #state{}.

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts the server
%%--------------------------------------------------------------------
-spec start_link(start_args()) -> startlink_ret().
start_link(Opts) ->
    'true' = valid_options(Opts),
    gen_server:start_link(?SERVER, [Opts], []).

-spec start_link(atom(), start_args()) -> startlink_ret().
start_link(Name, Opts) ->
    'true' = valid_options(Opts),
    gen_server:start_link({'local', Name}, ?MODULE, [Opts], []).

-spec valid_options(start_args()) -> boolean().
valid_options(Opts) ->
    (TID = props:get_value('table_id', Opts)) =/= 'undefined'
        andalso is_atom(TID)
        andalso is_function(props:get_value('find_me_function', Opts), 0)
        orelse props:is_defined('local', Opts).

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
-spec init(list()) -> {'ok', state()}.
init([Opts]) ->
    process_flag('trap_exit', 'true'),

    TableId = opt_table_id(Opts),
    TableOptions = opt_table_options(Opts),

    kz_util:put_callid(<<"etssrv_", (kz_term:to_binary(TableId))/binary>>),
    gen_server:cast(self(), {'begin', TableId, TableOptions}),

    lager:debug("started etsmgr for table ~p", [TableId]),

    {'ok', #state{table_id=TableId
                 ,find_me_fun=opt_find_me_fun(Opts)
                 ,gift_data=opt_gift_data(Opts)
                 }}.

-define(DEFAULT_TABLE_OPTIONS, ['set', 'protected', {'keypos', 2}]).
-spec default_table_options() -> list().
default_table_options() ->
    ?DEFAULT_TABLE_OPTIONS.

opt_table_id(Opts) -> props:get_value('table_id', Opts).
opt_table_options(Opts) ->
    case props:get_value('table_options', Opts) of
        'undefined' -> ?DEFAULT_TABLE_OPTIONS;
        TblOpts when is_list(TblOpts) -> TblOpts
    end.

opt_find_me_fun(Opts) -> props:get_value('find_me_function', Opts).
opt_gift_data(Opts) -> props:get_value('gift_data', Opts, 'ok').

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
handle_call(_Request, _From, State) ->
    lager:debug("unhandled call: ~p", [_Request]),
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
handle_cast({'begin', TableId, TableOptions}, #state{gift_data=GiftData}=State) ->
    TID = ets:new(TableId, TableOptions),

    lager:debug("created new table ~p(~p): ~p", [TableId, TID, TableOptions]),
    ets:setopts(TID, {'heir', self(), GiftData}),
    send_give_away_retry(TID),
    {'noreply', State#state{table_id=TID}};
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
handle_info({'EXIT', Pid, 'killed'}, #state{give_away_pid=Pid}=State) ->
    lager:debug("ets mgr ~p killed", [Pid]),
    {'noreply', State#state{give_away_pid='undefined'}};
handle_info({'EXIT', Pid, 'shutdown'}, #state{give_away_pid=Pid}=State) ->
    lager:debug("ets mgr ~p shutdown", [Pid]),
    {'noreply', State#state{give_away_pid='undefined'}};
handle_info({'ETS-TRANSFER', Tbl, Pid, _Data}, #state{table_id=Tbl
                                                     ,give_away_pid=Pid
                                                     }=State) ->
    lager:debug("ets table ~p transferred back to ourselves", [Tbl]),
    send_give_away_retry(Tbl),
    {'noreply', State#state{give_away_pid='undefined'}};
handle_info({'give_away', Tbl}, #state{table_id=Tbl
                                      ,give_away_pid='undefined'
                                      ,find_me_fun='undefined'
                                      }=State) ->
    lager:debug("no find_me_fun, ets table ~p will live here", [Tbl]),
    {'noreply', State};
handle_info({'give_away', Tbl}, #state{table_id=Tbl
                                      ,give_away_pid='undefined'
                                      ,find_me_fun=F
                                      }=State) ->
    lager:debug("give away ~p", [Tbl]),
    FindMe = kz_util:spawn_monitor(fun find_me/2, [F, self()]),
    lager:debug("finding the successor in ~p", [FindMe]),
    {'noreply', State#state{find_me_pid_ref=FindMe}};
handle_info({'found_me', Pid}, #state{table_id=Tbl
                                     ,give_away_pid='undefined'
                                     ,find_me_pid_ref={_FindMePid, FindMeRef}
                                     ,gift_data=GiftData
                                     }=State) ->
    lager:debug("found our new writer pid: ~p", [Pid]),
    erlang:demonitor(FindMeRef, ['flush']),
    link(Pid),
    ets:give_away(Tbl, Pid, GiftData),
    {'noreply', State#state{give_away_pid=Pid
                           ,find_me_pid_ref='undefined'
                           }, 'hibernate'};
handle_info({'DOWN', Ref, 'process', Pid, _Reason}, #state{table_id=Tbl
                                                          ,find_me_pid_ref={Pid, Ref}
                                                          }=State) ->
    lager:debug("our find_me pid ~p went down: ~p", [Pid, _Reason]),
    send_give_away_retry(Tbl),
    {'noreply', State#state{find_me_pid_ref='undefined'
                           ,give_away_pid='undefined'
                           }};
handle_info(_Info, State) ->
    lager:debug("unhandled message: ~p", [_Info]),
    {'noreply', State}.

send_give_away_retry(Tbl) ->
    self() ! {'give_away', Tbl},
    'ok'.

%% @private
-spec find_me(find_me_fun(), pid()) -> 'ok'.
find_me(Fun, Srv) ->
    lager:debug("trying to find successor for ~p", [Srv]),
    try Fun() of
        P when is_pid(P) ->
            Srv ! {'found_me', P},
            lager:debug("successor ~p found", [P]);
        'undefined' ->
            timer:sleep(250),
            lager:debug("haven't found a successor yet"),
            find_me(Fun, Srv)
    catch
        _E:_R ->
            lager:debug("failed to find successor: ~s: ~p", [_E, _R])
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
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, _State) ->
    lager:debug("ETS mgr going down: ~p", [_Reason]).

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
