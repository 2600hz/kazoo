%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600Hz
%%% @doc
%%% Manage the ETS table lookup for token server to account/client IP
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(kazoo_etsmgr_srv).

-behaviour(gen_server).

%% API
-export([start_link/3]).

%% gen_server callbacks
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,terminate/2
         ,code_change/3
        ]).

-export([find_me/2]).

-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_log.hrl").

-define(SERVER, ?MODULE).
-define(TABLE_DATA, 0).

-type find_me_fun() :: fun(() -> pid()).
-export_type([find_me_fun/0]).

-record(state, {table_id :: ets:tid()
                ,give_away_pid :: pid()
                ,find_me_fun :: find_me_fun()
                ,find_me_pid_ref :: {pid(), reference()}
               }).

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
-spec start_link(ets:tid(), list(), find_me_fun()) -> startlink_ret().
start_link(TableId, TableOptions, FindMeFun) when is_function(FindMeFun, 0) ->
    gen_server:start_link(?MODULE, [TableId, TableOptions, FindMeFun], []).

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
init([TableId, TableOptions, FindMeFun]) ->
    process_flag('trap_exit', 'true'),
    put('callid', <<"etssrv_", (wh_util:to_binary(TableId))/binary>>),
    gen_server:cast(self(), {'begin', TableId, TableOptions}),

    lager:debug("started etsmgr for stats for ~s", [TableId]),

    {'ok', #state{table_id=TableId
                  ,find_me_fun=FindMeFun
                 }}.

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
handle_cast({'begin', TableId, TableOptions}, #state{}=State) ->
    Tbl = ets:new(TableId, TableOptions),

    ets:setopts(Tbl, {'heir', self(), 'ok'}),
    send_give_away_retry(Tbl),
    {'noreply', State#state{table_id=Tbl}};
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
handle_info({'EXIT', Pid, 'killed'}, #state{give_away_pid=Pid}=State) ->
    lager:debug("ets mgr ~p killed", [Pid]),
    {'noreply', State#state{give_away_pid='undefined'}};
handle_info({'EXIT', Pid, 'shutdown'}, #state{give_away_pid=Pid}=State) ->
    lager:debug("ets mgr ~p shutdown", [Pid]),
    {'noreply', State#state{give_away_pid='undefined'}};
handle_info({'ETS-TRANSFER', Tbl, Pid, _Data}, #state{table_id=Tbl
                                                      ,give_away_pid=Pid
                                                     }=State) ->
    lager:debug("ets table ~p transfered back to ourselves", [Tbl]),
    send_give_away_retry(Tbl),
    {'noreply', State#state{give_away_pid='undefined'}};
handle_info({'give_away', Tbl, Data}, #state{table_id=Tbl
                                             ,give_away_pid='undefined'
                                             ,find_me_fun=F
                                            }=State) ->
    lager:debug("give away ~p: ~p", [Tbl, Data]),
    {_P, _R}=FindMe = spawn_monitor(?MODULE, 'find_me', [F, self()]),
    lager:debug("finding the successor in ~p", [FindMe]),
    {'noreply', State#state{find_me_pid_ref=FindMe}};
handle_info({'found_me', Pid}, #state{table_id=Tbl
                                      ,give_away_pid='undefined'
                                     }=State) ->
    lager:debug("found our new writer pid: ~p", [Pid]),
    link(Pid),
    ets:give_away(Tbl, Pid, ?TABLE_DATA),
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
    erlang:send(self(), {'give_away', Tbl, ?TABLE_DATA}).

find_me(Fun, Srv) ->
    P = Fun(),
    'true' = is_pid(P),
    Srv ! {'found_me', P}.

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
    lager:debug("ETS mgr going down: ~p", [_Reason]).

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
