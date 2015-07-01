%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600Hz
%%% @doc
%%% Manage the ETS table lookup for token server to account/client IP
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(acdc_stats_etsmgr).

-behaviour(gen_server).

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,terminate/2
         ,code_change/3
        ]).

-include("acdc.hrl").

-define(SERVER, ?MODULE).

-record(state, {table_id :: ets:tid()
                ,etssrv :: pid()
                ,give_away_ref :: reference()
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
start_link(TableId, TableOptions) ->
    gen_server:start_link(?MODULE, [TableId, TableOptions], []).

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
init([TableId, TableOptions]) ->
    process_flag('trap_exit', 'true'),
    wh_util:put_callid(?MODULE),
    gen_server:cast(self(), {'begin', TableId, TableOptions}),

    lager:debug("started etsmgr for stats for ~s", [TableId]),

    {'ok', #state{}}.

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
handle_cast({'begin', TableId, TableOptions}, State) ->
    Tbl = ets:new(TableId, TableOptions),

    ets:setopts(Tbl, {'heir', self(), 'ok'}),
    {'noreply', State#state{table_id=Tbl
                            ,give_away_ref=send_give_away_retry(Tbl, 'ok', 0)
                           }};
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
handle_info({'EXIT', Etssrv, 'killed'}, #state{etssrv=Etssrv}=State) ->
    lager:debug("ets mgr ~p killed", [Etssrv]),
    {'noreply', State#state{etssrv='undefined'}};
handle_info({'EXIT', EtsMgr, 'shutdown'}, #state{etssrv=EtsMgr}=State) ->
    lager:debug("ets mgr ~p shutdown", [EtsMgr]),
    {'noreply', State#state{etssrv='undefined'}};
handle_info({'EXIT', EtsMgr, _Reason}, #state{etssrv=EtsMgr}=State) ->
    lager:debug("ets mgr ~p exited: ~p", [EtsMgr, _Reason]),
    {'noreply', State#state{etssrv='undefined'}};
handle_info({'ETS-TRANSFER', Tbl, Etssrv, Data}, #state{table_id=Tbl
                                                        ,etssrv=Etssrv
                                                        ,give_away_ref='undefined'
                                                       }=State) ->
    lager:debug("ets table ~p transferred back to ourselves", [Tbl]),
    {'noreply', State#state{etssrv='undefined'
                            ,give_away_ref=send_give_away_retry(Tbl, Data, 0)
                           }};
handle_info({'give_away', Tbl, Data}, #state{table_id=Tbl
                                             ,etssrv='undefined'
                                             ,give_away_ref=Ref
                                            }=State) when is_reference(Ref) ->
    lager:debug("give away ~p: ~p", [Tbl, Data]),
    case find_ets_mgr(Tbl, Data) of
        P when is_pid(P) ->
            lager:debug("handing tbl ~p back to ~p and then to ~p", [Tbl, self(), P]),
            {'noreply', State#state{etssrv=P
                                    ,give_away_ref='undefined'
                                   }};
        Ref when is_reference(Ref) ->
            lager:debug("ets mgr died already, hasn't resumed life yet; waiting"),
            {'noreply', State#state{etssrv='undefined'
                                    ,give_away_ref=Ref
                                   }}
    end;
handle_info({'EXIT', _Pid, _Reason}, State) ->
    {'noreply', State};
handle_info(_Info, State) ->
    lager:debug("unhandled message: ~p", [_Info]),
    {'noreply', State}.

find_ets_mgr(Tbl, Data) ->
    case acdc_stats_sup:stats_srv() of
        {'error', 'not_found'} -> send_give_away_retry(Tbl, Data);
        {'ok', P} when is_pid(P) ->
            link(P),
            ets:give_away(Tbl, P, Data),
            P
    end.

send_give_away_retry(Tbl, Data) ->
    send_give_away_retry(Tbl, Data, 10).
send_give_away_retry(Tbl, Data, Timeout) ->
    erlang:send_after(Timeout, self(), {'give_away', Tbl, Data}).

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
