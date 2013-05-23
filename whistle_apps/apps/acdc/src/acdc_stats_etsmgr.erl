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
    put('callid', ?MODULE),
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
    EtsSrv = find_ets_srv(),
    Tbl = ets:new(TableId, TableOptions),

    ets:setopts(Tbl, {'heir', self(), 'ok'}),
    ets:give_away(Tbl, EtsSrv, 'ok'),

    lager:debug("gave away ETS table ~p to ~p", [Tbl, EtsSrv]),
    {'noreply', State#state{table_id=Tbl
                            ,etssrv=EtsSrv
                           }};
handle_cast(_Msg, State) ->
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
handle_info({'EXIT', _OldSrv, _Reason}, State) ->
    lager:debug("old ets srv ~p died: ~p", [_OldSrv, _Reason]),
    {'noreply', State};
handle_info({'ETS-TRANSFER', Tbl, Etssrv, Data}, #state{table_id=Tbl
                                                        ,etssrv=Etssrv
                                                       }=State) ->
    NewEtsSrv = find_ets_srv(),
    lager:debug("ets srv ~p dying, handing tbl ~p back to ~p and then to ~p", [Etssrv, Tbl, self(), NewEtsSrv]),
    ets:give_away(Tbl, NewEtsSrv, Data),
    {'noreply', State#state{etssrv=NewEtsSrv}};

handle_info({'ETS-TRANSFER', Tbl, _P, Data}, #state{table_id=Tbl
                                                        ,etssrv='undefined'
                                                       }=State) ->
    NewEtsSrv = find_ets_srv(),
    lager:debug("ets mgr ~p dying, handing tbl ~p back to ~p and then to ~p", [_P, Tbl, self(), NewEtsSrv]),
    ets:give_away(Tbl, NewEtsSrv, Data),
    {'noreply', State#state{etssrv=NewEtsSrv}};

handle_info(_Info, State) ->
    lager:debug("unhandled message: ~p", [_Info]),
    {'noreply', State}.

find_ets_srv() ->
    case acdc_stats_sup:stats_srv() of
        {'error', 'not_found'} ->
            timer:sleep(5),
            find_ets_srv();
        {'ok', P} when is_pid(P) ->
            link(P),
            P
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
