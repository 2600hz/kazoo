%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600Hz
%%% @doc
%%% Manage the ETS table lookup for token server to account/client IP
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_buckets_mgr).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,terminate/2
         ,code_change/3
        ]).

-include("./src/crossbar.hrl").

-define(SERVER, ?MODULE). 

-record(state, {table_id :: ets:tid()
                ,etsmgr :: pid()
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
init([]) ->
    process_flag('trap_exit', 'true'),
    put('callid', ?MODULE),
    gen_server:cast(self(), 'begin'),
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
handle_cast('begin', State) ->
    Tbl = ets:new(cb_buckets_ets:table_id()
                  ,['protected', 'named_table'
                    ,{'keypos', cb_buckets_ets:key_pos()}
                   ]
                 ),

    ets:setopts(Tbl, {'heir', self(), 'ok'}),

    case find_ets_mgr(Tbl, 'ok') of
        P when is_pid(P) ->
            lager:debug("handing tbl ~p to ~p and then to ~p", [Tbl, self(), P]),
            {'noreply', State#state{table_id=Tbl
                                    ,etsmgr=P
                                    ,give_away_ref='undefined'
                                   }};
        Ref when is_reference(Ref) ->
            lager:debug("waiting for ets mgr to start with ~p", [Ref]),
            {'noreply', State#state{table_id=Tbl
                                    ,etsmgr='undefined'
                                    ,give_away_ref=Ref
                                   }}
    end;
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
handle_info({'EXIT', EtsMgr, 'killed'}, #state{etsmgr=EtsMgr
                                               ,give_away_ref='undefined'
                                              }=State) ->
    lager:debug("ets mgr ~p killed", [EtsMgr]),
    {'noreply', State#state{etsmgr='undefined'
                            ,give_away_ref='undefined'
                           }};
handle_info({'EXIT', EtsMgr, 'shutdown'}, #state{etsmgr=EtsMgr
                                                 ,give_away_ref='undefined'
                                                }=State) ->
    lager:debug("ets mgr ~p shutdown", [EtsMgr]),
    {'noreply', State#state{etsmgr='undefined'
                            ,give_away_ref='undefined'
                           }};
handle_info({'ETS-TRANSFER', Tbl, EtsMgr, Data}, #state{table_id=Tbl
                                                        ,etsmgr=EtsMgr
                                                        ,give_away_ref='undefined'
                                                       }=State) ->
    case find_ets_mgr(Tbl, Data) of
        P when is_pid(P) ->
            lager:debug("ets mgr ~p dying, handing tbl ~p back to ~p and then to ~p", [EtsMgr, Tbl, self(), P]),
            {'noreply', State#state{etsmgr=P}};
        Ref when is_reference(Ref) ->
            lager:debug("ets mgr ~p died, hasn't resumed life yet; waiting", [EtsMgr]),
            {'noreply', State#state{etsmgr='undefined'
                                    ,give_away_ref=Ref
                                   }}
    end;
handle_info({'ETS-TRANSFER', Tbl, _P, Data}, #state{table_id=Tbl
                                                    ,etsmgr='undefined'
                                                    ,give_away_ref='undefined'
                                                   }=State) ->
    case find_ets_mgr(Tbl, Data) of
        P when is_pid(P) ->
            lager:debug("ets mgr ~p dying, handing tbl ~p back to ~p and then to ~p", [_P, Tbl, self(), P]),
            {'noreply', State#state{etsmgr=P}};
        Ref when is_reference(Ref) ->
            lager:debug("ets mgr died already, hasn't resumed life yet; waiting"),
            {'noreply', State#state{etsmgr='undefined'
                                    ,give_away_ref=Ref
                                   }}
    end;

handle_info({'give_away', Tbl, Data}, #state{table_id=Tbl
                                             ,etsmgr='undefined'
                                             ,give_away_ref=Ref
                                            }=State) when is_reference(Ref) ->
    case find_ets_mgr(Tbl, Data) of
        P when is_pid(P) ->
            lager:debug("handing tbl ~p back to ~p and then to ~p", [Tbl, self(), P]),
            {'noreply', State#state{etsmgr=P
                                    ,give_away_ref='undefined'
                                   }};
        Ref when is_reference(Ref) ->
            lager:debug("ets mgr died already, hasn't resumed life yet; waiting"),
            {'noreply', State#state{etsmgr='undefined'
                                    ,give_away_ref=Ref
                                   }}
    end;

handle_info(_Info, State) ->
    lager:debug("unhandled message: ~p", [_Info]),
    {'noreply', State}.

find_ets_mgr(Tbl, Data) ->
    case whereis('cb_buckets_ets') of
        'undefined' ->
            erlang:send_after(10, self(), {'give_away', Tbl, Data});
        P when is_pid(P) ->
            link(P),
            ets:give_away(Tbl, P, Data),
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
    lager:debug("buckets mgr going down: ~p", [_Reason]).

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
