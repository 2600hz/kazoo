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

-define(SERVER, ?MODULE). 

-record(state, {table_id :: ets:tid()
                ,etsmgr :: pid()
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
    Reply = ok,
    {reply, Reply, State}.

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
    EtsMgr = find_ets_mgr(),
    Tbl = ets:new(cb_buckets_ets:table_id()
                  ,['protected', 'named_table'
                    ,{'keypos', cb_buckets_ets:key_pos()}
                   ]
                 ),

    ets:setopts(Tbl, {'heir', self(), 'ok'}),
    ets:give_away(Tbl, EtsMgr, 'ok'),

    {'noreply', State#state{table_id=Tbl
                            ,etsmgr=EtsMgr
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
handle_info({'EXIT', EtsMgr, 'killed'}, #state{etsmgr=EtsMgr}=State) ->
    lager:debug("ets mgr ~p killed", [EtsMgr]),
    {'noreply', State#state{etsmgr='undefined'}};
handle_info({'ETS-TRANSFER', Tbl, EtsMgr, Data}, #state{table_id=Tbl
                                                        ,etsmgr=EtsMgr
                                                       }=State) ->
    NewEtsMgr = find_ets_mgr(),
    lager:debug("ets mgr ~p dying, handing tbl ~p back to ~p and then to ~p", [EtsMgr, Tbl, self(), NewEtsMgr]),
    ets:give_away(Tbl, NewEtsMgr, Data),
    {'noreply', State#state{etsmgr=NewEtsMgr}};

handle_info({'ETS-TRANSFER', Tbl, _P, Data}, #state{table_id=Tbl
                                                        ,etsmgr='undefined'
                                                       }=State) ->
    NewEtsMgr = find_ets_mgr(),
    lager:debug("ets mgr ~p dying, handing tbl ~p back to ~p and then to ~p", [_P, Tbl, self(), NewEtsMgr]),
    ets:give_away(Tbl, NewEtsMgr, Data),
    {'noreply', State#state{etsmgr=NewEtsMgr}};

handle_info(_Info, State) ->
    lager:debug("unhandled message: ~p", [_Info]),
    {'noreply', State}.

find_ets_mgr() ->
    case whereis('cb_buckets_ets') of
        'undefined' ->
            timer:sleep(5),
            find_ets_mgr();
        P when is_pid(P) ->
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
