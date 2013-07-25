%%%-------------------------------------------------------------------
%%% @author Ben Wann <bwann@tickbook.local>
%%% @copyright (C) 2013, Ben Wann
%%% @doc
%%%
%%% @end
%%% Created : 15 Jul 2013 by Ben Wann <bwann@tickbook.local>
%%%-------------------------------------------------------------------
-module(cdr_v3_migrate_server).

-behaviour(gen_server).

-include("cdr.hrl").

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

-record(state, {
          account_list :: wh_proplist()
          ,date_list :: wh_proplist()
          ,pid :: pid() %% pid of the processing of the response
          ,ref :: reference() %% monitor ref for the pid
         }).

-type state() :: #state{}.

-define(SERVER, ?MODULE). 

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
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

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
    lager:debug("starting to migrate accounts to the new sharded db format"),
    gen_server:cast(self(), 'start_migrate'),
    {'ok', {}}.
   

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
    lager:debug("catchall handle_call executed"),
    Reply = 'ok',
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
handle_cast('start_migrate', {}) ->
    lager:debug("handle_cast: start_migrate called"),
    Accounts  = whapps_util:get_all_accounts(),
    DateList = cdr_v3_migrate_lib:get_n_month_date_list(calendar:universal_time(), 4),
    [FirstAccount | RestAccounts] = Accounts,
    {PID, REF} = spawn_monitor('cdr_v3_migrate_worker', 'migrate_account_cdrs', [FirstAccount, DateList]),
    
    {'noreply', State#state{account_list=RestAccounts, date_list=DateList, pid=PID, ref=REF}};

handle_cast(_Msg, State) ->
    lager:debug("catchall handle_cast executed"),
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
handle_info({'DOWN', Ref, 'process', Pid, Reason}, #state{account_list=Accounts
                                                          ,date_list=DateList
                                                          ,pid=Pid
                                                          ,ref=Ref
                                                         }=State) ->
    lager:debug("response pid ~p(~p) down: ~p", [Pid, Ref, Reason]),
    case Accounts of
        [] -> {'noreply', State};
        _  -> 
            [NextAccount | RestAccounts] = Accounts,
            {NEWPID, NEWREF} = spawn_monitor('cdr_v3_migrate_worker', 'migrate_account_cdrs', [NextAccount, DateList]),
            {'noreply', State#state{account_list=RestAccounts, date_list=DateList, pid=NEWPID, ref=NEWREF}}
    end,
    {'noreply', State#state{pid='undefined',ref='undefined'}, 'hibernate'};
    

handle_info(_Info, State) ->
    lager:debug("catchall handle_info executed"),
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminate
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    'ok'.

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
    
