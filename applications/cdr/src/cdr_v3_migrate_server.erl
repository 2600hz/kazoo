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
-spec handle_call(atom(), any(), state()) -> {_,_,_}.
handle_call('status', _, #state{account_list=AccountsLeft}=_State) ->
    Status = {'num_accounts_left', lists:flatlength(AccountsLeft)},
    {'reply', 'ok', Status};

handle_call(_Request, _From, State) ->
    lager:debug("unhandled handle_call executed ~p~p", [_Request, _From]),
    Reply = 'ok',
    {'reply', Reply, State}.

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
handle_cast('start_migrate', State) ->
    lager:debug("handle_cast: start_migrate called"),
    Accounts  = whapps_util:get_all_accounts(),
    DateList = cdr_v3_migrate_lib:get_n_month_date_list(calendar:universal_time(), 4), 
    gen_server:cast(self(), 'start_next_worker'),
    {'noreply', State#state{account_list=Accounts, date_list=DateList}};

handle_cast('start_next_worker', #state{account_list=[]}=State) ->
    lager:debug("reached end of accounts, exiting..."),
    {'stop', 'normal', State};

handle_cast('start_next_worker', #state{account_list=[NextAccount|RestAccounts]
                                        ,date_list=DateList
                                       }=State) ->
    lager:debug("cdr_v3_migrate_server: handle_cast start_next_worker executed"),
    {NEWPID, NEWREF} = spawn_monitor('cdr_v3_migrate_worker', 'migrate_account_cdrs', [NextAccount, DateList]),
    {'noreply', State#state{account_list=RestAccounts, date_list=DateList, pid=NEWPID, ref=NEWREF}};

handle_cast(_Msg, State) ->
    lager:debug("unhandled call to handle_cast executed"),
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
handle_info({'DOWN', Ref, 'process', Pid, _Reason}, #state{pid=Pid
                                                           ,ref=Ref
                                                          }=State) ->
    lager:debug("response pid ~p(~p) down: ~p", [Pid, Ref, _Reason]),
    gen_server:cast(self(), 'start_next_worker'),
    {'noreply', State#state{pid='undefined',ref='undefined'}};
    

handle_info(_Info, State) ->
    lager:debug("unhanled call to cdr_v3_migrate_server handle_info executed"),
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
    lager:debug("cdr_v3_migrate_server terminated: ~p", [_Reason]),
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
    
