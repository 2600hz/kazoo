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
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {current_account_id :: api_binary()
		,account_list :: ne_binaries()
		,date_list :: api_binary()
		,current_date :: api_binary()
		,pid :: pid()
		,ref :: reference()
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
    lager:debug("Starting to migrate accounts to the new sharded db format"),
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
handle_cast('start_migrate', State) ->
    lager:debug("handle_cast: start_migrate called"),

    %%Make Call to Couch to get all Account Documents
    [CurrentAccount | RestAccounts]  = whapps_util:get_all_accounts(),
    [CurrentDate | DateList] = v3_migrate_lib:get_n_month_datetime_list(calendar:local_time(), 4),
    {PID, REF} = spawn_monitor('cdr_v3_migrate_worker', 'migrate_cdr_records', [CurrentAccount, [CurrentDate]]),
    {'reply', State#state{account_list=RestAccounts, current_account_id=CurrentAccount, date_list=DateList, current_date=CurrentDate, pid=PID, ref=REF}};

handle_cast('start_next_worker', #state{account_list=AccountList}=State) ->
    lager:debug("handle_cast: start_next_worker called"),
    lager:debug("AccountList: ~p", [AccountList]),
    lager:debug("State: ~p", [State]).
    

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
handle_info({'DOWN', Ref, 'process', Pid, Reason}, #state{pid=Pid
                                                          ,ref=Ref
                                                         }=State) ->
    lager:debug("response pid ~p(~p) down: ~p", [Pid, Ref, Reason]),
    lager:debug("Placeholder for next call"),
    {'noreply', State#state{pid='undefined'}, 'hibernate'}.

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
    
