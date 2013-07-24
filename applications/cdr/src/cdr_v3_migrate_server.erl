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
-export([migrate_account/2]).


%% gen_server callbacks
-export([init/1
	 ,handle_call/3
	 ,handle_cast/2
	 ,handle_info/2
	 ,terminate/2
	 ,code_change/3
	]).

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

-spec migrate_account(ne_binary(), list()) -> 'ok'.
migrate_account(Account, DateList) ->
    AccountDb = wh_util:format_account_id(Account, 'encoded'),
    lists:foreach(fun(Date) -> 
                          migrate_account_day(Account, AccountDb, Date) 
                  end, DateList).			 
    
-spec migrate_account_day(ne_binary(), ne_binary(), {pos_integer(), pos_integer(), any()}) -> any().
migrate_account_day(AccountId, AccountDb, {Year, Month, _}=Date) ->
    ViewOptions = create_view_options(Date),
    case couch_mgr:get_results(AccountDb, <<"cdrs/crossbar_listing">>, ViewOptions) of
        {'ok', []} -> [];
        {'error', _E} -> 
            lager:debug("failed to lookup cdrs for ~s: ~p", [AccountDb, _E]), [];
        {'ok', Cdrs} -> 
            lists:foreach(fun(Cdr) -> 
                                  copy_cdr_to_account_mod(AccountId
                                                          ,AccountDb
                                                          ,Cdr
                                                          ,Year
                                                          ,Month
                                                         ) 
                          end, Cdrs)
    end.
    
-spec copy_cdr_to_account_mod(ne_binary(), ne_binary(), wh_json:object(), pos_integer(), pos_integer()) -> any().
copy_cdr_to_account_mod(AccountId, AccountDb, Cdr, Year, Month) ->
    AccountMODb = wh_util:format_account_id(AccountId, Year, Month),
    MODDocId = cdr_util:get_cdr_doc_id(Year, Month),
    JObj = wh_json:set_value(<<"_id">>, MODDocId, Cdr),
    case cdr_util:save_cdr(AccountMODb, JObj) of
        {'error', 'max_retries'} -> lager:error("could not migrate cdr, max_retries reached");
        'ok' -> 'ok'
    end,
    couch_mgr:save_doc(AccountDb, wh_json:set_value(<<"pvt_deleted">>, true, Cdr)).

-spec create_view_options({{pos_integer(), pos_integer(), pos_integer()},{pos_integer(),pos_integer(),pos_integer()}}) -> list().
create_view_options(Date) ->
    StartTime = calendar:datetime_to_gregorian_seconds({Date, {0,0,0}}),
    EndTime = calendar:datetime_to_gregorian_seconds({Date, {23,59,59}}),
    [{<<"startkey">>, StartTime}, {<<"endkey">>, EndTime}].

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
    gen_server:cast(self(), 'migrate_account'),
    {'noreply', {Accounts, DateList}};

handle_cast('migrate_account', {Accounts, DateList}) ->
    [CurrentAccount | RestAccounts] = Accounts,
    migrate_account(CurrentAccount, DateList),
    case RestAccounts of
	[] -> {'noreply', {}};
        _ -> 
	    gen_server:cast(self(), 'migrate_account'),
	    {'noreply', {RestAccounts, DateList}}
    end;

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
    
