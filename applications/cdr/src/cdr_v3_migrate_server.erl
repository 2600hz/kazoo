%%%-------------------------------------------------------------------
%%% @copyright (c) 2010-2013, 2600Hz
%%% @doc
%%% Migration gen_server used for spawning the workers to migrate
%%% data from the v2.x branch to x3.x branch
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Ben Wann
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
         ,status/1
        ]).

-record(state, {account_list :: wh_proplist()
                ,migrate_date_list :: wh_proplist()
                ,archive_batch_size :: pos_integer()
                ,pid :: pid()
                ,ref :: reference()
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
    lager:debug("cdr_v3_migrate_server init"),
    gen_server:cast(self(), 'start_migrate'),
    {'ok', #state{}}.

-spec status(pid()) -> handle_call_ret() | wh_std_return().
status(ServerPid) ->
    case is_pid(ServerPid) of
        'true' ->
            gen_server:call(ServerPid, 'status');
        'false' -> {'error', 'process_not_found'}
    end.

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
-spec handle_call(atom(), any(), state()) -> handle_call_ret().
handle_call('status', _, #state{account_list=AccountsLeft}=State) ->
    Status = {'num_accounts_left', length(AccountsLeft)},
    {'reply', Status, State};
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
    NumMonthsToShard = whapps_config:get_integer(?CONFIG_CAT, <<"v3_migrate_num_months">>, 4),
    ArchiveBatchSize = whapps_config:get_integer(?CONFIG_CAT, <<"v3_migrate_batch_size">>, 1000),
    Accounts  = whapps_util:get_all_accounts(),
    MigrateDateList = cdr_v3_migrate_lib:get_prev_n_month_date_list(
                        calendar:universal_time()
                        ,NumMonthsToShard),
    gen_server:cast(self(), 'start_next_worker'),
    {'noreply', State#state{account_list=Accounts
                            ,migrate_date_list=MigrateDateList
                            ,archive_batch_size=ArchiveBatchSize
                           }};
handle_cast('start_next_worker', #state{account_list=[]}=State) ->
    lager:debug("reached end of accounts, exiting..."),
    {'stop', 'normal', State};
handle_cast('start_next_worker', #state{account_list=[NextAccount|RestAccounts]
                                        ,migrate_date_list=MigrateDateList
                                        ,archive_batch_size=ArchiveBatchSize
                                       }=State) ->
    {NewPid, NewRef} = spawn_monitor('cdr_v3_migrate_worker'
                                     ,'migrate_account_cdrs'
                                     ,[NextAccount
                                       ,MigrateDateList
                                       ,ArchiveBatchSize
                                      ]),
    {'noreply', State#state{account_list=RestAccounts, pid=NewPid, ref=NewRef}};
handle_cast(_Msg, State) ->
    lager:debug("unhandled handle_cast ~p", [_Msg]),
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
    {'noreply', reset(State)};
handle_info(_Info, State) ->
    lager:debug("unhandled message: ~p", [_Info]),
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
terminate(_Reason, #state{pid=PID}=_State) ->
    lager:debug("cdr_v3_migrate_server terminated: ~p", [_Reason]),
    case is_pid(PID) of
        'false' -> lager:debug("no pid reference");
        'true' -> exit(PID, 'ok')
    end,
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
reset(State) ->
    State#state{pid='undefined'
                ,ref='undefined'}.
