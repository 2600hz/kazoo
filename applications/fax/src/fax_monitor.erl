%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2016, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   Luis Azedo
%%%-------------------------------------------------------------------
-module(fax_monitor).

-behaviour(gen_server).

-export([start_link/0]).
-export([cleanup_jobs/0]).
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3
        ]).

-include("fax.hrl").

-define(NAME, ?MODULE).
-define(SERVER, {via, kz_globals, ?NAME}).

-define(POLLING_INTERVAL, 5000).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts the server
%%--------------------------------------------------------------------
-spec start_link() -> startlink_ret().
start_link() ->
    case gen_server:start_link(?SERVER, ?MODULE, [], []) of
        {'error', {'already_started', Pid}}
          when is_pid(Pid)->
            erlang:link(Pid),
            {'ok', Pid};
        Other -> Other
    end.

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
    _ = kz_util:spawn(fun cleanup_jobs/0),
    {'ok', 'undefined', ?POLLING_INTERVAL}.

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
    {'reply', {'error', 'not_implemented'}, State, ?POLLING_INTERVAL}.

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
handle_cast(_Msg, State) ->
    lager:debug("unhandled cast: ~p", [_Msg]),
    {'noreply', State, ?POLLING_INTERVAL}.

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
handle_info('timeout', State) ->
    ViewOptions = ['reduce'
                  ,'group'
                  ,{'group_level', 1}
                  ],
    case kz_datamgr:get_result_keys(?KZ_FAXES_DB, <<"faxes/schedule_accounts">>, ViewOptions) of
        {'ok', []} -> {'noreply', State, ?POLLING_INTERVAL};
        {'ok', AccountIds} ->
            _ = distribute_accounts(AccountIds),
            {'noreply', State, ?POLLING_INTERVAL};
        {'error', _Reason} ->
            lager:debug("failed to fetch fax account jobs: ~p", [_Reason]),
            {'noreply', State, ?POLLING_INTERVAL}
    end;
handle_info(_Info, State) ->
    lager:debug("unhandled message: ~p", [_Info]),
    {'noreply', State, ?POLLING_INTERVAL}.

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
    lager:debug("fax jobs terminating: ~p", [_Reason]).

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
-spec distribute_accounts(ne_binaries()) -> ne_binaries().
distribute_accounts([]) -> [];
distribute_accounts([AccountId|AccountIds]) ->
    maybe_start_account(fax_jobs:is_running(AccountId), AccountId),
    distribute_accounts(AccountIds).

-spec maybe_start_account(boolean(), ne_binary()) -> 'ok'.
maybe_start_account('true', _AccountId) -> 'ok';
maybe_start_account('false', AccountId) ->
    lager:debug("sending start fax account jobs for ~s", [AccountId]),
    Payload = [{<<"Account-ID">>, AccountId}
               | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
              ],
    kz_amqp_worker:cast(Payload, fun kapi_fax:publish_start_account/1).

-spec cleanup_jobs() -> 'ok'.
cleanup_jobs() ->
    ViewOptions = [{<<"key">>, kz_util:to_binary(node())}],
    case kz_datamgr:get_results(?KZ_FAXES_DB, <<"faxes/processing_by_node">>, ViewOptions) of
        {'ok', JObjs} ->
            _ = [begin
                     DocId = kz_doc:id(JObj),
                     lager:debug("moving zombie job ~s status to pending", [DocId]),
                     kz_datamgr:update_doc(?KZ_FAXES_DB, DocId, [{<<"pvt_job_status">>, <<"pending">>}])
                 end
                 || JObj <- JObjs
                ],
            'ok';
        {'error', _R} -> lager:debug("unable to cleanup jobs: ~p", [_R])
    end.
