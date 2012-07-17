%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(j5_reconciler).

-behaviour(gen_server).

-export([start_link/0]).
-export([process_account/1]).
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,terminate/2
         ,code_change/3
        ]).

-include_lib("jonny5/src/jonny5.hrl").

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
    gen_server:start_link(?MODULE, [], []).

-spec process_account/1 :: (ne_binary()) -> 'ok' | {'error', _}. 
process_account(Account) ->
    AccountDb = wh_util:format_account_id(Account, encoded),
    ViewOptions = [reduce, group],
    case couch_mgr:get_results(AccountDb, <<"transactions/reconcile_by_callid">>, ViewOptions) of
        {error, _R}=E ->
            lager:debug("unable to fetch unreconciled call ids from ~s: ~p", [Account, _R]),
            E;
        {ok, JObjs} ->
            _ = [correct_discrepancy(AccountDb, wh_json:get_value(<<"key">>, JObj), Amount)
                 || JObj <- JObjs
                        ,not wh_util:is_empty(Amount = wh_json:get_value(<<"value">>, JObj, 0))
                ],
            ok
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
    gen_server:cast(self(), process_account),
    {ok, []}.

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
    {reply, {error, not_implemented}, State}.

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
handle_cast(process_account, []) ->
    timer:sleep(crypto:rand_uniform(10000, 30000)),
    gen_server:cast(self(), process_account),
    {noreply, shuffle(whapps_util:get_all_accounts())};
handle_cast(process_account, [Account|Accounts]) ->
    lager:debug("attempting to reconcile jonny5 credit/debit for account ~s", [Account]),
    timer:sleep(crypto:rand_uniform(1000, 3000)),
    process_account(Account),
    gen_server:cast(self(), process_account),
    {noreply, Accounts};
handle_cast(_Msg, State) ->
    {noreply, State}.

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
    {noreply, State}.

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
    lager:debug("jonny5 reconciler terminating: ~p", [_Reason]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec correct_discrepancy/3 :: (ne_binary(), ne_binary(), integer()) -> {'ok', wh_json:json_object()} |
                                                                        {'error', _}.
correct_discrepancy(Ledger, CallId, Amount) ->
    case whapps_call_command:channel_status(CallId) of
        {ok, _} -> ok;
        {error, _} ->
            LedgerId = wh_util:format_account_id(Ledger, raw),
            LedgerDb = wh_util:format_account_id(Ledger, encoded),
            Timestamp = wh_util:current_tstamp(),
            Id = <<CallId/binary, "-discrepancy">>,
            Type = case Amount > 0 of true -> <<"debit">>; false -> <<"credit">> end,
            Entry = wh_json:from_list([{<<"_id">>, Id}
                                       ,{<<"reason">>, <<"jonny5 discrepancy correction">>}
                                       ,{<<"account_id">>, LedgerId}
                                       ,{<<"call_id">>, CallId}
                                       ,{<<"amount">>, abs(Amount)}
                                       ,{<<"pvt_account_id">>, LedgerId}
                                       ,{<<"pvt_account_db">>, LedgerDb}
                                       ,{<<"pvt_type">>, wh_util:to_binary(Type)}
                                       ,{<<"pvt_created">>, Timestamp}
                                       ,{<<"pvt_modified">>, Timestamp}
                                       ,{<<"pvt_vsn">>, 1}
                                       ,{<<"pvt_whapp">>, ?APP_NAME}
                                      ]),
            io:format("correcting $~p discrepancy for call ~s on ~s~n", [wapi_money:units_to_dollars(Amount), CallId, LedgerId]),
            couch_mgr:save_doc(LedgerDb, Entry)
    end.

-spec shuffle/1 :: (list()) -> list().
-spec shuffle/2 :: (list(), integer()) -> list().
shuffle(List) ->
    shuffle(List, length(List)).
shuffle(List, Len) ->
    randomize(round(math:log(Len) + 0.5), List).

-spec randomize/2 :: (pos_integer(), list()) -> list().
randomize(1, List) ->
    randomize(List);
randomize(T, List) ->
    lists:foldl(fun(_E, Acc) ->
                        randomize(Acc)
                end, randomize(List), lists:seq(1, (T - 1))).

-spec randomize/1 :: (list()) -> list().
randomize(List) ->
    D = lists:keysort(1, [{random:uniform(), A} || A <- List]),
    {_, D1} = lists:unzip(D),
    D1.
