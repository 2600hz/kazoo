%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(crossbar_cleanup).

-behaviour(gen_server).

%% API
-export([start_link/0
         ,cleanup_soft_deletes/1
        ]).

%% gen_server callbacks
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,terminate/2
         ,code_change/3
        ]).

-include_lib("crossbar/include/crossbar.hrl").

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
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

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
    put(callid, ?LOG_SYSTEM_ID),
    {ok, start_cleanup_timer()}.

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
handle_info(cleanup, _Ref) ->
    lager:debug("cleaning up soft deletes"),

    Fs = find_cleanup_methods(),
    AcctDbs = whapps_util:get_all_accounts(encoded),

    lager:debug("cleanup methods: ~p", [Fs]),
    _ = [catch clean_acct(AcctDb, Fs) || AcctDb <- AcctDbs],

    lager:debug("done with cleanup"),
    {noreply, start_cleanup_timer()}.

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
    ok.

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
-spec start_cleanup_timer/0 :: () -> reference().
start_cleanup_timer() ->
    Expiry = whapps_config:get_integer(?CONFIG_CAT, <<"cleanup_timer">>, ?SECONDS_IN_DAY),
    lager:debug("starting cleanup timer for ~b s", [Expiry]),
    erlang:send_after(Expiry * 1000, self(), cleanup).

-spec find_cleanup_methods/0 :: () -> [atom(),...].
find_cleanup_methods() ->
    [F || {F, 1} <- ?MODULE:module_info(exports),
          is_cleanup_method(F)
    ].

-spec is_cleanup_method/1 :: (atom() | ne_binary()) -> boolean().
is_cleanup_method(F) when is_atom(F) ->
    is_cleanup_method(wh_util:to_binary(F));
is_cleanup_method(<<"cleanup_", _/binary>>) -> true;
is_cleanup_method(_) -> false.

-spec clean_acct/2 :: (ne_binary(), [atom(),...]) -> any().
clean_acct(AcctDb, Fs) ->
    [?MODULE:F(AcctDb) || F <- Fs].

-spec cleanup_soft_deletes/1 :: (ne_binary()) -> any().
cleanup_soft_deletes(AcctDb) ->
    case couch_mgr:get_results(AcctDb, <<"maintenance/soft_deleted">>, [{limit, 1000}]) of
        {ok, []} -> ok;
        {ok, L} ->
            lager:debug("removing ~b soft-deleted docs from ~s", [length(L), AcctDb]),
            couch_mgr:del_docs(AcctDb, prepare_docs_for_deletion(L));
        {error, _E} ->
            lager:debug("failed to lookup soft-deleted tokens: ~p", [_E])
    end.

-spec prepare_docs_for_deletion/1 :: (wh_json:objects()) -> wh_json:json_objects().
-spec prepare_doc_for_deletion/1 :: (wh_json:object()) -> wh_json:json_object().
prepare_docs_for_deletion(L) ->
    [prepare_doc_for_deletion(D) || D <- L].
prepare_doc_for_deletion(D) ->
    wh_json:from_list([{<<"_id">>, wh_json:get_value(<<"id">>, D)}
                       ,{<<"_rev">>, wh_json:get_value(<<"value">>, D)}
                      ]).
