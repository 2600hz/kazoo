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
         ,cleanup_heard_voicemail/1
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

-include("crossbar.hrl").

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
handle_info(cleanup, Ref) ->
    lager:debug("cleaning up soft deletes"),
    _ = erlang:cancel_timer(Ref),

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
-spec start_cleanup_timer() -> reference().
start_cleanup_timer() ->
    Expiry = whapps_config:get_integer(?CONFIG_CAT, <<"cleanup_timer">>, ?SECONDS_IN_DAY),
    lager:debug("starting cleanup timer for ~b s", [Expiry]),
    erlang:send_after(Expiry * 1000, self(), cleanup).

-spec find_cleanup_methods() -> [atom(),...].
find_cleanup_methods() ->
    [F || {F, 1} <- ?MODULE:module_info(exports),
          is_cleanup_method(F)
    ].

-spec is_cleanup_method(atom() | ne_binary()) -> boolean().
is_cleanup_method(F) when is_atom(F) ->
    is_cleanup_method(wh_util:to_binary(F));
is_cleanup_method(<<"cleanup_", _/binary>>) -> true;
is_cleanup_method(_) -> false.

-spec clean_acct(ne_binary(), [atom(),...]) -> any().
clean_acct(AcctDb, Fs) ->
    _ = [begin
             Old = put(callid, F),
             catch ?MODULE:F(AcctDb),
             put(callid, Old)
         end || F <- Fs],
    Wait = whapps_config:get_integer(<<"whistle_couch">>, <<"sleep_between_compaction">>, 60000),
    lager:debug("done cleaning account ~s, waiting ~b ms", [AcctDb, Wait]),
    timer:sleep(Wait).

-spec cleanup_soft_deletes(ne_binary()) -> any().
cleanup_soft_deletes(AcctDb) ->
    case couch_mgr:get_results(AcctDb, <<"maintenance/soft_deleted">>, [{limit, 1000}]) of
        {ok, []} -> ok;
        {ok, L} ->
            lager:debug("removing ~b soft-deleted docs from ~s", [length(L), AcctDb]),
            couch_mgr:del_docs(AcctDb, prepare_docs_for_deletion(L));
        {error, _E} ->
            lager:debug("failed to lookup soft-deleted tokens: ~p", [_E])
    end.

cleanup_heard_voicemail(AcctDb) ->
    case whapps_account_config:get(AcctDb
                                   ,<<"callflow">>
                                   ,[<<"voicemail">>, <<"message_retention_duration">>]
                                  ) of
        undefined -> ok;
        Duration ->
            lager:debug("retaining messages for ~p days, delete those older", [Duration]),
            cleanup_heard_voicemail(AcctDb, wh_util:to_integer(Duration))
    end.
cleanup_heard_voicemail(AcctDb, Duration) ->
    Today = wh_util:current_tstamp(),
    DurationS = Duration * 86400, % duration in seconds
    case couch_mgr:get_results(AcctDb, <<"vmboxes/crossbar_listing">>, [include_docs]) of
        {ok, []} -> lager:debug("no voicemail boxes in ~s", [AcctDb]);
        {ok, View} ->
            lager:debug("cleaning up ~b voicemail boxes in ~s", [length(View), AcctDb]),
            cleanup_heard_voicemail(AcctDb
                                    ,Today - DurationS
                                    ,[{wh_json:get_value(<<"doc">>, V)
                                       ,wh_json:get_value([<<"doc">>, <<"messages">>], V)
                                      } || V <- View]
                                   );
        {error, _E} ->
            lager:debug("failed to get voicemail boxes in ~s: ~p", [AcctDb, _E])
    end.
cleanup_heard_voicemail(AcctDb, Timestamp, Boxes) ->
    [cleanup_voicemail_box(AcctDb, Timestamp, Box) || Box <- Boxes].

cleanup_voicemail_box(AcctDb, Timestamp, {Box, Msgs}) ->
    case lists:partition(fun(Msg) ->
                                 %% must be old enough, and not in the NEW folder
                                 wh_json:get_integer_value(<<"timestamp">>, Msg) < Timestamp andalso
                                     wh_json:get_value(<<"folder">>, Msg) =/= <<"new">>
                         end, Msgs) of
        {[], _} -> lager:debug("there are no old messages to remove from ~s", [wh_json:get_value(<<"_id">>, Box)]);
        {Older, Newer} ->
            lager:debug("there are ~b old messages to remove", [length(Older)]),

            _ = [catch delete_media(AcctDb, wh_json:get_value(<<"media_id">>, Msg)) || Msg <- Older],
            lager:debug("soft-deleted old messages"),

            Box1 = wh_json:set_value(<<"messages">>, Newer, Box),
            {ok, Box2} = couch_mgr:save_doc(AcctDb, Box1),
            lager:debug("updated messages in voicemail box ~s", [wh_json:get_value(<<"_id">>, Box2)])
    end.

delete_media(AcctDb, MediaId) ->
    {ok, JObj} = couch_mgr:open_doc(AcctDb, MediaId),
    couch_mgr:ensure_saved(AcctDb, wh_json:set_value(<<"pvt_deleted">>, true, JObj)).

-spec prepare_docs_for_deletion(wh_json:objects()) -> wh_json:objects().
-spec prepare_doc_for_deletion(wh_json:object()) -> wh_json:object().
prepare_docs_for_deletion(L) ->
    [prepare_doc_for_deletion(D) || D <- L].
prepare_doc_for_deletion(D) ->
    wh_json:from_list([{<<"_id">>, wh_json:get_value(<<"id">>, D)}
                       ,{<<"_rev">>, wh_json:get_value(<<"value">>, D)}
                      ]).
