%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
-module(knm_number_crawler).

-behaviour(gen_server).

%% API
-export([start_link/0
         ,stop/0
         ,crawl_numbers/0
        ]).

%% gen_server callbacks
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,terminate/2
         ,code_change/3
        ]).

-include("knm.hrl").

-define(DISCOVERY_EXPIRY
        ,whapps_config:get_integer(?KNM_CONFIG_CAT, <<"discovery_expiry_d">>, 90)
       ).

-define(DELETED_EXPIRY
        ,whapps_config:get_integer(?KNM_CONFIG_CAT, <<"deleted_expiry_d">>, 90)
       ).

-define(NUMBERS_TO_CRAWL
        ,whapps_config:get_integer(?SYSCONFIG_COUCH, <<"default_chunk_size">>, 1000)
       ).

-record(state, {cleanup_ref :: reference()}).

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

-spec stop() -> 'ok'.
stop() ->
    gen_server:cast(?MODULE, 'stop').

crawl_numbers() ->
    wh_util:put_callid(?MODULE),
    lager:debug("beginning a number crawl"),
    _ = [crawl_number_db(Db) || Db <- knm_util:get_all_number_dbs()],
    lager:debug("finished the number crawl").

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
    wh_util:put_callid(?MODULE),
    lager:debug("started ~s", [?MODULE]),
    {'ok', #state{cleanup_ref=cleanup_timer()}}.

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
    {'reply', {'error', 'not_implemented'}, State}.

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
handle_cast('stop', State) ->
    lager:debug("crawler has been stopped"),
    {'stop', 'normal', State};
handle_cast(_Msg, State) ->
    lager:debug("unhandled cast: ~p", [_Msg]),
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
handle_info({'timeout', Ref, _Msg}, #state{cleanup_ref=Ref}=State) ->
    _P = wh_util:spawn(fun crawl_numbers/0),
    {'noreply', State#state{cleanup_ref=cleanup_timer()}};
handle_info(_Msg, State) ->
    lager:debug("unhandled msg: ~p", [_Msg]),
    {'noreply', State}.

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
    lager:debug("~s terminating: ~p", [?MODULE, _Reason]).

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
-spec cleanup_timer() -> reference().
cleanup_timer() ->
    Timeout = whapps_config:get_integer(?KNM_CONFIG_CAT, <<"crawler_timer_ms">>, ?MILLISECONDS_IN_DAY),
    erlang:start_timer(Timeout, self(), 'ok').

-spec crawl_number_db(ne_binary()) -> 'ok'.
crawl_number_db(Db) ->
    crawl_number_docs(Db, couch_mgr:all_docs(Db, ['include_docs'])).

-spec crawl_number_docs(ne_binary(), couch_mgr:get_results_return()) -> 'ok'.
crawl_number_docs(_Db, {'error', _E}) ->
    lager:debug(" failed to crawl number db ~s: ~p", [_Db, _E]);
crawl_number_docs(Db, {'ok', Docs}) ->
    lager:debug(" starting to crawl '~s'", [Db]),
    _ = [crawl_number_doc(
           knm_phone_number:from_json(wh_json:get_value(<<"doc">>, Doc))
          )
         || Doc <- Docs,
            is_number_doc(Doc)
        ],
    lager:debug(" finished crawling '~s'", [Db]).

-spec is_number_doc(wh_json:object()) -> boolean().
is_number_doc(Doc) ->
    case wh_doc:id(Doc) of
        <<"_design/", _/binary>> -> 'false';
        _Id -> 'true'
    end.

-spec crawl_number_doc(knm_phone_number:knm_phone_number()) -> 'ok'.
crawl_number_doc(PhoneNumber) ->
    Fs = [fun maybe_remove_discovery/1
          ,fun maybe_remove_deleted/1
         ],
    try run_crawler_funs(PhoneNumber, Fs) of
        _ -> 'ok'
    catch
        'throw':'number_purged' ->
            lager:debug(" number '~s' was purged from the sytem"
                        ,[knm_phone_number:number(PhoneNumber)]
                       );
        'throw':{'error', _E} ->
            lager:debug(" number '~s' encountered an error: ~p"
                        ,[knm_phone_number:number(PhoneNumber), _E]
                       );
        _E:_R ->
            ST = erlang:get_stacktrace(),
            lager:debug(" '~s' encountered with ~s: ~p"
                        ,[_E, knm_phone_number:number(PhoneNumber), _R]
                       ),
            wh_util:log_stacktrace(ST)
    end.

-spec run_crawler_funs(knm_phone_number:knm_phone_number(), functions()) ->
                              knm_phone_number:knm_phone_number().
run_crawler_funs(PhoneNumber, Fs) ->
    lists:foldl(fun(F, PN) -> F(PN) end
                ,PhoneNumber
                ,Fs
               ).

-spec maybe_remove_discovery(knm_phone_number:knm_phone_number()) ->
                                    knm_phone_number:knm_phone_number().
-spec maybe_remove_discovery(knm_phone_number:knm_phone_number(), ne_binary()) ->
                                    knm_phone_number:knm_phone_number().
maybe_remove_discovery(PhoneNumber) ->
    maybe_remove_discovery(PhoneNumber, knm_phone_number:state(PhoneNumber)).

maybe_remove_discovery(PhoneNumber, ?NUMBER_STATE_DISCOVERY) ->
    JObj = knm_phone_number:doc(PhoneNumber),
    maybe_discovery_number_expired(PhoneNumber, wh_doc:created(JObj));
maybe_remove_discovery(PhoneNumber, _State) ->
    PhoneNumber.

-type created() :: gregorian_seconds() | 'undefined'.

-spec maybe_discovery_number_expired(knm_phone_number:knm_phone_number(), created()) ->
                                            knm_phone_number:knm_phone_number().
maybe_discovery_number_expired(PhoneNumber, 'undefined') ->
    PhoneNumber;
maybe_discovery_number_expired(PhoneNumber, Created) ->
    maybe_remove(PhoneNumber, Created, ?DISCOVERY_EXPIRY * ?SECONDS_IN_DAY).

-spec maybe_remove_deleted(knm_phone_number:knm_phone_number()) ->
                                  knm_phone_number:knm_phone_number().
-spec maybe_remove_deleted(knm_phone_number:knm_phone_number(), ne_binary()) ->
                                  knm_phone_number:knm_phone_number().
maybe_remove_deleted(PhoneNumber) ->
    maybe_remove_deleted(PhoneNumber, knm_phone_number:state(PhoneNumber)).

maybe_remove_deleted(PhoneNumber, ?NUMBER_STATE_DELETED) ->
    JObj = knm_phone_number:doc(PhoneNumber),
    maybe_deleted_number_expired(PhoneNumber, wh_doc:created(JObj));
maybe_remove_deleted(PhoneNumber, _State) ->
    PhoneNumber.

-spec maybe_deleted_number_expired(knm_phone_number:knm_phone_number(), created()) ->
                                          knm_phone_number:knm_phone_number().
maybe_deleted_number_expired(PhoneNumber, 'undefined') ->
    PhoneNumber;
maybe_deleted_number_expired(PhoneNumber, Created) ->
    maybe_remove(PhoneNumber, Created, ?DELETED_EXPIRY * ?SECONDS_IN_DAY).

-spec maybe_remove(knm_phone_number:knm_phone_number(), gregorian_seconds(), pos_integer()) ->
                          knm_phone_number:knm_phone_number().
maybe_remove(PhoneNumber, Created, Expiry) ->
    Now = wh_util:current_tstamp(),
    case (Now - Expiry) > Created of
        'true' -> PhoneNumber;
        'false' -> knm_phone_number:delete(PhoneNumber)
    end.
