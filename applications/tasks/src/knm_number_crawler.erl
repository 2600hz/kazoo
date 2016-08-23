%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
-module(knm_number_crawler).
-behaviour(gen_server).

-export([start_link/0
        ,stop/0
        ,crawl_numbers/0
        ]).

-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3
        ]).

-include_lib("kazoo/include/kz_databases.hrl").
-include_lib("kazoo_number_manager/include/knm_phone_number.hrl").

-define(KNM_CONFIG_CAT, <<"number_manager">>).

-define(DISCOVERY_EXPIRY,
        kapps_config:get_integer(?KNM_CONFIG_CAT, <<"discovery_expiry_d">>, 90)).

-define(DELETED_EXPIRY,
        kapps_config:get_integer(?KNM_CONFIG_CAT, <<"deleted_expiry_d">>, 90)).

-define(NUMBERS_TO_CRAWL,
        kapps_config:get_integer(?SYSCONFIG_COUCH, <<"default_chunk_size">>, 1000)).

-define(TIME_BETWEEN_CRAWLS,
        kapps_config:get_integer(?KNM_CONFIG_CAT, <<"crawler_timer_ms">>, ?MILLISECONDS_IN_DAY)).

-record(state, {cleanup_ref :: reference()
               }).
-type state() :: #state{}.

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> startlink_ret().
start_link() ->
    gen_server:start_link(?MODULE, [], []).

-spec stop() -> 'ok'.
stop() ->
    gen_server:cast(?MODULE, 'stop').

-spec crawl_numbers() -> 'ok'.
crawl_numbers() ->
    kz_util:put_callid(?MODULE),
    lager:debug("beginning a number crawl"),
    lists:foreach(fun crawl_number_db/1, knm_util:get_all_number_dbs()),
    lager:debug("finished the number crawl").

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
-spec init([]) -> {'ok', state()}.
init([]) ->
    kz_util:put_callid(?MODULE),
    lager:debug("started ~s", [?MODULE]),
    {'ok', #state{cleanup_ref=cleanup_timer()}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
-spec handle_call(any(), pid_ref(), state()) -> handle_call_ret_state(state()).
handle_call(_Request, _From, State) ->
    {'reply', {'error', 'not_implemented'}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(any(), state()) -> handle_cast_ret_state(state()).
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
%% @end
%%--------------------------------------------------------------------
-spec handle_info(any(), state()) -> handle_info_ret_state(state()).
handle_info({'timeout', Ref, _Msg}, #state{cleanup_ref=Ref}=State) ->
    _P = kz_util:spawn(fun crawl_numbers/0),
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
%% @end
%%--------------------------------------------------------------------
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, _State) ->
    lager:debug("~s terminating: ~p", [?MODULE, _Reason]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec cleanup_timer() -> reference().
cleanup_timer() ->
    erlang:start_timer(?TIME_BETWEEN_CRAWLS, self(), 'ok').

-spec crawl_number_db(ne_binary()) -> 'ok'.
crawl_number_db(Db) ->
    crawl_number_docs(Db, kz_datamgr:all_docs(Db, ['include_docs'])).

-spec crawl_number_docs(ne_binary(), kz_data:get_results_return()) -> 'ok'.
crawl_number_docs(_Db, {'error', _E}) ->
    lager:debug(" failed to crawl number db ~s: ~p", [_Db, _E]);
crawl_number_docs(_Db, {'ok', Docs}) ->
    lager:debug(" starting to crawl '~s'", [_Db]),
    _ = [crawl_number_doc(
           knm_phone_number:from_json_with_options(kz_json:get_value(<<"doc">>, Doc), [])
          )
         || Doc <- Docs,
            kz_doc:type(Doc) =:= <<"number">>
        ],
    lager:debug(" finished crawling '~s'", [_Db]).

-spec crawl_number_doc(knm_phone_number:knm_phone_number()) -> 'ok'.
crawl_number_doc(PhoneNumber) ->
    Fs = [fun maybe_remove_deleted/1
         ,fun maybe_remove_discovery/1
         ],
    try lists:foldl(fun(F, PN) -> F(PN) end, PhoneNumber, Fs) of
        _ -> 'ok'
    catch
        _E:_R ->
            ST = erlang:get_stacktrace(),
            lager:debug(" '~s' encountered with ~s: ~p"
                       ,[_E, knm_phone_number:number(PhoneNumber), _R]),
            kz_util:log_stacktrace(ST)
    end.

-spec maybe_remove_deleted(knm_phone_number:knm_phone_number()) ->
                                  knm_phone_number:knm_phone_number().
maybe_remove_deleted(PhoneNumber) ->
    case knm_phone_number:state(PhoneNumber) of
        ?NUMBER_STATE_DELETED ->
            Created = knm_phone_number:created(PhoneNumber),
            maybe_remove(PhoneNumber, Created, ?DELETED_EXPIRY * ?SECONDS_IN_DAY);
        _State ->
            PhoneNumber
    end.

-spec maybe_remove_discovery(knm_phone_number:knm_phone_number()) ->
                                    knm_phone_number:knm_phone_number().
maybe_remove_discovery(PhoneNumber) ->
    case knm_phone_number:state(PhoneNumber) of
        ?NUMBER_STATE_DISCOVERY ->
            Created = knm_phone_number:created(PhoneNumber),
            maybe_remove(PhoneNumber, Created, ?DISCOVERY_EXPIRY * ?SECONDS_IN_DAY);
        _State ->
            PhoneNumber
    end.

-spec maybe_remove(knm_phone_number:knm_phone_number(), gregorian_seconds(), pos_integer()) ->
                          knm_phone_number:knm_phone_number().
maybe_remove(PhoneNumber, Created, Expiry) ->
    case (kz_util:current_tstamp() - Expiry) > Created of
        'true' -> PhoneNumber;
        'false' ->
            PN =  knm_phone_number:delete(PhoneNumber),
            lager:debug(" number '~s' was purged from the sytem"
                       ,[knm_phone_number:number(PhoneNumber)]),
            PN
    end.
