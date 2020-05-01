%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2020, 2600Hz
%%% @doc
%%% @author Hesaam Farhang
%%% @author Pierre Fenoll
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_account_crawler).
-behaviour(gen_server).

-export([start_link/0]).
-export([stop/0, check/1]).
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3
        ]).

-include("tasks.hrl").

-define(SERVER, ?MODULE).

-record(state, {cleanup_ref = cleanup_cycle_timer() :: reference()
               ,account_ids = [] :: kz_term:ne_binaries()
               }).
-type state() :: #state{}.

-define(TIME_BETWEEN_CRAWLS,
        kapps_config:get_integer(?CONFIG_CAT, <<"interaccount_delay_ms">>, 10 * ?MILLISECONDS_IN_SECOND)).

-define(TIME_BETWEEN_WHOLE_CRAWLS,
        kapps_config:get_integer(?CONFIG_CAT, <<"cycle_delay_time_ms">>, 5 * ?MILLISECONDS_IN_MINUTE)).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the server
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    gen_server:start_link({'local', ?MODULE}, ?MODULE, [], []).

-spec stop() -> 'ok'.
stop() ->
    gen_server:cast(?SERVER, 'stop').

-spec check(kz_term:ne_binary()) -> 'ok'.
check(Account)
  when is_binary(Account) ->
    AccountId = kzs_util:format_account_id(Account),
    case kz_datamgr:open_doc(?KZ_ACCOUNTS_DB, AccountId) of
        {'ok', AccountJObj} ->
            process_account(AccountId, AccountJObj);
        {'error', _R} ->
            lager:warning("unable to open account definition for ~s: ~p", [AccountId, _R])
    end;
check(Account) ->
    check(kz_term:to_binary(Account)).

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server.
%% @end
%%------------------------------------------------------------------------------
-spec init([]) -> {'ok', state()}.
init([]) ->
    kz_log:put_callid(?MODULE),
    lager:debug("started ~s", [?MODULE]),
    {'ok', #state{}}.

%%------------------------------------------------------------------------------
%% @doc Handling call messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call(_Request, _From, State) ->
    {'reply', {'error', 'not_implemented'}, State}.

%%------------------------------------------------------------------------------
%% @doc Handling cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast('stop', State) ->
    lager:debug("crawler has been stopped"),
    {'stop', 'normal', State};
handle_cast(_Msg, State) ->
    lager:debug("unhandled cast: ~p", [_Msg]),
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info({'timeout', Ref, _Msg}, #state{cleanup_ref = Ref
                                          ,account_ids = []
                                          }=State) ->
    NewState =
        case kz_datamgr:all_docs(?KZ_ACCOUNTS_DB) of
            {'ok', JObjs} ->
                IDs = [ID || JObj <- JObjs,
                             ?MATCH_ACCOUNT_RAW(ID) <- [kz_doc:id(JObj)]
                      ],
                lager:debug("beginning crawling accounts"),
                State#state{cleanup_ref = cleanup_timer()
                           ,account_ids = kz_term:shuffle_list(IDs)
                           };
            {error, _R} ->
                lager:warning("unable to list all docs in ~s: ~p", [?KZ_ACCOUNTS_DB, _R]),
                State#state{cleanup_ref = cleanup_cycle_timer()}
        end,
    {'noreply', NewState};

handle_info({'timeout', Ref, _Msg}, #state{cleanup_ref = Ref
                                          ,account_ids = [AccountId]
                                          }=State) ->
    _ = crawl_account(AccountId),
    lager:info("account crawler completed a full crawl"),
    {'noreply', State#state{cleanup_ref = cleanup_cycle_timer()
                           ,account_ids = []
                           }};

handle_info({'timeout', Ref, _Msg}, #state{cleanup_ref = Ref
                                          ,account_ids = [AccountId | AccountIds]
                                          }=State) ->
    _ = crawl_account(AccountId),
    {'noreply', State#state{cleanup_ref = cleanup_timer()
                           ,account_ids = AccountIds
                           }};

handle_info(_Info, State) ->
    lager:debug("unhandled msg: ~p", [_Info]),
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc This function is called by a `gen_server' when it is about to
%% terminate. It should be the opposite of `Module:init/1' and do any
%% necessary cleaning up. When it returns, the `gen_server' terminates
%% with Reason. The return value is ignored.
%% @end
%%------------------------------------------------------------------------------
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, _State) ->
    lager:debug("~s terminating: ~p", [?MODULE, _Reason]).

%%------------------------------------------------------------------------------
%% @doc Convert process state when code is changed.
%% @end
%%------------------------------------------------------------------------------
-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec cleanup_timer() -> reference().
cleanup_timer() ->
    erlang:start_timer(?TIME_BETWEEN_CRAWLS, self(), 'ok').

-spec cleanup_cycle_timer() -> reference().
cleanup_cycle_timer() ->
    erlang:start_timer(?TIME_BETWEEN_WHOLE_CRAWLS, self(), 'ok').

-spec crawl_account(kz_term:ne_binary()) -> ok.
crawl_account(AccountId) ->
    lager:debug("crawling account ~s", [AccountId]),
    %% do not open the account def in the account db or we will
    %% be wasting BigCouch file descriptors
    OpenResult = kz_datamgr:open_doc(?KZ_ACCOUNTS_DB, AccountId),
    check_then_process_account(AccountId, OpenResult).

-spec check_then_process_account(kz_term:ne_binary(), {'ok', kzd_accounts:doc()} | {'error',any()}) -> 'ok'.
check_then_process_account(AccountId, {'ok', AccountJObj}) ->
    case kz_doc:is_soft_deleted(AccountJObj)
        orelse not kzd_accounts:is_enabled(AccountJObj) of
        'true' ->
            lager:debug("not processing account ~p (soft-destroyed)", [AccountId]);
        'false' ->
            process_account(AccountId, AccountJObj)
    end;
check_then_process_account(AccountId, {'error', _R}) ->
    lager:warning("unable to open account definition for ~s: ~p", [AccountId, _R]).

-spec process_account(kz_term:ne_binary(), kzd_accounts:doc()) -> 'ok'.
process_account(AccountId, AccountJObj) ->
    lager:debug("account crawler processing account ~s", [AccountId]),
    _ = tasks_bindings:pmap(<<"tasks.account_crawler">>, [AccountId, AccountJObj]),
    'ok'.
