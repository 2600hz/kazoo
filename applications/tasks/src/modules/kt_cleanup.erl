%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2020, 2600Hz
%%% @doc
%%% @author Pierre Fenoll
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kt_cleanup).

%% behaviour: tasks_provider
-export([init/0]).

%% Triggerables
-export([cleanup_soft_deletes/1]).

-include("tasks.hrl").

%% How long to pause before attempting to delete the next chunk of soft-deleted docs
-define(SOFT_DELETE_PAUSE
       ,kapps_config:get_integer(?CONFIG_CAT, <<"soft_delete_pause_ms">>, 10 * ?MILLISECONDS_IN_SECOND)
       ).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    _ = tasks_bindings:bind(?TRIGGER_ALL_DBS, ?MODULE, 'cleanup_soft_deletes').

%%% Triggerables

-spec cleanup_soft_deletes(kz_term:ne_binary()) -> ok.
cleanup_soft_deletes(?KZ_ACCOUNTS_DB) ->
    do_cleanup(?KZ_ACCOUNTS_DB);
cleanup_soft_deletes(Account) ->
    kz_datamgr:suppress_change_notice(),
    case kapps_util:is_account_db(Account) of
        'true' -> cleanup_account_soft_deletes(Account);
        'false' ->
            _ = kz_datamgr:enable_change_notice(),
            %% no longer checking other dbs for soft deletes
            'ok'
    end.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec cleanup_account_soft_deletes(kz_term:ne_binary()) -> 'ok'.
cleanup_account_soft_deletes(Account) ->
    AccountDb = kzs_util:format_account_db(Account),
    do_cleanup(AccountDb).

-spec do_cleanup(kz_term:ne_binary()) -> 'ok'.
do_cleanup(Db) ->
    View = <<"maintenance/soft_deletes">>,
    ViewOptions = [{'limit', kz_datamgr:max_bulk_insert()}],
    case kz_datamgr:get_results(Db, View, ViewOptions) of
        {'ok', []} -> 'ok';
        {'ok', L} ->
            lager:debug("removing ~b soft-deleted docs from ~s", [length(L), Db]),
            _ = kz_datamgr:del_docs(Db, L),
            'ok' = timer:sleep(?SOFT_DELETE_PAUSE),
            do_cleanup(Db);
        {'error', 'not_found'} ->
            lager:warning("db ~s or view '~s' not found", [Db, View]);
        {'error', _E} ->
            lager:debug("failed to lookup soft-deleted tokens: ~p", [_E])
    end.

%%% End of Module.
