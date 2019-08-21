%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2019, 2600Hz
%%% @doc
%%% @author Pierre Fenoll
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kt_webhooks).

%% behaviour: tasks_provider

-export([init/0
        ]).

%% Triggerables
-export([cleanup/1
        ]).

-include("tasks.hrl").


%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    _ = tasks_bindings:bind(?TRIGGER_SYSTEM, ?MODULE, cleanup).

%%% Triggerables

-spec cleanup(kz_term:ne_binary()) -> 'ok'.
cleanup(?KZ_WEBHOOKS_DB) ->
    lager:debug("checking ~s for abandoned accounts", [?KZ_WEBHOOKS_DB]),
    cleanup_orphaned_hooks();
cleanup(_SystemDb) -> 'ok'.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec cleanup_orphaned_hooks() -> 'ok'.
cleanup_orphaned_hooks() ->
    case kz_datamgr:get_results(?KZ_WEBHOOKS_DB
                               ,<<"webhooks/accounts_listing">>
                               ,['group']
                               )
    of
        {'ok', []} -> lager:debug("no hooks configured");
        {'ok', Accounts} -> cleanup_orphaned_hooks(Accounts);
        {'error', _E} ->
            lager:debug("failed to lookup accounts in ~s: ~p", [?KZ_WEBHOOKS_DB, _E])
    end.

-spec cleanup_orphaned_hooks(kz_json:objects()) -> 'ok'.
cleanup_orphaned_hooks(Accounts) ->
    _Rm = [begin
               delete_account_webhooks(AccountId),
               timer:sleep(5 * ?MILLISECONDS_IN_SECOND)
           end
           || Account <- Accounts,
              begin
                  AccountId = kz_json:get_value(<<"key">>, Account),
                  not kz_datamgr:db_exists(kz_util:format_account_id(AccountId, 'encoded'))
              end
          ],
    _Rm =/= []
        andalso lager:debug("removed ~p accounts' webhooks", [length(_Rm)]),
    'ok'.

-spec delete_account_webhooks(kz_term:ne_binary()) -> 'ok'.
delete_account_webhooks(AccountId) ->
    case fetch_account_hooks(AccountId) of
        {'ok', []} -> 'ok';
        {'error', _E} ->
            lager:debug("failed to fetch webhooks for account ~s: ~p", [AccountId, _E]);
        {'ok', ViewJObjs} ->
            _ = delete_account_hooks(ViewJObjs),
            lager:debug("deleted ~p hooks from account ~s", [length(ViewJObjs), AccountId])
    end.

-spec fetch_account_hooks(kz_term:ne_binary()) -> kazoo_data:get_results_return().
fetch_account_hooks(AccountId) ->
    kz_datamgr:get_results(?KZ_WEBHOOKS_DB
                          ,<<"webhooks/accounts_listing">>
                          ,[{'key', AccountId}
                           ,{'reduce', 'false'}
                           ,'include_docs'
                           ]
                          ).

-spec delete_account_hooks(kz_json:objects()) -> any().
delete_account_hooks(ViewJObjs) ->
    kz_datamgr:del_docs(?KZ_WEBHOOKS_DB
                       ,[kz_json:get_value(<<"doc">>, ViewJObj)
                         || ViewJObj <- ViewJObjs
                        ]
                       ).

%%% End of Module.
