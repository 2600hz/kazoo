%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2016-2020, 2600Hz
%%% @doc
%%% @author Pierre Fenoll
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kt_services).

%% behaviour: tasks_provider

-export([init/0
        ,help/1, help/2, help/3
        ,output_header/1
        ]).

%% Verifiers
-export([]).

%% Appliers
-export([descendant_quantities/2]).

%% Triggerables
-export([cleanup/1]).

-include("tasks.hrl").
-include_lib("kazoo_services/include/kazoo_services.hrl").

-define(CATEGORY, "services").
-define(ACTIONS, [<<"descendant_quantities">>
                 ]).

-ifdef(TEST).
-export([rows_for_quantities/5]).
-endif.

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    _ = tasks_bindings:bind(?TRIGGER_SYSTEM, ?MODULE, 'cleanup'),
    _ = tasks_bindings:bind(<<"tasks.help">>, ?MODULE, 'help'),
    _ = tasks_bindings:bind(<<"tasks."?CATEGORY".output_header">>, ?MODULE, 'output_header'),
    tasks_bindings:bind_actions(<<"tasks."?CATEGORY>>, ?MODULE, ?ACTIONS).

-spec output_header(kz_term:ne_binary()) -> kz_tasks:output_header().
output_header(<<"descendant_quantities">>) ->
    [<<"account_id">>
    ,<<"year">>
    ,<<"month">>
    ,<<"category">>
    ,<<"item">>
    ,<<"quantity_bom">>
    ,<<"quantity_eom">>
    ].

-spec help(kz_json:object()) -> kz_json:object().
help(JObj) -> help(JObj, <<?CATEGORY>>).

-spec help(kz_json:object(), kz_term:ne_binary()) -> kz_json:object().
help(JObj, <<?CATEGORY>>=Category) ->
    lists:foldl(fun(Action, J) -> help(J, Category, Action) end, JObj, ?ACTIONS).

-spec help(kz_json:object(), kz_term:ne_binary(), kz_term:ne_binary()) -> kz_json:object().
help(JObj, <<?CATEGORY>>=Category, Action) ->
    kz_json:set_value([Category, Action], kz_json:from_list(action(Action)), JObj).

-spec action(kz_term:ne_binary()) -> kz_term:proplist().
action(<<"descendant_quantities">>) ->
    [{<<"description">>, <<"List per-month descendant accounts quantities">>}
    ,{<<"doc">>, <<"Attempts to create a month-on-month listing of quantities used by descendant accounts.\n"
                   "This task returns the following fields:\n"
                   "* `account_id`: a sub-account of the creator of this task.\n"
                   "* `year`: integral year as 4 characters.\n"
                   "* `month`: integral month as 2 characters (left-padded with a zero).\n"
                   "* `category`: name of the quantity's category.\n"
                   "* `item`: name of the category's item.\n"
                   "* `quantity_bom`: integral quantity's value or empty.\n"
                   "* `quantity_eom`: integral quantity's value or empty.\n"
                   "Note: some beginning-of-month and end-of-month quantities documents may be missing.\n"
                   "Note: when both an account's BoM & EoM documents for a given month are missing, no rows are a created for this month.\n"
                   "Note: in all other cases the documents' value is printed verbatim: if unset the empty string is returned.\n"
                   "E.g.: an integer quantity (such as 1, 10 or 0 (zero)) represents was the system has. If no quantity was found, the empty value is used.\n"
                 >>}
    ].

%%% Verifiers


%%% Appliers

-spec descendant_quantities(kz_tasks:extra_args(), kz_tasks:iterator()) -> kz_tasks:iterator().
descendant_quantities(#{account_id := AccountId}, 'init') ->
    Descendants = kapps_util:account_descendants(AccountId),
    DescendantsMoDBs = lists:flatmap(fun kapps_util:get_account_mods/1, Descendants),
    lager:debug("found ~p descendants & ~p MoDBs in total"
               ,[length(Descendants), length(DescendantsMoDBs)]
               ),
    {'ok', DescendantsMoDBs};
descendant_quantities(_, []) -> 'stop';
descendant_quantities(_, [SubAccountMoDB | DescendantsMoDBs]) ->
    ?MATCH_MODB_SUFFIX_ENCODED(A, B, Rest, YYYY, MM) = SubAccountMoDB,
    AccountId = ?MATCH_ACCOUNT_RAW(A, B, Rest),
    BoM = modb_service_quantities(SubAccountMoDB, ?SERVICES_BOM),
    EoM = modb_service_quantities(SubAccountMoDB, ?SERVICES_EOM),
    case rows_for_quantities(AccountId, YYYY, MM, BoM, EoM) of
        [] ->
            %% No rows generated: ask worker to skip writing for this step.
            {'ok', DescendantsMoDBs};
        Rows -> {Rows, DescendantsMoDBs}
    end.

%%% Triggerables

-spec cleanup(kz_term:ne_binary()) -> 'ok'.
cleanup(?KZ_SERVICES_DB) ->
    lager:debug("checking ~s for abandoned accounts", [?KZ_SERVICES_DB]),
    cleanup_orphaned_services_docs();
cleanup(_SystemDb) -> 'ok'.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec rows_for_quantities(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object(), kz_json:object()) -> [kz_csv:mapped_row()].
rows_for_quantities(AccountId, YYYY, MM, BoM, EoM) ->
    lists:append(
      [quantities_for_items(AccountId, YYYY, MM, Category, BoMItem, EoMItem)
       || Category <- fields(BoM, EoM),
          BoMItem <- [kz_json:get_value(Category, BoM)],
          EoMItem <- [kz_json:get_value(Category, EoM)]
      ]).

-spec quantities_for_items(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:api_object(), kz_term:api_object()) -> [kz_csv:mapped_row()].
quantities_for_items(AccountId, YYYY, MM, Category, BoMItem, EoMItem) ->
    [#{<<"account_id">> => AccountId
      ,<<"year">> => YYYY
      ,<<"month">> => MM
      ,<<"category">> => Category
      ,<<"item">> => Item
      ,<<"quantity_bom">> => maybe_integer_to_binary(Item, BoMItem)
      ,<<"quantity_eom">> => maybe_integer_to_binary(Item, EoMItem)
      }
     || Item <- fields(BoMItem, EoMItem)
    ].

-spec modb_service_quantities(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_json:object().
modb_service_quantities(MoDB, Id) ->
    case kz_datamgr:open_doc(MoDB, Id) of
        {'ok', JObj} -> kz_json:get_value(<<"quantities">>, JObj);
        {'error', _R} ->
            lager:debug("could not fetch ~s in modb ~s: ~p", [Id, MoDB, _R]),
            kz_json:new()
    end.

-spec fields(kz_term:api_object(), kz_term:api_object()) -> kz_term:ne_binaries().
fields('undefined', JObjB) ->
    fields(kz_json:new(), JObjB);
fields(JObjA, 'undefined') ->
    fields(JObjA, kz_json:new());
fields(JObjA, JObjB) ->
    lists:usort(kz_json:get_keys(JObjA) ++ kz_json:get_keys(JObjB)).

-spec maybe_integer_to_binary(kz_term:ne_binary(), kz_term:api_object()) -> kz_term:api_non_neg_integer().
maybe_integer_to_binary(_, 'undefined') -> 'undefined';
maybe_integer_to_binary(Item, JObj) ->
    case kz_json:get_integer_value(Item, JObj) of
        'undefined' -> 'undefined';
        Quantity -> integer_to_binary(Quantity)
    end.

-spec cleanup_orphaned_services_docs() -> 'ok'.
cleanup_orphaned_services_docs() ->
    case kz_datamgr:all_docs(?KZ_SERVICES_DB) of
        {'ok', Docs} -> cleanup_orphaned_services_docs(Docs);
        {'error', _E} ->
            lager:debug("failed to get all docs from ~s: ~p", [?KZ_SERVICES_DB, _E])
    end.

-spec cleanup_orphaned_services_docs(kz_json:objects()) -> 'ok'.
cleanup_orphaned_services_docs([]) -> 'ok';
cleanup_orphaned_services_docs([View|Views]) ->
    cleanup_orphaned_services_doc(View),
    cleanup_orphaned_services_docs(Views).

-spec cleanup_orphaned_services_doc(kz_json:object() | kz_term:ne_binary()) -> 'ok'.
cleanup_orphaned_services_doc(<<"_design/", _/binary>>) -> 'ok';
cleanup_orphaned_services_doc(AccountId=?NE_BINARY) ->
    case kz_datamgr:db_exists(kzs_util:format_account_db(AccountId)) of
        'true' -> 'ok';
        'false' ->
            lager:info("account ~s no longer exists but has a services doc", [AccountId]),
            _ = kz_datamgr:del_doc(?KZ_SERVICES_DB, AccountId),
            timer:sleep(5 * ?MILLISECONDS_IN_SECOND)
    end;
cleanup_orphaned_services_doc(View) ->
    cleanup_orphaned_services_doc(kz_doc:id(View)).

%%% End of Module.
