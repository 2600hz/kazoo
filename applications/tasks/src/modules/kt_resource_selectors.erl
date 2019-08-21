%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2016-2019, 2600Hz
%%% @doc
%%% @author Sergey Korobkov
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kt_resource_selectors).

%% behaviour: tasks_provider

-export([init/0
        ,help/1, help/2, help/3
        ,cleanup/2
        ]).

%% Verifiers
-export([account_id/1
        ]).

%% Appliers
-export([import/3
        ,delete/3
        ]).

-include("tasks.hrl").

-define(CATEGORY, "resource_selectors").
-define(ACTIONS, [<<"import">>
                 ,<<"delete">>
                 ]).

-record(state, {db :: kz_term:api_binary()
               ,auth_account_id :: kz_term:api_binary()
               ,bulk_limit = 500 :: non_neg_integer()
               ,processed_rows = 0 :: non_neg_integer()
               ,acc = [] :: kz_term:ne_binaries() | kz_json:objects()
               }).
-type state() :: #state{}.

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec init() -> ok.
init() ->
    _ = tasks_bindings:bind(<<"tasks.help">>, ?MODULE, 'help'),
    _ = tasks_bindings:bind(<<"tasks."?CATEGORY".account_id">>, ?MODULE, 'account_id'),
    _ = tasks_bindings:bind(<<"tasks."?CATEGORY".cleanup">>, ?MODULE, 'cleanup'),
    tasks_bindings:bind_actions(<<"tasks."?CATEGORY>>, ?MODULE, ?ACTIONS).

-spec help(kz_json:object()) -> kz_json:object().
help(JObj) -> help(JObj, <<?CATEGORY>>).

-spec help(kz_json:object(), kz_term:ne_binary()) -> kz_json:object().
help(JObj, <<?CATEGORY>>=Category) ->
    lists:foldl(fun(Action, J) -> help(J, Category, Action) end, JObj, ?ACTIONS).

-spec help(kz_json:object(), kz_term:ne_binary(), kz_term:ne_binary()) -> kz_json:object().
help(JObj, <<?CATEGORY>>=Category, Action) ->
    kz_json:set_value([Category, Action], kz_json:from_list(action(Action)), JObj).

-spec action(kz_term:ne_binary()) -> kz_term:proplist().
action(<<"import">>) ->
    [{<<"description">>, <<"Import stepswitch resource selectors">>}
    ,{<<"doc">>, <<"Creates stepswitch resource selectors from CSV file.">>}
    ,{<<"expected_content">>, <<"text/csv">>}
    ,{<<"mandatory">>, [<<"name">>
                       ,<<"selector">>
                       ,<<"resource">>
                       ]}
    ,{<<"optional">>, [<<"value">>
                      ,<<"start_time">>
                      ,<<"stop_time">>
                      ]}
    ];
action(<<"delete">>) ->
    [{<<"description">>, <<"Delete selectors from DB">>}
    ,{<<"doc">>, <<"Delete selectors from DB">>}
    ,{<<"expected_content">>, <<"text/csv">>}
    ,{<<"mandatory">>, [<<"name">>
                       ,<<"selector">>
                       ,<<"resource">>
                       ]}
    ,{<<"optional">>, []}
    ].

%%% Verifiers
-spec account_id(kz_term:ne_binary()) -> boolean().
account_id(?MATCH_ACCOUNT_RAW(_)) -> 'true';
account_id(_) -> 'false'.

%%% Appliers

-spec import(kz_tasks:extra_args(), kz_tasks:iterator() | state(), kz_tasks:args()) ->
                    {kz_tasks:return(), state()} | 'stop'.
import(#{auth_account_id := AuthBy}=ExtraArgs, 'init', Args) ->
    kz_datamgr:suppress_change_notice(),
    State = #state{db = get_selectors_db(ExtraArgs)
                  ,auth_account_id = AuthBy
                  },
    import(ExtraArgs, State, Args);
import(_ExtraArgs
      ,#state{bulk_limit = Limit
             ,acc = Docs
             ,processed_rows = ProcessedRows
             ,auth_account_id = AuthAccountId
             ,db = Db
             }=State
      ,#{<<"name">> := Name
        ,<<"selector">> := Selector
        ,<<"resource">> := Resource
        ,<<"value">> := Value
        ,<<"start_time">> := Start
        ,<<"stop_time">> := Stop
        }
      ) ->
    Doc = generate_selector_doc(AuthAccountId, Resource, Name, Selector, Value, Start, Stop),
    Acc = case (ProcessedRows + 1) rem Limit =:= 0 of
              'false' -> [Doc | Docs];
              'true' ->
                  do_insert(Db, [Doc | Docs]),
                  []
          end,
    {[], State#state{acc=Acc, processed_rows=ProcessedRows+1}};
import(_ExtraArgs, State, _Args) ->
    lager:error("wrong state: ~p", [State]),
    'stop'.

-spec delete(kz_tasks:extra_args(), kz_tasks:iterator(), kz_tasks:args()) ->
                    {kz_tasks:return(), state()} | 'stop'.
delete(ExtraArgs, 'init', Args) ->
    kz_datamgr:suppress_change_notice(),
    State = #state{db = get_selectors_db(ExtraArgs)},
    delete(ExtraArgs, State, Args);
delete(_ExtraArgs
      ,#state{bulk_limit = Limit
             ,acc = Keys
             ,processed_rows = ProcessedRows
             ,db = Db
             }=State
      ,#{<<"name">> := Name
        ,<<"selector">> := Selector
        ,<<"resource">> := Resource
        }
      ) ->
    Key = [Resource, Name, Selector],
    Acc = case (ProcessedRows + 1) rem Limit =:= 0 of
              'false' -> [Key | Keys];
              'true' ->
                  do_delete(Db, [Key | Keys]),
                  []
          end,
    NewState = State#state{acc = Acc
                          ,processed_rows = ProcessedRows + 1
                          },
    {[], NewState};
delete(_ExtraArgs, State, _Args) ->
    lager:error("wrong state: ~p", [State]),
    'stop'.

-spec cleanup(kz_term:ne_binary(), any()) -> any().
cleanup(<<"import">>, #state{db=Db, acc=Docs}) ->
    do_insert(Db, Docs),
    kz_datamgr:enable_change_notice();
cleanup(<<"delete">>, #state{db=Db, acc=Keys}) ->
    do_delete(Db, Keys),
    kz_datamgr:enable_change_notice().

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec do_insert(kz_term:api_binary(), kz_json:objects()) -> 'ok'.
do_insert(_Db, []) -> 'ok';
do_insert(Db, Docs) ->
    case kz_datamgr:save_docs(Db, Docs) of
        {'ok', _Result} -> refresh_selectors_index(Db);
        {'error', 'not_found'} ->
            init_db(Db),
            do_insert(Db, Docs);
        {'error', E} -> lager:error("error adding selectors: ~p",[E])
    end.

-spec do_delete(kz_term:api_binary(), kz_term:api_list()) -> 'ok'.
do_delete(_Db, []) -> 'ok';
do_delete(Db, DelKeys) ->
    BulkLimit = kz_datamgr:max_bulk_insert(),
    DelKeysBlocks = split_keys(DelKeys, BulkLimit),
    F = fun (Keys, Acc) -> do_delete_fold(Db, Keys, Acc) end,
    DelIDs = lists:foldl(F, [], DelKeysBlocks),
    DelIDsBlocks = split_keys(DelIDs, BulkLimit),
    lists:foreach(fun(Block) ->
                          _Result = kz_datamgr:del_docs(Db, Block),
                          refresh_selectors_index(Db)
                  end
                 ,DelIDsBlocks
                 ).

do_delete_fold(Db, Keys, AccIDs) ->
    Options = [{keys, Keys}, include_docs],
    View = <<"resource_selectors/resource_name_selector_listing">>,
    case kz_datamgr:get_results(Db, View, Options) of
        {'ok', Result} ->
            lists:foldl(fun(R, Acc) ->
                                ID = kz_json:get_ne_value(<<"id">>, R, ""),
                                Rev = kz_json:get_ne_value([<<"doc">>,<<"_rev">>], R, ""),
                                JObj = kz_json:from_list([{<<"_id">>, ID}, {<<"_rev">>, Rev}]),
                                [JObj | Acc]
                        end
                       ,AccIDs
                       ,Result
                       );
        {'error', E} ->
            lager:error("E: ~p", [E]),
            AccIDs
    end.

-spec get_selectors_db(kz_tasks:extra_args()) -> kz_term:api_binary().
get_selectors_db(#{account_id := AccountId
                  ,auth_account_id := AuthAccountId
                  }) ->
    true =  kzd_accounts:is_in_account_hierarchy(AuthAccountId, AccountId, true),
    kz_util:format_resource_selectors_db(AccountId).

-spec split_keys(kz_term:ne_binaries(), non_neg_integer()) -> [kz_term:ne_binaries()].
split_keys(Keys, BlockSize) -> split_keys(Keys, [], BlockSize).

-spec split_keys(kz_term:ne_binaries(), [kz_term:ne_binaries()], non_neg_integer()) -> [kz_term:ne_binaries()].
split_keys([], Acc, _BlockSize) -> Acc;
split_keys(Keys, Acc, BlockSize) when length(Keys) =< BlockSize -> [Keys ++ Acc];
split_keys(Keys, Acc, BlockSize) ->
    {Block, Rest} = lists:split(BlockSize, Keys),
    split_keys(Rest, [Block | Acc], BlockSize).

-spec refresh_selectors_index(kz_term:ne_binary()) -> 'ok'.
refresh_selectors_index(Db) ->
    {'ok', _} = kz_datamgr:all_docs(Db, [{limit, 1}]),
    {'ok', _} = kz_datamgr:get_results(Db, <<"resource_selectors/resource_name_selector_listing">>, [{'limit', 1}]),
    'ok'.

-spec generate_selector_doc(kz_term:ne_binary(), kz_term:api_binary(), kz_term:api_binary(), kz_term:api_binary()
                           ,kz_term:api_binary(), kz_term:api_binary(), kz_term:api_binary()
                           ) ->
                                   kz_json:object().
generate_selector_doc(AuthAccountId, Resource, Name, Selector, Value, Start, Stop) ->
    kz_json:from_list(
      [{<<"pvt_type">>, <<"resource_selector">>}
      ,{<<"name">>, Name}
      ,{<<"selector">>, Selector}
      ,{<<"resource">>, Resource}
      ,{<<"value">>, Value}
      ,{<<"start_time">>, Start}
      ,{<<"stop_time">>, Stop}
      ,{<<"pvt_auth_account_id">>, AuthAccountId}
      ,{<<"pvt_created">>, kz_time:now_s()}
      ]).

-spec init_db(kz_term:ne_binary()) -> 'ok'.
init_db(Db) when is_binary(Db) ->
    _ = kz_datamgr:db_create(Db),
    _ = kapps_maintenance:refresh(Db),
    'ok'.
