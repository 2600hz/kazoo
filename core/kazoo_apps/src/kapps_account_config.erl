%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2020, 2600Hz
%%% @doc
%%% @author Karl Anderson
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapps_account_config).

-export([get/2, get/3, get/4

        ,get_ne_binary/3, get_ne_binary/4
        ,get_ne_binaries/3, get_ne_binaries/4
        ,get_pos_integer/3, get_pos_integer/4

        ,get_global/2, get_global/3, get_global/4
        ,get_from_reseller/3, get_from_reseller/4
        ,get_with_strategy/4, get_with_strategy/5
        ,get_hierarchy/3, get_hierarchy/4

        ,set/4
        ,set_global/4

        ,flush/2, flush_all_strategies/2, flush/3
        ,migrate/1
        ]).

-include("kazoo_apps.hrl").

-type api_account() :: kz_term:api_ne_binary() | kz_json:object().
-type account_or_not() :: kz_term:ne_binary() | 'no_account_id'.
-type config_key() :: kz_json:path() | kz_json:key().

%% @equiv get_ne_binary(Account, Category, Path, undefined)

-spec get_ne_binary(api_account(), kz_term:ne_binary(), config_key()) -> kz_term:api_ne_binary().
get_ne_binary(Account, Category, Path) ->
    get_ne_binary(Account, Category, Path, 'undefined').

%%------------------------------------------------------------------------------
%% @doc Get a non-empty configuration key for a given category and cast
%% it as a binary.
%% @end
%%------------------------------------------------------------------------------

-spec get_ne_binary(api_account(), kz_term:ne_binary(), config_key(), Default) -> kz_term:ne_binary() | Default.
get_ne_binary(Account, Category, Path, Default) ->
    Value = get(Account, Category, Path, Default),
    case kz_term:is_empty(Value) of
        'true' -> Default;
        'false' -> kz_term:to_binary(Value)
    end.

%% @equiv get_ne_binaries(Account, Category, Path, undefined)

-spec get_ne_binaries(api_account(), kz_term:ne_binary(), config_key()) -> kz_term:ne_binaries().
get_ne_binaries(Account, Category, Path) ->
    get_ne_binaries(Account, Category, Path, 'undefined').

%%------------------------------------------------------------------------------
%% @doc Get a non-empty configuration key for a given category and cast it as
%% a list of binary.
%% @end
%%------------------------------------------------------------------------------

-spec get_ne_binaries(api_account(), kz_term:ne_binary(), config_key(), Default) -> kz_term:ne_binaries() | Default.
get_ne_binaries(Account, Category, Path, Default) ->
    Values = get(Account, Category, Path, Default),
    case kz_term:is_ne_binaries(Values) of
        'false' -> Default;
        'true' -> Values
    end.

%% @equiv get_pos_integer(Account, Category, Path, 'undefined')

-spec get_pos_integer(api_account(), kz_term:ne_binary(), config_key()) -> 'undefined' | pos_integer().
get_pos_integer(Account, Category, Path) ->
    get_pos_integer(Account, Category, Path, 'undefined').

%%------------------------------------------------------------------------------
%% @doc Get a configuration key for a given category and cast it
%% as a `pos_integer'.
%% @end
%%------------------------------------------------------------------------------

-spec get_pos_integer(api_account(), kz_term:ne_binary(), config_key(), Default) -> pos_integer() | Default.
get_pos_integer(Account, Category, Path, Default) ->
    to_pos_integer(get(Account, Category, Path, Default), Default).

-spec to_pos_integer(any(), Default) -> pos_integer() | Default.
to_pos_integer(Value, Default) ->
    try kz_term:to_integer(Value) of
        PosInteger when is_integer(Value), Value > 0 ->
            PosInteger;
        _ -> Default
    catch
        _:_ -> Default
    end.

%% @equiv get_global(Account, Category, Key, undefined)

-spec get_global(api_account(), kz_term:ne_binary(), config_key()) ->
          kz_json:api_json_term().
get_global(Account, Category, Key) ->
    get_global(Account, Category, Key, 'undefined').

%%------------------------------------------------------------------------------
%% @doc Searches for configuration in `Category' and `Key' by first trying
%% the account db then `system_config' for value.
%% @end
%%------------------------------------------------------------------------------

-spec get_global(api_account(), kz_term:ne_binary(), config_key(), Default) ->
          kz_json:json_term() | Default.
get_global(Account, Category, Key, Default) ->
    case load_config_from_account(account_id(Account), Category) of
        {'ok', JObj} ->
            get_global_from_doc(Category, Key, Default, JObj);
        {'error', _} ->
            get_from_reseller(Account, Category, Key, Default)
    end.

%%------------------------------------------------------------------------------
%% @doc Same as {@link get_global/3}, but returns the configuration document of
%% `Category' instead.
%% @see get_global/3
%% @end
%%------------------------------------------------------------------------------

-spec get_global(api_account(), kz_term:ne_binary()) -> kz_json:object().
get_global(Account, Category) ->
    case load_config_from_account(account_id(Account), Category) of
        {'ok', JObj} -> JObj;
        {'error', 'no_account_id'} ->
            maybe_new_doc(load_config_from_system(Account, Category), Category);
        {'error', _} ->
            case load_config_from_reseller(Account, Category) of
                {'ok', JObj} -> JObj;
                {'error', _} ->
                    maybe_new_doc(load_config_from_system(Account, Category), Category)
            end
    end.

%% @equiv get_from_reseller(Account, Category, Key, undefined)

-spec get_from_reseller(api_account(), kz_term:ne_binary(), config_key()) -> kz_json:api_json_term().
get_from_reseller(Account, Category, Key) ->
    get_from_reseller(Account, Category, Key, 'undefined').

%%------------------------------------------------------------------------------
%% @doc Get configuration from reseller (if any) or global config.
%% It would make sure to skip reading from Account (i.e. sub-account of reseller).
%% @end
%%------------------------------------------------------------------------------

-spec get_from_reseller(api_account(), kz_term:ne_binary(), config_key(), kz_json:api_json_term()) -> kz_json:api_json_term().
get_from_reseller(Account, Category, Key, Default) ->
    case maybe_load_config_from_reseller(Account, Category) of
        {'ok', JObj} ->
            get_global_from_doc(Category, Key, Default, JObj);
        {'error', _} ->
            kapps_config:get(Category, Key, Default)
    end.

maybe_load_config_from_reseller(Account, Category) ->
    load_config_from_reseller(Account, Category).

%% @equiv get_hierarchy(Account, Category, Key, undefined)

-spec get_hierarchy(api_account(), kz_term:ne_binary(), config_key()) -> kz_json:json_term().
get_hierarchy(Account, Category, Key) ->
    get_hierarchy(Account, Category, Key, 'undefined').

%% @equiv get_with_strategy(<<"hierarchy_merge">>, Account, Category, Key, Default)

-spec get_hierarchy(api_account(), kz_term:ne_binary(), config_key(), kz_json:api_json_term()) -> kz_json:json_term().
get_hierarchy(Account, Category, Key, Default) ->
    get_with_strategy(<<"hierarchy_merge">>, Account, Category, Key, Default).

%% @equiv get_with_strategy(Strategy, Account, Category, Key, undefined)

-spec get_with_strategy(kz_term:ne_binary(), api_account(), kz_term:ne_binary(), config_key()) ->
          kz_json:json_term().
get_with_strategy(Strategy, Account, Category, Key) ->
    get_with_strategy(Strategy, Account, Category, Key, 'undefined').

%%------------------------------------------------------------------------------
%% @doc Get configuration `Key' for a given `Category' using a given strategy.
%%
%% Strategies are:
%% <dl>
%%   <dt>`<<"global">>'</dt>
%%   <dd>Try to get from account db, if any, otherwise from reseller.
%%   If not found or there is no account id get from `system_config'.
%%   </dd>
%%
%%   <dt>`<<"reseller">>'</dt>
%%   <dd>Try to get from direct reseller, if not found get from `system_config'.
%%   </dd>
%%
%%   <dt>`<<"hierarchy_merge">>'</dt>
%%   <dd>Get from account and parents of the account until reach to the reseller
%%   account, then get from `system_config' and merge all results together.
%%   </dd>
%% </dl>
%%
%% There is merge edition for `global' and `reseller': `global_merge' and `reseller_merge'.
%% These are same as their normal counterpart but they do merge the results at the end.
%% For example in `global_merge', it returns merge result of the account config
%% (if any) and `system_config' (if any).
%%
%% Merge is recursive from right to left, meaning settings in account supersedes
%% settings from parents, reseller and system_config.
%% @end
%%------------------------------------------------------------------------------

-spec get_with_strategy(kz_term:ne_binary(), api_account(), kz_term:ne_binary(), config_key(), kz_json:api_json_term()) ->
          kz_json:json_term().
get_with_strategy(Strategy, Account, Category, Key, Default) ->
    ShouldMerge = is_merge_strategy(Strategy),
    case get_from_strategy_cache(Strategy, account_id(Account), Category, Key, ShouldMerge) of
        {'ok', JObj} ->
            case kz_json:get_value(Key, JObj) of
                'undefined' -> Default;
                Value -> Value
            end;
        {'error', 'no_account_id'} ->
            kapps_config:get(Category, Key, Default);
        {'error', _} ->
            _ = kapps_config:set(Category, Key, Default),
            Default
    end.

-spec get_from_strategy_cache(kz_term:ne_binary(), account_or_not(), config_key(), kz_term:ne_binary(), boolean()) ->
          {'ok', kz_json:object()} |
          {'error', any()}.
get_from_strategy_cache(_, 'no_account_id', _, _, _) ->
    {'error', 'no_account_id'};
get_from_strategy_cache(Strategy, AccountId, Category, Key, 'true') ->
    %% Only read from cache if it is merge strategy
    case kz_cache:fetch_local(?KAPPS_CONFIG_CACHE, strategy_cache_key(AccountId, Category, Strategy)) of
        {'ok', _}=OK ->
            OK;
        {'error', _} ->
            walk_the_walk(strategy_options(Strategy, AccountId, Category, 'true', Key))
    end;
get_from_strategy_cache(Strategy, AccountId, Category, Key, 'false') ->
    walk_the_walk(strategy_options(Strategy, AccountId, Category, 'false', Key)).

%%------------------------------------------------------------------------------
%% @doc Get Key's value from account db document if defined, otherwise get
%% from `system_config'.
%% @end
%%------------------------------------------------------------------------------

-spec get_global_from_doc(kz_term:ne_binary(), config_key(), kz_json:api_json_term(), kz_json:object()) -> kz_json:object().
get_global_from_doc(Category, Key, Default, JObj) ->
    case kz_json:get_value(Key, JObj) of
        'undefined' ->
            kapps_config:get(Category, Key, Default);
        Value ->
            Value
    end.

%% @equiv get(Account, Category, Key, undefined)

-spec get(api_account(), kz_term:ne_binary(), config_key()) -> kz_json:api_json_term().
get(Account, Category, Key) ->
    get(Account, Category, Key, 'undefined').

%%------------------------------------------------------------------------------
%% @doc Get `Key' value at `Category' from account db document if defined.
%% @end
%%------------------------------------------------------------------------------

-spec get(api_account(), kz_term:ne_binary(), config_key(), Default) -> kz_json:json_term() | Default.
get(Account, Category, Key, Default) ->
    kz_json:get_value(Key, get(Account, Category), Default).

%%------------------------------------------------------------------------------
%% @doc Returns `Category' document from account db document if any.
%% @end
%%------------------------------------------------------------------------------

-spec get(api_account(), kz_term:ne_binary()) -> kz_json:object().
get(Account, Category) ->
    case load_config_from_account(account_id(Account), Category) of
        {'ok', JObj} -> JObj;
        {'error', _} -> kz_doc:set_id(kz_json:new(), kapps_config_util:account_doc_id(Category))
    end.

-spec load_config_from_system(kz_term:api_binary(), kz_term:ne_binary()) -> kazoo_data:get_results_return().
load_config_from_system(_Account, Category) ->
    case kapps_config:get_category(Category) of
        {'ok', JObj} ->
            Doc = kz_json:get_json_value(<<"default">>, JObj, kz_json:new()),
            {'ok', Doc};
        {'error', _}=Error -> Error
    end.

-spec load_config_from_reseller(api_account(), kz_term:ne_binary()) -> kazoo_data:get_results_return().
load_config_from_reseller(Account, Category) ->
    AccountId = account_id(Account),
    case AccountId =/= 'no_account_id'
        andalso find_reseller_account(AccountId)
    of
        'false' -> {'error', 'not_found'};
        [] -> {'error', 'not_found'};
        [ResellerId] -> load_config_from_account(ResellerId, Category)
    end.

-spec load_config_from_account(account_or_not(), kz_term:ne_binary()) -> kazoo_data:get_results_return().
load_config_from_account('no_account_id', _Category) ->
    {'error', 'no_account_id'};
load_config_from_account(AccountId, Category) ->
    DocId = kapps_config_util:account_doc_id(Category),
    AccountDb = kzs_util:format_account_db(AccountId),
    kz_datamgr:open_cache_doc(AccountDb, DocId, [{'cache_failures', ['not_found']}]).

%%------------------------------------------------------------------------------
%% @doc Get Accounts parent configuration for the Category.
%%  1. Read account definition
%%      1.1. If failed to read account definition, find its reseller
%%  2. Fold over ancestor Ids and fetch config doc from their db
%%      2.1. If document exists and the account is reseller, return
%%      2.2. If document does not exists:
%%          2.2.1. If the account is reseller return accumulator
%%          2.2.2. If not reseller, continue the fold
%% @end
%%------------------------------------------------------------------------------

-spec load_config_from_ancestors(kz_term:ne_binary(), kz_term:ne_binary()) -> kazoo_data:get_results_return().
load_config_from_ancestors(AccountId, Category) ->
    load_config_from_ancestors(AccountId, Category, kz_services_reseller:is_reseller(AccountId)).

-spec load_config_from_ancestors(kz_term:ne_binary(), kz_term:ne_binary(), boolean()) -> kazoo_data:get_results_return().
load_config_from_ancestors(_, _, 'true') ->
    %% account is reseller, no need to read from its parents
    %% Note: load_config_from_account is already read the config from this AccountId
    {'error', <<"account_is_reseller">>};
load_config_from_ancestors(AccountId, Category, 'false') ->
    %% not reseller, get its ancestors and walk
    Tree = get_account_ancestors_or_reseller(AccountId),
    load_config_from_ancestors_fold(Tree, Category, []).

-spec get_account_ancestors_or_reseller(kz_term:ne_binary()) -> kz_term:ne_binaries().
get_account_ancestors_or_reseller(AccountId) ->
    case get_account_tree(AccountId) of
        [] -> find_reseller_account(AccountId);
        Tree -> lists:reverse(Tree)
    end.

%%------------------------------------------------------------------------------
%% @doc Get accounts config and walk the account up to accounts reseller.
%% @end
%%------------------------------------------------------------------------------

-spec load_config_from_ancestors_fold(kz_term:ne_binaries(), kz_term:ne_binary(), kz_json:objects()) ->
          kazoo_data:get_results_return().
load_config_from_ancestors_fold([], _Category, JObjs) ->
    {'ok', JObjs};
load_config_from_ancestors_fold([ParentId|AncestorIds], Category, JObjs) ->
    case {load_config_from_account(ParentId, Category)
         ,kz_services_reseller:is_reseller(ParentId)
         }
    of
        {{'ok', JObj}, 'true'} ->
            lager:debug("reached to the reseller account ~s (for category ~s)", [ParentId, Category]),
            {'ok', [JObj|JObjs]};
        {{'ok', JObj}, _} ->
            load_config_from_ancestors_fold(AncestorIds, Category, [JObj|JObjs]);
        {{'error', _Reason}, 'true'} ->
            lager:debug("reached to the reseller account ~s (failed to get category ~s: ~p)"
                       ,[ParentId, Category, _Reason]
                       ),
            {'ok', JObjs};
        {{'error', _Reason}, _} ->
            lager:debug("failed to get category ~s for account ~s: ~p", [Category, ParentId, _Reason]),
            load_config_from_ancestors_fold(AncestorIds, Category, JObjs)
    end.

-spec find_reseller_account(kz_term:ne_binary()) -> kz_term:ne_binaries().
find_reseller_account(AccountId) ->
    case kz_services_reseller:get_id(AccountId) of
        'undefined' ->
            lager:debug("failed to find account ~s parents and reseller", [AccountId]),
            [];
        AccountId -> []; %% should get from direct reseller only
        ResellerId -> [ResellerId]
    end.

-spec get_account_tree(kz_term:ne_binary()) -> kz_term:ne_binaries().
get_account_tree(AccountId) ->
    case kzd_accounts:fetch(AccountId) of
        {'ok' , JObj} -> kzd_accounts:tree(JObj);
        {'error', _Reason} ->
            lager:debug("failed to find account ~p parents: ~p", [AccountId, _Reason]),
            []
    end.

%%------------------------------------------------------------------------------
%% @doc Sets the `Value' for the `Key' in account db.
%% @end
%%------------------------------------------------------------------------------

-spec set(api_account(), kz_term:ne_binary(), config_key(), kz_json:json_term()) -> kz_json:object().
set(Account, Category, Key, Value) ->
    maybe_set_account(account_id(Account), Category, Key, Value).

-spec maybe_set_account(account_or_not(), kz_term:ne_binary(), config_key(), kz_json:json_term()) -> kz_json:object().
maybe_set_account('no_account_id', _, Key, Value) ->
    kz_json:set_value(Key, Value, kz_json:new());
maybe_set_account(AccountId, Category, Key, Value) ->
    JObj = kz_json:set_value(Key, Value, get(AccountId, Category)),
    Updates = [{Key, Value} | updates_for_saving(AccountId, JObj)],
    AccountDb = kzs_util:format_account_db(AccountId),
    {'ok', JObj2} = kz_datamgr:update_doc(AccountDb
                                         ,kz_doc:id(JObj)
                                         ,[{'update', Updates}
                                          ,{'ensure_saved', 'true'}
                                          ]),
    JObj2.

%%------------------------------------------------------------------------------
%% @doc Sets the` Value' for the `Key' in account db if found, otherwise gets
%% `Category' document from `system_config' then sets the `Value' and save it
%% in the account db.
%% @end
%%------------------------------------------------------------------------------

-spec set_global(api_account(), kz_term:ne_binary(), config_key(), kz_json:json_term()) -> kz_json:object().
set_global(Account, Category, Key, Value) ->
    set_account_or_merge_global(account_id(Account), Category, Key, Value).

-spec set_account_or_merge_global(account_or_not(), kz_term:ne_binary(), config_key(), kz_json:json_term()) ->
          kz_json:object().
set_account_or_merge_global('no_account_id', _, Key, Value) ->
    kz_json:set_value(Key, Value, kz_json:new());
set_account_or_merge_global(AccountId, Category, Key, Value) ->
    GlobalConfig = get_global(AccountId, Category),

    AccountConfig = kz_json:set_values([{kz_doc:path_id(), kapps_config_util:account_doc_id(Category)}
                                       ,{Key, Value}
                                        | updates_for_saving(AccountId, GlobalConfig)
                                       ]
                                      ,GlobalConfig
                                      ),

    AccountDb = kzs_util:format_account_db(AccountId),
    {'ok', Saved} = kz_datamgr:save_doc(AccountDb, AccountConfig),
    Saved.

-spec updates_for_saving(kz_term:ne_binary(), kz_json:object()) -> kz_term:proplist().
updates_for_saving(AccountId, JObj) ->
    AccountDb = kzs_util:format_account_db(AccountId),
    kz_doc:get_pvt_updates(JObj, AccountDb, [{'type', <<"account_config">>}
                                            ,{'account_id', AccountId}
                                            ]).

%%------------------------------------------------------------------------------
%% @doc Flush account's configuration cache for the given `Category'.
%% @end
%%------------------------------------------------------------------------------

-spec flush(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
flush(Account, Category) ->
    AccountDb = kzs_util:format_account_db(Account),
    kz_datamgr:flush_cache_doc(AccountDb, kapps_config_util:account_doc_id(Category)),
    flush_all_strategies(Account, Category).

%%------------------------------------------------------------------------------
%% @doc Flush configuration cache fetched using all strategy for the given
%% `Category' and `Account'.
%% @end
%%------------------------------------------------------------------------------

-spec flush_all_strategies(kz_term:ne_binary(), kz_term:ne_binary()) -> ok.
flush_all_strategies(Account, Category) ->
    Strategies = [<<"hierarchy_merge">>
                 ,<<"global">>
                 ,<<"reseller">>
                 ,<<"global_merge">>
                 ,<<"reseller_merge">>
                 ],
    lists:foreach(fun(Strategy) -> flush(Account, Category, Strategy) end, Strategies).

%%------------------------------------------------------------------------------
%% @doc Flush configuration cache fetch using `Strategy' for the given
%% `Category' and `Account'.
%% @end
%%------------------------------------------------------------------------------

-spec flush(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> ok.
flush(Account, Category, Strategy) ->
    AccountId = kzs_util:format_account_id(Account),
    CacheKey = strategy_cache_key(AccountId, Category, Strategy),
    kz_cache:erase_local(?KAPPS_CONFIG_CACHE, CacheKey).


%%==============================================================================
%% Internal functions
%%==============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec walk_the_walk(map()) -> {'ok', kz_json:object()} | {'error', not_found}.
walk_the_walk(#{strategy_funs := []
               ,results := []
               }) ->
    {'error', not_found};
walk_the_walk(#{strategy_funs := []
               ,merge := ShouldMerge
               }=Map) ->
    maybe_merge_results(Map, ShouldMerge);
walk_the_walk(#{account_id := AccountId
               ,strategy_funs := [Fun|Funs]
               ,merge := ShouldMerge
               ,category := Category
               ,key := Key
               ,results := Results
               }=Map) ->
    case Fun(AccountId, Category) of
        {'ok', JObj} when not ShouldMerge ->
            %% requester does not want merge result from ancestors and system
            %% returning the result of the first function if defined
            case kz_json:get_value(Key, JObj) of
                'undefined' ->
                    %% key is not defined, continuing the walk
                    walk_the_walk(Map#{results := [JObj|Results], strategy_funs := Funs});
                _Value ->
                    walk_the_walk(Map#{results := [JObj], strategy_funs := []})
            end;
        {'ok', JObjs} when is_list(JObjs) ->
            %% the function returns list (load from ancestor), forcing merge
            walk_the_walk(Map#{results := lists:flatten([JObjs|Results]), strategy_funs := Funs, merge := 'true'});
        {'ok', JObj} ->
            %% requester wants merge result from ancestors and system
            walk_the_walk(Map#{results := [JObj|Results], strategy_funs := Funs});
        {'error', _} ->
            walk_the_walk(Map#{strategy_funs := Funs})
    end.

-spec maybe_merge_results(map(), boolean()) -> {'ok', kz_json:object()}.
maybe_merge_results(#{results := JObjs}=Map, 'true') ->
    store_in_strategy_cache(Map, kz_json:merge_recursive([kz_doc:public_fields(J, 'false') || J <- JObjs]));
maybe_merge_results(#{results := JObjs}, 'false') ->
    {'ok', lists:last(JObjs)}.

-spec store_in_strategy_cache(map(), kz_json:object()) -> {'ok', kz_json:object()}.
store_in_strategy_cache(#{account_id := AccountId
                         ,strategy := Strategy
                         ,category := Category
                         ,results := JObjs
                         }, Result) ->
    CacheKey = strategy_cache_key(AccountId, Category, Strategy),
    Origins = lists:foldl(fun config_origins/2, [], JObjs),
    kz_cache:store_local(?KAPPS_CONFIG_CACHE, CacheKey, Result, [{'origin', Origins}]),
    {'ok', Result}.

-spec config_origins(kz_json:object(), list()) -> list().
config_origins(Doc, Acc) ->
    case {kz_doc:account_db(Doc), kz_doc:account_id(Doc)} of
        {'undefined', 'undefined'} -> Acc;
        {'undefined', DocAccountId} ->
            Db = kzs_util:format_account_db(DocAccountId),
            [{'db', Db, kz_doc:id(Doc)}|Acc];
        {Db, _} -> [{'db', Db, kz_doc:id(Doc)}|Acc]
    end.

-spec strategy_cache_key(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) ->
          {?MODULE, kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()}.
strategy_cache_key(AccountId, Category, Strategy) ->
    {?MODULE, AccountId, Category, Strategy}.

-spec strategy_options(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), boolean(), config_key()) -> map().
strategy_options(Strategy, AccountId, Category, ShouldMerge, Key) ->
    #{account_id => AccountId
     ,strategy => Strategy
     ,strategy_funs => strategy_funs(Strategy)
     ,merge => ShouldMerge
     ,category => Category
     ,results => []
     ,key => Key
     }.

-spec is_merge_strategy(kz_term:ne_binary()) -> boolean().
is_merge_strategy(Strategy) ->
    case kz_binary:reverse(Strategy) of
        <<"egrem", _/binary>> -> 'true';
        _ -> 'false'
    end.

-spec strategy_funs(kz_term:ne_binary()) -> [function()].
strategy_funs(<<"global", _/binary>>) ->
    [fun load_config_from_account/2
    ,fun load_config_from_reseller/2
    ,fun load_config_from_system/2
    ];
strategy_funs(<<"reseller", _/binary>>) ->
    [fun load_config_from_reseller/2
    ,fun load_config_from_system/2
    ];
strategy_funs(<<"hierarchy_merge">>) ->
    [fun load_config_from_account/2
    ,fun load_config_from_ancestors/2
    ,fun load_config_from_system/2
    ].

%%------------------------------------------------------------------------------
%% @doc Find AccountId from binary, Call object or JObj.
%% @end
%%------------------------------------------------------------------------------

-spec account_id(api_account()) -> account_or_not().
account_id(?NE_BINARY=Account) -> kzs_util:format_account_id(Account);
account_id('undefined') -> 'no_account_id';
account_id(Obj) -> account_id_from_jobj(Obj, kz_json:is_json_object(Obj)).

-spec account_id_from_jobj(kz_json:object(), boolean()) -> account_or_not().
account_id_from_jobj(JObj, 'true') ->
    Paths = [<<"Account-ID">>
            ,<<"account_id">>
            ,<<"Account-DB">>
            ,<<"account_db">>
            ],
    maybe_format_account_id(kz_json:get_first_defined(Paths, JObj));
account_id_from_jobj(_Obj, 'false') ->
    lager:debug("unable to find account id from ~p", [_Obj]),
    'no_account_id'.

-spec maybe_format_account_id(kz_term:api_ne_binary()) -> account_or_not().
maybe_format_account_id('undefined') -> 'no_account_id';
maybe_format_account_id(Account) -> kzs_util:format_account_id(Account).

-spec maybe_new_doc({'ok', kz_json:object()} | {'error', any()}, kz_term:ne_binary()) -> kz_json:object().
maybe_new_doc({'ok', JObj}, _) -> JObj;
maybe_new_doc({'error', _}, Category) -> kz_doc:set_id(kz_json:new(), Category).

%%==============================================================================
%% Migrates config settings
%%==============================================================================

-type migrate_setting() :: {kz_term:ne_binary(), config_key()}.
-type migrate_value() :: {kz_term:ne_binary(), kz_term:ne_binary(), config_key(), _}.
-type migrate_values() :: [migrate_value()].

-define(ACCOUNT_CONFIG_MIGRATIONS
       ,[{{<<"callflow">>, <<"default_can_text_self">>}
         ,{<<"kazoo_endpoint">>, <<"default_can_text_self">>}
         }
        ,{{<<"callflow">>, <<"recorder_module">>}
         ,{<<"kazoo_endpoint">>, <<"recorder_module">>}
         }
        ]).

%%------------------------------------------------------------------------------
%% @doc Migrate values from old `Category/Key' to new category and key.
%%
%% Current migrations include:
%% ```
%% callflow.default_can_text_self -> kazoo_endpoint.default_can_text_self
%% callflow.recorder_module -> kazoo_endpoint.recorder_module
%% '''
%%
%% @end
%%------------------------------------------------------------------------------

-spec migrate(kz_term:ne_binary()) -> 'ok'.
migrate(Account) ->
    AccountDb = kzs_util:format_account_db(Account),
    _ = [migrate_config_setting(AccountDb, From, To)
         || {From, To} <- ?ACCOUNT_CONFIG_MIGRATIONS
        ],
    'ok'.

-spec migrate_config_setting(kz_term:ne_binary(), migrate_setting(), migrate_setting()) ->
          'ok' | {'error', any()}.
migrate_config_setting(AccountDb, From, To) ->
    case remove_config_setting(AccountDb, From) of
        {'ok', _, []} -> 'ok';
        {'ok', JObj, Removed} ->
            migrate_config_setting(AccountDb, JObj, Removed, To);
        {'error', 'not_found'} -> 'ok';
        {'error', Reason} -> {'error', {'remove', Reason}}
    end.

-spec migrate_config_setting(kz_term:ne_binary(), kz_json:object(), migrate_values(), migrate_setting()) ->
          'ok' | {'error', any()}.
migrate_config_setting(AccountDb, UpdatedFrom, Removed, To) ->
    case add_config_setting(AccountDb, To, Removed) of
        {'ok', UpdatedTo} ->
            {'ok', _} = kz_datamgr:save_doc(AccountDb, UpdatedTo),
            {'ok', _} = kz_datamgr:save_doc(AccountDb, UpdatedFrom),
            'ok';
        {'error', Reason} -> {'error', {'add', Reason}}
    end.

-spec add_config_setting(kz_term:ne_binary(), migrate_setting(), migrate_values()) ->
          {'ok', kz_json:object()} |
          {'error', any()}.
add_config_setting(AccountDb, {Id, Setting}, Values) ->
    add_config_setting(AccountDb, Id, Setting, Values).

-spec add_config_setting(kz_term:ne_binary(), kz_term:ne_binary() | kz_json:object(), config_key(), migrate_values()) ->
          {'ok', kz_json:object()} |
          {'error', any()}.
add_config_setting(AccountDb, <<Id/binary>>, Setting, Values) ->
    case kz_datamgr:open_doc(AccountDb, Id) of
        {'ok', JObj} -> add_config_setting(AccountDb, JObj, Setting, Values);
        {'error', 'not_found'} ->
            NewConfig = kz_json:set_values([{kz_doc:path_id(), Id} | updates_for_saving(AccountDb, kz_json:new())], kz_json:new()),
            add_config_setting(AccountDb, NewConfig, Setting, Values);
        {'error', _}=Error -> Error
    end;
add_config_setting(_AccountDb, JObj, _, []) -> {'ok', JObj};
add_config_setting(AccountDb, JObj, ToSetting, [{FromId, Node, FromSetting, Value} | Values]) ->
    ToId  = kz_doc:id(JObj),
    Key = config_setting_key(Node, ToSetting),
    case kz_json:get_value(Key, JObj) of
        'undefined' ->
            io:format("migrating setting in ~s from ~s ~s.~s to ~s ~s.~s value ~p~n"
                     ,[AccountDb
                      ,FromId, Node, FromSetting
                      ,ToId, Node, ToSetting
                      ,Value
                      ]
                     ),
            add_config_setting(AccountDb
                              ,kz_json:set_value(Key, Value, JObj)
                              ,ToSetting
                              ,Values
                              );
        Value -> add_config_setting(AccountDb, JObj, ToSetting, Values);
        _Else ->
            io:format("the system tried to move the parameter listed below"
                      " but found a different setting already there,"
                      " you need to correct this disparity manually!~n"
                     ),
            io:format("  Source~n    db: ~s~n    id: ~s~n    key: ~s ~s~n    value: ~p~n"
                     ,[AccountDb, FromId, Node, FromSetting, Value]
                     ),
            io:format("  Destination~n    db: ~s~n    id: ~s~n    key: ~s ~s~n    value: ~p~n"
                     ,[AccountDb, ToId, Node, ToSetting, _Else]
                     ),
            {'error', 'disparity'}
    end.

-spec remove_config_setting(kz_term:ne_binary(), migrate_setting()) ->
          {'ok', kz_json:object(), migrate_values()} |
          {'error', any()}.
remove_config_setting(AccountDb, {Id, Setting}) ->
    remove_config_setting(AccountDb, Id, Setting).

-spec remove_config_setting(kz_term:ne_binary(), kz_term:ne_binary() | kz_json:object(), config_key()) ->
          {'ok', kz_json:object(), migrate_values()} |
          {'error', any()}.
remove_config_setting(AccountDb, Id, Setting) when is_binary(Id) ->
    case kz_datamgr:open_doc(AccountDb, Id) of
        {'ok', JObj} -> remove_config_setting(AccountDb, JObj, Setting);
        {'error', _}=Error -> Error
    end;
remove_config_setting(AccountDb, JObj, Setting) ->
    Id = kz_doc:id(JObj),
    Keys =
        [{Id, Node, Setting}
         || Node <- kz_doc:get_public_keys(JObj)
        ],
    remove_config_setting(AccountDb, Keys, JObj, []).

-spec remove_config_setting(kz_term:ne_binary(), [{kz_term:ne_binary(), kz_term:ne_binary(), config_key()}], kz_json:object(), migrate_values()) ->
          {'ok', kz_json:object(), migrate_values()}.
remove_config_setting(_AccountDb, [], JObj, Removed) ->
    {'ok', JObj, Removed};
remove_config_setting(AccountDb, [{Id, Node, Setting} | Keys], JObj, Removed) ->
    Key = config_setting_key(Node, Setting),
    case kz_json:get_value(Key, JObj) of
        'undefined' -> remove_config_setting(AccountDb, Keys, JObj, Removed);
        Value ->
            remove_config_setting(AccountDb
                                 ,Keys
                                 ,kz_json:delete_key(Key, JObj)
                                 ,[{Id, Node, Setting, Value} | Removed]
                                 )
    end.

-spec config_setting_key(kz_term:ne_binary(), config_key()) -> kz_term:ne_binaries().
%% NOTE: to support nested keys, update this merge function
config_setting_key(Node, Setting) ->
    [Node, Setting].
