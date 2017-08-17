%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2017, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(kapps_account_config).

-include("kazoo_config.hrl").

-export([get/2, get/3, get/4

        ,get_ne_binary/3, get_ne_binary/4
        ,get_ne_binaries/3, get_ne_binaries/4

        ,get_global/2, get_global/3, get_global/4
        ,get_from_reseller/3, get_from_reseller/4
        ,get_with_strategy/4, get_with_strategy/5
        ,get_hierarchy/3, get_hierarchy/4

        ,set/4
        ,set_global/4

        ,flush/2, flush_all_strategies/2, flush/3
        ,migrate/1
        ]).

-type api_account() :: api_ne_binary() | kapps_call:call() | kz_json:object().
-type account_or_not() :: ne_binary() | no_account_id.


%%-----------------------------------------------------------------------------
%% @public
%% @doc
%% Get a non-empty configuration key for a given category and cast it as a binary
%% @end
%%-----------------------------------------------------------------------------
-spec get_ne_binary(api_account(), ne_binary(), kz_json:path()) -> api_ne_binary().
-spec get_ne_binary(api_account(), ne_binary(), kz_json:path(), Default) -> ne_binary() | Default.
get_ne_binary(Account, Category, Path) ->
    get_ne_binary(Account, Category, Path, undefined).
get_ne_binary(Account, Category, Path, Default) ->
    Value = get(Account, Category, Path, Default),
    case kz_term:is_empty(Value) of
        true -> Default;
        false -> kz_term:to_binary(Value)
    end.

%%-----------------------------------------------------------------------------
%% @public
%% @doc
%% Get a non-empty configuration key for a given category and cast it as
%% a list of binary
%% @end
%%-----------------------------------------------------------------------------
-spec get_ne_binaries(api_account(), ne_binary(), kz_json:path()) -> ne_binaries().
-spec get_ne_binaries(api_account(), ne_binary(), kz_json:path(), Default) -> ne_binaries() | Default.
get_ne_binaries(Account, Category, Path) ->
    get_ne_binaries(Account, Category, Path, undefined).
get_ne_binaries(Account, Category, Path, Default) ->
    Values = get(Account, Category, Path, Default),
    case kz_term:is_ne_binaries(Values) of
        false -> Default;
        true -> Values
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Will search the account db first, then system_config for values.
%% @end
%%--------------------------------------------------------------------
-spec get_global(api_account(), ne_binary(), kz_json:path()) ->
                        kz_json:json_term().
-spec get_global(api_account(), ne_binary(), kz_json:path(), kz_json:api_json_term()) ->
                        kz_json:json_term().
get_global(Account, Category, Key) ->
    get_global(Account, Category, Key, undefined).

get_global(Account, Category, Key, Default) ->
    case load_config_from_account(account_id(Account), Category) of
        {ok, JObj} ->
            get_global_from_doc(Category, Key, Default, JObj);
        {error, _} ->
            get_from_reseller(Account, Category, Key, Default)
    end.

-spec get_global(api_account(), ne_binary()) -> kz_json:object().
get_global(Account, Category) ->
    case load_config_from_account(account_id(Account), Category) of
        {ok, JObj} -> JObj;
        {error, no_account_id} ->
            maybe_new_system_doc(load_config_from_system(Account, Category), Category);
        {error, _} ->
            case load_config_from_reseller(Account, Category) of
                {ok, JObj} -> JObj;
                {error, _} ->
                    maybe_new_system_doc(load_config_from_system(Account, Category), Category)
            end
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Get global starting from reseller config.
%% i.e. makes sure to skip reading from Account (i.e. sub-account of reseller).
%% @end
%%--------------------------------------------------------------------
-spec get_from_reseller(api_account(), ne_binary(), kz_json:path()) -> kz_json:api_json_term().
get_from_reseller(Account, Category, Key) ->
    get_from_reseller(Account, Category, Key, undefined).

-spec get_from_reseller(api_account(), ne_binary(), kz_json:path(), kz_json:api_json_term()) -> kz_json:api_json_term().
get_from_reseller(Account, Category, Key, Default) ->
    case load_config_from_reseller(Account, Category) of
        {ok, JObj} ->
            get_global_from_doc(Category, Key, Default, JObj);
        {error, _} ->
            kapps_config:get(Category, Key, Default)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Same as get_hierarchy/4 with Default set to undefined
%% @end
%%--------------------------------------------------------------------
-spec get_hierarchy(api_account(), ne_binary(), kz_json:path()) -> kz_json:json_term().
get_hierarchy(Account, Category, Key) ->
    get_hierarchy(Account, Category, Key, undefined).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Same as get_with_strategy/5 with Strategy "hierarchy_merge"
%% @end
%%--------------------------------------------------------------------
-spec get_hierarchy(api_account(), ne_binary(), kz_json:path(), kz_json:api_json_term()) -> kz_json:json_term().
get_hierarchy(Account, Category, Key, Default) ->
    get_with_strategy(<<"hierarchy_merge">>, Account, Category, Key, Default).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Get a configuration key for a given category with a given strategy.
%%
%% Strategies are:
%%   * global: get from account db if found, otherwise from reseller.
%%             In both cases if not found or there is no account id
%%             get from system_config
%%   * reseller: get from direct reseller, if not found get from system_config
%%   * hierarchy_merge: get from account and parent accounts until reach
%%                      to the reseller account, then get from system_config
%%                      and merge all results together
%%
%%    For global_merge and reseller_merge, do the same above then merge
%%    the results at the end.
%% @end
%%--------------------------------------------------------------------
-spec get_with_strategy(ne_binary(), api_account(), ne_binary(), kz_json:path()) ->
                               kz_json:json_term().
get_with_strategy(Strategy, Account, Category, Key) ->
    get_with_strategy(Strategy, Account, Category, Key, undefined).

-spec get_with_strategy(ne_binary(), api_account(), ne_binary(), kz_json:path(), kz_json:api_json_term()) ->
                               kz_json:json_term().
get_with_strategy(Strategy, Account, Category, Key, Default) ->
    ShouldMerge = is_merge_strategy(Strategy),
    case get_from_strategy_cache(Strategy, account_id(Account), Category, Key, ShouldMerge) of
        {ok, JObj} ->
            case kz_json:get_value(Key, JObj) of
                undefined ->
                    _ = kapps_config:set(Category, Key, Default),
                    Default;
                Value ->
                    Value
            end;
        {error, no_account_id} ->
            kapps_config:get(Category, Key, Default);
        {error, _} ->
            _ = kapps_config:set(Category, Key, Default),
            Default
    end.

-spec get_from_strategy_cache(ne_binary(), account_or_not(), kz_json:path(), ne_binary(), boolean()) ->
                                     {ok, kz_json:object()} |
                                     {error, any()}.
get_from_strategy_cache(_, no_account_id, _, _, _) ->
    {error, no_account_id};
get_from_strategy_cache(Strategy, AccountId, Category, Key, true) ->
    %% Only read from cache if it is merge strategy
    case kz_cache:fetch_local(?KAPPS_CONFIG_CACHE, strategy_cache_key(AccountId, Category, Strategy)) of
        {ok, _}=OK ->
            OK;
        {error, _} ->
            walk_the_walk(strategy_options(Strategy, AccountId, Category, true, Key))
    end;
get_from_strategy_cache(Strategy, AccountId, Category, Key, false) ->
    walk_the_walk(strategy_options(Strategy, AccountId, Category, false, Key)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Get Key's value from account db document if defined, otherwise get
%% from system_config
%% @end
%%--------------------------------------------------------------------
-spec get_global_from_doc(ne_binary(), kz_json:path(), kz_json:api_json_term(), kz_json:object()) -> kz_json:object().
get_global_from_doc(Category, Key, Default, JObj) ->
    case kz_json:get_value(Key, JObj) of
        undefined ->
            kapps_config:get(Category, Key, Default);
        Value ->
            Value
    end.

-spec get(api_account(), ne_binary()) -> kz_json:object().
-spec get(api_account(), ne_binary(), kz_json:path()) -> kz_json:api_json_term().
-spec get(api_account(), ne_binary(), kz_json:path(), Default) -> kz_json:json_term() | Default.

get(Account, Category, Key) ->
    get(Account, Category, Key, undefined).

get(Account, Category, Key, Default) ->
    kz_json:get_value(Key, get(Account, Category), Default).

get(Account, Category) ->
    case load_config_from_account(account_id(Account), Category) of
        {ok, JObj} -> JObj;
        {error, _} -> kz_doc:set_id(kz_json:new(), kapps_config_util:account_doc_id(Category))
    end.

-spec load_config_from_system(api_binary(), ne_binary()) -> kazoo_data:get_results_return().
load_config_from_system(_Account, Category) ->
    case kapps_config:get_category(Category) of
        {ok, JObj} ->
            Doc = kz_json:get_value(<<"default">>, JObj, kz_json:new()),
            {ok, Doc};
        {error, _}=Error -> Error
    end.

-spec load_config_from_reseller(api_account(), ne_binary()) -> kazoo_data:get_results_return().
-ifdef(TEST).
load_config_from_reseller(Account, Category) ->
    AccountId = account_id(Account),
    case AccountId =/= no_account_id
        andalso find_reseller_account(AccountId)
    of
        false -> {error, not_found};
        [] -> {error, not_found};
        [ResellerId] ->
            give_me_something(<<"reseller_jobj">>, ResellerId, Category)
    end.
-else.
load_config_from_reseller(Account, Category) ->
    AccountId = account_id(Account),
    case AccountId =/= no_account_id
        andalso find_reseller_account(AccountId)
    of
        false -> {error, not_found};
        [] -> {error, not_found};
        [ResellerId] -> load_config_from_account(ResellerId, Category)
    end.
-endif.

-spec load_config_from_account(account_or_not(), ne_binary()) -> kazoo_data:get_results_return().
-ifdef(TEST).
load_config_from_account(no_account_id, _Category) ->
    {error, no_account_id};
load_config_from_account(AccountId, Category) ->
    give_me_something(<<"account">>, AccountId, Category).
-else.
load_config_from_account(no_account_id, _Category) ->
    {error, no_account_id};
load_config_from_account(AccountId, Category) ->
    DocId = kapps_config_util:account_doc_id(Category),
    AccountDb = kz_util:format_account_db(AccountId),
    kz_datamgr:open_cache_doc(AccountDb, DocId, [{cache_failures, [not_found]}]).
-endif.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%  Get Accounts parent configuration for the Category
%%  1. Read account definition
%%      1.1. If failed to read account definition, find its reseller
%%  2. Fold over ancestor Ids and fetch config doc from their db
%%      2.1. If document exists and the account is reseller, return
%%      2.2. If document does not exists:
%%          2.2.1. If the account is reseller return accumulator
%%          2.2.2. If not reseller, continue the fold
%% @end
%%--------------------------------------------------------------------
-spec load_config_from_ancestors(ne_binary(), ne_binary()) -> kazoo_data:get_results_return().
load_config_from_ancestors(AccountId, Category) ->
    load_config_from_ancestors(AccountId, Category, is_reseller_account(AccountId)).

-spec load_config_from_ancestors(ne_binary(), ne_binary(), boolean()) -> kazoo_data:get_results_return().
load_config_from_ancestors(_, _, true) ->
    %% account is reseller, no need to read from its parents
    %% Note: load_config_from_account is already read the config from this AccountId
    {error, <<"account_is_reseller">>};
load_config_from_ancestors(AccountId, Category, false) ->
    %% not reseller, get its ancestors and walk
    Tree = get_account_ancestors_or_reseller(AccountId),
    load_config_from_ancestors_fold(Tree, Category, []).

-spec get_account_ancestors_or_reseller(ne_binary()) -> ne_binaries().
get_account_ancestors_or_reseller(AccountId) ->
    case get_account_tree(AccountId) of
        [] -> find_reseller_account(AccountId);
        Tree -> lists:reverse(Tree)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Get accounts config and walk the account up to accounts reseller
%% @end
%%--------------------------------------------------------------------
-spec load_config_from_ancestors_fold(ne_binaries(), ne_binary(), kz_json:objects()) ->
                                             kazoo_data:get_results_return().
load_config_from_ancestors_fold([], _Category, JObjs) ->
    {ok, JObjs};
load_config_from_ancestors_fold([ParentId|AncestorIds], Category, JObjs) ->
    case {load_config_from_account(ParentId, Category)
         ,is_reseller_account(ParentId)
         }
    of
        {{ok, JObj}, true} ->
            lager:debug("reached to the reseller account ~s (for category ~s)", [ParentId, Category]),
            {ok, [JObj|JObjs]};
        {{ok, JObj}, _} ->
            load_config_from_ancestors_fold(AncestorIds, Category, [JObj|JObjs]);
        {{error, _Reason}, true} ->
            lager:debug("reached to the reseller account ~s (failed to get category ~s: ~p)"
                       ,[ParentId, Category, _Reason]
                       ),
            {ok, JObjs};
        {{error, _Reason}, _} ->
            lager:debug("failed to get category ~s for account ~s: ~p", [Category, ParentId, _Reason]),
            load_config_from_ancestors_fold(AncestorIds, Category, JObjs)
    end.

-ifdef(TEST).
is_reseller_account(AccountId) ->
    give_me_something(<<"is_reseller">>, AccountId, undefined).
find_reseller_account(AccountId) ->
    case give_me_something(<<"reseller_id">>, AccountId, undefined) of
        [AccountId] -> [];
        Other -> Other
    end.
get_account_tree(AccountId) ->
    give_me_something(<<"parent_id">>, AccountId, undefined).
-else.
-spec is_reseller_account(ne_binary()) -> boolean().
is_reseller_account(AccountId) ->
    kz_services:is_reseller(AccountId).

-spec find_reseller_account(ne_binary()) -> ne_binaries().
find_reseller_account(AccountId) ->
    case kz_services:find_reseller_id(AccountId) of
        undefined ->
            lager:debug("failed to find account ~s parents and reseller"),
            [];
        AccountId -> []; %% should get from direct reseller only
        ResellerId -> [ResellerId]
    end.

-spec get_account_tree(ne_binary()) -> ne_binaries().
get_account_tree(AccountId) ->
    case kz_account:fetch(AccountId) of
        {ok , JObj} -> kz_account:tree(JObj);
        {error, _Reason} ->
            lager:debug("failed to find account ~p parents: ~p", [AccountId, _Reason]),
            []
    end.
-endif.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Set the Value for the Key in account db
%% @end
%%--------------------------------------------------------------------
-spec set(api_account(), ne_binary(), kz_json:path(), kz_json:json_term()) -> kz_json:object().
-ifdef(TEST).
set(_, _, Key, Value) ->
    kz_json:set_value(Key, Value, kz_json:new()).
-else.
set(Account, Category, Key, Value) ->
    maybe_set_account(account_id(Account), Category, Key, Value).

-spec maybe_set_account(account_or_not(), ne_binary(), kz_json:path(), kz_json:json_term()) -> kz_json:object().
maybe_set_account(no_account_id, _, Key, Value) ->
    kz_json:set_value(Key, Value, kz_json:new());
maybe_set_account(AccountId, Category, Key, Value) ->
    JObj = kz_json:set_value(Key, Value, get(AccountId, Category)),
    JObj1 = update_config_for_saving(AccountId, JObj),
    AccountDb = kz_util:format_account_db(AccountId),
    {ok, JObj2} = kz_datamgr:ensure_saved(AccountDb, JObj1),
    JObj2.
-endif.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Set the Value for the Key in account db if found, otherwise get
%% system_config value then save in account db.
%% @end
%%--------------------------------------------------------------------
-spec set_global(api_account(), ne_binary(), kz_json:path(), kz_json:json_term()) -> kz_json:object().
-ifdef(TEST).
set_global(_, _, Key, Value) ->
    kz_json:set_value(Key, Value, kz_json:new()).
-else.
set_global(Account, Category, Key, Value) ->
    set_account_or_merge_global(account_id(Account), Category, Key, Value).

-spec set_account_or_merge_global(account_or_not(), ne_binary(), kz_json:path(), kz_json:json_term()) ->
                                         kz_json:object().
set_account_or_merge_global(no_account_id, _, Key, Value) ->
    kz_json:set_value(Key, Value, kz_json:new());
set_account_or_merge_global(AccountId, Category, Key, Value) ->
    Doc = kz_doc:set_id(get_global(AccountId, Category), kapps_config_util:account_doc_id(Category)),
    Doc1 = kz_json:set_value(Key, Value, update_config_for_saving(AccountId, Doc)),

    AccountDb = kz_util:format_account_db(AccountId),
    {ok, JObj1} = kz_datamgr:ensure_saved(AccountDb, Doc1),
    JObj1.
-endif.

-spec update_config_for_saving(ne_binary(), kz_json:object()) -> kz_json:object().
update_config_for_saving(AccountId, JObj) ->
    AccountDb = kz_util:format_account_db(AccountId),
    kz_doc:update_pvt_parameters(JObj
                                ,AccountDb
                                ,[{type, <<"account_config">>}
                                 ,{account_id, AccountId}
                                 ]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Flush accounts specific config cache
%% @end
%%--------------------------------------------------------------------
-spec flush(ne_binary(), ne_binary()) -> ok.
flush(Account, Category) ->
    AccountDb = kz_util:format_account_db(Account),
    kz_datamgr:flush_cache_doc(AccountDb, kapps_config_util:account_doc_id(Category)),
    flush_all_strategies(Account, Category).

-spec flush_all_strategies(ne_binary(), ne_binary()) -> ok.
flush_all_strategies(Account, Category) ->
    Strategies = [<<"hierarchy_merge">>
                 ,<<"global">>
                 ,<<"reseller">>
                 ,<<"global_merge">>
                 ,<<"reseller_merge">>
                 ],
    lists:foreach(fun(Strategy) -> flush(Account, Category, Strategy) end, Strategies).

-spec flush(ne_binary(), ne_binary(), ne_binary()) -> ok.
flush(Account, Category, Strategy) ->
    AccountId = kz_util:format_account_id(Account),
    CacheKey = strategy_cache_key(AccountId, Category, Strategy),
    kz_cache:erase_local(?KAPPS_CONFIG_CACHE, CacheKey).


%% ====================================================================
%% Internal functions
%% ====================================================================


-spec walk_the_walk(map()) -> {ok, kz_json:object()} | {error, not_found}.
walk_the_walk(#{strategy_funs := []
               ,results := []
               }) ->
    {error, not_found};
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
        {ok, JObj} when not ShouldMerge ->
            %% requester does not want merge result from ancestors and system
            %% returning the result of the first function if defined
            case kz_json:get_value(Key, JObj) of
                undefined ->
                    %% key is not defined, continuing the walk
                    walk_the_walk(Map#{results := [JObj|Results], strategy_funs := Funs});
                _Value ->
                    walk_the_walk(Map#{results := [JObj], strategy_funs := []})
            end;
        {ok, JObjs} when is_list(JObjs) ->
            %% the function returns list (load from ancestor), forcing merge
            walk_the_walk(Map#{results := lists:flatten([JObjs|Results]), strategy_funs := Funs, merge := true});
        {ok, JObj} ->
            %% requester wants merge result from ancestors and system
            walk_the_walk(Map#{results := [JObj|Results], strategy_funs := Funs});
        {error, _} ->
            walk_the_walk(Map#{strategy_funs := Funs})
    end.

-spec maybe_merge_results(map(), boolean()) -> {ok, kz_json:object()}.
maybe_merge_results(#{results := JObjs}=Map, true) ->
    store_in_strategy_cache(Map, kz_json:merge_recursive([kz_doc:public_fields(J, false) || J <- JObjs]));
maybe_merge_results(#{results := JObjs}, false) ->
    {ok, lists:last(JObjs)}.

-spec store_in_strategy_cache(map(), kz_json:object()) -> {ok, kz_json:object()}.
-ifdef(TEST).
store_in_strategy_cache(#{account_id := AccountId
                         ,strategy := Strategy
                         ,category := Category
                         ,results := JObjs
                         }, Result) ->
    _CacheKey = strategy_cache_key(AccountId, Category, Strategy),
    _Origins = lists:foldl(fun config_origins/2, [], JObjs),
    {ok, Result}.
-else.
store_in_strategy_cache(#{account_id := AccountId
                         ,strategy := Strategy
                         ,category := Category
                         ,results := JObjs
                         }, Result) ->
    CacheKey = strategy_cache_key(AccountId, Category, Strategy),
    Origins = lists:foldl(fun config_origins/2, [], JObjs),
    kz_cache:store_local(?KAPPS_CONFIG_CACHE, CacheKey, Result, [{origin, Origins}]),
    {ok, Result}.
-endif.

-spec config_origins(kz_json:object(), list()) -> list().
config_origins(Doc, Acc) ->
    case {kz_doc:account_db(Doc), kz_doc:account_id(Doc)} of
        {undefined, undefined} -> Acc;
        {undefined, DocAccountId} ->
            Db = kz_util:format_account_db(DocAccountId),
            [{db, Db, kz_doc:id(Doc)}|Acc];
        {Db, _} -> [{db, Db, kz_doc:id(Doc)}|Acc]
    end.

-spec strategy_cache_key(ne_binary(), ne_binary(), ne_binary()) ->
                                {?MODULE, ne_binary(), ne_binary(), ne_binary()}.
strategy_cache_key(AccountId, Category, Strategy) ->
    {?MODULE, AccountId, Category, Strategy}.

-spec strategy_options(ne_binary(), ne_binary(), ne_binary(), boolean(), kz_json:path()) -> map().
strategy_options(Strategy, AccountId, Category, ShouldMerge, Key) ->
    #{account_id => AccountId
     ,strategy => Strategy
     ,strategy_funs => strategy_funs(Strategy)
     ,merge => ShouldMerge
     ,category => Category
     ,results => []
     ,key => Key
     }.

-spec is_merge_strategy(ne_binary()) -> boolean().
is_merge_strategy(Strategy) ->
    case kz_binary:reverse(Strategy) of
        <<"egrem", _/binary>> -> true;
        _ -> false
    end.

-spec strategy_funs(ne_binary()) -> [function()].
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

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Find AccountId from binary, Call object or JObj.
%% @end
%%--------------------------------------------------------------------
-spec account_id(api_account()) -> account_or_not().
account_id(?NE_BINARY=Account) -> kz_util:format_account_id(Account);
account_id(undefined) -> no_account_id;
account_id(Obj) -> account_id_from_call(Obj, kapps_call:is_call(Obj)).

-spec account_id_from_call(kapps_call:call() | kz_json:object(), boolean()) -> account_or_not().
account_id_from_call(Call, true) -> maybe_format_account_id(kapps_call:account_id(Call));
account_id_from_call(Obj, false) -> account_id_from_jobj(Obj, kz_json:is_json_object(Obj)).

-spec account_id_from_jobj(kz_json:object(), true) -> account_or_not().
account_id_from_jobj(JObj, true) ->
    Paths = [<<"Account-ID">>
            ,<<"account_id">>
            ,<<"Account-DB">>
            ,<<"account_db">>
            ],
    maybe_format_account_id(kz_json:get_first_defined(Paths, JObj));
account_id_from_jobj(_Obj, false) ->
    lager:debug("unable to find account id from ~p", [_Obj]),
    no_account_id.


-spec maybe_format_account_id(api_ne_binary()) -> account_or_not().
maybe_format_account_id(undefined) -> no_account_id;
maybe_format_account_id(Account) -> kz_util:format_account_id(Account).

-spec maybe_new_system_doc({ok, kz_json:object()} | {error, any()}, ne_binary()) -> kz_json:object().
maybe_new_system_doc({ok, JObj}, Category) ->
    kz_doc:set_id(kz_doc:set_account_db(JObj, ?KZ_CONFIG_DB), Category);
maybe_new_system_doc({error, _}, Category) ->
    kz_doc:set_id(kz_doc:set_account_db(kz_json:new(), ?KZ_CONFIG_DB), Category).


%% ====================================================================
%% Migrates config settings
%% ====================================================================


-type migrate_setting() :: {ne_binary(), kz_json:path()}.
-type migrate_value() :: {ne_binary(), ne_binary(), kz_json:path(), _}.
-type migrate_values() :: [migrate_value()].

-define(ACCOUNT_CONFIG_MIGRATIONS
       ,[{{<<"callflow">>, <<"default_can_text_self">>}
         ,{<<"kazoo_endpoint">>, <<"default_can_text_self">>}
         }
        ,{{<<"callflow">>, <<"recorder_module">>}
         ,{<<"kazoo_endpoint">>, <<"recorder_module">>}
         }
        ]).

-spec migrate(ne_binary()) -> ok.
migrate(Account) ->
    AccountDb = kz_util:format_account_db(Account),
    _ = [migrate_config_setting(AccountDb, From, To)
         || {From, To} <- ?ACCOUNT_CONFIG_MIGRATIONS
        ],
    ok.

-spec migrate_config_setting(ne_binary(), migrate_setting(), migrate_setting()) ->
                                    ok | {error, any()}.
migrate_config_setting(AccountDb, From, To) ->
    case remove_config_setting(AccountDb, From) of
        {ok, _, []} -> ok;
        {ok, JObj, Removed} ->
            migrate_config_setting(AccountDb, JObj, Removed, To);
        {error, not_found} -> ok;
        {error, Reason} -> {error, {remove, Reason}}
    end.

-spec migrate_config_setting(ne_binary(), kz_json:object(), migrate_values(), migrate_setting()) ->
                                    ok | {error, any()}.
migrate_config_setting(AccountDb, UpdatedFrom, Removed, To) ->
    case add_config_setting(AccountDb, To, Removed) of
        {ok, UpdatedTo} ->
            {ok, _} = kz_datamgr:save_doc(AccountDb, UpdatedTo),
            {ok, _} = kz_datamgr:save_doc(AccountDb, UpdatedFrom),
            ok;
        {error, Reason} -> {error, {add, Reason}}
    end.

-spec add_config_setting(ne_binary(), migrate_setting(), migrate_values()) ->
                                ok | {error, any()}.
add_config_setting(AccountDb, {Id, Setting}, Values) ->
    add_config_setting(AccountDb, Id, Setting, Values).

-spec add_config_setting(ne_binary(), ne_binary(), kz_json:path(), migrate_values()) ->
                                ok | {error, any()}.
add_config_setting(AccountDb, Id, Setting, Values) when is_binary(Id) ->
    case kz_datamgr:open_doc(AccountDb, Id) of
        {ok, JObj} -> add_config_setting(JObj, Setting, Values);
        {error, not_found} ->
            add_config_setting(AccountDb
                              ,update_config_for_saving(AccountDb, kz_doc:set_id(kz_json:new(), Id))
                              ,Setting
                              ,Values
                              );
        {error, _}=Error -> Error
    end;
add_config_setting(_AccountDb, JObj, _, []) -> {ok, JObj};
add_config_setting(AccountDb, JObj, ToSetting, [{FromId, Node, FromSetting, Value} | Values]) ->
    ToId  = kz_doc:id(JObj),
    Key = config_setting_key(Node, ToSetting),
    case kz_json:get_value(Key, JObj) of
        undefined ->
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
                      " you need to correct this disparity manually!~n"),
            io:format("  Source~n    db: ~s~n    id: ~s~n    key: ~s ~s~n    value: ~p~n"
                     ,[AccountDb, FromId, Node, FromSetting, Value]),
            io:format("  Destination~n    db: ~s~n    id: ~s~n    key: ~s ~s~n    value: ~p~n"
                     ,[AccountDb, ToId, Node, ToSetting, _Else]),
            {error, disparity}
    end.

-spec remove_config_setting(ne_binary(), migrate_setting()) ->
                                   {ok, kz_json:object(), migrate_values()} |
                                   {error, any()}.
remove_config_setting(AccountDb, {Id, Setting}) ->
    remove_config_setting(AccountDb, Id, Setting).

-spec remove_config_setting(ne_binary(), ne_binary() | kz_json:object(), kz_json:path()) ->
                                   {ok, kz_json:object(), migrate_values()} |
                                   {error, any()}.
remove_config_setting(AccountDb, Id, Setting) when is_binary(Id) ->
    case kz_datamgr:open_doc(AccountDb, Id) of
        {ok, JObj} -> remove_config_setting(AccountDb, JObj, Setting);
        {error, _}=Error -> Error
    end;
remove_config_setting(AccountDb, JObj, Setting) ->
    Id = kz_doc:id(JObj),
    Keys =
        [{Id, Node, Setting}
         || Node <- kz_doc:get_public_keys(JObj)
        ],
    remove_config_setting(AccountDb, Keys, JObj, []).

-spec remove_config_setting(ne_binary(), [{ne_binary(), ne_binary(), kz_json:path()}], kz_json:object(), migrate_values()) ->
                                   {ok, kz_json:object(), migrate_values()}.
remove_config_setting(_AccountDb, [], JObj, Removed) ->
    {ok, JObj, Removed};
remove_config_setting(AccountDb, [{Id, Node, Setting} | Keys], JObj, Removed) ->
    Key = config_setting_key(Node, Setting),
    case kz_json:get_value(Key, JObj) of
        undefined -> remove_config_setting(AccountDb, Keys, JObj, Removed);
        Value ->
            remove_config_setting(AccountDb
                                 ,Keys
                                 ,kz_json:delete_key(Key, JObj)
                                 ,[{Id, Node, Setting, Value} | Removed]
                                 )
    end.

-spec config_setting_key(ne_binary(), kz_json:path()) -> ne_binaries().
%% NOTE: to support nested keys, update this merge function
config_setting_key(Node, Setting) ->
    [Node, Setting].

-ifdef(TEST).
-spec give_me_something(ne_binary(), no_account_id, any()) ->
                               undefined |
                               boolean() |
                               {ok, kz_json:object()} |
                               {error, not_found | no_account_id}.
%% An Account Config
give_me_something(<<"account">>, AccountId, _)
  when AccountId =:= ?CUSTOMIZED_RESELLER_UNDEFINED;
       AccountId =:= ?CUSTOMIZED_SUBACCOUNT_1_UNDEFINED ->
    {ok, kz_json:new()};
give_me_something(<<"account">>, AccountId, _)
  when AccountId =:= ?CUSTOMIZED_RESELLER_HIER; %%only load_from_ancestors
       AccountId =:= ?SELF_RESELLER ->
    {ok, kapps_config_util:fixture("test_cat_reseller")};
give_me_something(<<"account">>, ?CUSTOMIZED_SUBACCOUNT_1, _) ->
    {ok, kapps_config_util:fixture("test_cat_subaccount_1")};
%% only load_from_ancestors (clause below)
give_me_something(<<"account">>, AccountId, _)
  when AccountId =:= ?CUST_A_CUST_P_CUST_R;
       AccountId =:= ?CUST_A_CUST_P_404_R;
       AccountId =:= ?CUST_A_CUST_P_EMPTY_R;
       AccountId =:= ?CUST_A_404_P_CUST_R;
       AccountId =:= ?CUST_A_404_P_404_R ->
    {ok, kapps_config_util:fixture("test_cat_subaccount_2")};
give_me_something(<<"account">>, _AccountId, _) ->
    {error, not_found};

%% Reseller Id (load_from_reseller, load_from_ancestors when account tree is empty)
give_me_something(<<"reseller_id">>, ?CUSTOMIZED_RESELLER, _) ->
    [?SELF_RESELLER];
give_me_something(<<"reseller_id">>, ?SELF_RESELLER, _) ->
    [?SELF_RESELLER];
give_me_something(<<"reseller_id">>, _AccountId, _) ->
    [];

%% A Reseller Config (only load_from_reseller)
give_me_something(<<"reseller_jobj">>, ?SELF_RESELLER, _) ->
    {ok, kapps_config_util:fixture("test_cat_reseller")};

%% Account Tree (only load_from_ancestors)
give_me_something(<<"parent_id">>, ?CUST_A_CUST_P_CUST_R, _) ->
    [?A_MASTER_ACCOUNT_ID
    ,?CUSTOMIZED_RESELLER_HIER
    ,?CUSTOMIZED_SUBACCOUNT_1
    ];
give_me_something(<<"parent_id">>, ?CUST_A_CUST_P_404_R, _) ->
    [?A_MASTER_ACCOUNT_ID
    ,?NOT_CUSTOMIZED_RESELLER
    ,?CUSTOMIZED_SUBACCOUNT_1
    ];
give_me_something(<<"parent_id">>, ?CUST_A_CUST_P_EMPTY_R, _) ->
    [?A_MASTER_ACCOUNT_ID
    ,?CUSTOMIZED_RESELLER_UNDEFINED
    ,?CUSTOMIZED_SUBACCOUNT_1
    ];
give_me_something(<<"parent_id">>, ?CUST_A_404_P_CUST_R, _) ->
    [?A_MASTER_ACCOUNT_ID
    ,?CUSTOMIZED_RESELLER_HIER
    ,?NOT_CUSTOMIZED_ALL_ACCOUNTS
    ];
give_me_something(<<"parent_id">>, ?CUST_A_404_P_404_R, _) ->
    [?A_MASTER_ACCOUNT_ID
    ,?NOT_CUSTOMIZED_RESELLER
    ,?NOT_CUSTOMIZED_ALL_ACCOUNTS
    ];
give_me_something(<<"parent_id">>, ?CUSTOMIZED_SUBACCOUNT_1, _) ->
    [?A_MASTER_ACCOUNT_ID
    ,?CUSTOMIZED_RESELLER_HIER
    ];
give_me_something(<<"parent_id">>, _AccountId, _) ->
    [];

%% Is Reseller (only load_from_ancestors)
give_me_something(<<"is_reseller">>, AccountId, _)
  when AccountId =:= ?SELF_RESELLER;
       AccountId =:= ?CUSTOMIZED_RESELLER_UNDEFINED;
       AccountId =:= ?CUSTOMIZED_RESELLER_HIER;
       AccountId =:= ?NOT_CUSTOMIZED_RESELLER ->
    true;
give_me_something(<<"is_reseller">>, _AccountId, _) ->
    false.

-endif.
