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

        ,set/4
        ,set_global/4

        ,flush/2
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
            %% ?LOG_DEBUG("get_global ok"),
            get_global_from_doc(Category, Key, Default, JObj);
        {error, _} ->
            %% ?LOG_DEBUG("get_global error, getting from reseller"),
            get_from_reseller(Account, Category, Key, Default)
    end.

-spec get_global(api_account(), ne_binary()) -> kz_json:object().
get_global(Account, Category) ->
    case load_config_from_account(account_id(Account), Category) of
        {ok, JObj} -> JObj;
        {error, no_account_id} -> maybe_new(load_config_from_system(Account, Category));
        {error, _} ->
            case load_config_from_reseller(Account, Category) of
                {ok, JObj} -> JObj;
                {error, _} ->
                    %% ?LOG_DEBUG("get_global/2 error reseller, getting system_config"),
                    maybe_new(load_config_from_system(Account, Category))
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
            %% ?LOG_DEBUG("get_from_reseller ok"),
            get_global_from_doc(Category, Key, Default, JObj);
        {error, _} ->
            %% ?LOG_DEBUG("get_from_reseller error, getting system_configs"),
            kapps_config:get(Category, Key, Default)
    end.

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
            %% ?LOG_DEBUG("strategy ok"),
            case kz_json:get_value(Key, JObj) of
                undefined ->
                    %% ?LOG_DEBUG("strategy ok undefined"),
                    _ = kapps_config:set(Category, Key, Default),
                    Default;
                V ->
                    %% ?LOG_DEBUG("strategy ok value"),
                    V
            end;
        {error, no_account_id} ->
            %% ?LOG_DEBUG("strategy no_account_id"),
            kapps_config:get(Category, Key, Default);
        {error, _} ->
            %% ?LOG_DEBUG("strategy error"),
            _ = kapps_config:set(Category, Key, Default),
            Default
    end.

-spec get_from_strategy_cache(ne_binary(), account_or_not(), kz_json:path(), ne_binary(), boolean()) ->
                                     {ok, kz_json:object()} |
                                     {error, any()}.
get_from_strategy_cache(_, no_account_id, _, _, _) ->
    %% ?LOG_DEBUG("cache no_account_id"),
    {error, no_account_id};
get_from_strategy_cache(Strategy, AccountId, Category, Key, true) ->
    %% Only read from cache if it is merge strategy
    case kz_cache:fetch_local(?KAPPS_CONFIG_CACHE, strategy_cache_key(AccountId, Category, Strategy)) of
        {ok, _}=OK ->
            %% ?LOG_DEBUG("cache ok"),
            OK;
        {error, _} ->
            %% ?LOG_DEBUG("cache error"),
            walk_the_walk(strategy_options(Strategy, AccountId, Category, true, Key))
    end;
get_from_strategy_cache(Strategy, AccountId, Category, Key, false) ->
    %% ?LOG_DEBUG("no cache for you"),
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
    %% ?LOG_DEBUG("get global doc"),
    case kz_json:get_value(Key, JObj) of
        undefined ->
            %% ?LOG_DEBUG("get global doc undefined, getting system_configs"),
            kapps_config:get(Category, Key, Default);
        V ->
            %% ?LOG_DEBUG("get global doc defined"),
            V
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
    %% ?LOG_DEBUG("load_system ~s", [Category]),
    case kapps_config:get_category(Category) of
        {ok, JObj} ->
            %% ?LOG_DEBUG("load_system ok ~s", [Category]),
            {ok, kz_json:get_value(<<"default">>, JObj, kz_json:new())};
        {error, _}=Error -> Error
    end.
-spec load_config_from_reseller(api_account(), ne_binary()) -> kazoo_data:get_results_return().
-ifdef(TEST).
load_config_from_reseller(Account, Category) ->
    %% ?LOG_DEBUG("load_reseller"),
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
    %% ?LOG_DEBUG("load_reseller"),
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
    %% ?LOG_DEBUG("load_account"),
    DocId = kapps_config_util:account_doc_id(Category),
    AccountDb = kz_util:format_account_db(AccountId),
    kz_datamgr:open_cache_doc(AccountDb, DocId, [{cache_failures, [not_found]}]).
-endif.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%  Get Accounts parent configuration for the Category
%%  1. Read account definition
%%      1.1. If failed to read account definiation, find its reseller
%%  2. Fold over ancestor Ids and fetch config doc from thier db
%%      2.1. If document exists and the account is reseller, return
%%      2.2. If document does not exists:
%%          2.2.1. If the account is reseller return accumulator
%%          2.2.2. If not reseller, continue the fold
%% @end
%%--------------------------------------------------------------------
-spec load_config_from_ancestors(ne_binary(), ne_binary()) -> kazoo_data:get_results_return().
load_config_from_ancestors(AccountId, Category) ->
    %% ?LOG_DEBUG("init load_ancestores"),
    load_config_from_ancestors(AccountId, Category, is_reseller_account(AccountId)).

-spec load_config_from_ancestors(ne_binary(), ne_binary(), boolean()) -> kazoo_data:get_results_return().
load_config_from_ancestors(_, _, true) ->
    %% ?LOG_DEBUG("init load_ancestores account_is_reseller"),
    {error, <<"account_is_reseller">>};
load_config_from_ancestors(AccountId, Category, false) ->
    %% ?LOG_DEBUG("init load_ancestores not reseller"),
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
    %% ?LOG_DEBUG("ancestors parent undefined"),
    {ok, JObjs};
load_config_from_ancestors_fold([ParentId|AncestorIds], Category, JObjs) ->
    %% ?LOG_DEBUG("ancestors run"),
    case {load_config_from_account(ParentId, Category)
         ,is_reseller_account(ParentId)
         }
    of
        {{ok, JObj}, true} ->
            %% ?LOG_DEBUG("ancestors ok reseller"),
            lager:debug("reached to the reseller account ~s (for category ~s)", [ParentId, Category]),
            {ok, [JObj|JObjs]};
        {{ok, JObj}, _} ->
            %% ?LOG_DEBUG("ancestors ok account"),
            load_config_from_ancestors_fold(AncestorIds, Category, [JObj|JObjs]);
        {{error, _Reason}, true} ->
            %% ?LOG_DEBUG("ancestors nok reseller"),
            lager:debug("reached to the reseller account ~s (failed to get category ~s: ~p)"
                       ,[ParentId, Category, _Reason]
                       ),
            {ok, JObjs};
        {{error, _Reason}, _} ->
            %% ?LOG_DEBUG("ancestors nok account"),
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
            lager:debug("failed to find accout ~s parents and reseller"),
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
    AccountDb = kz_util:format_account_id(Account, encoded),
    kz_datamgr:flush_cache_doc(AccountDb, kapps_config_util:account_doc_id(Category)).


%% ====================================================================
%% Internal functions
%% ====================================================================


-spec walk_the_walk(map()) -> {ok, kz_json:object()} | {error, not_found}.
walk_the_walk(#{strategy_funs := []
               ,results := []
               }) ->
    %% ?LOG_DEBUG("walk not_found"),
    {error, not_found};
walk_the_walk(#{strategy_funs := []
               ,merge := ShouldMerge
               }=Map) ->
    %% ?LOG_DEBUG("walk done"),
    maybe_merge_results(Map, ShouldMerge);
walk_the_walk(#{account_id := AccountId
               ,strategy_funs := [Fun|Funs]
               ,merge := ShouldMerge
               ,category := Category
               ,key := Key
               ,results := Results
               }=Map) ->
    %% ?LOG_DEBUG("walk run"),
    case Fun(AccountId, Category) of
        {ok, JObj} when not ShouldMerge ->
            %% requester does not want merge result from ancestors and system
            %% returning the result of the first function if defined
            %% ?LOG_DEBUG("walk run ok not merge"),
            case kz_term:is_not_empty(Key)
                andalso kz_json:get_value(Key, JObj)
            of
                false ->
                    %% ?LOG_DEBUG("walk run ok not merge no key"),
                    walk_the_walk(Map#{results := [JObj], strategy_funs := []});
                undefined ->
                    %% key is not defined, continuing the walk
                    %% ?LOG_DEBUG("walk run ok not merge undefined"),
                    walk_the_walk(Map#{results := [JObj|Results], strategy_funs := Funs});
                _Value ->
                    %% ?LOG_DEBUG("walk run ok not merge value"),
                    walk_the_walk(Map#{results := [JObj], strategy_funs := []})
            end;
        {ok, JObjs} when is_list(JObjs) ->
            %% the function returns list (load from ancestor), forcing merge
            %% ?LOG_DEBUG("walk run ok list"),
            walk_the_walk(Map#{results := lists:flatten([JObjs|Results]), strategy_funs := Funs, merge := true});
        {ok, JObj} ->
            %% requester wants merge result from ancestors and system
            %% ?LOG_DEBUG("walk run ok normal"),
            walk_the_walk(Map#{results := [JObj|Results], strategy_funs := Funs});
        {error, _} ->
            %% ?LOG_DEBUG("walk run error"),
            walk_the_walk(Map#{strategy_funs := Funs})
    end.

-spec maybe_merge_results(map(), boolean()) -> {ok, kz_json:object()}.
maybe_merge_results(#{results := JObjs}=Map, true) ->
    store_in_strategy_cache(Map, kz_json:merge([kz_doc:public_fields(J) || J <- JObjs]));
maybe_merge_results(#{results := JObjs}, false) ->
    %% ?LOG_DEBUG("5 no merge"),
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

-spec strategy_options(ne_binary(), ne_binary(), ne_binary(), boolean(), undefined | kz_json:path()) -> map().
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
    ];
strategy_funs(_) ->
    [].

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

-spec maybe_new({ok, kz_json:object()} | {error, any()}) -> kz_json:object().
maybe_new({ok, JObj}) -> JObj;
maybe_new({error, _}) -> kz_json:new().


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
give_me_something(<<"account">>, ?CUSTOMIZED_RESELLER_UNDEFINED, _) ->
    %% ?LOG_DEBUG("load_account CUSTOMIZED_RESELLER_UNDEFINED"),
    {ok, kz_json:new()};
give_me_something(<<"account">>, ?CUSTOMIZED_RESELLER_HIER, _) ->
    %% ?LOG_DEBUG("load_account CUSTOMIZED_RESELLER_HIER"),
    {ok, kapps_config_util:fixture("test_cat_reseller")};
give_me_something(<<"account">>, ?SELF_RESELLER, _) ->
    %% ?LOG_DEBUG("load_account SELF_RESELLER"),
    {ok, kapps_config_util:fixture("test_cat_reseller")};
give_me_something(<<"account">>, ?CUSTOMIZED_SUBACCOUNT_1_UNDEFINED, _) ->
    %% ?LOG_DEBUG("load_account CUSTOMIZED_SUBACCOUNT_1_UNDEFINED"),
    {ok, kz_json:new()};
give_me_something(<<"account">>, ?CUSTOMIZED_SUBACCOUNT_1, _) ->
    %% ?LOG_DEBUG("load_account CUSTOMIZED_SUBACCOUNT_1"),
    {ok, kapps_config_util:fixture("test_cat_subaccount_1")};
give_me_something(<<"account">>, ?CUST_A_CUST_P_CUST_R, _) ->
    %% ?LOG_DEBUG("load_account CUST_A_CUST_P_CUST_R"),
    {ok, kapps_config_util:fixture("test_cat_subaccount_2")};
give_me_something(<<"account">>, ?CUST_A_CUST_P_404_R, _) ->
    %% ?LOG_DEBUG("load_account CUST_A_CUST_P_404_R"),
    {ok, kapps_config_util:fixture("test_cat_subaccount_2")};
give_me_something(<<"account">>, ?CUST_A_CUST_P_EMPTY_R, _) ->
    %% ?LOG_DEBUG("load_account CUST_A_CUST_P_EMPTY_R"),
    {ok, kapps_config_util:fixture("test_cat_subaccount_2")};
give_me_something(<<"account">>, ?CUST_A_404_P_CUST_R, _) ->
    %% ?LOG_DEBUG("load_account CUST_A_404_P_CUST_R"),
    {ok, kapps_config_util:fixture("test_cat_subaccount_2")};
give_me_something(<<"account">>, ?CUST_A_404_P_404_R, _) ->
    %% ?LOG_DEBUG("load_account CUST_A_404_P_404_R"),
    {ok, kapps_config_util:fixture("test_cat_subaccount_2")};
give_me_something(<<"account">>, _AccountId, _) ->
    %% ?LOG_DEBUG("load_account ~s", [_AccountId]),
    {error, not_found};

%% Reseller Id
give_me_something(<<"reseller_id">>, ?CUSTOMIZED_RESELLER, _) ->
    %% ?LOG_DEBUG("reseller_id CUSTOMIZED_RESELLER"),
    [?SELF_RESELLER];
give_me_something(<<"reseller_id">>, ?SELF_RESELLER, _) ->
    %% ?LOG_DEBUG("reseller_id SELF_RESELLER"),
    [?SELF_RESELLER];
give_me_something(<<"reseller_id">>, _AccountId, _) ->
    %% ?LOG_DEBUG("reseller_id ~s", [_AccountId]),
    [];

%% A Reseller Config
give_me_something(<<"reseller_jobj">>, ?CUSTOMIZED_RESELLER, _) ->
    %% ?LOG_DEBUG("load_reseller CUSTOMIZED_RESELLER"),
    {ok, kapps_config_util:fixture("test_cat_reseller")};
give_me_something(<<"reseller_jobj">>, ?SELF_RESELLER, _) ->
    %% ?LOG_DEBUG("load_reseller SELF_RESELLER"),
    {ok, kapps_config_util:fixture("test_cat_reseller")};
give_me_something(<<"reseller_jobj">>, _AccountId, _) ->
    %% ?LOG_DEBUG("load_reseller ~s", [_AccountId]),
    {error, not_found};

%% A Parent AccountId
give_me_something(<<"parent_id">>, ?CUST_A_CUST_P_CUST_R, _) ->
    %% ?LOG_DEBUG("parent_id CUST_A_CUST_P_CUST_R -> master, cust_reseller_hier, cust_sub1"),
    [?A_MASTER_ACCOUNT_ID
    ,?CUSTOMIZED_RESELLER_HIER
    ,?CUSTOMIZED_SUBACCOUNT_1
    ];
give_me_something(<<"parent_id">>, ?CUST_A_CUST_P_404_R, _) ->
    %% ?LOG_DEBUG("parent_id CUST_A_CUST_P_404_R -> master, not_exists_reseller, cust_sub1"),
    [?A_MASTER_ACCOUNT_ID
    ,?NOT_CUSTOMIZED_RESELLER
    ,?CUSTOMIZED_SUBACCOUNT_1
    ];
give_me_something(<<"parent_id">>, ?CUST_A_CUST_P_EMPTY_R, _) ->
    %% ?LOG_DEBUG("parent_id CUST_A_CUST_P_EMPTY_R -> master, empty_reseller, cust_sub1"),
    [?A_MASTER_ACCOUNT_ID
    ,?CUSTOMIZED_RESELLER_UNDEFINED
    ,?CUSTOMIZED_SUBACCOUNT_1
    ];
give_me_something(<<"parent_id">>, ?CUST_A_404_P_CUST_R, _) ->
    %% ?LOG_DEBUG("parent_id CUST_A_404_P_CUST_R -> master, cust_reseller_hier, not_exists_sub1"),
    [?A_MASTER_ACCOUNT_ID
    ,?CUSTOMIZED_RESELLER_HIER
    ,?NOT_CUSTOMIZED_ALL_ACCOUNTS
    ];
give_me_something(<<"parent_id">>, ?CUST_A_404_P_404_R, _) ->
    %% ?LOG_DEBUG("parent_id CUST_A_404_P_CUST_R -> master, not_exists_reseller, not_exists_sub1"),
    [?A_MASTER_ACCOUNT_ID
    ,?NOT_CUSTOMIZED_RESELLER
    ,?NOT_CUSTOMIZED_ALL_ACCOUNTS
    ];
give_me_something(<<"parent_id">>, ?CUSTOMIZED_SUBACCOUNT_1, _) ->
    %% ?LOG_DEBUG("parent_id CUSTOMIZED_SUBACCOUNT_1 -> master, cust_reseller_hier"),
    [?A_MASTER_ACCOUNT_ID
    ,?CUSTOMIZED_RESELLER_HIER
    ];
give_me_something(<<"parent_id">>, _AccountId, _) ->
    %% ?LOG_DEBUG("parent_id empty: ~s", [_AccountId]),
    [];

%% Is Reseller
give_me_something(<<"is_reseller">>, ?SELF_RESELLER, _) ->
    %% ?LOG_DEBUG("is_reseller SELF_RESELLER"),
    true;
give_me_something(<<"is_reseller">>, ?CUSTOMIZED_RESELLER, _) ->
    %% ?LOG_DEBUG("is_reseller CUSTOMIZED_RESELLER"),
    true;
give_me_something(<<"is_reseller">>, ?CUSTOMIZED_RESELLER_UNDEFINED, _) ->
    %% ?LOG_DEBUG("is_reseller CUSTOMIZED_RESELLER_UNDEFINED"),
    true;
give_me_something(<<"is_reseller">>, ?CUSTOMIZED_RESELLER_HIER, _) ->
    %% ?LOG_DEBUG("is_reseller CUSTOMIZED_RESELLER_HIER"),
    true;
give_me_something(<<"is_reseller">>, ?NOT_CUSTOMIZED_RESELLER, _) ->
    %% ?LOG_DEBUG("is_reseller NOT_CUSTOMIZED_RESELLER"),
    true;
give_me_something(<<"is_reseller">>, ?A_MASTER_ACCOUNT_ID, _) ->
    %% ?LOG_DEBUG("is_reseller A_MASTER_ACCOUNT_ID"),
    true;
give_me_something(<<"is_reseller">>, _AccountId, _) ->
    %% ?LOG_DEBUG("is_reseller ~s", [_AccountId]),
    false.

-endif.
