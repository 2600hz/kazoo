%%%-------------------------------------------------------------------
%%% @copyright
%%% @doc
%%%
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(cb_resource_selectors).

-export([init/0
        ,authorize/1
        ,allowed_methods/0, allowed_methods/1, allowed_methods/2, allowed_methods/4
        ,resource_exists/0, resource_exists/1, resource_exists/2, resource_exists/4
        ,content_types_accepted/1, content_types_accepted/2, content_types_accepted/3, content_types_accepted/5
        ,validate/1, validate/2, validate/3, validate/5
        ,put/5
        ,post/1, post/5
        ,delete/5
        ]).


-export([normalize_view_results/2]).

-include("crossbar.hrl").

-define(SRS_LIST, <<"resource_selectors/resource_listing">>).
-define(SRS_SEARCH, <<"resource_selectors/crossbar_search">>).
-define(MOD_CONFIG_CAT, <<(?CONFIG_CAT)/binary, ".resource_selectors">>).
-define(SUPPRESS_SRS_NOTICE, kapps_config:get_is_true(?MOD_CONFIG_CAT, <<"suppress_selectors_change_notice">>, 'false')).
-define(UPLOAD_MIME_TYPES, [{<<"text">>, <<"csv">>}
                           ,{<<"text">>, <<"comma-separated-values">>}
                           ]).
-define(NAME, <<"name">>).
-define(RESOURCE, <<"resource">>).
-define(ZERO_STATS, [{'total', 0}
                    ,{'success', 0}
                    ,{'error', 0}
                    ]).
-define(DEFAULT_RULES, {[]}).
-define(RULES_PVT_TYPE, <<"resource_selector_rules">>).
-define(DEFAULT_CSV_CONFIG, {[{<<"selector_column">>, 1}
                             ,{<<"value_column">>, 2}
                             ]}).

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    [crossbar_bindings:bind(Binding, ?MODULE, F)
     || {Binding, F} <- [{<<"*.allowed_methods.resource_selectors">>, 'allowed_methods'}
                        ,{<<"*.resource_exists.resource_selectors">>, 'resource_exists'}
                        ,{<<"*.content_types_accepted.resource_selectors">>, 'content_types_accepted'}
                        ,{<<"*.validate.resource_selectors">>, 'validate'}
                        ,{<<"*.execute.put.resource_selectors">>, 'put'}
                        ,{<<"*.execute.post.resource_selectors">>, 'post'}
                        ,{<<"*.execute.delete.resource_selectors">>, 'delete'}

                        ,{<<"*.authorize">>, 'authorize'}
                        ]
    ].

-spec authorize(cb_context:context()) ->
                       boolean() | {'halt', cb_context:context()}.
-spec authorize(cb_context:context(), req_nouns()) ->
                       boolean() | {'halt', cb_context:context()}.
authorize(Context) ->
    authorize(Context, cb_context:req_nouns(Context)).

authorize(Context, [{<<"resource_selectors">>, _} | _]) ->
    case cb_context:account_id(Context) of
        'undefined' -> maybe_authorize_admin(Context);
        _AccountId -> 'true'
    end;
authorize(_Context, _Nouns) ->
    'false'.

-spec maybe_authorize_admin(cb_context:context()) ->
                                   'true' |
                                   {'halt', cb_context:context()}.
maybe_authorize_admin(Context) ->
    case cb_modules_util:is_superduper_admin(Context) of
        'true' ->
            lager:debug("authz the request for global resources"),
            'true';
        'false' -> {'halt', Context}
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines the verbs that are appropriate for the
%% given Nouns.  IE: '/accounts/' can only accept GET and PUT
%%
%% Failure here returns 405
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods() -> http_methods().
-spec allowed_methods(path_token()) -> http_methods().
-spec allowed_methods(path_token(), path_token()) -> http_methods().
-spec allowed_methods(path_token(), path_token(), path_token(), path_token()) -> http_methods().
allowed_methods() ->
    [?HTTP_GET, ?HTTP_POST].

allowed_methods(?NAME) ->
    [?HTTP_GET];
allowed_methods(?RESOURCE) ->
    [?HTTP_GET].
allowed_methods(?NAME, _SelectorName) ->
    [?HTTP_GET];
allowed_methods(?RESOURCE, _ResourceId) ->
    [?HTTP_GET].
allowed_methods(?RESOURCE, _ResourceId, ?NAME, _SelectorName) ->
    [?HTTP_GET, ?HTTP_PUT, ?HTTP_POST, ?HTTP_DELETE];
allowed_methods(?NAME, _SelectorName, ?RESOURCE, _ResourceId) ->
    [?HTTP_GET, ?HTTP_PUT, ?HTTP_POST, ?HTTP_DELETE].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines if the provided list of Nouns are valid.
%%
%% Failure here returns 404
%% @end
%%--------------------------------------------------------------------
-spec resource_exists() -> 'true'.
-spec resource_exists(path_token()) -> 'true'.
-spec resource_exists(path_token(), path_token()) -> 'true'.
-spec resource_exists(path_token(), path_token(), path_token(), path_token()) -> 'true'.
resource_exists() -> 'true'.
resource_exists(?NAME) -> 'true';
resource_exists(?RESOURCE) -> 'true'.
resource_exists(?NAME, _SelectorName) -> 'true';
resource_exists(?RESOURCE, _ResourceId) -> 'true'.
resource_exists(?RESOURCE, _ResourceId, ?NAME, _SelectorName) -> 'true';
resource_exists(?NAME, _SelectorName, ?RESOURCE, _ResourceId) -> 'true'.

-spec content_types_accepted(cb_context:context()) -> cb_context:context().
-spec content_types_accepted(cb_context:context(), path_token()) -> cb_context:context().
-spec content_types_accepted(cb_context:context(), path_token(), path_token()) -> cb_context:context().
-spec content_types_accepted(cb_context:context(), path_token(), path_token(), path_token(), path_token()) -> cb_context:context().
content_types_accepted(Context) ->
    Context.
content_types_accepted(Context, ?NAME) ->
    Context;
content_types_accepted(Context, ?RESOURCE) ->
    Context.
content_types_accepted(Context, ?NAME, _SelectorName) ->
    Context;
content_types_accepted(Context, ?RESOURCE, _ResourceId) ->
    Context.
content_types_accepted(Context, ?RESOURCE, _ResourceId, ?NAME, _SelectorName) ->
    content_types_accepted_by_verb(Context, cb_context:req_verb(Context));
content_types_accepted(Context, ?NAME, _SelectorName, ?RESOURCE, _ResourceId) ->
    content_types_accepted_by_verb(Context, cb_context:req_verb(Context)).

-spec content_types_accepted_by_verb(cb_context:context(), http_method()) -> cb_context:context().
content_types_accepted_by_verb(Context, ?HTTP_POST) ->
    cb_context:set_content_types_accepted(Context, [{'from_binary', ?UPLOAD_MIME_TYPES}]);
content_types_accepted_by_verb(Context, _) ->
    Context.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
-spec validate(cb_context:context(), path_token()) -> cb_context:context().
-spec validate(cb_context:context(), path_token(), path_token()) -> cb_context:context().
-spec validate(cb_context:context(), path_token(), path_token(),  path_token(), path_token()) -> cb_context:context().
validate(Context) ->
    validate_rules(set_account_db(Context), cb_context:req_verb(Context)).

validate(Context, ?NAME) ->
    validate_selector(set_selectors_db(Context));
validate(Context, ?RESOURCE) ->
    validate_resource(set_selectors_db(Context)).

validate(Context, ?NAME, SelectorName) ->
    validate_selector(set_selectors_db(Context), SelectorName);
validate(Context, ?RESOURCE, ResourceId) ->
    validate_resource(set_selectors_db(Context), ResourceId).

validate(Context, ?RESOURCE, ResourceId, ?NAME, SelectorName) ->
    validate_resource_selector(set_selectors_db(Context), ResourceId, SelectorName, cb_context:req_verb(Context));
validate(Context, ?NAME, SelectorName, ?RESOURCE, ResourceId) ->
    validate_resource_selector(set_selectors_db(Context), ResourceId, SelectorName, cb_context:req_verb(Context)).

-spec set_selectors_db(cb_context:context()) -> cb_context:context().
set_selectors_db(Context) ->
    case is_global_request(Context) of
        'true' ->
            {'ok', MasterAccountId} = kapps_util:get_master_account_id(),
            MasterSelectorsDb = kz_util:format_resource_selectors_db(MasterAccountId),
            cb_context:set_account_db(Context, MasterSelectorsDb);
        'false' ->
            AccountId = cb_context:account_id(Context),
            SelectorsDb = kz_util:format_resource_selectors_db(AccountId),
            cb_context:set_account_db(Context, SelectorsDb)
    end.

-spec set_account_db(cb_context:context()) -> cb_context:context().
set_account_db(Context) ->
    case is_global_request(Context) of
        'true' ->
            {'ok', MasterAccountDb} = kapps_util:get_master_account_db(),
            cb_context:set_account_db(Context, MasterAccountDb);
        'false' -> Context
    end.

-spec validate_rules(cb_context:context(), http_method()) -> cb_context:context().
validate_rules(Context, ?HTTP_GET) ->
    load_rules(Context);
validate_rules(Context, ?HTTP_POST) ->
    OnSuccess = fun on_successful_rules_validation/1,
    cb_context:validate_request_data(<<"resource_selectors.rules">>, Context, OnSuccess).

-spec validate_resource(cb_context:context()) -> cb_context:context().
-spec validate_resource(cb_context:context(), path_token()) -> cb_context:context().
validate_resource(Context) ->
    summary_resource(Context).
validate_resource(Context, ResourceId) ->
    summary_resource(Context, ResourceId).

-spec validate_selector(cb_context:context()) -> cb_context:context().
-spec validate_selector(cb_context:context(), path_token()) -> cb_context:context().
validate_selector(Context) ->
    summary_selector(Context).
validate_selector(Context, SelectorName) ->
    summary_selector(Context, SelectorName).

-spec validate_resource_selector(cb_context:context(), path_token(), path_token(), http_method()) -> cb_context:context().
validate_resource_selector(Context, ResourceId, SelectorName, ?HTTP_GET) ->
    summary_resource_selector(Context, ResourceId, SelectorName);
validate_resource_selector(Context, ResourceId, SelectorName, ?HTTP_PUT) ->
    validate_put_resource_selector(Context, ResourceId, SelectorName);
validate_resource_selector(Context, ResourceId, SelectorName, ?HTTP_POST) ->
    validate_post_resource_selector(Context, ResourceId, SelectorName);
validate_resource_selector(Context, ResourceId, SelectorName, ?HTTP_DELETE) ->
    validate_delete_resource_selector(Context, ResourceId, SelectorName).

-spec validate_put_resource_selector(cb_context:context(), path_token(), path_token()) -> cb_context:context().
validate_put_resource_selector(Context, _ResourceId, _SelectorName) ->
    cb_context:validate_request_data(<<"resource_selectors">>, Context).

-spec validate_post_resource_selector(cb_context:context(), path_token(), path_token()) -> cb_context:context().
validate_post_resource_selector(Context, _ResourceId, _SelectorName) ->
    check_uploaded_file(Context).

-spec validate_delete_resource_selector(cb_context:context(), path_token(), path_token()) -> cb_context:context().
validate_delete_resource_selector(Context, _ResourceId, _SelectorName) ->
    cb_context:validate_request_data(<<"resource_selectors">>, Context).

-spec post(cb_context:context()) -> cb_context:context().
-spec post(cb_context:context(), path_token(), path_token(), path_token(), path_token()) -> cb_context:context().
post(Context) ->
    post_rules(Context).
post(Context, ?RESOURCE, ResourceId, ?NAME, SelectorName) ->
    post_resource_selector(Context, ResourceId, SelectorName);
post(Context, ?NAME, SelectorName, ?RESOURCE, ResourceId) ->
    post_resource_selector(Context, ResourceId, SelectorName).

-spec post_rules(cb_context:context()) -> cb_context:context().
post_rules(Context) ->
    crossbar_doc:save(Context).

-spec post_resource_selector(cb_context:context(), path_token(), path_token()) -> cb_context:context().
post_resource_selector(Context, ResourceId, SelectorName) ->
    _ = init_db(Context),
    upload_selectors_csv(Context, ResourceId, SelectorName).

-spec put(cb_context:context(), path_token(), path_token(), path_token(), path_token()) -> cb_context:context().
put(Context, ?RESOURCE, ResourceId, ?NAME, SelectorName) ->
    put_resource_selector(Context, ResourceId, SelectorName);
put(Context, ?NAME, SelectorName, ?RESOURCE, ResourceId) ->
    put_resource_selector(Context, ResourceId, SelectorName).

-spec put_resource_selector(cb_context:context(), ne_binary(), ne_binary()) -> cb_context:context().
put_resource_selector(Context, ResourceId, SelectorName) ->
    maybe_suppress_change_notice(),
    Data = kz_json:get_ne_value(<<"selectors">>, cb_context:req_data(Context), []),
    Db = cb_context:account_db(Context),
    _ = init_db(Context),
    BulkLimit = kz_datamgr:max_bulk_insert(),
    {Stats, LastJObjs} = lists:foldl(fun({[{S, V}]}, {AccStats, JObjs}) ->
                                             J = generate_selector_doc(Context, ResourceId, SelectorName, S, V),
                                             maybe_save_selectors(Db, AccStats, [J|JObjs], BulkLimit);
                                        (S, {AccStats, JObjs}) ->
                                             J = generate_selector_doc(Context, ResourceId, SelectorName, S, 'undefined'),
                                             maybe_save_selectors(Db, AccStats, [J|JObjs], BulkLimit)
                                     end
                                    ,{?ZERO_STATS, []}
                                    ,Data
                                    ),
    FinalStats = do_save_selectors(Db, Stats, LastJObjs),
    maybe_send_db_change_notice(Db, FinalStats),
    crossbar_util:response(kz_json:from_list(FinalStats), Context).

-spec generate_selector_doc(cb_context:context(), ne_binary(), ne_binary(), ne_binary(), api_binary()) ->
                                   kz_json:object().
generate_selector_doc(Context, ResourceId, SelectorName, Selector, Value) ->
    Props = [{<<"pvt_type">>, <<"resource_selector">>}
            ,{<<"name">>, SelectorName}
            ,{<<"selector">>, Selector}
            ,{<<"resource">>, ResourceId}
            ,{<<"value">>, Value}
            ,{<<"pvt_auth_account_id">>, cb_context:auth_account_id(Context)}
            ,{<<"pvt_request_id">>, cb_context:req_id(Context)}
            ],
    kz_json:from_list(props:filter_undefined(Props)).

-spec delete(cb_context:context(), path_token(), path_token(), path_token(), path_token()) -> cb_context:context().
delete(Context, ?RESOURCE, ResourceId, ?NAME, SelectorName) ->
    delete_resource_selector(Context, ResourceId, SelectorName);
delete(Context, ?NAME, SelectorName, ?RESOURCE, ResourceId) ->
    delete_resource_selector(Context, ResourceId, SelectorName).

-spec delete_resource_selector(cb_context:context(), path_token(), path_token()) -> cb_context:context().
delete_resource_selector(Context, ResourceId, SelectorName) ->
    Data = kz_json:get_ne_value(<<"selectors">>, cb_context:req_data(Context), []),
    bulk_delete_selectors(Context, ResourceId, SelectorName, Data).

-spec bulk_delete_selectors(cb_context:context(), path_token(), path_token(), ne_binaries()) -> cb_context:context().
bulk_delete_selectors(Context, ResourceId, SelectorName, [<<"_all">>]) ->
    kz_datamgr:suppress_change_notice(),
    do_bulk_delete_all_selectors(Context, ResourceId, SelectorName);
bulk_delete_selectors(Context, ResourceId, SelectorName, DelKeys) ->
    maybe_suppress_change_notice(),
    Db = cb_context:account_db(Context),
    BulkLimit = kz_datamgr:max_bulk_insert(),
    DelKeysBlocks = split_keys(DelKeys, BulkLimit),
    DelIDs = lists:foldl(fun(Block, AccIDs) ->
                                 Keys = [ [ResourceId, SelectorName, K ] || K <- Block ],
                                 Options = [{'keys', Keys}],
                                 {'ok', Result} = kz_datamgr:get_results(Db, <<"resource_selectors/resource_name_selector_listing">>, Options),
                                 lists:foldl(fun(R, Acc) ->
                                                     ID = kz_json:get_ne_value(<<"id">>, R, []),
                                                     [ ID |  Acc ]
                                             end
                                            ,AccIDs
                                            ,Result
                                            )
                         end
                        ,[]
                        ,DelKeysBlocks
                        ),
    DelIDsBlocks = split_keys(DelIDs, BulkLimit),
    Stats = lists:foldl(fun(Block, AccStats) ->
                                NewStats = do_delete_selectors(Db, Block, AccStats),
                                _ = refresh_selectors_index(Db),
                                NewStats
                        end
                       ,?ZERO_STATS
                       ,DelIDsBlocks
                       ),
    maybe_send_db_change_notice(Db, Stats),
    crossbar_util:response(kz_json:from_list(Stats), Context).

-spec do_bulk_delete_all_selectors(cb_context:context(), path_token(), path_token()) -> cb_context:context().
do_bulk_delete_all_selectors(Context, ResourceId, SelectorName) ->
    Db = cb_context:account_db(Context),
    BulkLimit = kz_datamgr:max_bulk_insert(),
    Options = [{'key', [ResourceId, SelectorName]}
              ,{'limit', BulkLimit}
              ,{'reduce', 'false'}
              ],
    Stats = do_delete_all_selectors(Db, Options, ?ZERO_STATS),
    crossbar_util:response(kz_json:from_list(Stats), Context).

-spec do_delete_all_selectors(ne_binary(), kz_proplist(), kz_proplist()) ->
                                     kz_proplist().
do_delete_all_selectors(Db, Options, AccStats) ->
    maybe_suppress_change_notice(),
    {'ok', SearchResult}  = kz_datamgr:get_results(Db, <<"resource_selectors/resource_name_listing">>, Options),
    Stats = case [ kz_json:get_ne_value(<<"id">>, R, []) || R <- SearchResult ] of
                [] -> AccStats;
                [[]] -> AccStats;
                IDs ->
                    NewStats = do_delete_selectors(Db, IDs, AccStats),
                    do_delete_all_selectors(Db, Options, NewStats)
            end,
    _ = refresh_selectors_index(Db),
    maybe_send_db_change_notice(Db, Stats),
    Stats.

-spec do_delete_selectors(ne_binary(), ne_binaries(), kz_proplist()) ->
                                 kz_proplist().
do_delete_selectors(Db, IDs, AccStats) ->
    {'ok', Result} = kz_datamgr:del_docs(Db, IDs),
    get_stat_from_result(Result, AccStats).

-spec split_keys(ne_binaries(), non_neg_integer()) -> [ne_binaries()].
-spec split_keys(ne_binaries(), [ne_binaries()], non_neg_integer()) -> [ne_binaries()].
split_keys(Keys, BlockSize) -> split_keys(Keys, [], BlockSize).
split_keys(Keys, Acc, BlockSize) when length(Keys) =< BlockSize -> [Keys | Acc];
split_keys(Keys, Acc, BlockSize) ->
    {Block, Rest} = lists:split(BlockSize, Keys),
    split_keys(Rest, [Block | Acc], BlockSize).

-spec refresh_selectors_index(ne_binary()) -> 'ok'.
refresh_selectors_index(Db) ->
    %% {'ok', _} = kz_datamgr:all_docs(Db, [{limit, 1}]),
    {'ok', _} = kz_datamgr:get_results(Db, ?SRS_LIST, [{'limit', 1}]),
    'ok'.

-spec get_stat_from_result(kz_json:objects(), kz_proplist()) -> kz_proplist().
get_stat_from_result(JObj, AccStats) ->
    lists:foldl(fun(Row, Acc) ->
                        case kz_json:get_value(<<"rev">>, Row) of
                            'undefined' ->
                                Err = props:get_integer_value('error', Acc),
                                Total = props:get_integer_value('total', Acc),
                                props:set_values([{'error', Err + 1}
                                                 ,{'total', Total + 1}
                                                 ]
                                                ,Acc);
                            _ ->
                                Success = props:get_integer_value('success', Acc),
                                Total = props:get_integer_value('total', Acc),
                                props:set_values([{'success', Success + 1}
                                                 ,{'total', Total + 1}
                                                 ]
                                                ,Acc)
                        end
                end
               ,AccStats
               ,JObj
               ).

-spec maybe_suppress_change_notice() -> 'ok'.
maybe_suppress_change_notice() ->
    case ?SUPPRESS_SRS_NOTICE of
        'false' -> 'ok';
        'true' ->
            kz_datamgr:suppress_change_notice(),
            'ok'
    end.

-spec maybe_send_db_change_notice(ne_binary(), kz_proplist()) -> 'ok'.
maybe_send_db_change_notice(Db, Stats) ->
    case props:get_integer_value('success', Stats, 0) > 0
        andalso ?SUPPRESS_SRS_NOTICE
    of
        'true' ->
            _ = kz_util:spawn(fun() -> kzs_publish:publish_db(Db, 'edited') end),
            'ok';
        'false' -> 'ok'
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

init_db(Context) ->
    Db = cb_context:account_db(Context),
    _ = kz_datamgr:db_create(Db),
    kz_datamgr:revise_doc_from_file(Db, 'crossbar', "views/resource_selectors.json").

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%--------------------------------------------------------------------
-spec load_rules(cb_context:context()) -> cb_context:context().
load_rules(Context) ->
    crossbar_doc:load(?RULES_PVT_TYPE, Context, ?TYPE_CHECK_OPTION(?RULES_PVT_TYPE)).

-spec summary_resource(cb_context:context()) -> cb_context:context().
-spec summary_resource(cb_context:context(), path_token()) -> cb_context:context().
summary_resource(Context) ->
    Fun = fun normalize_view_results/2,
    Options = ['group'],
    crossbar_doc:load_view(<<"resource_selectors/resource_listing">>, Options, Context, Fun).
summary_resource(Context, ResourceId) ->
    Fun = fun normalize_resource_result/2,
    Options = [{'startkey', [ResourceId]}
              ,{'endkey', [ResourceId, "{}"]}
              ,'group'
              ],
    crossbar_doc:load_view(<<"resource_selectors/resource_name_listing">>, Options, Context, Fun).

-spec summary_selector(cb_context:context()) -> cb_context:context().
-spec summary_selector(cb_context:context(), path_token()) -> cb_context:context().
summary_selector(Context) ->
    Fun = fun normalize_view_results/2,
    Options = ['group'],
    crossbar_doc:load_view(<<"resource_selectors/name_listing">>, Options, Context, Fun).
summary_selector(Context, SelectorName) ->
    Fun = fun normalize_selector_result/2,
    Options = [{'startkey', [SelectorName]}
              ,{'endkey', [SelectorName, "{}"]}
              ,'group'
              ],
    crossbar_doc:load_view(<<"resource_selectors/name_resource_listing">>, Options, Context, Fun).

-spec summary_resource_selector(cb_context:context(), path_token(), path_token()) -> cb_context:context().
summary_resource_selector(Context, ResourceId, SelectorName) ->
    Fun = fun normalize_resource_selector_result/2,
    Options = [{'key', [ResourceId, SelectorName]}
              ,{'reduce', 'false'}
              ],
    crossbar_doc:load_view(<<"resource_selectors/resource_name_listing">>, Options, Context, Fun).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the resuts of a view
%% @end
%%--------------------------------------------------------------------
-spec normalize_view_results(kz_json:object(), kz_json:objects()) -> kz_json:objects().
normalize_view_results(JObj, Acc) ->
    Key = kz_json:get_value(<<"key">>, JObj),
    Value = kz_json:get_value(<<"value">>, JObj),
    [kz_json:from_list([{Key, Value}]) | Acc].

-spec normalize_selector_result(kz_json:object(), kz_json:objects()) -> kz_json:objects().
normalize_selector_result(JObj, Acc) ->
    [ _, Key ] = kz_json:get_value(<<"key">>, JObj),
    Value = kz_json:get_value(<<"value">>, JObj),
    [kz_json:from_list([{Key, Value}]) | Acc].

-spec normalize_resource_result(kz_json:object(), kz_json:objects()) -> kz_json:objects().
normalize_resource_result(JObj, Acc) ->
    [ _, Key ] = kz_json:get_value(<<"key">>, JObj),
    Value = kz_json:get_value(<<"value">>, JObj),
    [kz_json:from_list([{Key, Value}]) | Acc].

-spec normalize_resource_selector_result(kz_json:object(), kz_json:objects()) -> kz_json:objects().
normalize_resource_selector_result(JObj, Acc) ->
    Selector = kz_json:get_value([<<"value">>, <<"selector">>], JObj),
    case kz_json:get_value([<<"value">>, <<"value">>], JObj) of
        'undefined' -> [Selector | Acc];
        Value -> [{[{Selector, Value}]} | Acc]
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Check the uploaded file for CSV
%% resource.
%% @end
%%--------------------------------------------------------------------
-spec check_uploaded_file(cb_context:context()) -> cb_context:context().
check_uploaded_file(Context) ->
    check_uploaded_file(Context, cb_context:req_files(Context)).

check_uploaded_file(Context, [{_Name, File}|_]) ->
    lager:debug("checking file ~s", [_Name]),
    case kz_json:get_value(<<"contents">>, File) of
        'undefined' ->
            error_no_file(Context);
        Bin when is_binary(Bin) ->
            lager:debug("file: ~s", [Bin]),
            cb_context:set_resp_status(Context, 'success')
    end;
check_uploaded_file(Context, _ReqFiles) ->
    error_no_file(Context).

-spec error_no_file(cb_context:context()) -> cb_context:context().
error_no_file(Context) ->
    cb_context:add_validation_error(<<"file">>
                                   ,<<"required">>
                                   ,kz_json:from_list([{<<"message">>, <<"no file to process">>}])
                                   ,Context
                                   ).

-spec on_successful_rules_validation(cb_context:context()) -> cb_context:context().
on_successful_rules_validation(Context) ->
    maybe_handle_load_failure(crossbar_doc:load_merge(?RULES_PVT_TYPE, Context, ?TYPE_CHECK_OPTION(?RULES_PVT_TYPE))).

-spec is_global_request(cb_context:context()) -> boolean().
is_global_request(Context) ->
    case cb_context:account_id(Context) of
        'undefined' ->
            lager:debug("request is for global resources"),
            'true';
        AccountId ->
            lager:debug("request is for local account ~s", [AccountId]),
            'false'
    end.

-spec maybe_handle_load_failure(cb_context:context()) ->
                                       cb_context:context().
-spec maybe_handle_load_failure(cb_context:context(), pos_integer()) ->
                                       cb_context:context().
maybe_handle_load_failure(Context) ->
    maybe_handle_load_failure(Context, cb_context:resp_error_code(Context)).

maybe_handle_load_failure(Context, 404) ->
    JObj = kz_doc:set_type(kz_doc:set_id(cb_context:req_data(Context),?RULES_PVT_TYPE), ?RULES_PVT_TYPE),
    cb_context:setters(Context
                      ,[{fun cb_context:set_resp_status/2, 'success'}
                       ,{fun cb_context:set_resp_data/2, kz_json:public_fields(JObj)}
                       ,{fun cb_context:set_doc/2, crossbar_doc:update_pvt_parameters(JObj, Context)}
                       ]);
maybe_handle_load_failure(Context, _RespCode) -> Context.

%%
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert the file, based on content-type, to selector documents
%% @end
%%--------------------------------------------------------------------
-spec upload_selectors_csv(cb_context:context(), ne_binary(), ne_binary()) -> cb_context:context().
upload_selectors_csv(Context, RessourceId, SelectorName) ->
    _ = cb_context:put_reqid(Context),
    process_upload_file(Context, cb_context:req_files(Context), RessourceId, SelectorName).

-spec process_upload_file(cb_context:context(), req_files(), ne_binary(), ne_binary()) ->
                                 cb_context:context().
process_upload_file(Context, [{_Name, File}|_], RessourceId, SelectorName) ->
    maybe_suppress_change_notice(),
    lager:debug("converting file ~s", [_Name]),
    Stats = convert_file(kz_json:get_binary_value([<<"headers">>, <<"content_type">>], File)
                        ,kz_json:get_value(<<"contents">>, File)
                        ,Context
                        ,RessourceId
                        ,SelectorName
                        ),
    Db = cb_context:account_db(Context),
    maybe_send_db_change_notice(Db, Stats),
    crossbar_util:response(kz_json:from_list(Stats), Context);
process_upload_file(Context, _ReqFiles, _, _) ->
    error_no_file(Context).

-spec convert_file(ne_binary(), ne_binary(), cb_context:context(), ne_binary(), ne_binary()) ->
                          kz_proplist().
convert_file(<<"text/csv">>, FileContents, Context, RessourceId, SelectorName) ->
    csv_to_selectors(FileContents, Context, RessourceId, SelectorName);
convert_file(<<"text/comma-separated-values">>, FileContents, Context, RessourceId, SelectorName) ->
    csv_to_selectors(FileContents, Context, RessourceId, SelectorName);
convert_file(ContentType, _, _, _, _) ->
    lager:debug("unknown content type: ~s", [ContentType]),
    throw({'unknown_content_type', ContentType}).

-spec csv_to_selectors(ne_binary(), cb_context:context(), ne_binary(), ne_binary()) ->
                              kz_proplist().
csv_to_selectors(CSV, Context, ResourceId, SelectorName) ->
    BulkLimit = kz_datamgr:max_bulk_insert(),
    Db = cb_context:account_db(Context),
    CsvConfig = maybe_override_csv_config(Context
                                         ,kapps_config:get(?MOD_CONFIG_CAT
                                                          ,<<"csv_config">>
                                                          ,?DEFAULT_CSV_CONFIG
                                                          )
                                         ),
    SelectorCol = kz_json:get_integer_value(<<"selector_column">>, CsvConfig),
    ValueCol = kz_json:get_integer_value(<<"value_column">>, CsvConfig),
    Fun = fun(Row, {AccStats, JObjs}) ->
                  {Selector, Value} = get_selector_data_from_row(Row, SelectorCol, ValueCol),
                  JObj = generate_selector_doc(Context, ResourceId, SelectorName, Selector, Value),
                  maybe_save_selectors(Db, AccStats, [JObj | JObjs], BulkLimit)
          end,
    {'ok', {Stats, LastJObjs}} = ecsv:process_csv_binary_with(CSV
                                                             ,Fun
                                                             ,{?ZERO_STATS, []}
                                                             ),
    do_save_selectors(Db, Stats, LastJObjs).

-spec maybe_override_csv_config(cb_context:context(), kz_json:object()) -> kz_json:object().
maybe_override_csv_config(Context, CsvConfig) ->
    kz_json:foldl(fun(Key, Value, Acc) ->
                          NewValue = cb_context:req_value(Context, Key, Value),
                          kz_json:set_value(Key, NewValue, Acc)
                  end
                 ,kz_json:new()
                 ,CsvConfig
                 ).

-spec get_selector_data_from_row(strings(), integer(), integer()) -> {api_binary(), api_binary()}.
get_selector_data_from_row(Row, SelectorCol, ValueCol)
  when is_integer(SelectorCol) andalso is_integer(ValueCol)
       andalso SelectorCol > 0 andalso ValueCol > 0
       andalso length(Row) >= SelectorCol andalso length(Row) >= ValueCol
       ->
    {kz_util:to_binary(lists:nth(SelectorCol, Row)), kz_util:to_binary(lists:nth(ValueCol, Row))};
get_selector_data_from_row(Row, SelectorCol, _ValueCol)
  when is_integer(SelectorCol) andalso SelectorCol > 0 andalso length(Row) >= SelectorCol
       ->
    {kz_util:to_binary(lists:nth(SelectorCol, Row)), 'undefined'};
get_selector_data_from_row(_Row, _SelectorCol, _ValueCol) -> {'undefined', 'undefined'}.

-spec maybe_save_selectors(ne_binary(), kz_proplist(), kz_json:objects(), integer()) ->
                                  {kz_proplist(), kz_json:objects()}.
maybe_save_selectors(Db, AccStats, JObjs, BulkLimit) when length(JObjs) >= BulkLimit ->
    NewStats = do_save_selectors(Db, AccStats, JObjs),
    {NewStats, []};
maybe_save_selectors(_Db, AccStats, JObjs, _BulkLimit) ->
    {AccStats, JObjs}.

-spec do_save_selectors(ne_binary(), kz_proplist(), kz_json:objects()) ->
                               kz_proplist().
do_save_selectors(_Db, AccStats, []) -> AccStats;
do_save_selectors(Db, AccStats, JObjs) ->
    {'ok', Result} = kz_datamgr:save_docs(Db, JObjs),
    _ = refresh_selectors_index(Db),
    get_stat_from_result(Result, AccStats).
