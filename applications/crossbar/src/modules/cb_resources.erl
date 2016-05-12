%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz INC
%%% @doc
%%%
%%% Handle client requests for resource documents
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_resources).

-export([init/0
         ,authorize/1
         ,allowed_methods/0, allowed_methods/1, allowed_methods/2, allowed_methods/3
         ,resource_exists/0, resource_exists/1, resource_exists/2, resource_exists/3
         ,content_types_accepted/1, content_types_accepted/2, content_types_accepted/3, content_types_accepted/4
         ,validate/1, validate/2, validate/3, validate/4
         ,put/1, put/2, put/4
         ,post/2, post/4
         ,delete/2, delete/4
        ]).

-include("crossbar.hrl").

-define(CB_LIST, <<"resources/crossbar_listing">>).
-define(SRS_LIST, <<"selectors/crossbar_listing">>).
-define(SRS_SEARCH, <<"selectors/crossbar_search">>).
-define(SRS_RULES, <<"selector_rules">>).
-define(JOBS_LIST, <<"resources/jobs_listing">>).
-define(MOD_CONFIG_CAT, <<(?CONFIG_CAT)/binary, ".resources">>).
-define(SUPPRESS_SRS_NOTICE, kapps_config:get_is_true(?MOD_CONFIG_CAT, <<"suppress_selectors_change_notice">>, 'false')).
-define(UPLOAD_MIME_TYPES, [{<<"text">>, <<"csv">>}
                            ,{<<"text">>, <<"comma-separated-values">>}
                           ]).
-define(COLLECTION, <<"collection">>).
-define(JOBS, <<"jobs">>).
-define(SELECTORS, <<"selectors">>).
%% TODO: get list from kz_srs_* modules
-define(SELECTOR_TYPES, [<<"prefix">>
                         ,<<"cid_prefix">>
                         ,<<"flag">>
                         ,<<"weight">>
                         ,<<"regex">>
                         ,<<"regex_cid">>
                        ]).
-define(ZERO_STATS, [{'total', 0}
                     ,{'success', 0}
                     ,{'error', 0}
                    ]).

-define(KEY_SUCCESS, <<"success">>).

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    _ = kz_datamgr:revise_doc_from_file(?KZ_SIP_DB, 'crossbar', "views/resources.json"),
    _ = kz_datamgr:revise_doc_from_file(?KZ_OFFNET_DB, 'crossbar', "views/resources.json"),
    _ = kz_datamgr:revise_doc_from_file(?KZ_RESOURCE_SELECTORS_DB, 'crossbar', "views/resource_selectors.json"),

    _Pid = maybe_start_jobs_listener(),
    lager:debug("started jobs listener: ~p", [_Pid]),

    [crossbar_bindings:bind(Binding, ?MODULE, F)
     || {Binding, F} <- [{<<"*.allowed_methods.resources">>, 'allowed_methods'}
                         ,{<<"*.resource_exists.resources">>, 'resource_exists'}
                         ,{<<"*.content_types_accepted.resources">>, 'content_types_accepted'}
                         ,{<<"*.validate.resources">>, 'validate'}
                         ,{<<"*.execute.put.resources">>, 'put'}
                         ,{<<"*.execute.post.resources">>, 'post'}
                         ,{<<"*.execute.delete.resources">>, 'delete'}

                         ,{<<"*.allowed_methods.global_resources">>, 'allowed_methods'}
                         ,{<<"*.resource_exists.global_resources">>, 'resource_exists'}
                         ,{<<"*.content_types_accepted.global_resources">>, 'content_types_accepted'}
                         ,{<<"*.validate.global_resources">>, 'validate'}
                         ,{<<"*.execute.put.global_resources">>, 'put'}
                         ,{<<"*.execute.post.global_resources">>, 'post'}
                         ,{<<"*.execute.delete.global_resources">>, 'delete'}

                         ,{<<"*.allowed_methods.local_resources">>, 'allowed_methods'}
                         ,{<<"*.resource_exists.local_resources">>, 'resource_exists'}
                         ,{<<"*.content_types_accepted.local_resources">>, 'content_types_accepted'}
                         ,{<<"*.validate.local_resources">>, 'validate'}
                         ,{<<"*.execute.put.local_resources">>, 'put'}
                         ,{<<"*.execute.post.local_resources">>, 'post'}
                         ,{<<"*.execute.delete.local_resources">>, 'delete'}

                         ,{<<"*.authorize">>, 'authorize'}
                        ]
    ].

-spec maybe_start_jobs_listener() -> pid().
maybe_start_jobs_listener() ->
    case jobs_listener_pid() of
        'undefined' ->
            {'ok', Pid} = crossbar_module_sup:start_child('cb_jobs_listener'),
            Pid;
        Pid -> Pid
    end.

-spec jobs_listener_pid() -> api_pid().
jobs_listener_pid() ->
    whereis('cb_jobs_listener').

-spec authorize(cb_context:context()) ->
                       boolean() |
                       {'halt', cb_context:context()}.
-spec authorize(cb_context:context(), req_nouns()) ->
                       boolean() |
                       {'halt', cb_context:context()}.
authorize(Context) ->
    authorize(Context, cb_context:req_nouns(Context)).

authorize(Context, [{<<"global_resources">>, _}|_]) ->
    maybe_authorize_admin(Context);
authorize(Context, [{<<"resources">>, _} | _]) ->
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
-spec allowed_methods(path_token(), path_token(), path_token()) -> http_methods().
allowed_methods() ->
    [?HTTP_GET, ?HTTP_PUT].

allowed_methods(?COLLECTION) ->
    [?HTTP_PUT, ?HTTP_POST];
allowed_methods(?JOBS) ->
    [?HTTP_GET, ?HTTP_PUT];
allowed_methods(?SELECTORS) ->
    [?HTTP_GET, ?HTTP_POST];
allowed_methods(_ResourceId) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_DELETE].
allowed_methods(?JOBS, _JobId) ->
    [?HTTP_GET].
allowed_methods(_ResourceId, ?SELECTORS, _SelectorType) ->
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
-spec resource_exists(path_token(), path_token(), path_token()) -> 'true'.
resource_exists() -> 'true'.
resource_exists(_) -> 'true'.
resource_exists(?JOBS, _ID) -> 'true'.
resource_exists(_ID, ?SELECTORS, SelectorType) -> lists:member(SelectorType,  ?SELECTOR_TYPES).

-spec content_types_accepted(cb_context:context()) -> cb_context:context().
-spec content_types_accepted(cb_context:context(), path_token()) -> cb_context:context().
-spec content_types_accepted(cb_context:context(), path_token(), path_token()) -> cb_context:context().
-spec content_types_accepted(cb_context:context(), path_token(), path_token(), path_token()) -> cb_context:context().
content_types_accepted(Context) ->
    Context.
content_types_accepted(Context, _) ->
    Context.
content_types_accepted(Context, _, _) ->
    Context.
content_types_accepted(Context, _ResourceId, ?SELECTORS, _SelectorType) ->
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
-spec validate(cb_context:context(), path_token(), path_token(), path_token()) -> cb_context:context().
validate(Context) ->
    case is_global_resource_request(Context) of
        'true' ->
            validate_resources(cb_context:set_account_db(Context, ?KZ_OFFNET_DB)
                               ,cb_context:req_verb(Context)
                              );
        'false' ->
            validate_resources(Context, cb_context:req_verb(Context))
    end.

validate(Context, ?COLLECTION) ->
    case is_global_resource_request(Context) of
        'true' ->
            validate_collection(cb_context:set_account_db(Context, ?KZ_OFFNET_DB));
        'false' ->
            validate_collection(Context)
    end;
validate(Context, ?JOBS) ->
    validate_jobs(maybe_set_account_to_master(Context), cb_context:req_verb(Context));
validate(Context, ?SELECTORS) ->
    validate_selector_rules(Context, cb_context:req_verb(Context));
validate(Context, Id) ->
    case is_global_resource_request(Context) of
        'true' ->
            validate_resource(cb_context:set_account_db(Context, ?KZ_OFFNET_DB)
                              ,Id
                              ,cb_context:req_verb(Context)
                             );
        'false' ->
            validate_resource(Context, Id, cb_context:req_verb(Context))
    end.

validate(Context, ?JOBS, JobId) ->
    read_job(maybe_set_account_to_master(Context), JobId).

validate(Context, ResourceId, ?SELECTORS, SelectorType) ->
    validate_selectors(set_selectors_db(Context), ResourceId, SelectorType, cb_context:req_verb(Context)).

-spec set_selectors_db(cb_context:context()) -> cb_context:context().
set_selectors_db(Context) ->
    case is_global_resource_request(Context) of
        'true' -> cb_context:set_account_db(Context, ?KZ_RESOURCE_SELECTORS_DB);
        'false' ->
            AccountId = cb_context:account_id(Context),
            AccountDb = kz_util:format_resource_selectors_db(AccountId),
            cb_context:set_account_db(Context, AccountDb)
    end.

-spec set_selector_rules_db(cb_context:context()) -> cb_context:context().
set_selector_rules_db(Context) ->
    case is_global_resource_request(Context) of
        'true' -> 
            {'ok', MasterAccountDb} = kapps_util:get_master_account_db(),
            cb_context:set_account_db(Context, MasterAccountDb);
        'false' -> Context
    end.

-spec maybe_set_account_to_master(cb_context:context()) -> cb_context:context().
maybe_set_account_to_master(Context) ->
    case cb_context:account_id(Context) of
        'undefined' -> set_account_to_master(Context);
        _AccountId -> Context
    end.

-spec set_account_to_master(cb_context:context()) -> cb_context:context().
set_account_to_master(Context) ->
    {'ok', AccountId} = kapps_util:get_master_account_id(),
    cb_context:set_account_id(Context, AccountId).

-spec validate_resources(cb_context:context(), http_method()) -> cb_context:context().
validate_resources(Context, ?HTTP_GET) ->
    summary(Context);
validate_resources(Context, ?HTTP_PUT) ->
    case cb_context:account_db(Context) of
        ?KZ_OFFNET_DB -> create(Context);
        _AccountDb -> create_local(Context)
    end.

-spec validate_resource(cb_context:context(), path_token(), http_method()) -> cb_context:context().
validate_resource(Context, Id, ?HTTP_GET) ->
    read(Id, Context);
validate_resource(Context, Id, ?HTTP_POST) ->
    case cb_context:account_db(Context) of
        ?KZ_OFFNET_DB -> update(Id, Context);
        _AccountDb -> update_local(Context, Id)
    end;
validate_resource(Context, Id, ?HTTP_DELETE) ->
    read(Id, Context).

validate_collection(Context) ->
    lists:foldl(fun validate_collection_fold/2
                ,cb_context:setters(Context
                                    ,[{fun cb_context:set_doc/2, kz_json:new()}
                                      ,{fun cb_context:set_resp_data/2, kz_json:new()}
                                      ,{fun cb_context:set_resp_status/2, 'success'}
                                     ]
                                   )
                ,cb_context:req_data(Context)
               ).

-type collection_fold_acc() :: cb_context:context().
-spec validate_collection_fold(kz_json:object(), collection_fold_acc()) -> collection_fold_acc().
validate_collection_fold(Resource, C) ->
    Id = kz_doc:id(Resource, kz_datamgr:get_uuid()),
    case validate_collection_resource(kz_doc:set_id(Resource, Id)
                                     ,C
                                     ,cb_context:req_verb(C)
                                     )
    of
        {'ok', C1} ->
            lager:debug("~s loaded successfully", [Id]),
            cb_context:set_resp_data(C
                                     ,kz_json:set_value([?KEY_SUCCESS, Id], cb_context:doc(C1), cb_context:resp_data(C))
                                    );
        {'error', 'not_found'} ->
            RespData = cb_context:resp_data(C),
            lager:debug("~s not found", [Id]),
            cb_context:set_resp_data(C, kz_json:set_value([<<"errors">>, Id], <<"resource does not exist">>, RespData));
        {'error', Errors} ->
            RespData = cb_context:resp_data(C),
            lager:debug("~s failed validation: ~p", [Id, Errors]),
            lager:debug("Adding to ~p", [RespData]),
            cb_context:set_resp_data(C, kz_json:set_value([<<"errors">>, Id], Errors, RespData))
    end.

-spec validate_collection_resource(kz_json:object(), cb_context:context(), http_method()) ->
                                          {'ok', cb_context:context()} |
                                          {'error', 'not_found' | kz_json:object()}.
validate_collection_resource(Resource, Context, ?HTTP_POST) ->
    C1 = crossbar_doc:load(kz_doc:id(Resource), Context, ?TYPE_CHECK_OPTION(<<"resource">>)),
    case cb_context:resp_status(C1) of
        'success' -> validate_collection_resource_patch(Resource, C1);
        _Status -> {'error', 'not_found'}
    end;
validate_collection_resource(Resource, Context, ?HTTP_PUT) ->
    Context1 = create(cb_context:set_req_data(Context, Resource)),
    case cb_context:resp_status(Context1) of
        'success' -> {'ok', Context1};
        _Status -> {'error', cb_context:validation_errors(Context1)}
    end.

-spec validate_collection_resource_patch(kz_json:object(), cb_context:context()) ->
                                                {'ok', cb_context:context()} |
                                                {'error', kz_json:object()}.
validate_collection_resource_patch(PatchJObj, Context) ->
    PatchedJObj = kz_json:merge_jobjs(kz_doc:public_fields(PatchJObj), cb_context:doc(Context)),
    Context1 = update(kz_doc:id(PatchedJObj)
                      ,cb_context:set_req_data(Context, PatchedJObj)
                     ),
    case cb_context:resp_status(Context1) of
        'success' -> {'ok', Context1};
        _Status -> {'error', cb_context:validation_errors(Context1)}
    end.

-spec validate_jobs(cb_context:context(), http_method()) -> cb_context:context().
validate_jobs(Context, ?HTTP_GET) ->
    jobs_summary(Context);
validate_jobs(Context, ?HTTP_PUT) ->
    create_job(Context).

-spec validate_selector_rules(cb_context:context(), http_method()) -> cb_context:context().
validate_selector_rules(Context, ?HTTP_GET) ->
    read_selector_rules(set_selector_rules_db(Context));
validate_selector_rules(Context, ?HTTP_POST) ->
    OnSuccess = fun(C) -> on_successful_selector_rules_validation(C) end,
    cb_context:validate_request_data(<<"resource_selector_rules">>, Context, OnSuccess).

-spec validate_selectors(cb_context:context(), path_token(), path_token(), http_method()) -> cb_context:context().
validate_selectors(Context, ResourceId, SelectorType, ?HTTP_GET) ->
    read_selectors(ResourceId, SelectorType, Context);
validate_selectors(Context, _ResourceId, _SelectorType, ?HTTP_PUT) ->
    cb_context:validate_request_data(<<"resource_selectors">>, Context);
validate_selectors(Context, _ResourceId, _SelectorType, ?HTTP_DELETE) ->
    cb_context:validate_request_data(<<"resource_selectors">>, Context);
validate_selectors(Context, _ResourceId, _SelectorType, ?HTTP_POST) ->
    check_uploaded_file(Context).

-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, ?COLLECTION) ->
    do_collection(Context, cb_context:account_db(Context));
post(Context, Id) ->
    do_post(Context, Id, cb_context:account_db(Context)).
-spec post(cb_context:context(), path_token(), path_token(), path_token()) -> cb_context:context().
post(Context, ResourceId, ?SELECTORS, SelectorType) ->
    upload_selectors_csv(Context, ResourceId, SelectorType).

-spec do_collection(cb_context:context(), ne_binary()) -> cb_context:context().
do_collection(Context, ?KZ_OFFNET_DB) ->
    _ = reload_acls(),
    _ = reload_gateways(),
    collection_process(Context);
do_collection(Context, _AccountDb) ->
    collection_process(Context).

-spec do_post(cb_context:context(), path_token(), ne_binary()) -> cb_context:context().
do_post(Context, _Id, ?KZ_OFFNET_DB) ->
    _ = reload_acls(),
    _ = reload_gateways(),
    crossbar_doc:save(Context);
do_post(Context, _Id, _AccountDb) ->
    Context1 = crossbar_doc:save(Context),
    _ = cb_local_resources:maybe_aggregate_resource(Context1),
    Context1.

-spec put(cb_context:context()) -> cb_context:context().
-spec put(cb_context:context(), path_token()) -> cb_context:context().
put(Context) ->
    do_put(Context, cb_context:account_db(Context)).

put(Context, ?COLLECTION) ->
    put_collection(Context, cb_context:account_db(Context));
put(Context, ?JOBS) ->
    put_job(Context).
put(Context, ResourceId, ?SELECTORS, SelectorType) ->
    put_selectors(Context, ResourceId, SelectorType).

-spec do_put(cb_context:context(), ne_binary()) -> cb_context:context().
do_put(Context, ?KZ_OFFNET_DB) ->
    _ = reload_acls(),
    _ = reload_gateways(),
    crossbar_doc:save(Context);
do_put(Context, _AccountDb) ->
    Context1 = crossbar_doc:save(Context),
    _ = cb_local_resources:maybe_aggregate_resource(Context1),
    Context1.

-spec put_collection(cb_context:context(), ne_binary()) -> cb_context:context().
put_collection(Context, ?KZ_OFFNET_DB) ->
    collection_process(Context);
put_collection(Context, _AccountDb) ->
    _ = reload_acls(),
    _ = reload_gateways(),
    collection_process(Context).

-spec put_job(cb_context:context()) -> cb_context:context().
put_job(Context) ->
    Modb = cb_context:account_modb(Context),
    Context1 = crossbar_doc:save(cb_context:set_account_db(Context, Modb)),

    case cb_context:resp_status(Context1) of
        'success' ->
            _ = cb_jobs_listener:publish_new_job(Context),
            crossbar_util:response_202(<<"Job scheduled">>, cb_context:resp_data(Context1), Context1);
        _Status ->
            Context1
    end.

-spec put_selectors(cb_context:context(), ne_binary(), ne_binary()) -> cb_context:context().
put_selectors(Context, ResourceId, SelectorType) ->
    maybe_suppress_change_notice(),
    Data = kz_json:get_ne_value(<<"selectors_data">>, cb_context:req_data(Context), []),
    Db = cb_context:account_db(Context),
    BulkLimit = kz_datamgr:max_bulk_insert(),
    {Stats, LastJObjs} = lists:foldl(fun(E, {AccStats, JObjs}) ->
                                             J = generate_selector_doc(Context, ResourceId, SelectorType, E),
                                             maybe_save_selectors(Db, AccStats, [J|JObjs], BulkLimit)
                                     end
                                     ,{?ZERO_STATS, []}
                                     ,Data
                                    ),
    FinalStats = do_save_selectors(Db, Stats, LastJObjs),
    maybe_send_db_change_notice(Db, FinalStats),
    crossbar_util:response(kz_json:from_list(FinalStats), Context).

-spec generate_selector_doc(cb_context:context(), ne_binary(), ne_binary(), ne_binary()) ->
    kz_json:object().
generate_selector_doc(Context, ResourceId, SelectorType, SelectorData) ->
    kz_json:from_list([{<<"pvt_type">>, <<"resource_selector">>}
                       ,{<<"selector_type">>, SelectorType}
                       ,{<<"selector_data">>, SelectorData}
                       ,{<<"resource">>, ResourceId}
                       ,{<<"pvt_auth_account_id">>, cb_context:auth_account_id(Context)}
                       ,{<<"pvt_request_id">>, cb_context:req_id(Context)}
                      ]).

-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, ResourceId) ->
    do_delete(Context, ResourceId, cb_context:account_db(Context)).

-spec delete(cb_context:context(), path_token(), path_token(), path_token()) -> cb_context:context().
delete(Context, ResourceId, ?SELECTORS, SelectorType) ->
    delete_selectors(Context, ResourceId, SelectorType).

-spec do_delete(cb_context:context(), ne_binary(), ne_binary()) -> cb_context:context().
do_delete(Context, _ResourceId, ?KZ_OFFNET_DB) ->
    _ = reload_acls(),
    _ = reload_gateways(),
    crossbar_doc:delete(Context);
do_delete(Context, ResourceId, _AccountDb) ->
    Context1 = crossbar_doc:delete(Context),
    _ = cb_local_resources:maybe_remove_aggregate(ResourceId, Context1),
    Context1.

-spec delete_selectors(cb_context:context(), path_token(), path_token()) -> cb_context:context().
delete_selectors(Context, ResourceId, SelectorType) ->
    Data = kz_json:get_ne_value(<<"selectors_data">>, cb_context:req_data(Context), []),
    bulk_delete_selectors(Context, ResourceId, SelectorType, Data).

-spec bulk_delete_selectors(cb_context:context(), path_token(), path_token(), ne_binaries()) -> cb_context:context().
bulk_delete_selectors(Context, ResourceId, SelectorType, [<<"_all">>]) ->
    kz_datamgr:suppress_change_notice(),
    do_bulk_delete_all_selectors(Context, ResourceId, SelectorType);
bulk_delete_selectors(Context, ResourceId, SelectorType, DelKeys) ->
    maybe_suppress_change_notice(),
    Db = cb_context:account_db(Context),
    BulkLimit = kz_datamgr:max_bulk_insert(),
    DelKeysBlocks = split_keys(DelKeys, BulkLimit),
    DelIDs = lists:foldl(fun(Block, AccIDs) ->
                                 Keys = [ [ResourceId, SelectorType, K ] || K <- Block ],
                                 Options = [{'keys', Keys}],
                                 {'ok', Result} = kz_datamgr:get_results(Db, ?SRS_SEARCH, Options),
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
do_bulk_delete_all_selectors(Context, ResourceId, SelectorType) ->
    Db = cb_context:account_db(Context),
    BulkLimit = kz_datamgr:max_bulk_insert(),
    Options = [{'key', [ResourceId, SelectorType]}
               ,{'limit', BulkLimit}
              ],
    Stats = do_delete_all_selectors(Db, Options, ?ZERO_STATS),
    crossbar_util:response(kz_json:from_list(Stats), Context).

-spec do_delete_all_selectors(ne_binary(), kz_proplist(), kz_proplist()) ->
    kz_proplist().
do_delete_all_selectors(Db, Options, AccStats) ->
    maybe_suppress_change_notice(),
    {'ok', SearchResult}  = kz_datamgr:get_results(Db, ?SRS_LIST, Options),
    Stats = case [ kz_json:get_ne_value(<<"id">>, R, []) || R <- SearchResult ] of
                [] -> AccStats;
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
    {'ok', _} = kz_datamgr:get_results(Db, ?SRS_LIST, [{limit, 1}]),
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load an instance from the database
%% @end
%%--------------------------------------------------------------------
-spec read(ne_binary(), cb_context:context()) -> cb_context:context().
read(Id, Context) ->
    crossbar_doc:load(Id, Context, ?TYPE_CHECK_OPTION(<<"resource">>)).

-spec read_job(cb_context:context(), ne_binary()) -> cb_context:context().
read_job(Context, ?MATCH_MODB_PREFIX(Year,Month,_) = JobId) ->
    Modb = cb_context:account_modb(Context, kz_util:to_integer(Year), kz_util:to_integer(Month)),
    leak_job_fields(crossbar_doc:load(JobId, cb_context:set_account_db(Context, Modb), ?TYPE_CHECK_OPTION(<<"resource">>)));
read_job(Context, ?MATCH_MODB_PREFIX_M1(Year,Month,_) = JobId) ->
    Modb = cb_context:account_modb(Context, kz_util:to_integer(Year), kz_util:to_integer(Month)),
    leak_job_fields(crossbar_doc:load(JobId, cb_context:set_account_db(Context, Modb), ?TYPE_CHECK_OPTION(<<"resource">>)));
read_job(Context, JobId) ->
    lager:debug("invalid job id format: ~s", [JobId]),
    crossbar_util:response_bad_identifier(JobId, Context).

-spec leak_job_fields(cb_context:context()) -> cb_context:context().
leak_job_fields(Context) ->
    case cb_context:resp_status(Context) of
        'success' ->
            JObj = cb_context:doc(Context),
            cb_context:set_resp_data(Context
                                     ,kz_json:set_values([{<<"timestamp">>, kz_doc:created(JObj)}
                                                          ,{<<"status">>, kz_json:get_value(<<"pvt_status">>, JObj)}
                                                         ]
                                                         ,cb_context:resp_data(Context)
                                                        )
                                    );
        _Status -> Context
    end.

-spec read_selector_rules(cb_context:context()) -> cb_context:context().
read_selector_rules(Context) ->
    crossbar_doc:load(?SRS_RULES, Context).

-spec read_selectors(ne_binary(), ne_binary(), cb_context:context()) -> cb_context:context().
read_selectors(ResourceId, SelectorType, Context) ->
    %% !!!
    %% TODO: change view, to be pagination friendly
    %% !!!
    crossbar_doc:load_view(?SRS_LIST
                           ,[{'key', [ResourceId, SelectorType]}]
                           ,Context
                           ,fun normalize_view_results/2
                          ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%--------------------------------------------------------------------
-spec summary(cb_context:context()) -> cb_context:context().
summary(Context) ->
    crossbar_doc:load_view(?CB_LIST, [], Context, fun normalize_view_results/2).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%--------------------------------------------------------------------
-spec jobs_summary(cb_context:context()) -> cb_context:context().
jobs_summary(Context) ->
    case cb_modules_util:range_view_options(Context) of
        {CreatedFrom, CreatedTo} ->
            crossbar_doc:load_view(?JOBS_LIST
                                   ,[{'startkey', CreatedFrom}
                                     ,{'endkey', CreatedTo}
                                     ,{'limit', crossbar_doc:pagination_page_size(Context)}
                                     ,{'databases', databases(Context, CreatedFrom, CreatedTo)}
                                    ]
                                   ,cb_context:set_account_db(Context, cb_context:account_modb(Context))
                                   ,fun normalize_view_results/2
                                  );
        Context1 -> Context1
    end.

-spec databases(cb_context:context(), pos_integer(), pos_integer()) -> ne_binaries().
databases(Context, CreatedFrom, CreatedTo) ->
    FromDb = cb_context:account_modb(Context, CreatedFrom),
    case cb_context:account_modb(Context, CreatedTo) of
        FromDb -> [FromDb];
        ToDb -> [FromDb, ToDb]
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the resuts of a view
%% @end
%%--------------------------------------------------------------------
-spec normalize_view_results(kz_json:object(), kz_json:objects()) -> kz_json:objects().
normalize_view_results(JObj, Acc) ->
    [kz_json:get_value(<<"value">>, JObj)|Acc].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Create a new instance with the data provided, if it is valid
%% @end
%%--------------------------------------------------------------------
-spec create(cb_context:context()) -> cb_context:context().
create(Context) ->
    OnSuccess = fun(C) -> on_successful_validation('undefined', C) end,
    cb_context:validate_request_data(<<"resources">>, Context, OnSuccess).

-spec create_job(cb_context:context()) -> cb_context:context().
create_job(Context) ->
    OnSuccess = fun(C) -> on_successful_job_validation('undefined', C) end,
    cb_context:validate_request_data(<<"resource_jobs">>, Context, OnSuccess).

-spec create_local(cb_context:context()) -> cb_context:context().
create_local(Context) ->
    OnSuccess = fun(C) -> on_successful_local_validation('undefined', C) end,
    cb_context:validate_request_data(<<"resources">>, Context, OnSuccess).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update an existing instance with the data provided, if it is
%% valid
%% @end
%%--------------------------------------------------------------------
-spec update(ne_binary(), cb_context:context()) -> cb_context:context().
update(Id, Context) ->
    OnSuccess = fun(C) -> on_successful_validation(Id, C) end,
    cb_context:validate_request_data(<<"resources">>, Context, OnSuccess).

-spec update_local(cb_context:context(), ne_binary()) -> cb_context:context().
update_local(Context, Id) ->
    OnSuccess = fun(C) -> on_successful_local_validation(Id, C) end,
    cb_context:validate_request_data(<<"resources">>, Context, OnSuccess).

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

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec on_successful_validation(api_binary(), cb_context:context()) -> cb_context:context().
on_successful_validation('undefined', Context) ->
    cb_context:set_doc(Context, kz_doc:set_type(cb_context:doc(Context), <<"resource">>));
on_successful_validation(Id, Context) ->
    crossbar_doc:load_merge(Id, Context, ?TYPE_CHECK_OPTION(<<"resource">>)).


-spec on_successful_local_validation(api_binary(), cb_context:context()) -> cb_context:context().
on_successful_local_validation(Id, Context) ->
    cb_local_resources:validate_request(Id, Context).

-spec on_successful_job_validation('undefined', cb_context:context()) -> cb_context:context().
on_successful_job_validation('undefined', Context) ->
    {Year, Month, _} = erlang:date(),
    Id = list_to_binary([kz_util:to_binary(Year)
                         ,kz_util:pad_month(Month)
                         ,"-"
                         ,kz_util:rand_hex_binary(8)
                        ]),

    cb_context:set_doc(Context
                       ,kz_json:set_values([{<<"pvt_type">>, <<"resource_job">>}
                                            ,{<<"pvt_status">>, <<"pending">>}
                                            ,{<<"pvt_auth_account_id">>, cb_context:auth_account_id(Context)}
                                            ,{<<"pvt_request_id">>, cb_context:req_id(Context)}
                                            ,{<<"_id">>, Id}

                                            ,{?KEY_SUCCESS, kz_json:new()}
                                            ,{<<"errors">>, kz_json:new()}
                                           ]
                                           ,cb_context:doc(Context)
                                          )
                      ).

-spec on_successful_selector_rules_validation(cb_context:context()) -> cb_context:context().
on_successful_selector_rules_validation(Context) ->
    cb_context:set_doc(Context, kz_doc:set_type(cb_context:doc(Context), <<"resource_selector_rules">>)).

-spec reload_acls() -> 'ok'.
reload_acls() ->
    lager:debug("published reloadacl"),
    kz_amqp_worker:cast([], fun(_) -> kapi_switch:publish_reload_acls() end).

-spec reload_gateways() -> 'ok'.
reload_gateways() ->
    lager:debug("published reload_gateways"),
    kz_amqp_worker:cast([], fun(_) -> kapi_switch:publish_reload_gateways() end).

-spec collection_process(cb_context:context()) -> cb_context:context().
-spec collection_process(cb_context:context(), kz_json:objects()) -> cb_context:context().
collection_process(Context) ->
    RespData = cb_context:resp_data(Context),

    case kz_util:is_empty(kz_json:get_value(<<"errors">>, RespData)) of
        'true' -> collection_process(Context, kz_json:get_value(?KEY_SUCCESS, RespData));
        'false' -> cb_context:set_resp_data(Context, kz_json:delete_key(?KEY_SUCCESS, RespData))
    end.
collection_process(Context, []) -> Context;
collection_process(Context, Successes) ->
    {Resources, _} = kz_json:get_values(Successes),
    _ = [lager:debug("save ~p", [Resource]) || Resource <- Resources],
    Context1 = crossbar_doc:save(cb_context:set_doc(Context, Resources)),
    case cb_context:resp_status(Context1) of
        'success' ->
            (cb_context:account_db(Context1) =/= ?KZ_OFFNET_DB)
                andalso cb_local_resources:maybe_aggregate_resources(Resources),
            summary(Context1);
        _Status -> 'ok'
    end.

-spec is_global_resource_request(cb_context:context()) -> boolean().
-spec is_global_resource_request(req_nouns(), api_binary()) -> boolean().
is_global_resource_request(Context) ->
    is_global_resource_request(cb_context:req_nouns(Context), cb_context:account_id(Context)).

is_global_resource_request(_ReqNouns, 'undefined') ->
    lager:debug("request is for global resources"),
    'true';
is_global_resource_request([{<<"global_resources">>, _}|_], _AccountId) ->
    lager:debug("request is for global resources"),
    'true';
is_global_resource_request(_ReqNouns, _AccountId) ->
    lager:debug("request is for local resources for account ~s", [_AccountId]),
    'false'.
%%
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert the file, based on content-type, to selector documents
%% @end
%%--------------------------------------------------------------------
-spec upload_selectors_csv(cb_context:context(), ne_binary(), ne_binary()) -> cb_context:context().
upload_selectors_csv(Context, RessourceId, SelectorType) ->
    _ = cb_context:put_reqid(Context),
    process_upload_file(Context, cb_context:req_files(Context), RessourceId, SelectorType).

-spec process_upload_file(cb_context:context(), req_files(), ne_binary(), ne_binary()) ->
    cb_context:context().
process_upload_file(Context, [{_Name, File}|_], RessourceId, SelectorType) ->
    maybe_suppress_change_notice(),
    lager:debug("converting file ~s", [_Name]),
    Stats = convert_file(kz_json:get_binary_value([<<"headers">>, <<"content_type">>], File)
                         ,kz_json:get_value(<<"contents">>, File)
                         ,Context
                         ,RessourceId
                         ,SelectorType
                        ),
    Db = cb_context:account_db(Context),
    maybe_send_db_change_notice(Db, Stats),
    crossbar_util:response(kz_json:from_list(Stats), Context);
process_upload_file(Context, _ReqFiles, _, _) ->
    error_no_file(Context).

-spec convert_file(ne_binary(), ne_binary(), cb_context:context(), ne_binary(), ne_binary()) ->
    kz_proplist().
convert_file(<<"text/csv">>, FileContents, Context, RessourceId, SelectorType) ->
    csv_to_selectors(FileContents, Context, RessourceId, SelectorType);
convert_file(<<"text/comma-separated-values">>, FileContents, Context, RessourceId, SelectorType) ->
    csv_to_selectors(FileContents, Context, RessourceId, SelectorType);
convert_file(ContentType, _, _, _, _) ->
    lager:debug("unknown content type: ~s", [ContentType]),
    throw({'unknown_content_type', ContentType}).

-spec csv_to_selectors(ne_binary(), cb_context:context(), ne_binary(), ne_binary()) ->
    kz_proplist().
csv_to_selectors(CSV, Context, ResourceId, SelectorType) ->
    BulkLimit = kz_datamgr:max_bulk_insert(),
    Db = cb_context:account_db(Context),
    {'ok', {Stats, LastJObjs}} = ecsv:process_csv_binary_with(CSV
                                          ,fun(Row, {AccStats, JObjs}) ->
                                                   SelectorData = get_selector_data_from_row(Row),
                                                   JObj = generate_selector_doc(Context, ResourceId, SelectorType, SelectorData),
                                                   maybe_save_selectors(Db, AccStats, [JObj | JObjs], BulkLimit)
                                           end
                                          ,{?ZERO_STATS, []}
                                         ),
    do_save_selectors(Db, Stats, LastJObjs).

-spec get_selector_data_from_row(strings()) -> ne_binary().
get_selector_data_from_row([ Selector | _]) -> kz_util:to_binary(Selector).

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
