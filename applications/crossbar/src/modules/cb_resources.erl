%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2014, 2600Hz INC
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
         ,allowed_methods/0, allowed_methods/1, allowed_methods/2
         ,resource_exists/0, resource_exists/1, resource_exists/2
         ,validate/1, validate/2, validate/3
         ,put/1, put/2
         ,post/2
         ,delete/2
        ]).

-include("../crossbar.hrl").

-define(CB_LIST, <<"resources/crossbar_listing">>).
-define(JOBS_LIST, <<"resources/jobs_listing">>).
-define(MOD_CONFIG_CAT, <<(?CONFIG_CAT)/binary, ".resources">>).
-define(COLLECTION, <<"collection">>).
-define(JOBS, <<"jobs">>).

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    _ = couch_mgr:revise_doc_from_file(?WH_SIP_DB, 'crossbar', "views/resources.json"),
    _ = couch_mgr:revise_doc_from_file(?WH_OFFNET_DB, 'crossbar', "views/resources.json"),

    _Pid = maybe_start_jobs_listener(),
    lager:debug("started jobs listener: ~p", [_Pid]),

    [crossbar_bindings:bind(Binding, ?MODULE, F)
     || {Binding, F} <- [{<<"*.allowed_methods.resources">>, 'allowed_methods'}
                         ,{<<"*.resource_exists.resources">>, 'resource_exists'}
                         ,{<<"*.validate.resources">>, 'validate'}
                         ,{<<"*.execute.put.resources">>, 'put'}
                         ,{<<"*.execute.post.resources">>, 'post'}
                         ,{<<"*.execute.delete.resources">>, 'delete'}

                         ,{<<"*.allowed_methods.global_resources">>, 'allowed_methods'}
                         ,{<<"*.resource_exists.global_resources">>, 'resource_exists'}
                         ,{<<"*.validate.global_resources">>, 'validate'}
                         ,{<<"*.execute.put.global_resources">>, 'put'}
                         ,{<<"*.execute.post.global_resources">>, 'post'}
                         ,{<<"*.execute.delete.global_resources">>, 'delete'}

                         ,{<<"*.allowed_methods.local_resources">>, 'allowed_methods'}
                         ,{<<"*.resource_exists.local_resources">>, 'resource_exists'}
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
allowed_methods() ->
    [?HTTP_GET, ?HTTP_PUT].

allowed_methods(?COLLECTION) ->
    [?HTTP_PUT, ?HTTP_POST, ?HTTP_DELETE];
allowed_methods(?JOBS) ->
    [?HTTP_GET, ?HTTP_PUT];
allowed_methods(_) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_DELETE].

allowed_methods(?JOBS, _Id) ->
    [?HTTP_GET].

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
resource_exists() -> 'true'.
resource_exists(_) -> 'true'.
resource_exists(?JOBS, _ID) -> 'true'.

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
validate(Context) ->
    case cb_context:account_id(Context) of
        'undefined' ->
            lager:debug("validating global resources"),
            validate_resources(cb_context:set_account_db(Context, ?WH_OFFNET_DB)
                               ,cb_context:req_verb(Context)
                              );
        _AccountId ->
            validate_resources(Context, cb_context:req_verb(Context))
    end.

validate(Context, ?COLLECTION) ->
    case cb_context:account_id(Context) of
        'undefined' ->
            lager:debug("validating global resources collection"),
            validate_collection(cb_context:set_account_db(Context, ?WH_OFFNET_DB));
        _AccountId ->
            validate_collection(Context)
    end;
validate(Context, ?JOBS) ->
    validate_jobs(Context, cb_context:req_verb(Context));
validate(Context, Id) ->
    case cb_context:account_id(Context) of
        'undefined' ->
            lager:debug("validating global resource ~s", [Id]),
            validate_resource(cb_context:set_account_db(Context, ?WH_OFFNET_DB)
                              ,Id
                              ,cb_context:req_verb(Context)
                             );
        _AccountId ->
            validate_resource(Context, Id, cb_context:req_verb(Context))
    end.

validate(Context, ?JOBS, JobId) ->
    read_job(Context, JobId).

-spec validate_resources(cb_context:context(), http_method()) -> cb_context:context().
validate_resources(Context, ?HTTP_GET) ->
    summary(Context);
validate_resources(Context, ?HTTP_PUT) ->
    case cb_context:account_db(Context) of
        ?WH_OFFNET_DB -> create(Context);
        _AccountDb -> cb_local_resources:validate_request('undefined', Context)
    end.

-spec validate_resource(cb_context:context(), path_token(), http_method()) -> cb_context:context().
validate_resource(Context, Id, ?HTTP_GET) ->
    read(Id, Context);
validate_resource(Context, Id, ?HTTP_POST) ->
    case cb_context:account_db(Context) of
        ?WH_OFFNET_DB -> update(Id, Context);
        _AccountDb -> cb_local_resources:validate_request(Id, Context)
    end;
validate_resource(Context, Id, ?HTTP_DELETE) ->
    read(Id, Context).

validate_collection(Context) ->
    validate_collection(Context, cb_context:req_verb(Context)).

validate_collection(Context, ?HTTP_PUT) ->
    F = case cb_context:account_db(Context) of
            ?WH_OFFNET_DB -> fun create/1;
            _AccountDb -> fun(C) -> cb_local_resources:validate_request('undefined', C) end
        end,

    lists:foldl(fun(Resource, C) ->
                        F(cb_context:set_req_data(C, Resource))
                end, Context, cb_context:req_data(Context)
               );
validate_collection(Context, ?HTTP_POST) ->
    F = case cb_context:account_db(Context) of
            ?WH_OFFNET_DB -> fun update/2;
            _AccountDb -> fun cb_local_resources:validate_request/2
        end,

    lists:foldl(fun(Resource, C) ->
                        F(wh_json:get_value(<<"id">>, Resource)
                          ,cb_context:set_req_data(C, Resource)
                         )
                end, Context, cb_context:req_data(Context)
               );
validate_collection(Context, ?HTTP_DELETE) ->
    lists:all(fun(Resource) ->
                      not cb_context:has_errors(read(wh_json:get_value(<<"id">>, Resource), Context))
              end, cb_context:req_data(Context)
             ).

-spec validate_jobs(cb_context:context(), http_method()) -> cb_context:context().
validate_jobs(Context, ?HTTP_GET) ->
    jobs_summary(Context);
validate_jobs(Context, ?HTTP_PUT) ->
    create_job(Context).

-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, ?COLLECTION) ->
    do_collection(Context, cb_context:account_db(Context));
post(Context, Id) ->
    do_post(Context, Id, cb_context:account_db(Context)).

-spec do_collection(cb_context:context(), ne_binary()) -> cb_context:context().
do_collection(Context, ?WH_OFFNET_DB) ->
    reload_acls(),
    collection_process(Context, cb_context:req_verb(Context));
do_collection(Context, _AccountDb) ->
    collection_process(Context, cb_context:req_verb(Context)).

-spec do_post(cb_context:context(), path_token(), ne_binary()) -> cb_context:context().
do_post(Context, _Id, ?WH_OFFNET_DB) ->
    reload_acls(),
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

-spec do_put(cb_context:context(), ne_binary()) -> cb_context:context().
do_put(Context, ?WH_OFFNET_DB) ->
    reload_acls(),
    crossbar_doc:save(Context);
do_put(Context, _AccountDb) ->
    Context1 = crossbar_doc:save(Context),
    cb_local_resources:maybe_aggregate_resource(Context1),
    Context1.

-spec put_collection(cb_context:context(), ne_binary()) -> cb_context:context().
put_collection(Context, ?WH_OFFNET_DB) ->
    collection_process(Context, cb_context:req_verb(Context));
put_collection(Context, _AccountDb) ->
    reload_acls(),
    collection_process(Context, cb_context:req_verb(Context)).

-spec put_job(cb_context:context()) -> cb_context:context().
put_job(Context) ->
    Modb = cb_context:account_modb(Context),
    Context1 = crossbar_doc:save(cb_context:set_account_db(Context, Modb)),

    case cb_context:resp_status(Context1) of
        'success' ->
            cb_jobs_listener:publish_new_job(Context),
            crossbar_util:response_202(<<"Job scheduled">>, cb_context:resp_data(Context1), Context1);
        _Status ->
            Context1
    end.

-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, ?COLLECTION) ->
    delete_collection(Context, cb_context:account_db(Context));
delete(Context, ResourceId) ->
    do_delete(Context, ResourceId, cb_context:account_db(Context)).

-spec do_delete(cb_context:context(), ne_binary(), ne_binary()) -> cb_context:context().
do_delete(Context, _ResourceId, ?WH_OFFNET_DB) ->
    reload_acls(),
    crossbar_doc:delete(Context);
do_delete(Context, ResourceId, _AccountDb) ->
    Context1 = crossbar_doc:delete(Context),
    cb_local_resources:maybe_remove_aggregate(ResourceId, Context1),
    Context1.

-spec delete_collection(cb_context:context(), ne_binary()) -> cb_context:context().
delete_collection(Context, ?WH_OFFNET_DB) ->
    collection_process(Context, cb_context:req_verb(Context));
delete_collection(Context, _AccountDb) ->
    reload_acls(),
    collection_process(Context, cb_context:req_verb(Context)).

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
    crossbar_doc:load(Id, Context).

-spec read_job(cb_context:context(), ne_binary()) -> cb_context:context().
read_job(Context, <<Year:4/binary, Month:2/binary, "-", _/binary>> = JobId) ->
    Modb = cb_context:account_modb(Context, wh_util:to_integer(Year), wh_util:to_integer(Month)),
    leak_job_fields(crossbar_doc:load(JobId, cb_context:set_account_db(Context, Modb)));
read_job(Context, <<Year:4/binary, Month:1/binary, "-", _/binary>> = JobId) ->
    Modb = cb_context:account_modb(Context, wh_util:to_integer(Year), wh_util:to_integer(Month)),
    leak_job_fields(crossbar_doc:load(JobId, cb_context:set_account_db(Context, Modb)));
read_job(Context, JobId) ->
    lager:debug("invalid job id format: ~s", [JobId]),
    crossbar_util:response_bad_identifier(JobId, Context).

-spec leak_job_fields(cb_context:context()) -> cb_context:context().
leak_job_fields(Context) ->
    case cb_context:resp_status(Context) of
        'success' ->
            JObj = cb_context:doc(Context),
            cb_context:set_resp_data(Context
                                     ,wh_json:set_values([{<<"timestamp">>, wh_json:get_value(<<"pvt_created">>, JObj)}
                                                          ,{<<"status">>, wh_json:get_value(<<"pvt_status">>, JObj)}
                                                          ,{<<"carrier">>, wh_json:get_value(<<"pvt_carrier">>, JObj)}
                                                         ], cb_context:resp_data(Context))
                                    );
        _Status -> Context
    end.

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
-spec normalize_view_results(wh_json:object(), wh_json:objects()) -> wh_json:objects().
normalize_view_results(JObj, Acc) ->
    [wh_json:get_value(<<"value">>, JObj)|Acc].

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

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec on_successful_validation(api_binary(), cb_context:context()) -> cb_context:context().
on_successful_validation('undefined', Context) ->
    cb_context:set_doc(Context
                       ,wh_json:set_value(<<"pvt_type">>, <<"resource">>, cb_context:doc(Context))
                      );
on_successful_validation(Id, Context) ->
    crossbar_doc:load_merge(Id, Context).

-spec on_successful_job_validation('undefined', cb_context:context()) -> cb_context:context().
on_successful_job_validation('undefined', Context) ->
    {Year, Month, _} = erlang:date(),
    Id = list_to_binary([wh_util:to_binary(Year)
                         ,wh_util:pad_month(Month)
                         ,"-"
                         ,wh_util:rand_hex_binary(8)
                        ]),

    cb_context:set_doc(Context
                       ,wh_json:set_values([{<<"pvt_type">>, <<"resource_job">>}
                                            ,{<<"pvt_status">>, <<"pending">>}
                                            ,{<<"pvt_carrier">>, get_job_carrier(Context)}
                                            ,{<<"pvt_auth_account_id">>, cb_context:auth_account_id(Context)}
                                            ,{<<"pvt_request_id">>, cb_context:req_id(Context)}
                                            ,{<<"_id">>, Id}

                                            ,{<<"successes">>, wh_json:new()}
                                            ,{<<"errors">>, wh_json:new()}
                                           ]
                                           ,wh_json:delete_keys([<<"carrier">>]
                                                                ,cb_context:doc(Context)
                                                               )
                                          )
                      ).

-spec get_job_carrier(cb_context:context()) -> ne_binary().
-spec get_job_carrier(api_binary(), boolean()) -> ne_binary().
get_job_carrier(Context) ->
    case cb_context:req_value(Context, <<"carrier">>) of
        <<"local">> -> <<"local">>;
        Carrier -> get_job_carrier(Carrier, cb_modules_util:is_superduper_admin(Context))
    end.

get_job_carrier('undefined', 'true') -> <<"other">>;
get_job_carrier(Carrier, 'true') -> Carrier;
get_job_carrier(_Carrier, 'false') -> <<"local">>.

-spec reload_acls() -> 'ok'.
reload_acls() ->
    lager:debug("published reloadacl"),
    wh_amqp_worker:cast([], fun(_) -> wapi_switch:publish_reload_acls() end).

-spec collection_process(cb_context:context(), http_method()) -> cb_context:context().
collection_process(Context, ?HTTP_POST) ->
    Db = cb_context:account_db(Context),
    Updates = [{wh_json:get_value(<<"id">>, JObj), clean_resource(JObj)}
               || JObj <- cb_context:req_data(Context)
              ],
    Ids = props:get_keys(Updates),
    ViewOptions = [{'keys', Ids}
                   ,'include_docs'
                  ],
    case couch_mgr:all_docs(Db, ViewOptions) of
        {'error', _R} ->
            lager:error("could not open ~p in ~p", [Ids, Db]),
            crossbar_util:response('error', <<"failed to open resources">>, Context);
        {'ok', JObjs} ->
            Resources = [update_resource(JObj, Updates) || JObj <- JObjs],
            case couch_mgr:save_docs(Db, Resources) of
                {'error', _R} ->
                    lager:error("failed to update ~p in ~p", [Ids, Db]),
                    crossbar_util:response('error', <<"failed to update resources">>, Context);
                {'ok', _} ->
                    (Db =/= ?WH_OFFNET_DB) andalso cb_local_resources:maybe_aggregate_resources(Resources),
                    cb_context:set_resp_data(Context, [clean_resource(Resource) || Resource <- Resources])
            end
    end;
collection_process(Context, ?HTTP_PUT) ->
    Db = cb_context:account_db(Context),
    Options = [{'type', <<"resource">>}],
    Resources = [wh_doc:update_pvt_parameters(JObj, 'undefined', Options)
                 || JObj <- cb_context:req_data(Context)
                ],
    case couch_mgr:save_docs(Db, Resources) of
        {'error', _R} ->
            lager:error("failed to create resources"),
            crossbar_util:response('error', <<"failed to create resources">>, Context);
        {'ok', JObjs} ->
            Ids = [wh_json:get_value(<<"id">>, JObj) || JObj <- JObjs],
            ViewOptions = [{'keys', Ids}
                           ,'include_docs'
                          ],
            (Db =/= ?WH_OFFNET_DB) andalso cb_local_resources:maybe_aggregate_resources(Resources),
            case couch_mgr:all_docs(Db, ViewOptions) of
                {'error', _R} ->
                    lager:error("could not open ~p in ~p", [Ids, Db]),
                    cb_context:set_resp_data(Context, Ids);
                {'ok', NewResources} ->
                    cb_context:set_resp_data(Context, [clean_resource(Resource) || Resource <- NewResources])
            end
    end;
collection_process(Context, ?HTTP_DELETE) ->
    ReqData = cb_context:req_data(Context),
    Db = cb_context:account_db(Context),
    case couch_mgr:del_docs(Db, ReqData) of
        {'error', _R} ->
            lager:error("failed to delete resources"),
            crossbar_util:response('error', <<"failed to delete resources">>, Context);
        {'ok', JObjs} ->
            (Db =/= ?WH_OFFNET_DB) andalso cb_local_resources:maybe_remove_aggregates(ReqData),
            cb_context:set_resp_data(Context, [wh_json:delete_key(<<"rev">>, JObj) || JObj <- JObjs])
    end.

-spec clean_resource(wh_json:object()) -> wh_json:object().
clean_resource(JObj) ->
    case wh_json:get_value(<<"doc">>, JObj) of
        'undefined' ->
            case wh_json:get_value(<<"_id">>, JObj) of
                'undefined' ->
                     JObj1 = wh_doc:public_fields(JObj),
                    wh_json:delete_key(<<"id">>, JObj1);
                Id ->
                    JObj1 = wh_json:set_value(<<"id">>, Id, JObj),
                    wh_doc:public_fields(JObj1)
            end;
        Doc -> clean_resource(Doc)
    end.

-spec update_resource(wh_json:object(), wh_proplist()) -> wh_json:object().
update_resource(JObj, Updates) ->
    Doc = wh_json:get_value(<<"doc">>, JObj),
    Id = wh_json:get_value(<<"_id">>, Doc),
    wh_json:merge_recursive([Doc, props:get_value(Id, Updates)]).
