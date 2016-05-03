%%%----------------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz INC
%%% @doc
%%% Callflow gen server for CRUD
%%%
%%% @end
%%% @contributors
%%%   Vladimir Darmin
%%%   James Aimonetti
%%%----------------------------------------------------------------------------
-module(cb_callflows).

-export([init/0
         ,allowed_methods/0, allowed_methods/1
         ,resource_exists/0, resource_exists/1
         ,validate/1, validate/2
         ,put/1
         ,post/2
         ,patch/2
         ,delete/2
        ]).

-include("crossbar.hrl").

-define(MOD_CONFIG_CAT, <<(?CONFIG_CAT)/binary, ".callflows">>).

-define(SERVER, ?MODULE).

-define(CALLFLOWS_LIST, <<"callflows/listing_by_id">>).
-define(CB_LIST, <<"callflows/crossbar_listing">>).

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.callflows">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.callflows">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.callflows">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.put.callflows">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"*.execute.post.callflows">>, ?MODULE, 'post'),
    _ = crossbar_bindings:bind(<<"*.execute.patch.callflows">>, ?MODULE, 'patch'),
    _ = crossbar_bindings:bind(<<"*.execute.delete.callflows">>, ?MODULE, 'delete').

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
allowed_methods() ->
    [?HTTP_GET, ?HTTP_PUT].
allowed_methods(_MediaID) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_PATCH, ?HTTP_DELETE].

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
resource_exists() -> 'true'.
resource_exists(_) -> 'true'.

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
validate(Context) ->
    validate_callflows(Context, cb_context:req_verb(Context)).

validate_callflows(Context, ?HTTP_GET) ->
    load_callflow_summary(Context);
validate_callflows(Context, ?HTTP_PUT) ->
    validate_request('undefined', Context).

validate(Context, CallflowId) ->
    validate_callflow(Context, CallflowId, cb_context:req_verb(Context)).

validate_callflow(Context, DocId, ?HTTP_GET) ->
    load_callflow(DocId, Context);
validate_callflow(Context, DocId, ?HTTP_POST) ->
    validate_request(DocId, Context);
validate_callflow(Context, DocId, ?HTTP_PATCH) ->
    validate_patch(DocId, Context);
validate_callflow(Context, DocId, ?HTTP_DELETE) ->
    load_callflow(DocId, Context).

-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, _DocId) ->
    Context1 = crossbar_doc:save(Context),
    case cb_context:resp_status(Context1) of
        'success' ->
            'ok' = track_assignment('post', Context),
            maybe_reconcile_numbers(Context1);
        _Status -> Context1
    end.

-spec patch(cb_context:context(), path_token()) -> cb_context:context().
patch(Context, _DocId) ->
    'ok' = track_assignment('post', Context),
    maybe_reconcile_numbers(crossbar_doc:save(Context)).

-spec put(cb_context:context()) -> cb_context:context().
put(Context) ->
    Context1 = crossbar_doc:save(Context),
    case cb_context:resp_status(Context1) of
        'success' ->
            'ok' = track_assignment('put', Context),
            maybe_reconcile_numbers(Context1);
        _Status -> Context1
    end.

-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, _DocId) ->
    Context1 = crossbar_doc:delete(Context),
    case cb_context:resp_status(Context1) of
        'success' ->
            'ok' = track_assignment('delete', Context),
            Context1;
        _Status -> Context1
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load list of accounts, each summarized.  Or a specific
%% account summary.
%% @end
%%--------------------------------------------------------------------
-spec load_callflow_summary(cb_context:context()) -> cb_context:context().
load_callflow_summary(Context) ->
    crossbar_doc:load_view(?CB_LIST, [], Context, fun normalize_view_results/2).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load a callflow document from the database
%% @end
%%--------------------------------------------------------------------
-spec load_callflow(ne_binary(), cb_context:context()) -> cb_context:context().
load_callflow(DocId, Context) ->
    Context1 = crossbar_doc:load(DocId, Context, ?TYPE_CHECK_OPTION(kzd_callflow:type())),
    case cb_context:resp_status(Context1) of
        'success' ->
            Meta = get_metadata(kz_json:get_value(<<"flow">>, cb_context:doc(Context1))
                                ,cb_context:account_db(Context1)
                                ,kz_json:new()
                               ),
            cb_context:set_resp_data(Context1
                                     ,kz_json:set_value(<<"metadata">>, Meta, cb_context:resp_data(Context1))
                                    );
        _Status -> Context1
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec validate_request(api(binary()), cb_context:context()) -> cb_context:context().
validate_request(CallflowId, Context) ->
    JObj = cb_context:req_data(Context),
    OriginalNumbers = kz_json:get_ne_value(<<"numbers">>, JObj, []),
    try normalize_numbers(OriginalNumbers) of
        [] -> prepare_patterns(CallflowId, Context);
        Numbers ->
            C = cb_context:set_req_data(Context
                                        ,kz_json:set_value(<<"numbers">>, Numbers, JObj)
                                       ),
            validate_unique_numbers(CallflowId, Numbers, C)
    catch
        _E:_R ->
            lager:debug("failed to convert all numbers to e164: ~s: ~p", [_E, _R]),
            Context1 = error_numbers_not_array(Context, OriginalNumbers),
            validate_unique_numbers(CallflowId
                                   ,[]
                                   ,cb_context:set_req_data(Context1
                                                           ,kz_json:set_value(<<"numbers">>, [], JObj)
                                                           )
                                   )
    end.

-spec error_numbers_not_array(cb_context:context(), kz_json:json_term()) ->
                                     cb_context:context().
error_numbers_not_array(Context, OriginalNumbers) ->
    cb_context:add_validation_error(<<"numbers">>
                                   ,<<"type">>
                                   ,kz_json:from_list(
                                      [{<<"message">>, <<"Value is not of type array">>}
                                      ,{<<"cause">>, OriginalNumbers}
                                      ])
                                   ,Context
                                   ).

-spec normalize_numbers(ne_binaries()) -> ne_binaries().
normalize_numbers(Ns) ->
    [knm_converters:normalize(N) || N <- Ns].

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec validate_patch(api(binary()), cb_context:context()) -> cb_context:context().
validate_patch(CallflowId, Context) ->
    crossbar_doc:patch_and_validate(CallflowId, Context, fun validate_request/2).

-spec prepare_patterns(api(binary()), cb_context:context()) -> cb_context:context().
prepare_patterns(CallflowId, Context) ->
    JObj = cb_context:req_data(Context),
    case kz_json:get_value(<<"patterns">>, JObj, []) of
        [] ->
            C = cb_context:add_validation_error(
                  <<"numbers">>
                  ,<<"required">>
                  ,kz_json:from_list(
                     [{<<"message">>, <<"Callflows must be assigned at least one number">>}]
                    )
                  ,Context
                 ),
            check_callflow_schema(CallflowId, C);
        _Else ->
            check_callflow_schema(CallflowId, Context)
    end.

-spec validate_unique_numbers(api(binary()), ne_binaries(), cb_context:context()) ->
                                     cb_context:context().
validate_unique_numbers(CallflowId, Numbers, Context) ->
    validate_unique_numbers(CallflowId, Numbers, Context, cb_context:account_db(Context)).

-spec validate_unique_numbers(api(binary()), ne_binaries(), cb_context:context(), api(binary())) ->
                                     cb_context:context().
validate_unique_numbers(CallflowId, _Numbers, Context, 'undefined') ->
    check_callflow_schema(CallflowId, Context);
validate_unique_numbers(CallflowId, [], Context, _AccountDb) ->
    check_callflow_schema(CallflowId, Context);
validate_unique_numbers(CallflowId, Numbers, Context, AccountDb) ->
    case kz_datamgr:get_results(AccountDb, ?CB_LIST, ['include_docs']) of
        {'error', _R} ->
            lager:debug("failed to load callflows from account: ~p", [_R]),
            check_callflow_schema(CallflowId, Context);
        {'ok', JObjs} ->
            FilteredJObjs = filter_callflow_list(CallflowId, JObjs),
            C = check_uniqueness(Numbers, FilteredJObjs, Context),
            check_callflow_schema(CallflowId, C)
    end.

-spec check_callflow_schema(api(binary()), cb_context:context()) -> cb_context:context().
check_callflow_schema(CallflowId, Context) ->
    OnSuccess = fun(C) ->
                        validate_callflow_elements(
                          on_successful_validation(CallflowId, C)
                         )
                end,
    cb_context:validate_request_data(<<"callflows">>, Context, OnSuccess).

-spec on_successful_validation(api(binary()), cb_context:context()) -> cb_context:context().
on_successful_validation('undefined', Context) ->
    cb_context:set_doc(Context
                       ,kz_doc:set_type(cb_context:doc(Context)
                                        ,kzd_callflow:type()
                                       )
                      );
on_successful_validation(CallflowId, Context) ->
    crossbar_doc:load_merge(CallflowId, Context, ?TYPE_CHECK_OPTION(kzd_callflow:type())).

-spec validate_callflow_elements(cb_context:context()) -> cb_context:context().
-spec validate_callflow_elements(cb_context:context(), kz_json:object()) -> cb_context:context().
validate_callflow_elements(Context) ->
    Flow = kz_json:get_value(<<"flow">>, cb_context:doc(Context)),
    validate_callflow_elements(Context, Flow).

validate_callflow_elements(Context, Flow) ->
    Module = kz_json:get_value(<<"module">>, Flow),
    Data = kz_json:get_value(<<"data">>, Flow),
    C = validate_callflow_element(
          validate_callflow_element_schema(Context, Module, Data)
          ,Module
          ,Data
         ),

    case cb_context:resp_status(C) of
        'success' ->
            validate_callflow_children(C, kz_json:get_value(<<"children">>, Flow));
        _Status -> C
    end.

-spec validate_callflow_element_schema(cb_context:context(), ne_binary(), kz_json:object()) ->
                                              cb_context:context().
validate_callflow_element_schema(Context, Module, Data) ->
    lager:debug("validating callflow el: ~s", [Module]),
    cb_context:validate_request_data(<<"callflows.", Module/binary>>
                                     ,cb_context:set_req_data(Context, Data)
                                     ,fun(_C) -> Context end
                                     ,fun(C) -> cb_context:set_doc(C, cb_context:doc(Context)) end
                                    ).

-spec validate_callflow_element(cb_context:context(), ne_binary(), kz_json:object()) ->
                                       cb_context:context().
validate_callflow_element(Context, <<"record_call">>, Data) ->
    Max = kz_media_util:max_recording_time_limit(),
    TimeLimit = kz_json:get_integer_value(<<"time_limit">>, Data),
    try kz_json:get_value(<<"action">>, Data) =:= <<"start">>
             andalso TimeLimit > Max
    of
        'true' ->
            lager:debug("the requested time limit is too damn high"),
            cb_context:add_validation_error(
              <<"time_limit">>
              ,<<"maximum">>
              ,kz_json:from_list(
                 [{<<"message">>, <<"Exceeds system limit of ", (kz_util:to_binary(Max))/binary, " seconds">>}
                  ,{<<"cause">>, TimeLimit}
                 ])
              ,Context
             );
        'false' -> Context
    catch
        _E:_R ->
            lager:debug("failed to get integer from data: ~s: ~p", [_E, _R]),
            cb_context:add_validation_error(
              <<"time_limit">>
              ,<<"type">>
              ,kz_json:from_list(
                 [{<<"message">>, <<"Must be an integer">>}
                  ,{<<"cause">>, TimeLimit}
                 ])
              ,Context
             )
    end;
validate_callflow_element(Context, _Module, _Data) ->
    Context.

-spec validate_callflow_children(cb_context:context(), api(kz_json:object())) ->
                                        cb_context:context().
validate_callflow_children(Context, 'undefined') ->
    Context;
validate_callflow_children(Context, Children) ->
    kz_json:foldl(fun validate_callflow_child/3, Context, Children).

-spec validate_callflow_child(ne_binary(), kz_json:object(), cb_context:context()) ->
                                     cb_context:context().
validate_callflow_child(_Key, Branch, Context) ->
    lager:debug("validating branch: ~s", [_Key]),
    validate_callflow_elements(Context, Branch).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the resuts of a view
%% @end
%%--------------------------------------------------------------------
-spec normalize_view_results(kz_json:object(), kz_json:objects()) ->
                                    kz_json:objects().
normalize_view_results(JObj, Acc) ->
    [kz_json:get_value(<<"value">>, JObj)|Acc].

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec maybe_reconcile_numbers(cb_context:context()) -> cb_context:context().
maybe_reconcile_numbers(Context) ->
    case kapps_config:get_is_true(?MOD_CONFIG_CAT, <<"default_reconcile_numbers">>, 'false') of
        'false' -> Context;
        'true' ->
            CurrentJObj = cb_context:fetch(Context, 'db_doc', kz_json:new()),
            Set1 = sets:from_list(kz_json:get_value(<<"numbers">>, CurrentJObj, [])),
            Set2 = sets:from_list(kz_json:get_value(<<"numbers">>, cb_context:doc(Context), [])),
            NewNumbers = sets:subtract(Set2, Set1),
            Options = [ {'assigned_to', cb_context:account_id(Context)}
                      , {'dry_run', not cb_context:accepting_charges(Context)}
                      ],
            _ = knm_numbers:reconcile(sets:to_list(NewNumbers), Options),
            Context
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec track_assignment(atom(), cb_context:context()) -> 'ok' | 'error'.
track_assignment('post', Context) ->
    NewNums = kz_json:get_value(<<"numbers">>, cb_context:doc(Context), []),
    OldNums = kz_json:get_value(<<"numbers">>, cb_context:fetch(Context, 'db_doc'), []),

    Unassigned = [{Num, <<>>}
                  || Num <- OldNums,
                     not(lists:member(Num, NewNums))
                         andalso Num =/= <<"undefined">>
                 ],
    Assigned =  [{Num, kzd_callflow:type()}
                 || Num <- NewNums,
                    Num =/= <<"undefined">>
                ],

    Updates = cb_modules_util:apply_assignment_updates(Unassigned ++ Assigned),
    cb_modules_util:log_assignment_updates(Updates);
track_assignment('put', Context) ->
    NewNums = kz_json:get_value(<<"numbers">>, cb_context:doc(Context), []),
    Assigned =  [{Num, kzd_callflow:type()}
                 || Num <- NewNums,
                    Num =/= <<"undefined">>
                ],

    Updates = cb_modules_util:apply_assignment_updates(Assigned),
    cb_modules_util:log_assignment_updates(Updates);
track_assignment('delete', Context) ->
    Nums = kz_json:get_value(<<"numbers">>, cb_context:doc(Context), []),
    Unassigned =  [{Num, <<>>}
                   || Num <- Nums,
                      Num =/= <<"undefined">>
                  ],
    Updates = cb_modules_util:apply_assignment_updates(Unassigned),
    cb_modules_util:log_assignment_updates(Updates).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec check_uniqueness(ne_binaries(), kz_json:objects(), cb_context:context()) ->
                              cb_context:context().
check_uniqueness(Numbers, JObjs, Context) ->
    Routines = [fun(C) -> check_numbers_uniqueness(Numbers, JObjs, C) end
                ,fun(C) -> check_patterns_uniqueness(Numbers, JObjs, C) end
               ],
    lists:foldl(fun(F, C) -> F(C) end, Context, Routines).

-spec check_numbers_uniqueness(ne_binaries(), kz_json:objects(), cb_context:context()) ->
                                      cb_context:context().
check_numbers_uniqueness([], _, Context) -> Context;
check_numbers_uniqueness([Number|Numbers], JObjs, Context) ->
    case lists:dropwhile(fun(J) -> is_number_unique(J, Number) end
                         ,JObjs
                        )
    of
        [] -> check_numbers_uniqueness(Numbers, JObjs, Context);
        [JObj|_] ->
            C = add_number_conflict(Number, JObj, Context),
            check_numbers_uniqueness(Numbers, JObjs, C)
    end.

-spec is_number_unique(kz_json:object(), ne_binary()) -> boolean().
is_number_unique(J, Number) ->
    N = kz_json:get_ne_value([<<"doc">>, <<"numbers">>], J, []),
    (not lists:member(Number, N)).

-spec check_patterns_uniqueness(ne_binaries(), kz_json:objects(), cb_context:context()) ->
                                       cb_context:context().
check_patterns_uniqueness([], _, Context) -> Context;
check_patterns_uniqueness([Number|Numbers], JObjs, Context) ->
    case lists:dropwhile(fun(J) -> is_pattern_unique(J, Number) end
                         ,JObjs
                        )
    of
        [] -> check_patterns_uniqueness(Numbers, JObjs, Context);
        [JObj|_] ->
            C = add_number_conflict(Number, JObj, Context),
            check_patterns_uniqueness(Numbers, JObjs, C)
    end.

-spec is_pattern_unique(kz_json:object(), ne_binary()) -> boolean().
is_pattern_unique(J, Number) ->
    Patterns = kz_json:get_ne_value([<<"doc">>, <<"patterns">>], J, []),
    patterns_dont_match(Number, Patterns).

-spec filter_callflow_list(api(binary()), kz_json:objects()) -> kz_json:objects().
filter_callflow_list('undefined', JObjs) ->
    [JObj
     || JObj <- JObjs,
        not(kzd_callflow:is_feature_code(kz_json:get_value(<<"doc">>, JObj)))
    ];
filter_callflow_list(CallflowId, JObjs) ->
    [JObj
     || JObj <- JObjs,
        kz_doc:id(JObj) =/= CallflowId
        andalso not(kzd_callflow:is_feature_code(kz_json:get_value(<<"doc">>, JObj)))
    ].

-spec patterns_dont_match(ne_binary(), ne_binaries()) -> boolean().
patterns_dont_match(Number, Patterns) ->
    [] =:= lists:dropwhile(fun(Pattern) ->
                                   re:run(Number, Pattern) =:= 'nomatch'
                           end, Patterns).

-spec add_number_conflict(ne_binary(), kz_json:object(), cb_context:context()) ->
                                 cb_context:context().
add_number_conflict(Number, JObj, Context) ->
    Id = kz_doc:id(JObj),
    case kz_json:get_ne_value([<<"doc">>, <<"featurecode">>, <<"name">>], JObj) of
        'undefined' ->
            cb_context:add_validation_error(
              <<"numbers">>
              ,<<"unique">>
              ,kz_json:from_list(
                 [{<<"message">>, <<"Number ", Number/binary, " exists in callflow ", Id/binary>>}
                  ,{<<"cause">>, Number}
                 ])
              ,Context
             );
        _Else ->
            cb_context:add_validation_error(
              <<"numbers">>
              ,<<"unique">>
              ,kz_json:from_list(
                 [{<<"message">>, <<"Number ", Number/binary, " conflicts with featurecode callflow ", Id/binary>>}
                  ,{<<"cause">>, Number}
                 ])
              ,Context
             )
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% collect addional informat about the objects referenced in the flow
%% @end
%%--------------------------------------------------------------------
-spec get_metadata(api(kz_json:object()), ne_binary(), kz_json:object()) ->
                          kz_json:object().
get_metadata('undefined', _, JObj) -> JObj;
get_metadata(Flow, Db, JObj) ->
    JObj1 = case kz_json:get_first_defined(
                   [[<<"data">>, <<"id">>]
                    ,[<<"data">>, <<"faxbox_id">>]
                   ], Flow)
            of
                %% this node has no id, dont change the metadata
                'undefined' -> JObj;
                %% node has an id, try to update the metadata
                Id -> create_metadata(Db, Id, JObj)
            end,
    case kz_json:get_value(<<"children">>, Flow) of
        'undefined' -> JObj1;
        Children ->
            %% iterate through each child, collecting metadata on the
            %% branch name (things like temporal routes)
            lists:foldr(fun({K, Child}, J) ->
                                get_metadata(Child, Db, create_metadata(Db, K, J))
                        end, JObj1, kz_json:to_proplist(Children))
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Given the metadata json object, an ID and a db find the document
%% and add the fields to the metadata.  However, skip if the ID already
%% exists in metadata.
%% @end
%%--------------------------------------------------------------------
-spec create_metadata(ne_binary(), ne_binary(), kz_json:object()) ->
                             kz_json:object().
create_metadata(_, <<"_">>, JObj) -> JObj;
create_metadata(_, Id, JObj) when byte_size(Id) < 2 -> JObj;
create_metadata(Db, Id, JObj) ->
    case kz_json:get_value(Id, JObj) =:= 'undefined'
        andalso kz_datamgr:open_cache_doc(Db, Id)
    of
        'false'  -> JObj;
        {'ok', Doc} ->  kz_json:set_value(Id, create_metadata(Doc), JObj);
        {'error', _E} -> JObj
    end.

-spec create_metadata(kz_json:object()) -> kz_json:object().
create_metadata(Doc) ->
    %% simple funciton for setting the same key in one json object
    %% with the value of that key in another, unless it doesnt exist
    Metadata = fun(<<"name">> = K, D, J) ->
                       case kz_doc:type(D) of
                           <<"user">> ->
                               case <<(kz_json:get_binary_value(<<"first_name">>, D, <<>>))/binary
                                        ," "
                                        ,(kz_json:get_binary_value(<<"last_name">>, D, <<>>))/binary
                                      >>
                               of
                                   <<>> -> J;
                                   <<" ">> -> J;
                                   Name -> kz_json:set_value(<<"name">>, Name, J)
                               end;
                           _Type ->
                               case kz_json:get_value(K, D) of
                                   'undefined' -> J;
                                   V -> kz_json:set_value(K, V, J)
                               end
                       end;
                  (K, D, J) ->
                       case kz_json:get_value(K, D) of
                           'undefined' -> J;
                           V -> kz_json:set_value(K, V, J)
                       end
               end,
    %% list of keys to extract from documents and set on the metadata
    Funs = [fun(D, J) -> Metadata(<<"name">>, D, J) end
            ,fun(D, J) -> Metadata(<<"numbers">>, D, J) end
            ,fun(D, J) -> Metadata(<<"pvt_type">>, D, J) end
           ],
    %% do it
    lists:foldl(fun(Fun, JObj) ->
                         Fun(Doc, JObj)
                end, kz_json:new(), Funs
               ).
