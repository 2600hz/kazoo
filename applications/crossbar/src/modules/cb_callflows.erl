%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc Callflow gen server for CRUD
%%% @author Vladimir Darmin
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
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

-ifdef(TEST).
-include("test/cb_callflows_test.hrl").
-endif.

-define(MOD_CONFIG_CAT, <<(?CONFIG_CAT)/binary, ".callflows">>).

-define(SERVER, ?MODULE).

-define(CB_LIST, <<"callflows/crossbar_listing">>).
-define(CB_LIST_BY_NUMBER, <<"callflows/listing_by_number">>).
-define(CB_LIST_BY_PATTERN, <<"callflows/listing_by_pattern">>).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    _ = cb_modules_util:bind(?MODULE, [{<<"*.allowed_methods.callflows">>, 'allowed_methods'}
                                      ,{<<"*.resource_exists.callflows">>, 'resource_exists'}
                                      ,{<<"*.validate.callflows">>, 'validate'}
                                      ,{<<"*.execute.put.callflows">>, 'put'}
                                      ,{<<"*.execute.post.callflows">>, 'post'}
                                      ,{<<"*.execute.patch.callflows">>, 'patch'}
                                      ,{<<"*.execute.delete.callflows">>, 'delete'}
                                      ]),
    'ok'.

%%------------------------------------------------------------------------------
%% @doc This function determines the verbs that are appropriate for the
%% given Nouns. For example `/accounts/' can only accept `GET' and `PUT'.
%%
%% Failure here returns `405 Method Not Allowed'.
%% @end
%%------------------------------------------------------------------------------

-spec allowed_methods() -> http_methods().
allowed_methods() ->
    [?HTTP_GET, ?HTTP_PUT].

-spec allowed_methods(path_token()) -> http_methods().
allowed_methods(_CallflowId) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_PATCH, ?HTTP_DELETE].

%%------------------------------------------------------------------------------
%% @doc This function determines if the provided list of Nouns are valid.
%% Failure here returns `404 Not Found'.
%% @end
%%------------------------------------------------------------------------------

-spec resource_exists() -> 'true'.
resource_exists() -> 'true'.

-spec resource_exists(path_token()) -> 'true'.
resource_exists(_CallflowId) -> 'true'.

%%------------------------------------------------------------------------------
%% @doc This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400.
%% @end
%%------------------------------------------------------------------------------

-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    validate_callflows(Context, cb_context:req_verb(Context)).

validate_callflows(Context, ?HTTP_GET) ->
    load_callflow_summary(Context);
validate_callflows(Context, ?HTTP_PUT) ->
    validate_request('undefined', Context).

-spec validate(cb_context:context(), path_token()) -> cb_context:context().
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
post(Context, _CallflowId) ->
    Context1 = crossbar_doc:save(Context),
    case cb_context:resp_status(Context1) of
        'success' ->
            'ok' = track_assignment('post', Context),
            maybe_reconcile_numbers(Context1);
        _Status -> Context1
    end.

-spec patch(cb_context:context(), path_token()) -> cb_context:context().
patch(Context, _CallflowId) ->
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
delete(Context, _CallflowId) ->
    Context1 = crossbar_doc:delete(Context),
    case cb_context:resp_status(Context1) of
        'success' ->
            'ok' = track_assignment('delete', Context),
            Context1;
        _Status -> Context1
    end.

%%------------------------------------------------------------------------------
%% @doc Attempt to load list of accounts, each summarized. Or a specific
%% account summary.
%% @end
%%------------------------------------------------------------------------------
-spec load_callflow_summary(cb_context:context()) -> cb_context:context().
load_callflow_summary(Context) ->
    crossbar_doc:load_view(?CB_LIST, [], Context, fun normalize_view_results/2).

%%------------------------------------------------------------------------------
%% @doc Load a callflow document from the database
%% @end
%%------------------------------------------------------------------------------
-spec load_callflow(kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
load_callflow(CallflowId, Context) ->
    Context1 = crossbar_doc:load(CallflowId, Context, ?TYPE_CHECK_OPTION(kzd_callflows:type())),
    case cb_context:resp_status(Context1) of
        'success' ->
            Meta = get_metadata(kz_json:get_value(<<"flow">>, cb_context:doc(Context1))
                               ,cb_context:db_name(Context1)
                               ),
            cb_context:set_resp_data(Context1
                                    ,kz_json:set_value(<<"metadata">>, Meta, cb_context:resp_data(Context1))
                                    );
        _Status -> Context1
    end.

-spec request_numbers(cb_context:context()) -> kz_term:ne_binaries() | kz_json:json_term().
request_numbers(Context) ->
    kz_json:get_ne_value(<<"numbers">>, cb_context:req_data(Context), []).

-spec request_patterns(cb_context:context()) -> kz_term:ne_binaries() | kz_json:json_term().
request_patterns(Context) ->
    kz_json:get_ne_value(<<"patterns">>, cb_context:req_data(Context), []).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec validate_request(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
validate_request(CallflowId, Context) ->
    case request_numbers(Context) of
        [] -> validate_patterns(CallflowId, Context);
        OriginalNumbers
          when is_list(OriginalNumbers) ->
            validate_callflow_schema(CallflowId, normalize_numbers(Context, OriginalNumbers));
        OriginalNumbers ->
            Msg = kz_json:from_list(
                    [{<<"message">>, <<"Value is not of type array">>}
                    ,{<<"cause">>, OriginalNumbers}
                    ]),
            cb_context:add_validation_error(<<"numbers">>, <<"type">>, Msg, Context)
    end.

-spec normalize_numbers(cb_context:context(), kz_term:ne_binaries()) -> cb_context:context().
normalize_numbers(Context, Nums) ->
    Normalized = knm_converters:normalize(Nums, cb_context:account_id(Context)),
    NewReqData = kz_json:set_value(<<"numbers">>, Normalized, cb_context:req_data(Context)),
    cb_context:set_req_data(Context, NewReqData).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec validate_patch(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
validate_patch(CallflowId, Context) ->
    crossbar_doc:patch_and_validate(CallflowId, Context, fun validate_request/2).

-spec validate_patterns(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
validate_patterns(CallflowId, Context) ->
    case request_patterns(Context) of
        [] ->
            Msg = kz_json:from_list(
                    [{<<"message">>, <<"Callflows must be assigned at least one number or pattern">>}
                    ]),
            cb_context:add_validation_error(<<"numbers">>, <<"required">>, Msg, Context);
        Patterns
          when is_list(Patterns) ->
            validate_callflow_schema(CallflowId, Context);
        Patterns ->
            Msg = kz_json:from_list(
                    [{<<"message">>, <<"Value is not of type array">>}
                    ,{<<"cause">>, Patterns}
                    ]),
            cb_context:add_validation_error(<<"patterns">>, <<"type">>, Msg, Context)
    end.

-spec validate_uniqueness(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
validate_uniqueness(CallflowId, Context) ->
    Setters = [{fun validate_unique_numbers/2, CallflowId}
              ,{fun validate_unique_patterns/2, CallflowId}
              ],
    cb_context:setters(Context, Setters).

-spec validate_unique_numbers(cb_context:context(), kz_term:api_binary()) -> cb_context:context().
validate_unique_numbers(Context, CallflowId) ->
    validate_unique_numbers(Context, CallflowId, request_numbers(Context)).

-spec validate_unique_numbers(cb_context:context(), kz_term:api_binary(), kz_term:ne_binaries()) -> cb_context:context().
validate_unique_numbers(Context, _CallflowId, []) -> Context;
validate_unique_numbers(Context, CallflowId, Numbers) ->
    Options = [{'keys', Numbers}],
    case kz_datamgr:get_results(cb_context:db_name(Context), ?CB_LIST_BY_NUMBER, Options) of
        {'error', Error} ->
            lager:debug("failed to load callflows from account: ~p", [Error]),
            cb_context:add_system_error(Error, Context);
        {'ok', JObjs} ->
            validate_number_conflicts(Context, CallflowId, JObjs)
    end.

-spec validate_number_conflicts(cb_context:context(), kz_term:api_binary(), kz_json:objects()) -> cb_context:context().
validate_number_conflicts(Context, 'undefined', JObjs) ->
    add_number_conflicts(Context, JObjs);
validate_number_conflicts(Context, CallflowId, JObjs) ->
    add_number_conflicts(Context, filter_callflow_list(CallflowId, JObjs)).

-spec add_number_conflicts(cb_context:context(), kz_json:objects()) -> cb_context:context().
add_number_conflicts(Context, []) -> Context;
add_number_conflicts(Context, [JObj | JObjs]) ->
    add_number_conflicts(add_number_conflict(Context, JObj), JObjs).

-spec add_number_conflict(cb_context:context(), kz_json:object()) -> cb_context:context().
add_number_conflict(Context, JObj) ->
    Id = kz_doc:id(JObj),
    Name = kz_json:get_ne_binary_value([<<"value">>, <<"name">>], JObj, <<>>),
    Number = kz_json:get_value(<<"key">>, JObj),
    Msg = kz_json:from_list(
            [{<<"message">>, <<"Number ", Number/binary, " exists in callflow ", Id/binary, " ", Name/binary>>}
            ,{<<"cause">>, Number}
            ]),
    cb_context:add_validation_error(<<"numbers">>, <<"unique">>, Msg, Context).

-spec validate_unique_patterns(cb_context:context(), kz_term:api_binary()) -> cb_context:context().
validate_unique_patterns(Context, CallflowId) ->
    validate_unique_patterns(Context, CallflowId, request_patterns(Context)).

-spec validate_unique_patterns(cb_context:context(), kz_term:api_binary(), kz_term:ne_binaries()) -> cb_context:context().
validate_unique_patterns(Context, _CallflowId, []) -> Context;
validate_unique_patterns(Context, CallflowId, Patterns) ->
    Options = [{'keys', Patterns}],
    case kz_datamgr:get_results(cb_context:db_name(Context), ?CB_LIST_BY_PATTERN, Options) of
        {'error', Error} ->
            lager:debug("failed to load callflows from account: ~p", [Error]),
            cb_context:add_system_error(Error, Context);
        {'ok', JObjs} ->
            validate_pattern_conflicts(Context, CallflowId, JObjs)
    end.

-spec validate_pattern_conflicts(cb_context:context(), kz_term:api_binary(), kz_json:objects()) -> cb_context:context().
validate_pattern_conflicts(Context, 'undefined', JObjs) ->
    add_pattern_conflicts(Context, JObjs);
validate_pattern_conflicts(Context, CallflowId, JObjs) ->
    add_pattern_conflicts(Context, filter_callflow_list(CallflowId, JObjs)).

-spec add_pattern_conflicts(cb_context:context(), kz_json:objects()) -> cb_context:context().
add_pattern_conflicts(Context, []) -> Context;
add_pattern_conflicts(Context, [JObj | JObjs]) ->
    add_pattern_conflicts(add_pattern_conflict(Context, JObj), JObjs).

-spec add_pattern_conflict(cb_context:context(), kz_json:object()) -> cb_context:context().
add_pattern_conflict(Context, JObj) ->
    Id = kz_doc:id(JObj),
    Name = kz_json:get_ne_binary_value([<<"value">>, <<"name">>], JObj, <<>>),
    Pattern = kz_json:get_value(<<"key">>, JObj),
    Msg = kz_json:from_list(
            [{<<"message">>, <<"Pattern ", Pattern/binary, " exists in callflow ", Id/binary, " ", Name/binary>>}
            ,{<<"cause">>, Pattern}
            ]),
    cb_context:add_validation_error(<<"patterns">>, <<"unique">>, Msg, Context).

-spec validate_callflow_schema(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
validate_callflow_schema(CallflowId, Context) ->
    OnSuccess = fun(C) ->
                        C1 = validate_uniqueness(CallflowId, on_successful_validation(CallflowId, C)),

                        Doc = cb_context:doc(C1),
                        Nums = kz_json:get_list_value(<<"numbers">>, Doc, []),
                        cb_modules_util:validate_number_ownership(Nums, C1)
                end,
    cb_context:validate_request_data(<<"callflows">>, Context, OnSuccess).

-spec on_successful_validation(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
on_successful_validation('undefined', Context) ->
    cb_context:set_doc(Context
                      ,kz_doc:set_type(cb_context:doc(Context), kzd_callflows:type())
                      );
on_successful_validation(CallflowId, Context) ->
    crossbar_doc:load_merge(CallflowId, Context, ?TYPE_CHECK_OPTION(kzd_callflows:type())).

%%------------------------------------------------------------------------------
%% @doc Normalizes the results of a view.
%% @end
%%------------------------------------------------------------------------------
-spec normalize_view_results(kz_json:object(), kz_json:objects()) ->
          kz_json:objects().
normalize_view_results(JObj, Acc) ->
    [kz_json:get_value(<<"value">>, JObj)|Acc].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_reconcile_numbers(cb_context:context()) -> cb_context:context().
maybe_reconcile_numbers(Context) ->
    case kapps_config:get_is_true(?MOD_CONFIG_CAT, <<"default_reconcile_numbers">>, 'false') of
        'false' -> Context;
        'true' ->
            CurrentJObj = cb_context:fetch(Context, 'db_doc', kz_json:new()),
            Set1 = sets:from_list(kz_json:get_value(<<"numbers">>, CurrentJObj, [])),
            Set2 = sets:from_list(kz_json:get_value(<<"numbers">>, cb_context:doc(Context), [])),
            NewNumbers = sets:to_list(sets:subtract(Set2, Set1)),
            Options = [{'assign_to', cb_context:account_id(Context)}
                      ,{'dry_run', not cb_context:accepting_charges(Context)}
                      ],
            _ = knm_numbers:reconcile(NewNumbers, Options),
            Context
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec track_assignment(atom(), cb_context:context()) -> 'ok' | 'error'.
track_assignment('post', Context) ->
    NewNums = kz_json:get_value(<<"numbers">>, cb_context:doc(Context), []),
    OldNums = kz_json:get_value(<<"numbers">>, cb_context:fetch(Context, 'db_doc'), []),
    AccountId = cb_context:account_id(Context),

    Unassigned = [{Num, 'undefined'}
                  || Num <- OldNums,
                     not lists:member(Num, NewNums),
                     knm_converters:is_reconcilable(Num, AccountId)
                 ],
    Assigned =  [{Num, kzd_callflows:type()}
                 || Num <- NewNums,
                    knm_converters:is_reconcilable(Num, AccountId)
                ],

    Updates = cb_modules_util:apply_assignment_updates(Unassigned ++ Assigned, Context),
    cb_modules_util:log_assignment_updates(Updates);
track_assignment('put', Context) ->
    NewNums = kz_json:get_value(<<"numbers">>, cb_context:doc(Context), []),
    AccountId = cb_context:account_id(Context),
    Assigned =  [{Num, kzd_callflows:type()}
                 || Num <- NewNums,
                    knm_converters:is_reconcilable(Num, AccountId)
                ],

    Updates = cb_modules_util:apply_assignment_updates(Assigned, Context),
    cb_modules_util:log_assignment_updates(Updates);
track_assignment('delete', Context) ->
    Nums = kz_json:get_value(<<"numbers">>, cb_context:doc(Context), []),
    AccountId = cb_context:account_id(Context),
    Unassigned =  [{Num, 'undefined'}
                   || Num <- Nums,
                      knm_converters:is_reconcilable(Num, AccountId)
                  ],
    Updates = cb_modules_util:apply_assignment_updates(Unassigned, Context),
    cb_modules_util:log_assignment_updates(Updates).

-spec filter_callflow_list(kz_term:api_binary(), kz_json:objects()) -> kz_json:objects().
filter_callflow_list('undefined', JObjs) -> JObjs;
filter_callflow_list(CallflowId, JObjs) ->
    [JObj
     || JObj <- JObjs,
        kz_doc:id(JObj) =/= CallflowId
    ].

%%------------------------------------------------------------------------------
%% @doc collect additional information about the objects referenced in the flow
%% @end
%%------------------------------------------------------------------------------

-spec ids_in_flow(kz_json:object()) -> kz_term:ne_binaries().
ids_in_flow(FlowJObj) ->
    ids_in_data(kz_json:get_values(<<"data">>, FlowJObj)).

-spec ids_in_data({kz_json:json_terms(), kz_json:keys()}) -> kz_term:ne_binaries().
ids_in_data(Values) ->
    ids_in_data(Values, []).

-spec ids_in_data({kz_json:json_terms(), kz_json:keys()}, kz_term:ne_binaries()) -> kz_term:ne_binaries().
ids_in_data({[], []}, IDs) -> IDs;
ids_in_data({[V|Vs], [<<"id">>|Ks]}, IDs) ->
    ids_in_data({Vs, Ks}, [V | IDs]);
ids_in_data({[V|Vs], [K|Ks]}, IDs) ->
    case binary:matches(K, <<"_id">>) =/= []
        andalso kz_term:is_ne_binary(V)
    of
        'false' -> ids_in_data({Vs, Ks}, IDs);
        'true' -> ids_in_data({Vs, Ks}, [V | IDs])
    end.

-spec get_metadata(kz_term:api_object(), kz_term:ne_binary()) -> kz_json:object().
get_metadata('undefined', _Db) -> kz_json:new();
get_metadata(Flow, Db) -> get_metadata(Flow, Db, kz_json:new()).

-spec get_metadata(kz_json:object(), kz_term:ne_binary(), kz_json:object()) ->
          kz_json:object().
get_metadata(Flow, Db, Metadata) ->
    UpdatedMetadata
        = lists:foldl(fun(ID, MetaAcc) -> create_metadata(Db, ID, MetaAcc) end
                     ,Metadata
                     ,ids_in_flow(Flow)
                     ),

    case kz_json:get_json_value(<<"children">>, Flow) of
        'undefined' -> UpdatedMetadata;
        Children ->
            %% iterate through each child, collecting metadata on the
            %% branch name (things like temporal routes)
            kz_json:foldl(fun(Branch, ChildFlow, MetaAcc) ->
                                  get_metadata(ChildFlow, Db, create_metadata(Db, Branch, MetaAcc))
                          end
                         ,UpdatedMetadata
                         ,Children
                         )
    end.

%%------------------------------------------------------------------------------
%% @doc Given the metadata json object, an ID and a db find the document
%% and add the fields to the metadata.  However, skip if the ID already
%% exists in metadata.
%% @end
%%------------------------------------------------------------------------------
-spec create_metadata(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) ->
          kz_json:object().
create_metadata(_, <<"_">>, Metadata) -> Metadata;
create_metadata(_, Id, Metadata) when byte_size(Id) < 2 -> Metadata;
create_metadata(Db, Id, Metadata) ->
    create_metadata(Db, Id, Metadata, kz_json:get_ne_binary_value(Id, Metadata)).

create_metadata(Db, Id, Metadata, 'undefined') ->
    case fetch_id_from_db(Db, Id) of
        {'ok', Doc} ->  kz_json:set_value(Id, create_metadata(Doc), Metadata);
        {'error', _E} -> Metadata
    end;
create_metadata(_Db, _Id, Metadata, _Value) ->
    Metadata.

-spec fetch_id_from_db(kz_term:ne_binary(), kz_term:ne_binary()) ->
          {'ok', kz_json:object()} |
          kz_datamgr:data_error().
-ifdef(TEST).
fetch_id_from_db(_Db, <<"{USER_ID}">>) ->
    {'ok', ?TEST_USER};
fetch_id_from_db(_Db, <<"{VM_ID}">>) ->
    {'ok', ?TEST_VM};
fetch_id_from_db(_Db, <<"{RING_GROUP_ID}">>) ->
    {'ok', ?TEST_RING_GROUP}.
-else.
fetch_id_from_db(Db, Id) ->
    kz_datamgr:open_cache_doc(Db, Id).
-endif.

-spec create_metadata(kz_json:object()) -> kz_json:object().
create_metadata(Doc) ->
    lists:foldl(fun(Key, Meta) -> metadata_builder(Key, Doc, Meta) end
               ,kz_json:new()
               ,[<<"name">>
                ,<<"numbers">>
                ,<<"pvt_type">>
                ]
               ).

-spec metadata_builder(kz_json:key(), kz_json:object(), kz_json:object()) ->
          kz_json:object().
metadata_builder(<<"name">> = Key, Doc, Metadata) ->
    case kz_doc:type(Doc) of
        <<"user">> ->
            metadata_user_name(Doc, Metadata);
        _Type ->
            maybe_copy_value(Key, Doc, Metadata)
    end;
metadata_builder(Key, Doc, Metadata) ->
    maybe_copy_value(Key, Doc, Metadata).

-spec maybe_copy_value(kz_json:key(), kz_json:object(), kz_json:object()) ->
          kz_json:object().
maybe_copy_value(Key, Doc, Metadata) ->
    case kz_json:get_value(Key, Doc) of
        'undefined' -> Metadata;
        Value -> kz_json:set_value(Key, Value, Metadata)
    end.

-spec metadata_user_name(kz_json:object(), kz_json:object()) -> kz_json:object().
metadata_user_name(Doc, Metadata) ->
    case kzd_users:name(Doc) of
        <<>> -> Metadata;
        <<" ">> -> Metadata;
        Name -> kz_json:set_value(<<"name">>, Name, Metadata)
    end.
