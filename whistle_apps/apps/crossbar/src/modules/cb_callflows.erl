%%%----------------------------------------------------------------------------
%%% @copyright (C) 2011, VoIP INC
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
         ,delete/2
        ]).

-include("src/crossbar.hrl").

-define(MOD_CONFIG_CAT, <<(?CONFIG_CAT)/binary, ".callflows">>).

-define(SERVER, ?MODULE).

-define(CALLFLOWS_LIST, <<"callflows/listing_by_id">>).
-define(CB_LIST, <<"callflows/crossbar_listing">>).

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    _ = crossbar_bindings:bind(<<"v1_resource.allowed_methods.callflows">>, ?MODULE, allowed_methods),
    _ = crossbar_bindings:bind(<<"v1_resource.resource_exists.callflows">>, ?MODULE, resource_exists),
    _ = crossbar_bindings:bind(<<"v1_resource.validate.callflows">>, ?MODULE, validate),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.put.callflows">>, ?MODULE, put),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.post.callflows">>, ?MODULE, post),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.delete.callflows">>, ?MODULE, delete).

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
    [?HTTP_GET, ?HTTP_POST, ?HTTP_DELETE].

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
resource_exists() -> true.
resource_exists(_) -> true.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400
%% @end
%%--------------------------------------------------------------------
-spec validate(#cb_context{}) -> #cb_context{}.
-spec validate(#cb_context{}, path_token()) -> #cb_context{}.
validate(#cb_context{req_verb = ?HTTP_GET}=Context) ->
    load_callflow_summary(Context);
validate(#cb_context{req_verb = ?HTTP_PUT}=Context) ->
    validate_request(undefined, Context).

validate(#cb_context{req_verb = ?HTTP_GET}=Context, DocId) ->
    load_callflow(DocId, Context);
validate(#cb_context{req_verb = ?HTTP_POST}=Context, DocId) ->
    validate_request(DocId, Context);
validate(#cb_context{req_verb = ?HTTP_DELETE}=Context, DocId) ->
    load_callflow(DocId, Context).

-spec post(#cb_context{}, path_token()) -> #cb_context{}.
post(Context, _DocId) ->
    maybe_reconcile_numbers(crossbar_doc:save(Context)).

-spec put(#cb_context{}) -> #cb_context{}.
put(Context) ->
    maybe_reconcile_numbers(crossbar_doc:save(Context)).

-spec delete(#cb_context{}, path_token()) -> #cb_context{}.
delete(Context, _DocId) ->
    crossbar_doc:delete(Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load list of accounts, each summarized.  Or a specific
%% account summary.
%% @end
%%--------------------------------------------------------------------
-spec load_callflow_summary(#cb_context{}) -> #cb_context{}.
load_callflow_summary(Context) ->
    crossbar_doc:load_view(?CB_LIST, [], Context, fun normalize_view_results/2).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load a callflow document from the database
%% @end
%%--------------------------------------------------------------------
-spec load_callflow(ne_binary(), #cb_context{}) -> #cb_context{}.
load_callflow(DocId, Context) ->
    case crossbar_doc:load(DocId, Context) of
        #cb_context{resp_status=success, doc=Doc, resp_data=Data, db_name=Db}=Context1 ->
            Meta = get_metadata(wh_json:get_value(<<"flow">>, Doc), Db, wh_json:new()),
            Context1#cb_context{resp_data=wh_json:set_value(<<"metadata">>, Meta, Data)};
        Else ->
            Else
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
validate_request(CallflowId, Context) ->
    prepare_numbers(CallflowId, Context).

prepare_numbers(CallflowId, #cb_context{req_data=JObj}=Context) ->
    try [wnm_util:to_e164(Number) || Number <- wh_json:get_ne_value(<<"numbers">>, JObj, [])] of
        [_|_]=Numbers ->
            C = Context#cb_context{req_data=wh_json:set_value(<<"numbers">>, Numbers, JObj)},
            validate_unique_numbers(CallflowId, Numbers, C);
        [] ->
            prepare_patterns(CallflowId, Context)
    catch
        _:_ ->
            C = cb_context:add_validation_error(<<"numbers">>
                                                    ,<<"type">>
                                                    ,<<"Value is not of type array">>
                                                    ,Context),
            validate_unique_numbers(CallflowId, [], C#cb_context{req_data=wh_json:set_value(<<"numbers">>, [], JObj)})
    end.

prepare_patterns(CallflowId, #cb_context{req_data=JObj}=Context) ->
    case wh_json:get_value(<<"patterns">>, JObj, []) of
        [] ->
            C = cb_context:add_validation_error(<<"numbers">>
                                                    ,<<"required">>
                                                    ,<<"Callflows must be assigned at least one number">>
                                                    ,Context),
            check_callflow_schema(CallflowId, C);
        _Else ->
            check_callflow_schema(CallflowId, Context)
    end.

validate_unique_numbers(CallflowId, _, #cb_context{db_name=undefined}=Context) ->
    check_callflow_schema(CallflowId, Context);
validate_unique_numbers(CallflowId, [], Context) ->
    check_callflow_schema(CallflowId, Context);
validate_unique_numbers(CallflowId, Numbers, #cb_context{db_name=Db}=Context) ->
    case couch_mgr:get_results(Db, ?CB_LIST, [{<<"include_docs">>, true}]) of
        {error, _R} ->
            lager:debug("failed to load callflows from account: ~p", [_R]),
            check_callflow_schema(CallflowId, Context);
        {ok, JObjs} ->
            FilteredJObjs = filter_callflow_list(CallflowId, JObjs),
            C = check_uniqueness(Numbers, FilteredJObjs, Context),
            check_callflow_schema(CallflowId, C)
    end.

check_callflow_schema(CallflowId, Context) ->
    OnSuccess = fun(C) -> on_successful_validation(CallflowId, C) end,
    cb_context:validate_request_data(<<"callflows">>, Context, OnSuccess).

on_successful_validation(undefined, #cb_context{doc=Doc}=Context) ->
    Props = [{<<"pvt_type">>, <<"callflow">>}],
    Context#cb_context{doc=wh_json:set_values(Props, Doc)};
on_successful_validation(CallflowId, #cb_context{}=Context) ->
    crossbar_doc:load_merge(CallflowId, Context).

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
%%
%% @end
%%--------------------------------------------------------------------
maybe_reconcile_numbers(#cb_context{resp_status=success, doc=JObj
                                    ,account_id=AssignTo, auth_account_id=AuthBy}=Context) ->

    case whapps_config:get_is_true(?MOD_CONFIG_CAT, <<"default_reconcile_numbers">>, true) of
        false -> Context;
        true ->
            CurrentJObj = cb_context:fetch(db_doc, Context, wh_json:new()),
            Set1 = sets:from_list(wh_json:get_value(<<"numbers">>, CurrentJObj, [])),
            Set2 = sets:from_list(wh_json:get_value(<<"numbers">>, JObj, [])),
            NewNumbers = sets:subtract(Set2, Set1),
            _ = [wh_number_manager:reconcile_number(Number, AssignTo, AuthBy)
                 || Number <- sets:to_list(NewNumbers)
                ],
            Context
    end,
    track_assignment(Context);
maybe_reconcile_numbers(Context) -> Context.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec  track_assignment(cb_context:context()) ->cb_context:context().
track_assignment(#cb_context{doc=JObj, storage=Storage}=Context) ->
    OldNums = wh_json:get_value(<<"numbers">>, props:get_value('db_doc', Storage), []),
    NewNums = wh_json:get_value(<<"numbers">>, JObj, []),
    Unassigned = lists:foldl(
        fun(Num, Acc) ->
            case lists:member(Num, NewNums) of
                'true' -> Acc;
                'false' -> [Num|Acc]
            end
        end, [], OldNums
    ),
    wh_number_manager:track_assignment(Unassigned),
    wh_number_manager:track_assignment(NewNums, <<"callflow">>),
    Context.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
check_uniqueness(Numbers, JObjs, Context) ->
    Routines = [fun(C) -> check_numbers_uniqueness(Numbers, JObjs, C) end
                ,fun(C) -> check_patterns_uniqueness(Numbers, JObjs, C) end
               ],
    lists:foldl(fun(F, C) -> F(C) end, Context, Routines).

check_numbers_uniqueness([], _, Context) ->
    Context;
check_numbers_uniqueness([Number|Numbers], JObjs, Context) ->
    case lists:dropwhile(fun(J) ->
                                 N = wh_json:get_ne_value([<<"doc">>, <<"numbers">>], J, []),
                                 (not lists:member(Number, N))
                         end, JObjs)
    of
        [] -> check_numbers_uniqueness(Numbers, JObjs, Context);
        [JObj|_] ->
            C = add_number_conflict(Number, JObj, Context),
            check_numbers_uniqueness(Numbers, JObjs, C)
    end.

check_patterns_uniqueness([], _, Context) ->
    Context;
check_patterns_uniqueness([Number|Numbers], JObjs, Context) ->
    case lists:dropwhile(fun(J) ->
                                 Patterns = wh_json:get_ne_value([<<"doc">>, <<"patterns">>], J, []),
                                 patterns_dont_match(Number, Patterns)
                         end, JObjs)
    of
        [] -> check_patterns_uniqueness(Numbers, JObjs, Context);
        [JObj|_] ->
            C = add_number_conflict(Number, JObj, Context),
            check_patterns_uniqueness(Numbers, JObjs, C)
    end.
filter_callflow_list(undefined, JObjs) ->
    JObjs;
filter_callflow_list(CallflowId, JObjs) ->
    [JObj
     || JObj <- JObjs
            ,wh_json:get_value(<<"id">>, JObj) =/= CallflowId
    ].

patterns_dont_match(Number, Patterns) ->
    lists:dropwhile(fun(Pattern) -> re:run(Number, Pattern) =:=  nomatch end, Patterns) =:= [].

add_number_conflict(Number, JObj, Context) ->
    Id = wh_json:get_value(<<"id">>, JObj),
    case wh_json:get_ne_value([<<"doc">>, <<"featurecode">>, <<"name">>], JObj) of
        undefined ->
            cb_context:add_validation_error(<<"numbers">>
                                                ,<<"unique">>
                                                ,<<"Number ", Number/binary, " exists in callflow ", Id/binary>>
                                                ,Context);
        _Else ->

            cb_context:add_validation_error(<<"numbers">>
                                                ,<<"unique">>
                                                ,<<"Number ", Number/binary, " conflicts with featurecode callflow ", Id/binary>>
                                                ,Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% collect addional informat about the objects referenced in the flow
%% @end
%%--------------------------------------------------------------------
-spec get_metadata('undefined' | wh_json:object(), ne_binary(), wh_json:object()) -> wh_json:object().
get_metadata(undefined, _, JObj) ->
    JObj;
get_metadata(Flow, Db, JObj) ->
    JObj1 = case wh_json:get_value([<<"data">>, <<"id">>], Flow) of
                %% this node has no id, dont change the metadata
                undefined -> JObj;
                %% node has an id, try to update the metadata
                Id -> create_metadata(Db, Id, JObj)
            end,
    case wh_json:get_value(<<"children">>, Flow) of
        undefined -> JObj1;
        Children ->
            %% iterate through each child, collecting metadata on the
            %% branch name (things like temporal routes)
            lists:foldr(fun({K, Child}, J) ->
                                get_metadata(Child, Db, create_metadata(Db, K, J))
                        end, JObj1, wh_json:to_proplist(Children))
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Given the metadata json object, an ID and a db find the document
%% and add the fields to the metadata.  However, skip if the ID already
%% exists in metadata.
%% @end
%%--------------------------------------------------------------------
-spec create_metadata/3 :: (ne_binary(), ne_binary(), wh_json:object()) -> wh_json:object().
create_metadata(_, <<"_">>, JObj) -> JObj;
create_metadata(_, Id, JObj) when byte_size(Id) < 2 -> JObj;
create_metadata(Db, Id, JObj) ->
    case wh_json:get_value(Id, JObj) =:= undefined
        andalso couch_mgr:open_cache_doc(Db, Id) of
        false  ->
            %% the id already exists in the metadata
            JObj;
        {ok, Doc} ->
            %% the id was found in the db
            wh_json:set_value(Id, create_metadata(Doc), JObj);
        _ ->
            %% eh, whatevs
            JObj
    end.

-spec create_metadata(wh_json:object()) -> wh_json:object().
create_metadata(Doc) ->
    %% simple funciton for setting the same key in one json object
    %% with the value of that key in another, unless it doesnt exist
    Metadata = fun(<<"name">> = K, D, J) ->
                       case wh_json:get_value(<<"pvt_type">>, D) of
                           <<"user">> ->
                               Name = <<(wh_json:get_binary_value(<<"first_name">>, D, <<>>))/binary
                                        ," "
                                        ,(wh_json:get_binary_value(<<"last_name">>, D, <<>>))/binary>>,
                               case Name of
                                   <<>> -> J;
                                   _ -> wh_json:set_value(<<"name">>, Name, J)
                               end;
                           _ ->
                               case wh_json:get_value(K, D) of
                                   undefined -> J;
                                   V -> wh_json:set_value(K, V, J)
                               end
                       end;
                  (K, D, J) ->
                       case wh_json:get_value(K, D) of
                           undefined -> J;
                           V -> wh_json:set_value(K, V, J)
                       end
               end,
    %% list of keys to extract from documents and set on the metadata
    Funs = [fun(D, J) -> Metadata(<<"name">>, D, J) end,
            fun(D, J) -> Metadata(<<"numbers">>, D, J) end,
            fun(D, J) -> Metadata(<<"pvt_type">>, D, J) end],
    %% do it
    lists:foldl(fun(Fun, JObj) ->
                         Fun(Doc, JObj)
                end, wh_json:new(), Funs).
