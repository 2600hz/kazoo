%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2012, VoIP INC
%%% @doc
%%%
%%% Listing of all expected v1 callbacks
%%%
%%% @end
%%% @contributors:
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_bulk).

-export([init/0
         ,allowed_methods/0
         ,resource_exists/0
         ,validate/1
         ,post/1
         ,delete/1
        ]).

-include("../crossbar.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Initializes the bindings this module will respond to.
%% @end
%%--------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.bulk">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.bulk">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.bulk">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.put.bulk">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"*.execute.post.bulk">>, ?MODULE, 'post'),
    crossbar_bindings:bind(<<"*.execute.delete.bulk">>, ?MODULE, 'delete').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods() -> http_methods() | [].
allowed_methods() -> [?HTTP_GET, ?HTTP_POST, ?HTTP_DELETE].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Does the path point to a valid resource
%% So /bulk => []
%%    /bulk/foo => [<<"foo">>]
%%    /bulk/foo/bar => [<<"foo">>, <<"bar">>]
%% @end
%%--------------------------------------------------------------------
-spec resource_exists() -> 'true'.
resource_exists() -> 'true'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Check the request (request body, query string params, path tokens, etc)
%% and load necessary information.
%% /bulk might load a list of bulk_update objects
%% /bulk/123 might load the bulk_update object 123
%% Generally, use crossbar_doc to manipulate the cb_context{} record
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
validate(#cb_context{}=Context) ->
    maybe_load_docs(Context).

-spec maybe_load_docs(cb_context:context()) -> cb_context:context().
maybe_load_docs(Context) ->
    JObj = cb_context:req_data(Context),
    Ids = sets:from_list(wh_json:get_value(<<"ids">>, JObj, [])),
    Context1 = crossbar_doc:load(sets:to_list(Ids), Context),
    case cb_context:resp_status(Context1) of
        'success' -> maybe_follow_groups(Ids, Context1);
        _Else -> Context1
    end.

-spec maybe_follow_groups(set(), cb_context:context()) -> cb_context:context().
maybe_follow_groups(Ids, Context) ->
    JObjs = cb_context:doc(Context),
    Context1 = cb_context:set_doc(Context, []),
    maybe_follow_groups(JObjs, Ids, Context1).

-spec maybe_follow_groups(wh_json:objects(), set(), cb_context:context()) ->
                                 cb_context:context().
maybe_follow_groups([], _, Context) ->
    maybe_update_docs(Context);
maybe_follow_groups([JObj|JObjs], Ids, Context) ->
    Docs = cb_context:doc(Context),
    case wh_doc:type(JObj) of
        <<"group">> ->
            follow_group(JObj, JObjs, Ids, Context);
        _Else ->
            Context1 = cb_context:set_doc(Context, [JObj|Docs]),
            maybe_follow_groups(JObjs, Ids, Context1)
    end.

-spec follow_group(wh_json:object(), wh_json:objects(), set(), cb_context:context()) ->
                          cb_context:context().
follow_group(JObj, JObjs, Ids, Context) ->
    lager:debug("trying to follow group members"),
    Members = lists:foldr(fun(Id, S) ->
                                  case sets:is_element(Id, Ids) of
                                      'true' -> S;
                                      'false' -> sets:add_element(Id, S)
                                  end
                          end, sets:new(), wh_json:get_keys(<<"endpoints">>, JObj)),
    Context1 = crossbar_doc:load(sets:to_list(Members), Context),
    case cb_context:resp_status(Context1) of
        'success' ->
            NewJObjs = cb_context:doc(Context1),
            maybe_follow_groups(NewJObjs ++ JObjs
                                ,sets:union(Ids, Members)
                                ,Context);
        _Else ->
            lager:info("failed to follow group, continuing"),
            maybe_follow_groups(JObjs
                                ,sets:union(Ids, Members)
                                ,Context)
    end.

-spec maybe_update_docs(cb_context:context()) -> cb_context:context().
maybe_update_docs(Context) ->
    case get_doc_updates(Context) of
        'undefined' ->
            lager:debug("no update provided, returning docs"),
            Context;
        Updates ->
            revalidate_docs(update_docs(Updates, Context))
    end.

-spec revalidate_docs(cb_context:context()) -> cb_context:context().
revalidate_docs(Context) ->
    JObjs = cb_context:doc(Context),
    Context1 = cb_context:setters(Context
                                 ,[{fun cb_context:set_doc/2, []}
                                   ,{fun cb_context:set_resp_data/2, wh_json:new()}
                                  ]),
    revalidate_docs(JObjs, Context1).

-spec revalidate_docs(wh_json:objects(), cb_context:context()) -> cb_context:context().
revalidate_docs([], Context) ->
    Context;
revalidate_docs([JObj|JObjs], Context) ->
    revalidate_docs(JObjs, revalidate_doc(JObj, Context)).

-spec revalidate_doc(wh_json:object(), cb_context:context()) -> cb_context:context().
revalidate_doc(JObj, Context) ->
    case wh_doc:id(JObj) of
        'undefined' -> Context;
        Id -> revalidate_doc(Id, JObj, Context)
    end.

-spec revalidate_doc(ne_binary(), wh_json:object(), cb_context:context()) ->
                            cb_context:context().
revalidate_doc(Id, JObj, Context) ->
    case get_validate_binding(JObj) of
        'undefined' ->
            Details = [{'type', wh_doc:type(JObj)}],
            InterimContext = cb_context:add_system_error('invalid_bulk_type'
                                                         ,wh_json:from_list(Details)
                                                         ,cb_context:new()),
            import_results(Id, InterimContext, Context);
        Binding ->
            Setters = [{fun cb_context:set_req_verb/2, ?HTTP_POST}
                       ,{fun cb_context:set_method/2, ?HTTP_POST}
                       ,{fun cb_context:set_auth_token/2, cb_context:auth_token(Context)}
                       ,{fun cb_context:set_auth_account_id/2, cb_context:auth_account_id(Context)}
                       ,{fun cb_context:set_auth_doc/2, cb_context:auth_doc(Context)}
                       ,{fun cb_context:set_account_id/2, cb_context:account_id(Context)}
                       ,{fun cb_context:set_account_db/2, cb_context:account_db(Context)}
                       ,{fun cb_context:set_req_id/2, cb_context:req_id(Context)}
                       ,{fun cb_context:set_query_string/2, cb_context:query_string(Context)}
                       ,{fun cb_context:set_doc/2, JObj}
                       ,{fun cb_context:set_req_data/2, JObj}
                       ,{fun cb_context:set_resp_status/2, 'fatal'}
                       ,{fun cb_context:set_load_merge_bypass/2, JObj}
                      ],
            C = cb_context:setters(cb_context:new(), Setters),
            Payload = [C, Id],
            run_binding(Binding, Payload, Id, Context)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec post(cb_context:context()) -> cb_context:context().
post(Context) ->
    JObjs = cb_context:doc(Context),
    maybe_save_docs(JObjs, Context).

-type docs() :: {wh_json:object(), wh_json:object()}.

-spec maybe_save_docs([docs()], cb_context:context()) -> cb_context:context().
maybe_save_docs([], Context) ->
    Context;
maybe_save_docs([JObj|JObjs], Context) ->
    maybe_save_docs(JObjs, maybe_save_doc(JObj, Context)).

-spec maybe_save_doc(docs(), cb_context:context()) -> cb_context:context().
maybe_save_doc({JObj,DbDoc}, Context) ->
    case wh_doc:id(JObj) of
        'undefined' -> Context;
        Id -> maybe_save_doc(Id, JObj, DbDoc, Context)
    end.

-spec maybe_save_doc(ne_binary(), wh_json:object(), wh_json:object(), cb_context:context()) ->
                            cb_context:context().
maybe_save_doc(Id, JObj, DbDoc, Context) ->
    case get_post_binding(JObj) of
        'undefined' ->
            Details = [{'type', wh_doc:type(JObj)}],
            InterimContext = cb_context:add_system_error('invalid_bulk_type'
                                                         ,wh_json:from_list(Details)
                                                         ,cb_context:new()),
            import_results(Id, InterimContext, Context);
        Binding ->
            Setters = [{fun cb_context:set_req_verb/2, ?HTTP_POST}
                       ,{fun cb_context:set_method/2, ?HTTP_POST}
                       ,{fun cb_context:set_auth_token/2, cb_context:auth_token(Context)}
                       ,{fun cb_context:set_auth_account_id/2, cb_context:auth_account_id(Context)}
                       ,{fun cb_context:set_auth_doc/2, cb_context:auth_doc(Context)}
                       ,{fun cb_context:set_account_id/2, cb_context:account_id(Context)}
                       ,{fun cb_context:set_account_db/2, cb_context:account_db(Context)}
                       ,{fun cb_context:set_req_id/2, cb_context:req_id(Context)}
                       ,{fun cb_context:set_query_string/2, cb_context:query_string(Context)}
                       ,{fun cb_context:set_doc/2, JObj}
                       ,{fun cb_context:set_req_data/2, JObj}
                       ,{fun cb_context:set_resp_status/2, 'success'}
                      ],
            C = cb_context:setters(cb_context:new(), Setters),
            Payload = [cb_context:store(C, 'db_doc', DbDoc), Id],
            run_binding(Binding, Payload, Id, Context)
    end.

-spec delete(cb_context:context()) -> cb_context:context().
delete(Context) ->
    JObjs = cb_context:doc(Context),
    Context1 = cb_context:set_resp_data(Context, wh_json:new()),
    maybe_delete_docs(JObjs, Context1).

-spec maybe_delete_docs([docs()], cb_context:context()) -> cb_context:context().
maybe_delete_docs([], Context) ->
    Context;
maybe_delete_docs([JObj|JObjs], Context) ->
    maybe_delete_docs(JObjs, maybe_delete_doc(JObj, Context)).

-spec maybe_delete_doc(docs(), cb_context:context()) -> cb_context:context().
maybe_delete_doc({JObj,DbDoc}, Context) ->
    case wh_doc:id(JObj) of
        'undefined' -> Context;
        Id -> maybe_delete_doc(Id, JObj, DbDoc, Context)
    end.

-spec maybe_delete_doc(ne_binary(), wh_json:object(), wh_json:object(), cb_context:context()) ->
                              cb_context:context().
maybe_delete_doc(Id, JObj, DbDoc, Context) ->
    lager:debug("try to delete ~p", [Id]),
    case get_delete_binding(JObj) of
        'undefined' ->
            Details = [{'type', wh_doc:type(JObj)}],
            InterimContext = cb_context:add_system_error('invalid_bulk_type'
                                                         ,wh_json:from_list(Details)
                                                         ,cb_context:new()),
            import_results(Id, InterimContext, Context);
        Binding ->
            Setters = [{fun cb_context:set_req_verb/2, ?HTTP_DELETE}
                       ,{fun cb_context:set_method/2, ?HTTP_DELETE}
                       ,{fun cb_context:set_auth_token/2, cb_context:auth_token(Context)}
                       ,{fun cb_context:set_auth_account_id/2, cb_context:auth_account_id(Context)}
                       ,{fun cb_context:set_auth_doc/2, cb_context:auth_doc(Context)}
                       ,{fun cb_context:set_account_id/2, cb_context:account_id(Context)}
                       ,{fun cb_context:set_account_db/2, cb_context:account_db(Context)}
                       ,{fun cb_context:set_req_id/2, cb_context:req_id(Context)}
                       ,{fun cb_context:set_query_string/2, cb_context:query_string(Context)}
                       ,{fun cb_context:set_doc/2, JObj}
                       ,{fun cb_context:set_req_data/2, JObj}
                      ],
            C = cb_context:setters(cb_context:new(), Setters),
            Payload = [cb_context:store(C, 'db_doc', DbDoc), Id],
            run_binding(Binding, Payload, Id, Context)
    end.

-spec run_binding(ne_binary(), list(), ne_binary(), cb_context:context()) -> cb_context:context().
run_binding(Binding, Payload, Id, Context) ->
    lager:debug("bulk update running: ~p", [Binding]),
    InterimContext = crossbar_bindings:fold(Binding, Payload),
    import_results(Id, cb_context:import_errors(InterimContext), Context).

-spec import_results(ne_binary(), cb_context:context(), cb_context:context()) ->
                            cb_context:context().
import_results(Id, C, Context) ->
    case cb_context:resp_status(C) of
        'success' -> import_results_success(Id, C, Context);
        _Error ->    import_results_error(Id, C, Context)
    end.

-spec import_results_success(ne_binary(), cb_context:context(), cb_context:context()) ->
                                    cb_context:context().
import_results_success(Id, C, Context) ->
    Doc  = cb_context:doc(C),
    Docs = cb_context:doc(Context),
    JObj = cb_context:resp_data(Context),
    DbDoc = select_doc(Id, cb_context:fetch(Context, 'db_doc')),
    Resp = wh_json:from_list([{<<"status">>, <<"success">>}]),
    cb_context:setters(Context
                       ,[{fun cb_context:set_resp_data/2, wh_json:set_value(Id, Resp, JObj)}
                         ,{fun cb_context:set_doc/2, [{Doc, DbDoc} | Docs]}
                        ]).

-spec import_results_error(ne_binary(), cb_context:context(), cb_context:context()) ->
                                  cb_context:context().
import_results_error(Id, C, Context) ->
    Status    = cb_context:resp_status(C),
    ErrorCode = cb_context:resp_error_code(C),
    ErrorMsg  = cb_context:resp_error_msg(C),
    Errors    = cb_context:resp_data(C),
    JObj      = cb_context:resp_data(Context),
    Resp = wh_json:from_list([{<<"status">>, Status}
                             ,{<<"error">>, ErrorCode}
                             ,{<<"message">>, ErrorMsg}
                             ,{<<"data">>, Errors}
                             ]),
    cb_context:set_resp_data(Context, wh_json:set_value(Id, Resp, JObj)).

-spec select_doc(ne_binary(), wh_json:objects()) -> wh_json:object() | 'undefined'.
select_doc(_Id, []) -> 'undefined';
select_doc(Id, [JObj|JObjs]) ->
    case wh_doc:id(JObj) of
        Id -> JObj;
        _ -> select_doc(Id, JObjs)
    end.

-spec get_doc_updates(cb_context:context()) -> 'undefined' | wh_json:object().
get_doc_updates(Context) ->
    JObj = cb_context:req_data(Context),
    case wh_json:get_value(<<"updates">>, JObj) of
        'undefined' -> 'undefined';
        Updates -> wh_json:public_fields(Updates)
    end.

-spec update_docs(wh_json:object(), cb_context:context()) -> cb_context:context().
update_docs(Updates, Context) ->
    JObjs = [wh_json:merge_recursive(JObj, Updates)
             || JObj <- cb_context:doc(Context)
            ],
    cb_context:set_doc(Context, JObjs).

-spec get_post_binding(wh_json:object() | ne_binary()) -> api_binary().
get_post_binding(<<"device">>) ->     <<"v1_resource.execute.post.devices">>;
get_post_binding(<<"user">>) ->       <<"v1_resource.execute.post.users">>;
get_post_binding(<<"conference">>) -> <<"v1_resource.execute.post.conferences">>;
get_post_binding(<<"vmbox">>) ->      <<"v1_resource.execute.post.vmboxes">>;
get_post_binding(<<_/binary>>) ->
    'undefined';
get_post_binding(JObj) ->
    get_post_binding(wh_doc:type(JObj)).

-spec get_delete_binding(wh_json:object() | ne_binary()) -> api_binary().
get_delete_binding(<<"device">>) ->     <<"v1_resource.execute.delete.devices">>;
get_delete_binding(<<"user">>) ->       <<"v1_resource.execute.delete.users">>;
get_delete_binding(<<"conference">>) -> <<"v1_resource.execute.delete.conferences">>;
get_delete_binding(<<"vmbox">>) ->      <<"v1_resource.execute.delete.vmboxes">>;
get_delete_binding(<<_/binary>>) ->
    'undefined';
get_delete_binding(JObj) ->
    get_delete_binding(wh_doc:type(JObj)).

-spec get_validate_binding(wh_json:object() | ne_binary()) -> api_binary().
get_validate_binding(<<"device">>) ->     <<"v1_resource.validate.devices">>;
get_validate_binding(<<"user">>) ->       <<"v1_resource.validate.users">>;
get_validate_binding(<<"conference">>) -> <<"v1_resource.validate.conferences">>;
get_validate_binding(<<"vmbox">>) ->      <<"v1_resource.validate.vmboxes">>;
get_validate_binding(<<_/binary>>) ->
    'undefined';
get_validate_binding(JObj) ->
    get_validate_binding(wh_doc:type(JObj)).
