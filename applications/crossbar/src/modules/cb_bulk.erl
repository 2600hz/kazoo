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
-spec init/0 :: () -> 'ok'.
init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.bulk">>, ?MODULE, allowed_methods),
    _ = crossbar_bindings:bind(<<"*.resource_exists.bulk">>, ?MODULE, resource_exists),
    _ = crossbar_bindings:bind(<<"*.validate.bulk">>, ?MODULE, validate),
    _ = crossbar_bindings:bind(<<"*.execute.put.bulk">>, ?MODULE, put),
    _ = crossbar_bindings:bind(<<"*.execute.post.bulk">>, ?MODULE, post),
    crossbar_bindings:bind(<<"*.execute.delete.bulk">>, ?MODULE, delete).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods/0 :: () -> http_methods() | [].
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
-spec resource_exists/0 :: () -> 'true'.
resource_exists() -> true.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Check the request (request body, query string params, path tokens, etc)
%% and load necessary information.
%% /bulk mights load a list of bulk_update objects
%% /bulk/123 might load the bulk_update object 123
%% Generally, use crossbar_doc to manipulate the cb_context{} record
%% @end
%%--------------------------------------------------------------------
-spec validate/1 :: (#cb_context{}) -> #cb_context{}.
validate(#cb_context{}=Context) ->
    maybe_load_docs(Context).

-spec maybe_load_docs/1 :: (cb_context:context()) -> cb_context:context().
maybe_load_docs(#cb_context{req_data=JObj}=Context) ->
    Ids = sets:from_list(wh_json:get_value(<<"ids">>, JObj, [])),
    case crossbar_doc:load(sets:to_list(Ids), Context) of
        #cb_context{resp_status=success}=C ->
            maybe_follow_groups(Ids, C);
        Else -> Else
    end.

-spec maybe_follow_groups/2 :: (set(), cb_context:context()) -> cb_context:context().
maybe_follow_groups(Ids, #cb_context{doc=JObjs}=Context) ->
    maybe_follow_groups(JObjs, Ids, Context#cb_context{doc=[]}).

-spec maybe_follow_groups/3 :: (wh_json:objects(), set(), cb_context:context()) -> cb_context:context().
maybe_follow_groups([], _, Context) ->
    maybe_update_docs(Context);
maybe_follow_groups([JObj|JObjs], Ids, #cb_context{doc=Docs}=Context) ->
    case wh_json:get_value(<<"pvt_type">>, JObj) of
        <<"group">> ->
            follow_group(JObj, JObjs, Ids, Context);
        _Else ->
            maybe_follow_groups(JObjs, Ids, Context#cb_context{doc=[JObj|Docs]})
    end.

-spec follow_group/4 :: (wh_json:object(), wh_json:objects(), set(), cb_context:context()) -> cb_context:context().
follow_group(JObj, JObjs, Ids, Context) ->
    lager:debug("trying to follow group members", []),
    Members = lists:foldr(fun(Id, S) ->
                                  case sets:is_element(Id, Ids) of
                                      true -> S;
                                      false -> sets:add_element(Id, S)
                                  end
                          end, sets:new(), wh_json:get_keys(<<"endpoints">>, JObj)),
    case crossbar_doc:load(sets:to_list(Members), Context) of
        #cb_context{resp_status=success, doc=NewJObjs} ->
            maybe_follow_groups(NewJObjs ++ JObjs
                                ,sets:union(Ids, Members)
                                ,Context);
        _Else ->
            lager:info("failed to follow group, continuing", []),
            maybe_follow_groups(JObjs
                                ,sets:union(Ids, Members)
                                ,Context)
    end.

-spec maybe_update_docs/1 :: (cb_context:context()) -> cb_context:context().
maybe_update_docs(Context) ->
    case get_doc_updates(Context) of
        undefined ->
            lager:debug("no update provided, returing docs", []),
            Context;
        Updates ->
            revalidate_docs(update_docs(Updates, Context))
    end.

-spec revalidate_docs/1 :: (cb_context:context()) -> cb_context:context().
revalidate_docs(#cb_context{doc=JObjs}=Context) ->
    revalidate_docs(JObjs, Context#cb_context{doc=[], resp_data=wh_json:new()}).

-spec revalidate_docs/2 :: (wh_json:objects(), cb_context:context()) -> cb_context:context().
revalidate_docs([], Context) ->
    Context;
revalidate_docs([JObj|JObjs], Context) ->
    revalidate_docs(JObjs, revalidate_doc(JObj, Context)).

-spec revalidate_doc/2 :: (wh_json:object(), cb_context:context()) -> cb_context:context().
revalidate_doc(JObj, Context) ->
    case wh_json:get_value(<<"_id">>, JObj) of
        undefined -> Context;
        Id -> revalidate_doc(Id, JObj, Context)
    end.

-spec revalidate_doc/3 :: (ne_binary(), wh_json:object(), cb_context:context()) -> cb_context:context().
revalidate_doc(Id, JObj, Context) ->
    case get_validate_binding(JObj) of
        undefined ->
            Details = [{type, wh_json:get_value(<<"pvt_type">>, JObj)}],
            InterimContext = cb_context:add_system_error(invalid_bulk_type
                                                         ,Details
                                                         ,#cb_context{}),
            import_results(Id, InterimContext, Context);
        Binding ->
            Payload = [#cb_context{req_verb = ?HTTP_POST
                                   ,method = ?HTTP_POST
                                   ,auth_token = Context#cb_context.auth_token
                                   ,auth_account_id = Context#cb_context.auth_account_id
                                   ,auth_doc = Context#cb_context.auth_doc
                                   ,account_id = Context#cb_context.account_id
                                   ,db_name = Context#cb_context.db_name
                                   ,req_id = Context#cb_context.req_id
                                   ,query_json = Context#cb_context.query_json
                                   ,doc=JObj
                                   ,req_data=JObj
                                   ,resp_status=fatal
                                   ,load_merge_bypass=JObj}
                       | [Id]
                      ],
            run_binding(Binding, Payload, Id, Context)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec post/1 :: (cb_context:context()) -> cb_context:context().
post(#cb_context{doc=JObjs}=Context) ->
    maybe_save_docs(JObjs, Context).

-spec maybe_save_docs/2 :: (wh_json:objects(), cb_context:context()) -> cb_context:context().
maybe_save_docs([], Context) ->
    Context;
maybe_save_docs([JObj|JObjs], Context) ->
    maybe_save_docs(JObjs, maybe_save_doc(JObj, Context)).

-spec maybe_save_doc/2 :: (wh_json:object(), cb_context:context()) -> cb_context:context().
maybe_save_doc(JObj, Context) ->
    case wh_json:get_value(<<"_id">>, JObj) of
        undefined -> Context;
        Id ->
            maybe_save_doc(Id, JObj, Context)
    end.

-spec maybe_save_doc/3 :: (ne_binary(), wh_json:object(), cb_context:context()) -> cb_context:context().
maybe_save_doc(Id, JObj, Context) ->
    case get_post_binding(JObj) of
        undefined ->
            Details = [{type, wh_json:get_value(<<"pvt_type">>, JObj)}],
            InterimContext = cb_context:add_system_error(invalid_bulk_type
                                                         ,Details
                                                         ,#cb_context{}),
            import_results(Id, InterimContext, Context);
        Binding ->
            Payload = [#cb_context{req_verb = ?HTTP_POST
                                   ,method = ?HTTP_POST
                                   ,auth_token = Context#cb_context.auth_token
                                   ,auth_account_id = Context#cb_context.auth_account_id
                                   ,auth_doc = Context#cb_context.auth_doc
                                   ,account_id = Context#cb_context.account_id
                                   ,db_name = Context#cb_context.db_name
                                   ,req_id = Context#cb_context.req_id
                                   ,query_json = Context#cb_context.query_json
                                   ,doc=JObj
                                   ,req_data=JObj
                                   ,resp_status='success'}
                       | [Id]
                      ],
            run_binding(Binding, Payload, Id, Context)
    end.

-spec delete/1 :: (cb_context:context()) -> cb_context:context().
delete(#cb_context{doc=JObjs}=Context) ->
    maybe_delete_docs(JObjs, Context#cb_context{resp_data=wh_json:new()}).

-spec maybe_delete_docs/2 :: (wh_json:objects(), cb_context:context()) -> cb_context:context().
maybe_delete_docs([], Context) ->
    Context;
maybe_delete_docs([JObj|JObjs], Context) ->
    maybe_delete_docs(JObjs, maybe_delete_doc(JObj, Context)).

-spec maybe_delete_doc/2 :: (wh_json:object(), cb_context:context()) -> cb_context:context().
maybe_delete_doc(JObj, Context) ->
    case wh_json:get_value(<<"_id">>, JObj) of
        undefined -> Context;
        Id ->
            maybe_delete_doc(Id, JObj, Context)
    end.

-spec maybe_delete_doc/3 :: (ne_binary(), wh_json:object(), cb_context:context()) -> cb_context:context().
maybe_delete_doc(Id, JObj, Context) ->
    lager:debug("try to delete ~p", [Id]),
    case get_delete_binding(JObj) of
        undefined ->
            Details = [{type, wh_json:get_value(<<"pvt_type">>, JObj)}],
            InterimContext = cb_context:add_system_error(invalid_bulk_type
                                                         ,Details
                                                         ,#cb_context{}),
            import_results(Id, InterimContext, Context);
        Binding ->
            Payload = [#cb_context{req_verb = ?HTTP_DELETE
                                   ,method = ?HTTP_DELETE
                                   ,auth_token = Context#cb_context.auth_token
                                   ,auth_account_id = Context#cb_context.auth_account_id
                                   ,auth_doc = Context#cb_context.auth_doc
                                   ,account_id = Context#cb_context.account_id
                                   ,db_name = Context#cb_context.db_name
                                   ,req_id = Context#cb_context.req_id
                                   ,query_json = Context#cb_context.query_json
                                   ,doc=JObj
                                   ,req_data=JObj}
                       | [Id]
                      ],
            run_binding(Binding, Payload, Id, Context)
    end.

-spec run_binding/4 :: (ne_binary(), list(), ne_binary(), cb_context:context()) -> cb_context:context().
run_binding(Binding, Payload, Id, Context) ->
    lager:debug("bulk update running: ~p", [Binding]),
    InterimContext = crossbar_bindings:fold(Binding, Payload),
    import_results(Id, cb_context:import_errors(InterimContext), Context).

-spec import_results/3 :: (ne_binary(), cb_context:context(), cb_context:context()) -> cb_context:context().
import_results(Id, #cb_context{resp_status=success, doc=Doc}
               ,#cb_context{resp_data=JObj, doc=Docs}=Context) ->
    Resp = wh_json:from_list([{<<"status">>, <<"success">>}]),
    Context#cb_context{resp_data=wh_json:set_value(Id, Resp, JObj)
                       ,doc=[Doc|Docs]};
import_results(Id, #cb_context{resp_status=Status, resp_error_code=ErrorCode
                               ,resp_error_msg=ErrorMsg, resp_data=Errors}
               ,#cb_context{resp_data=JObj}=Context) ->
    Resp = wh_json:from_list([{<<"status">>, Status}
                             ,{<<"error">>, ErrorCode}
                             ,{<<"message">>, ErrorMsg}
                             ,{<<"data">>, Errors}
                             ]),
    Context#cb_context{resp_data=wh_json:set_value(Id, Resp, JObj)}.

-spec get_doc_updates/1 :: (cb_context:context()) -> 'undefined' | wh_json:object().
get_doc_updates(#cb_context{req_data=JObj}) ->
    case wh_json:get_value(<<"updates">>, JObj) of
        undefined -> undefined;
        Updates -> wh_json:public_fields(Updates)
    end.

-spec update_docs/2 :: (wh_json:object(), cb_context:context()) -> cb_context:context().
update_docs(Updates, #cb_context{doc=JObjs}=Context) ->
    Context#cb_context{doc=[wh_json:merge_recursive(JObj, Updates)
                            || JObj <- JObjs
                           ]}.

-spec get_post_binding/1 :: (wh_json:object() | ne_binary()) -> 'undefined' | ne_binary().
get_post_binding(<<"device">>) ->
    <<"v1_resource.execute.post.devices">>;
get_post_binding(<<"user">>) ->
    <<"v1_resource.execute.post.users">>;
get_post_binding(<<"conference">>) ->
    <<"v1_resource.execute.post.conferences">>;
get_post_binding(<<"vmbox">>) ->
    <<"v1_resource.execute.post.vmboxes">>;
get_post_binding(<<_/binary>>) ->
    undefined;
get_post_binding(JObj) ->
    get_post_binding(wh_json:get_value(<<"pvt_type">>, JObj)).

-spec get_delete_binding/1 :: (wh_json:object() | ne_binary()) -> 'undefined' | ne_binary().
get_delete_binding(<<"device">>) ->
    <<"v1_resource.execute.delete.devices">>;
get_delete_binding(<<"user">>) ->
    <<"v1_resource.execute.delete.users">>;
get_delete_binding(<<"conference">>) ->
    <<"v1_resource.execute.delete.conferences">>;
get_delete_binding(<<"vmbox">>) ->
    <<"v1_resource.execute.delete.vmboxes">>;
get_delete_binding(<<_/binary>>) ->
    undefined;
get_delete_binding(JObj) ->
    get_delete_binding(wh_json:get_value(<<"pvt_type">>, JObj)).

-spec get_validate_binding/1 :: (wh_json:object() | ne_binary()) -> 'undefined' | ne_binary().
get_validate_binding(<<"device">>) ->
    <<"v1_resource.validate.devices">>;
get_validate_binding(<<"user">>) ->
    <<"v1_resource.validate.users">>;
get_validate_binding(<<"conference">>) ->
    <<"v1_resource.validate.conferences">>;
get_validate_binding(<<"vmbox">>) ->
    <<"v1_resource.validate.vmboxes">>;
get_validate_binding(<<_/binary>>) ->
    undefined;
get_validate_binding(JObj) ->
    get_validate_binding(wh_json:get_value(<<"pvt_type">>, JObj)).
