%%%----------------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz INC
%%% @doc
%%% Match list module
%%%
%%% Handle client requests for match list documents, api v2
%%%
%%% @end
%%% @contributors
%%%   SIPLABS, LLC (Ilya Ashchepkov)
%%%----------------------------------------------------------------------------
-module(cb_lists_v2).

-export([maybe_migrate/1]).
-export([init/0
        ,allowed_methods/0, allowed_methods/1, allowed_methods/2, allowed_methods/3, allowed_methods/4
        ,resource_exists/0, resource_exists/1, resource_exists/2, resource_exists/3, resource_exists/4
        ,content_types_provided/1, content_types_provided/2, content_types_provided/3, content_types_provided/4, content_types_provided/5
        ,validate/1, validate/2, validate/3, validate/4, validate/5
        ,delete/2, delete/3, delete/4
        ,save/1, save/2, save/3, save/4, save/5
        ]).

-include("crossbar.hrl").

-define(CB_LIST, <<"lists/crossbar_listing">>).
-define(ENTRIES, <<"entries">>).
-define(VCARD, <<"vcard">>).
-define(PHOTO, <<"photo">>).
-define(TYPE_LIST, <<"list">>).
-define(TYPE_LIST_ENTRY, <<"list_entry">>).
%%%===================================================================
%%% API
%%%===================================================================
-spec maybe_migrate(ne_binary()) -> 'ok'.
maybe_migrate(Account) ->
    AccountDb = kz_util:format_account_id(Account, 'encoded'),
    case kz_datamgr:get_results(AccountDb, ?CB_LIST, ['include_docs']) of
        {'ok', []} -> 'ok';
        {'ok', Lists} -> migrate(AccountDb, Lists);
        {'error', _} -> 'ok'
    end.

-spec migrate(ne_binary(), kz_json:objects()) -> 'ok'.
migrate(AccountDb, [List | Lists]) ->
    DocId = kz_json:get_value(<<"id">>, List),
    Entries = kz_json:to_proplist(kz_json:get_value([<<"doc">>, <<"entries">>], List, kz_json:new())),

    MigrateEntryFun = fun({Id, Entry}) ->
                              kz_json:set_values([{<<"pvt_type">>, ?TYPE_LIST_ENTRY}
                                                 ,{<<"list_entry_old_id">>, Id}
                                                 ,{<<"list_id">>, DocId}]
                                                ,Entry)
                      end,
    Entries1 = lists:map(MigrateEntryFun, Entries),
    kz_datamgr:save_doc(AccountDb, Entries1),
    Doc = kz_json:delete_key(<<"entries">>, kz_json:get_value(<<"doc">>, List)),
    kz_datamgr:save_doc(AccountDb, Doc),
    migrate(AccountDb, Lists);
migrate(_AccountDb, []) ->
    'ok'.

-spec init() -> any().
init() ->
    [crossbar_bindings:bind(Binding, ?MODULE, F)
     || {Binding, F} <- [{<<"v2_resource.allowed_methods.lists">>, 'allowed_methods'}
                        ,{<<"v2_resource.resource_exists.lists">>, 'resource_exists'}
                        ,{<<"v2_resource.content_types_provided.lists">>, 'content_types_provided'}
                        ,{<<"v2_resource.validate.lists">>, 'validate'}
                        ,{<<"v2_resource.execute.put.lists">>, 'save'}
                        ,{<<"v2_resource.execute.post.lists">>, 'save'}
                        ,{<<"v2_resource.execute.patch.lists">>, 'save'}
                        ,{<<"v2_resource.execute.delete.lists">>, 'delete'}
                        ]
    ].

-spec allowed_methods() -> http_methods().
-spec allowed_methods(path_token()) -> http_methods().
-spec allowed_methods(path_token(), path_token()) -> http_methods().
-spec allowed_methods(path_token(), path_token(), path_token()) -> http_methods().
-spec allowed_methods(path_token(), path_token(), path_token(), path_token()) -> http_methods().
allowed_methods() ->
    [?HTTP_GET, ?HTTP_PUT].
allowed_methods(_ListId) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_PATCH, ?HTTP_DELETE].
allowed_methods(_ListId, ?ENTRIES) ->
    [?HTTP_GET, ?HTTP_PUT, ?HTTP_DELETE].
allowed_methods(_ListId, ?ENTRIES, _ListEntryId) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_PATCH, ?HTTP_DELETE].
allowed_methods(_ListId, ?ENTRIES, _ListEntryId, ?VCARD) ->
    [?HTTP_GET];
allowed_methods(_ListId, ?ENTRIES, _ListEntryId, ?PHOTO) ->
    [?HTTP_POST].

-spec resource_exists() -> 'true'.
-spec resource_exists(path_token()) -> 'true'.
-spec resource_exists(path_token(), path_token()) -> 'true'.
-spec resource_exists(path_token(), path_token(), path_token()) -> 'true'.
-spec resource_exists(path_token(), path_token(), path_token(), path_token()) -> 'true'.
resource_exists() -> 'true'.
resource_exists(_ListId) -> 'true'.
resource_exists(_ListId, ?ENTRIES) -> 'true'.
resource_exists(_ListId, ?ENTRIES, _EntryId) -> 'true'.
resource_exists(_ListId, ?ENTRIES, _EntryId, ?VCARD) -> 'true'.

-spec content_types_provided(cb_context:context()) ->
                                    cb_context:context().
-spec content_types_provided(cb_context:context(), path_token()) ->
                                    cb_context:context().
-spec content_types_provided(cb_context:context(), path_token(), path_token()) ->
                                    cb_context:context().
-spec content_types_provided(cb_context:context(), path_token(), path_token(), path_token()) ->
                                    cb_context:context().
-spec content_types_provided(cb_context:context(), path_token(), path_token(), path_token(), path_token()) ->
                                    cb_context:context().
content_types_provided(Context) ->
    Context.
content_types_provided(Context, _) ->
    Context.
content_types_provided(Context, _, _) ->
    Context.
content_types_provided(Context, _, _, _) ->
    Context.
content_types_provided(Context, _, ?ENTRIES, _, ?VCARD) ->
    cb_context:set_content_types_provided(Context
                                         ,[{'to_binary'
                                           ,[{<<"text">>, <<"x-vcard">>}
                                            ,{<<"text">>, <<"directory">>}
                                            ]
                                           }
                                          ]
                                         ).

-spec validate(cb_context:context()) -> cb_context:context().
-spec validate(cb_context:context(), path_token()) -> cb_context:context().
-spec validate(cb_context:context(), path_token(), path_token()) -> cb_context:context().
-spec validate(cb_context:context(), path_token(), path_token(), path_token()) -> cb_context:context().
-spec validate(cb_context:context(), path_token(), path_token(), path_token(), path_token()) -> cb_context:context().
validate(Context) ->
    validate_req(cb_context:req_verb(Context), Context, []).
validate(Context, ListId) ->
    validate_req(cb_context:req_verb(Context), Context, [ListId]).
validate(Context, ListId, ?ENTRIES) ->
    validate_req(cb_context:req_verb(Context), Context, [ListId, ?ENTRIES]).
validate(Context, ListId, ?ENTRIES, EntryId) ->
    validate_req(cb_context:req_verb(Context), Context, [ListId, ?ENTRIES, EntryId]).
validate(Context, ListId, ?ENTRIES, EntryId, ?VCARD) ->
    validate_req(cb_context:req_verb(Context), Context, [ListId, ?ENTRIES, EntryId, ?VCARD]).

-spec validate_req(http_method(), cb_context:context(), path_tokens()) -> cb_context:context().
validate_req(?HTTP_GET, Context, []) ->
    crossbar_doc:load_view(?CB_LIST, [], Context, fun normalize_list/2);
validate_req(?HTTP_PUT, Context, []) ->
    validate_doc('undefined', ?TYPE_LIST, Context);

validate_req(?HTTP_GET, Context, [ListId]) ->
    crossbar_doc:load(ListId, Context, ?TYPE_CHECK_OPTION(?TYPE_LIST));
validate_req(?HTTP_DELETE, Context, [ListId]) ->
    crossbar_doc:load_view(<<"lists/entries">>, [{'key', ListId}], Context);
validate_req(?HTTP_POST, Context, [ListId]) ->
    validate_doc(ListId, ?TYPE_LIST, Context);
validate_req(?HTTP_PATCH, Context, [ListId] = Path) ->
    crossbar_doc:patch_and_validate(ListId, Context, fun(_Id, C) -> validate_req(?HTTP_POST, C, Path) end);

validate_req(?HTTP_GET, Context, [ListId, ?ENTRIES]) ->
    crossbar_doc:load_view(<<"lists/entries">>, [{'key', ListId}], Context);
validate_req(?HTTP_PUT, Context, [ListId, ?ENTRIES]) ->
    ReqData = kz_json:set_values([{<<"list_id">>, ListId}], cb_context:req_data(Context)),
    validate_doc('undefined', ?TYPE_LIST_ENTRY, cb_context:set_req_data(Context, ReqData));
validate_req(?HTTP_DELETE, Context, [ListId, ?ENTRIES]) ->
    crossbar_doc:load_view(<<"lists/entries">>, [{'key', ListId}], Context);

validate_req(?HTTP_GET, Context, [_ListId, ?ENTRIES, EntryId]) ->
    crossbar_doc:load(EntryId, Context, ?TYPE_CHECK_OPTION(?TYPE_LIST_ENTRY));
validate_req(?HTTP_DELETE, Context, [_ListId, ?ENTRIES, EntryId]) ->
    crossbar_doc:load(EntryId, Context, ?TYPE_CHECK_OPTION(?TYPE_LIST_ENTRY));
validate_req(?HTTP_POST, Context, [_ListId, ?ENTRIES, EntryId]) ->
    validate_doc(EntryId, ?TYPE_LIST_ENTRY, Context);
validate_req(?HTTP_PATCH, Context, [_ListId, ?ENTRIES, EntryId] = Path) ->
    crossbar_doc:patch_and_validate(EntryId, Context, fun(_Id, C) -> validate_req(?HTTP_POST, C, Path) end);

validate_req(?HTTP_GET, Context, [_ListId, ?ENTRIES, EntryId, ?VCARD]) ->
    Context1 = crossbar_doc:load(EntryId, Context, ?TYPE_CHECK_OPTION(?TYPE_LIST_ENTRY)),
    JObj = cb_context:doc(Context1),
    JProfile = kz_json:get_value(<<"profile">>, JObj),
    JObj1 = kz_json:merge_jobjs(JObj, JProfile),
    JObj2 = kz_json:set_values(
              props:filter_empty(
                [{<<"first_name">>, kz_json:get_value(<<"firstname">>, JObj1)}
                ,{<<"last_name">>, kz_json:get_value(<<"lastname">>, JObj1)}
                ,{<<"middle_name">>, kz_json:get_value(<<"middlename">>, JObj1)}
                ,{<<"nicknames">>, [kz_json:get_value(<<"displayname">>, JObj1)]}])
                              ,JObj1),
    JObj3 = set_org(JObj2, Context1),
    JObj4 = set_photo(JObj3, Context1),
    RespData = kzd_user:to_vcard(JObj4),
    cb_context:set_resp_data(Context1, [RespData, "\n"]).

-spec validate_doc(api_binary(), ne_binary(), cb_context:context()) -> cb_context:context().
validate_doc(Id, Type, Context) ->
    OnSuccess = fun(C) -> on_successfull_validation(Id, Type, C) end,
    cb_context:validate_request_data(type_schema_name(Type), Context, OnSuccess).

-spec delete(cb_context:context(), path_token()) -> cb_context:context().
-spec delete(cb_context:context(), path_token(), path_token()) -> cb_context:context().
-spec delete(cb_context:context(), path_token(), path_token(), path_token()) -> cb_context:context().
delete(Context, ListId) ->
    delete(Context, ListId, ?ENTRIES),
    Context1 = crossbar_doc:load(ListId, Context, ?TYPE_CHECK_OPTION(?TYPE_LIST_ENTRY)),
    crossbar_doc:delete(Context1).
delete(Context, _ListId, ?ENTRIES) ->
    Docs = [kz_json:get_value(<<"id">>, Entry) || Entry <- cb_context:doc(Context)],
    AccountDb = kz_util:format_account_id(cb_context:account_db(Context), 'encoded'),
    %% do we need 'soft' delete as in crossbar_doc?
    kz_datamgr:del_docs(AccountDb, Docs),
    Context.
delete(Context, _ListId, ?ENTRIES, _EntryId) ->
    crossbar_doc:delete(Context).

-spec save(cb_context:context()) -> cb_context:context().
-spec save(cb_context:context(), path_token()) -> cb_context:context().
-spec save(cb_context:context(), path_token(), path_token()) -> cb_context:context().
-spec save(cb_context:context(), path_token(), path_token(), path_token()) -> cb_context:context().
-spec save(cb_context:context(), path_token(), path_token(), path_token(), path_token()) -> cb_context:context().
save(Context) ->
    crossbar_doc:save(Context).
save(Context, _) ->
    save(Context).
save(Context, _, ?ENTRIES) ->
    save(Context).
save(Context, _, ?ENTRIES, _) ->
    save(Context).
save(Context, _, ?ENTRIES, EntryId, ?PHOTO) ->
    %% May be move code from cb_users_v2 to crossbar_doc?
    cb_users_v2:post(Context, EntryId, ?PHOTO).

-spec type_schema_name(ne_binary()) -> ne_binary().
type_schema_name(?TYPE_LIST) -> <<"lists">>;
type_schema_name(?TYPE_LIST_ENTRY) -> <<"list_entries">>.

-spec on_successfull_validation(api_binary(), ne_binary(), cb_context:context()) -> cb_context:context().
on_successfull_validation('undefined', Type, Context) ->
    Doc = kz_json:set_values([{<<"pvt_type">>, Type}], cb_context:doc(Context)),
    cb_context:set_doc(Context, Doc);
on_successfull_validation(Id, Type, Context) ->
    crossbar_doc:load_merge(Id, Context, ?TYPE_CHECK_OPTION(Type)).

-spec set_org(kz_json:object(), cb_context:context()) -> kz_json:object().
set_org(JObj, Context) ->
    ListId = kz_json:get_value(<<"list_id">>, JObj),
    Context1 = crossbar_doc:load(ListId, Context, ?TYPE_CHECK_OPTION(?TYPE_LIST_ENTRY)),
    set_org(JObj, Context1, cb_context:resp_status(Context1), ListId).

-spec set_org(kz_json:object(), cb_context:context(), crossbar_status(), ne_binary()) -> kz_json:object().
set_org(JObj, Context, 'success', _) ->
    case kz_json:get_value(<<"org">>, cb_context:doc(Context)) of
        'undefined' -> JObj;
        Val -> kz_json:set_value(<<"org">>, Val, JObj)
    end;
set_org(JObj, _, _, ListId) ->
    lager:debug("failed to load list ~p while loading list entry ~p",
                [ListId, kz_json:get_value(<<"id">>, JObj)]
               ),
    JObj.

-spec set_photo(kz_json:object(), cb_context:context()) -> kz_json:object().
set_photo(JObj, Context) ->
    %% This code is copied from cb_users_v2. May be move it to crossbar_doc?
    DocId = kz_json:get_value(<<"_id">>, cb_context:doc(Context)),
    Attach = crossbar_doc:load_attachment(DocId, ?PHOTO, ?TYPE_CHECK_OPTION(?TYPE_LIST_ENTRY), Context),
    case cb_context:resp_status(Attach) of
        'error' -> JObj;
        'success' ->
            Data = cb_context:resp_data(Attach),
            CT = kz_doc:attachment_content_type(cb_context:doc(Context), ?PHOTO),
            kz_json:set_value(?PHOTO, kz_json:from_list([{CT, Data}]), JObj)
    end.

-spec normalize_list(kz_json:object(), kz_json:objects()) -> kz_json:objects().
normalize_list(JObj, Acc) ->
    [kz_json:get_value(<<"value">>, JObj) | Acc].
