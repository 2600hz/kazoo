%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc Match list module
%%% Handle client requests for match list documents, api v2
%%%
%%%
%%% @author SIPLABS, LLC (Ilya Ashchepkov)
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cb_lists).

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
-define(ENTRIES_VIEW, <<"lists/entries">>).
-define(ENTRIES, <<"entries">>).
-define(VCARD, <<"vcard">>).
-define(PHOTO, <<"photo">>).
-define(TYPE_LIST, <<"list">>).
-define(TYPE_LIST_ENTRY, <<"list_entry">>).
%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_migrate(kz_term:ne_binary()) -> 'ok'.
maybe_migrate(Account) ->
    AccountDb = kzs_util:format_account_db(Account),
    case kz_datamgr:get_results(AccountDb, ?CB_LIST, ['include_docs']) of
        {'ok', Lists} -> migrate(AccountDb, Lists);
        {'error', _} -> 'ok'
    end.

-spec migrate(kz_term:ne_binary(), kz_json:objects()) -> 'ok'.
migrate(AccountDb, [List | Lists]) ->
    ListJObj = kz_json:get_json_value(<<"doc">>, List),

    Entries = kz_json:foldl(fun(Id, EntryJObj, Acc) ->
                                    [migrate_to_entry_doc(AccountDb, Id, EntryJObj, ListJObj) | Acc]
                            end
                           ,[]
                           ,kz_json:get_json_value(<<"entries">>, ListJObj, kz_json:new())
                           ),

    {'ok', _} = kz_datamgr:save_docs(AccountDb, Entries),
    migrate_old_entry_docs(AccountDb, List),
    {'ok', _} = kz_datamgr:save_doc(AccountDb, kz_json:delete_key(<<"entries">>, ListJObj)),
    migrate(AccountDb, Lists);
migrate(_AccountDb, []) ->
    'ok'.

%% @doc Migrate old entry documents to the new format
%% @end
-spec migrate_old_entry_docs(kz_term:ne_binary(), kz_json:object()) -> 'ok'.
migrate_old_entry_docs(AccountDb, ListJObj) ->
    case kz_datamgr:get_results(AccountDb, ?ENTRIES_VIEW, ['include_docs']) of
        {'ok', Entries} ->
            Updated = [migrate_old_entry_doc(AccountDb, ListJObj, kz_json:get_json_value(<<"doc">>, Entry))
                       || Entry <- Entries
                      ],
            {'ok', _} = kz_datamgr:save_docs(AccountDb, Updated),
            'ok';
        {'error', _Reason} ->
            lager:debug("failed to fetch and migrate old entries to new format: ~p", [_Reason])
    end.

%% @doc Migrate old entry document to the new format, e.g. set capture_group_length (old length value),
%% capture_group_key (old cid_key) and move cid_name/cid_number to name/number
%% @end
-spec migrate_old_entry_doc(kz_term:ne_binary(), kz_json:object(), kz_json:object()) -> kz_json:object().
migrate_old_entry_doc(AccountDb, ListJObj, EntryJObj) ->
    case kz_json:get_first_defined([<<"capture_group_key">>, <<"cid_key">>, <<"list_entry_old_id">>], EntryJObj) of
        'undefined' ->
            lager:debug("unable to find entry id for ~s inside list ~p", [kz_doc:id(EntryJObj), kz_doc:id(ListJObj)]),
            EntryJObj;
        EntryId -> migrate_to_entry_doc(AccountDb, EntryId, EntryJObj, ListJObj)
    end.

-spec migrate_to_entry_doc(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object(), kz_json:object()) ->
          kz_json:object().
migrate_to_entry_doc(AccountDb, EntryId, EntryJObj, ListJObj) ->
    CaptureGroupKey = kz_json:get_first_defined([<<"capture_group_key">>, <<"cid_key">>, <<"list_entry_old_id">>]
                                               ,EntryJObj
                                               ,EntryId
                                               ),
    CaptureGroupLength = kz_json:get_first_defined([<<"capture_group_length">>, <<"length">>]
                                                  ,ListJObj
                                                  ),

    CleanedUpEntry = kz_json:delete_keys([<<"cid_name">>, <<"cid_number">>, <<"length">>, <<"list_entry_old_id">>]
                                        ,EntryJObj
                                        ),

    Updates = [{<<"name">>, kz_json:get_first_defined([<<"cid_name">>, <<"name">>], EntryJObj)}
              ,{<<"number">>, kz_json:get_first_defined([<<"cid_number">>, <<"number">>], EntryJObj)}
              ,{<<"capture_group_key">>, CaptureGroupKey}
              ,{<<"capture_group_length">>, kz_term:to_integer(CaptureGroupLength)}
              ,{<<"list_id">>, kz_doc:id(ListJObj)}
              ],
    Doc = kz_json:set_values(Updates, CleanedUpEntry),
    kz_doc:update_pvt_parameters(Doc, AccountDb, [{<<"type">>, ?TYPE_LIST_ENTRY}]).

-spec init() -> any().
init() ->
    [crossbar_bindings:bind(Binding, ?MODULE, F)
     || {Binding, F} <- [{<<"*.allowed_methods.lists">>, 'allowed_methods'}
                        ,{<<"*.resource_exists.lists">>, 'resource_exists'}
                        ,{<<"*.content_types_provided.lists">>, 'content_types_provided'}
                        ,{<<"*.validate.lists">>, 'validate'}
                        ,{<<"*.execute.put.lists">>, 'save'}
                        ,{<<"*.execute.post.lists">>, 'save'}
                        ,{<<"*.execute.patch.lists">>, 'save'}
                        ,{<<"*.execute.delete.lists">>, 'delete'}
                        ]
    ].

-spec allowed_methods() -> http_methods().
allowed_methods() ->
    [?HTTP_GET, ?HTTP_PUT].

-spec allowed_methods(path_token()) -> http_methods().
allowed_methods(_ListId) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_PATCH, ?HTTP_DELETE].

-spec allowed_methods(path_token(), path_token()) -> http_methods().
allowed_methods(_ListId, ?ENTRIES) ->
    [?HTTP_GET, ?HTTP_PUT, ?HTTP_DELETE].

-spec allowed_methods(path_token(), path_token(), path_token()) -> http_methods().
allowed_methods(_ListId, ?ENTRIES, _ListEntryId) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_PATCH, ?HTTP_DELETE].

-spec allowed_methods(path_token(), path_token(), path_token(), path_token()) -> http_methods().
allowed_methods(_ListId, ?ENTRIES, _ListEntryId, ?VCARD) ->
    [?HTTP_GET];
allowed_methods(_ListId, ?ENTRIES, _ListEntryId, ?PHOTO) ->
    [?HTTP_POST].

-spec resource_exists() -> 'true'.
resource_exists() -> 'true'.

-spec resource_exists(path_token()) -> 'true'.
resource_exists(_ListId) -> 'true'.

-spec resource_exists(path_token(), path_token()) -> 'true'.
resource_exists(_ListId, ?ENTRIES) -> 'true'.

-spec resource_exists(path_token(), path_token(), path_token()) -> 'true'.
resource_exists(_ListId, ?ENTRIES, _EntryId) -> 'true'.

-spec resource_exists(path_token(), path_token(), path_token(), path_token()) -> 'true'.
resource_exists(_ListId, ?ENTRIES, _EntryId, ?VCARD) -> 'true'.

-spec content_types_provided(cb_context:context()) ->
          cb_context:context().
content_types_provided(Context) ->
    Context.

-spec content_types_provided(cb_context:context(), path_token()) ->
          cb_context:context().
content_types_provided(Context, _) ->
    Context.

-spec content_types_provided(cb_context:context(), path_token(), path_token()) ->
          cb_context:context().
content_types_provided(Context, _, _) ->
    Context.

-spec content_types_provided(cb_context:context(), path_token(), path_token(), path_token()) ->
          cb_context:context().
content_types_provided(Context, _, _, _) ->
    Context.

-spec content_types_provided(cb_context:context(), path_token(), path_token(), path_token(), path_token()) ->
          cb_context:context().
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
validate(Context) ->
    validate_req(cb_context:req_verb(Context), Context, []).

-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context, ListId) ->
    validate_req(cb_context:req_verb(Context), Context, [ListId]).

-spec validate(cb_context:context(), path_token(), path_token()) -> cb_context:context().
validate(Context, ListId, ?ENTRIES) ->
    validate_req(cb_context:req_verb(Context), Context, [ListId, ?ENTRIES]).

-spec validate(cb_context:context(), path_token(), path_token(), path_token()) -> cb_context:context().
validate(Context, ListId, ?ENTRIES, EntryId) ->
    validate_req(cb_context:req_verb(Context), Context, [ListId, ?ENTRIES, EntryId]).

-spec validate(cb_context:context(), path_token(), path_token(), path_token(), path_token()) -> cb_context:context().
validate(Context, ListId, ?ENTRIES, EntryId, ?VCARD) ->
    validate_req(cb_context:req_verb(Context), Context, [ListId, ?ENTRIES, EntryId, ?VCARD]).

-spec validate_req(http_method(), cb_context:context(), path_tokens()) -> cb_context:context().
validate_req(?HTTP_GET, Context, []) ->
    crossbar_view:load(Context, ?CB_LIST, [{'mapper', crossbar_view:get_value_fun()}]);
validate_req(?HTTP_PUT, Context, []) ->
    validate_doc('undefined', ?TYPE_LIST, Context);

validate_req(?HTTP_GET, Context, [ListId]) ->
    crossbar_doc:load(ListId, Context, ?TYPE_CHECK_OPTION(?TYPE_LIST));
validate_req(?HTTP_DELETE, Context, [ListId]) ->
    crossbar_view:load(Context, ?ENTRIES_VIEW, [{'key', ListId}]);
validate_req(?HTTP_POST, Context, [ListId]) ->
    validate_doc(ListId, ?TYPE_LIST, Context);
validate_req(?HTTP_PATCH, Context, [ListId] = Path) ->
    crossbar_doc:patch_and_validate(ListId, Context, fun(_Id, C) -> validate_req(?HTTP_POST, C, Path) end);

validate_req(?HTTP_GET, Context, [ListId, ?ENTRIES]) ->
    %% FIXME: why this doesn't normalize the db result??
    crossbar_view:load(Context, ?ENTRIES_VIEW, [{'key', ListId}]);
validate_req(?HTTP_PUT, Context, [ListId, ?ENTRIES]) ->
    ReqData = kz_json:set_values([{<<"list_id">>, ListId}], cb_context:req_data(Context)),
    validate_doc('undefined', ?TYPE_LIST_ENTRY, cb_context:set_req_data(Context, ReqData));
validate_req(?HTTP_DELETE, Context, [ListId, ?ENTRIES]) ->
    crossbar_view:load(Context, ?ENTRIES_VIEW, [{'key', ListId}]);

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
    JProfile = kz_json:get_json_value(<<"profile">>, JObj),
    JObj1 = kz_json:merge_jobjs(JObj, JProfile),
    JObj2 = kz_json:set_values(props:filter_empty([{<<"first_name">>, kz_json:get_value(<<"firstname">>, JObj1)}
                                                  ,{<<"last_name">>, kz_json:get_value(<<"lastname">>, JObj1)}
                                                  ,{<<"middle_name">>, kz_json:get_value(<<"middlename">>, JObj1)}
                                                  ,{<<"nicknames">>, [kz_json:get_value(<<"displayname">>, JObj1)]}
                                                  ])
                              ,JObj1
                              ),
    JObj3 = set_org(JObj2, Context1),
    JObj4 = set_photo(JObj3, Context1),
    RespData = kzd_users:to_vcard(JObj4),
    cb_context:set_resp_data(Context1, [RespData, "\n"]).

-spec validate_doc(kz_term:api_binary(), kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
validate_doc(Id, Type, Context) ->
    OnSuccess = fun(C) -> on_successfull_validation(Id, Type, C) end,
    cb_context:validate_request_data(type_schema_name(Type), Context, OnSuccess).

-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, ListId) ->
    _ = delete(Context, ListId, ?ENTRIES),
    Context1 = crossbar_doc:load(ListId, Context, ?TYPE_CHECK_OPTION(?TYPE_LIST_ENTRY)),
    crossbar_doc:delete(Context1).

-spec delete(cb_context:context(), path_token(), path_token()) -> cb_context:context().
delete(Context, _ListId, ?ENTRIES) ->
    Docs = [kz_doc:id(Entry) || Entry <- cb_context:doc(Context)],
    AccountDb = kzs_util:format_account_db(cb_context:db_name(Context)),
    %% do we need 'soft' delete as in crossbar_doc?
    _ = kz_datamgr:del_docs(AccountDb, Docs),
    Context.

-spec delete(cb_context:context(), path_token(), path_token(), path_token()) -> cb_context:context().
delete(Context, _ListId, ?ENTRIES, _EntryId) ->
    crossbar_doc:delete(Context).

-spec save(cb_context:context()) -> cb_context:context().
save(Context) ->
    crossbar_doc:save(Context).

-spec save(cb_context:context(), path_token()) -> cb_context:context().
save(Context, _) ->
    save(Context).

-spec save(cb_context:context(), path_token(), path_token()) -> cb_context:context().
save(Context, _, ?ENTRIES) ->
    save(Context).

-spec save(cb_context:context(), path_token(), path_token(), path_token()) -> cb_context:context().
save(Context, _, ?ENTRIES, _) ->
    save(Context).

-spec save(cb_context:context(), path_token(), path_token(), path_token(), path_token()) -> cb_context:context().
save(Context, _, ?ENTRIES, EntryId, ?PHOTO) ->
    %% May be move code from cb_users to crossbar_doc?
    cb_users:post(Context, EntryId, ?PHOTO).

-spec type_schema_name(kz_term:ne_binary()) -> kz_term:ne_binary().
type_schema_name(?TYPE_LIST) -> <<"lists">>;
type_schema_name(?TYPE_LIST_ENTRY) -> <<"list_entries">>.

-spec on_successfull_validation(kz_term:api_binary(), kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
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

-spec set_org(kz_json:object(), cb_context:context(), crossbar_status(), kz_term:ne_binary()) -> kz_json:object().
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
    %% This code is copied from cb_users. May be move it to crossbar_doc?
    DocId = kz_json:get_value(<<"_id">>, cb_context:doc(Context)),
    Attach = crossbar_doc:load_attachment(DocId, ?PHOTO, ?TYPE_CHECK_OPTION(?TYPE_LIST_ENTRY), Context),
    case cb_context:resp_status(Attach) of
        'error' -> JObj;
        'success' ->
            Data = cb_context:resp_data(Attach),
            CT = kz_doc:attachment_content_type(cb_context:doc(Context), ?PHOTO),
            kz_json:set_value(?PHOTO, kz_json:from_list([{CT, Data}]), JObj)
    end.
