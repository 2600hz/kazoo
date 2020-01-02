%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2020, 2600Hz
%%% @doc Fax Box API
%%% @author Luis Azedo
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cb_faxboxes).

-export([init/0
        ,allowed_methods/0, allowed_methods/1
        ,resource_exists/0, resource_exists/1
        ,validate/1, validate/2
        ,validate_resource/1, validate_resource/2
        ,put/1
        ,post/2
        ,patch/2
        ,delete/2
        ]).

-include("crossbar.hrl").

-define(CB_LIST, <<"faxbox/crossbar_listing">>).

-define(DEFAULT_FAX_SMTP_DOMAIN, <<"fax.kazoo.io">>).

-define(SMTP_EMAIL_FIELD, <<"pvt_smtp_email_address">>).

-define(LEAKED_FIELDS, [?SMTP_EMAIL_FIELD
                       ]).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the bindings this module will respond to.
%% @end
%%------------------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.faxboxes">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.faxboxes">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.faxboxes">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.validate_resource.faxboxes">>, ?MODULE, 'validate_resource'),
    _ = crossbar_bindings:bind(<<"*.execute.put.faxboxes">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"*.execute.post.faxboxes">>, ?MODULE, 'post'),
    _ = crossbar_bindings:bind(<<"*.execute.patch.faxboxes">>, ?MODULE, 'patch'),
    crossbar_bindings:bind(<<"*.execute.delete.faxboxes">>, ?MODULE, 'delete').

%%------------------------------------------------------------------------------
%% @doc Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%------------------------------------------------------------------------------

-spec allowed_methods() -> http_methods().
allowed_methods() ->
    [?HTTP_GET, ?HTTP_PUT].

-spec allowed_methods(path_token()) -> http_methods().
allowed_methods(_FaxboxId) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_PATCH, ?HTTP_DELETE].

%%------------------------------------------------------------------------------
%% @doc Does the path point to a valid resource.
%% For example:
%%
%% ```
%%    /faxes => []
%%    /faxes/foo => [<<"foo">>]
%%    /faxes/foo/bar => [<<"foo">>, <<"bar">>]
%% '''
%% @end
%%------------------------------------------------------------------------------

-spec resource_exists() -> 'true'.
resource_exists() -> 'true'.

-spec resource_exists(path_token()) -> 'true'.
resource_exists(_BoxId) -> 'true'.

-spec validate_resource(cb_context:context()) -> cb_context:context().
validate_resource(Context) -> Context.

-spec validate_resource(cb_context:context(), path_token()) -> cb_context:context().
validate_resource(Context, FaxboxId) ->
    case kz_datamgr:open_cache_doc(cb_context:account_id(Context), FaxboxId) of
        {'ok', JObj} -> cb_context:store(Context, <<"faxbox">>, JObj);
        _ -> Context
    end.

%%------------------------------------------------------------------------------
%% @doc Check the request (request body, query string params, path tokens, etc)
%% and load necessary information.
%% /faxes might load a list of fax objects
%% /faxes/123 might load the fax object 123
%% Generally, use crossbar_doc to manipulate the cb_context{} record
%% @end
%%------------------------------------------------------------------------------

-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    validate_faxboxes(Context, cb_context:req_verb(Context)).

validate_faxboxes(Context, ?HTTP_PUT) ->
    validate_email_address(create_faxbox(remove_private_fields(Context)));
validate_faxboxes(Context, ?HTTP_GET) ->
    faxbox_listing(Context).

-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context, Id) ->
    validate_faxbox(Context, Id, cb_context:req_verb(Context)).

validate_faxbox(Context, Id, ?HTTP_GET) ->
    read(Id, Context);
validate_faxbox(Context, Id, ?HTTP_POST) ->
    validate_email_address(update_faxbox(Id, remove_private_fields(Context)));
validate_faxbox(Context, Id, ?HTTP_PATCH) ->
    validate_patch(update_faxbox(Id, remove_private_fields(Context)));
validate_faxbox(Context, Id, ?HTTP_DELETE) ->
    delete_faxbox(Id, Context).

-spec validate_email_address(cb_context:context()) -> cb_context:context().
validate_email_address(Context) ->
    Email = kz_json:get_value(<<"custom_smtp_email_address">>, cb_context:doc(Context)),
    IsValid =
        case Email of
            'undefined' -> 'true';
            Email -> is_faxbox_email_global_unique(Email, kz_doc:id(cb_context:doc(Context)))
        end,
    case IsValid of
        'true' -> Context;
        'false' ->
            cb_context:add_validation_error(<<"custom_smtp_email_address">>
                                           ,<<"unique">>
                                           ,kz_json:from_list(
                                              [{<<"message">>, <<"email address must be unique">>}
                                              ,{<<"cause">>, Email}
                                              ])
                                           ,Context
                                           )
    end.

-spec validate_patch(cb_context:context()) -> cb_context:context().
validate_patch(Context) ->
    DocId = kz_doc:id(cb_context:doc(Context)),
    IsValid = case kz_json:get_value(<<"custom_smtp_email_address">>, cb_context:doc(Context)) of
                  'undefined' -> 'true';
                  CustomEmail -> is_faxbox_email_global_unique(CustomEmail, DocId)
              end,
    case IsValid of
        'true' ->
            Context1 = crossbar_doc:load(DocId, Context, ?TYPE_CHECK_OPTION(kzd_fax_box:type())),
            case cb_context:resp_status(Context1) of
                'success' ->
                    PatchJObj = cb_context:req_data(Context),
                    ConfigsJObj = kz_json:merge_jobjs(PatchJObj, cb_context:doc(Context1)),
                    cb_context:set_doc(Context, ConfigsJObj);
                _Status ->
                    Context1
            end;
        'false' ->
            cb_context:add_validation_error(<<"custom_smtp_email_address">>
                                           ,<<"unique">>
                                           ,<<"email address must be unique">>
                                           ,Context
                                           )
    end.

%%------------------------------------------------------------------------------
%% @doc If the HTTP verb is PUT, execute the actual action, usually a db save.
%% @end
%%------------------------------------------------------------------------------
-spec put(cb_context:context()) -> cb_context:context().
put(Context) ->
    Context1 = save_faxbox_doc(Context, 'create'),
    case cb_context:resp_status(Context1) of
        'success' ->
            RespData = kz_doc:public_fields(leak_private_fields(cb_context:doc(Context1))),
            cb_context:set_resp_data(Context1, RespData);
        _ -> Context1
    end.

%%------------------------------------------------------------------------------
%% @doc If the HTTP verb is POST, execute the actual action, usually a db save
%% (after a merge perhaps).
%% @end
%%------------------------------------------------------------------------------
-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, _Id) ->
    Context1 = save_faxbox_doc(Context, 'update'),
    case cb_context:resp_status(Context1) of
        'success' ->
            RespData = kz_doc:public_fields(leak_private_fields(cb_context:doc(Context1))),
            cb_context:set_resp_data(Context1, RespData);
        _ -> Context1
    end.

%%------------------------------------------------------------------------------
%% @doc If the HTTP verb is PATCH, execute the actual action, usually a db save
%% (after a merge).
%% @end
%%------------------------------------------------------------------------------
-spec patch(cb_context:context(), path_token()) -> cb_context:context().
patch(Context, Id) ->
    post(Context, Id).

%%------------------------------------------------------------------------------
%% @doc If the HTTP verb is DELETE, execute the actual action, usually a db delete
%% @end
%%------------------------------------------------------------------------------
-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, Id) ->
    faxbox_doc_delete(Context, Id).

-spec create_faxbox(cb_context:context()) -> cb_context:context().
create_faxbox(Context) ->
    OnSuccess = fun(C) -> on_faxbox_successful_validation('undefined', C) end,
    cb_context:validate_request_data(<<"faxbox">>, Context, OnSuccess).

-spec delete_faxbox(kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
delete_faxbox(Id, Context) ->
    read(Id, Context).

%%------------------------------------------------------------------------------
%% @doc Load an instance from the database
%% @end
%%------------------------------------------------------------------------------
-spec read(kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
read(Id, Context) ->
    crossbar_doc:load(Id, Context, ?TYPE_CHECK_OPTION(kzd_fax_box:type())).

-spec leak_private_fields(kz_json:object()) -> kz_json:object().
leak_private_fields(JObj) ->
    J = kz_json:set_value(<<"id">>, kz_doc:id(JObj), JObj),
    lists:foldl(fun leak_private_field/2, J, ?LEAKED_FIELDS).

-spec leak_private_field(kz_term:ne_binary(), kz_json:object()) -> kz_json:object().
leak_private_field(<<"pvt_", K1/binary>> = K, Acc) ->
    case kz_json:get_value(K, Acc) of
        'undefined' -> Acc;
        Value -> leak_private_field_value(K, K1, Value, Acc)
    end;
leak_private_field(_K, Acc) -> Acc.

-spec leak_private_field_value(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:json_term(), kz_json:object()) ->
          kz_json:object().
leak_private_field_value(_K, K1, V, Acc) ->
    kz_json:set_value(K1, V, Acc).

-spec remove_private_fields(cb_context:context()) -> cb_context:context().
remove_private_fields(Context) ->
    JObj1 = lists:foldl(fun remove_private_fields_fold/2
                       ,cb_context:req_data(Context)
                       ,?LEAKED_FIELDS
                       ),
    cb_context:set_req_data(Context, JObj1).

-spec remove_private_fields_fold(kz_term:ne_binary(), kz_json:object()) -> kz_json:object().
remove_private_fields_fold(<<"pvt_", K1/binary>>, Acc) ->
    case kz_json:get_value(K1, Acc) of
        'undefined' -> Acc;
        _Value -> kz_json:delete_key(K1, Acc)
    end.

%%------------------------------------------------------------------------------
%% @doc Update an existing instance with the data provided, if it is
%% valid
%% @end
%%------------------------------------------------------------------------------
-spec update_faxbox(kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
update_faxbox(Id, Context) ->
    OnSuccess = fun(C) -> on_faxbox_successful_validation(Id, C) end,
    cb_context:validate_request_data(<<"faxbox">>, Context, OnSuccess).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec on_faxbox_successful_validation(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
on_faxbox_successful_validation('undefined', Context) ->
    cb_context:set_doc(Context
                      ,kz_json:set_values([{<<"pvt_type">>, kzd_fax_box:type()}
                                          ,{<<"pvt_account_id">>, cb_context:account_id(Context)}
                                          ,{<<"pvt_account_db">>, cb_context:db_name(Context)}
                                          ,{<<"pvt_reseller_id">>, cb_context:reseller_id(Context)}
                                          ,{<<"_id">>, kz_binary:rand_hex(16)}
                                          ,{<<"pvt_smtp_email_address">>, generate_email_address(Context)}
                                          ]
                                         ,cb_context:doc(Context)
                                         )
                      );
on_faxbox_successful_validation(DocId, Context) ->
    crossbar_doc:load_merge(DocId, Context, ?TYPE_CHECK_OPTION(kzd_fax_box:type())).

-spec generate_email_address(cb_context:context()) -> kz_term:ne_binary().
generate_email_address(Context) ->
    ResellerId =  cb_context:reseller_id(Context),
    Domain = kapps_account_config:get_global(ResellerId, <<"fax">>, <<"default_smtp_domain">>, ?DEFAULT_FAX_SMTP_DOMAIN),
    New = kz_binary:rand_hex(4),
    <<New/binary, ".", Domain/binary>>.

%%------------------------------------------------------------------------------
%% @doc Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%------------------------------------------------------------------------------
-spec faxbox_listing(cb_context:context()) -> cb_context:context().
faxbox_listing(Context) ->
    ViewOptions = ['include_docs'],
    crossbar_doc:load_view(<<"faxbox/crossbar_listing">>
                          ,ViewOptions
                          ,Context
                          ,fun normalize_view_results/2
                          ).

-spec normalize_view_results(kz_json:object(), kz_json:objects()) -> kz_json:objects().
normalize_view_results(JObj, Acc) ->
    [leak_private_fields(kz_json:get_value(<<"doc">>, JObj)) | Acc].

-spec is_faxbox_email_global_unique(kz_term:ne_binary(), kz_term:ne_binary()) -> boolean().
is_faxbox_email_global_unique(Email, FaxBoxId) ->
    ViewOptions = [{'key', kz_term:to_lower_binary(Email)}],
    case kz_datamgr:get_results(?KZ_FAXES_DB, <<"faxbox/email_address">>, ViewOptions) of
        {'ok', []} -> 'true';
        {'ok', [JObj]} -> kz_doc:id(JObj) =:= FaxBoxId;
        {'error', 'not_found'} -> 'true';
        _ -> 'false'
    end.

-spec save_faxbox_doc(cb_context:context(), 'create' | 'update') -> cb_context:context().
save_faxbox_doc(Context0, Action) ->
    case maybe_save_faxbox_doc(Context0) of
        {'ok', Context1} ->
            save_doc_to_faxdb(Context1, Action);
        {'error', Context1} ->
            lager:error("failed to save doc to account db"),
            Context1
    end.

-spec maybe_save_faxbox_doc(cb_context:context()) -> {'ok' | 'error', cb_context:context()}.
maybe_save_faxbox_doc(Context) ->
    maybe_save_faxbox_doc(Context, cb_context:resp_status(Context) =:= 'success').

-spec maybe_save_faxbox_doc(cb_context:context(), boolean()) -> {'ok' | 'error', cb_context:context()}.
maybe_save_faxbox_doc(Context, 'true') ->
    Context1 = crossbar_doc:save(Context),
    case cb_context:resp_status(Context1) =:= 'success' of
        'true' -> {'ok', Context1};
        'false' -> {'error', Context1}
    end;
maybe_save_faxbox_doc(Context, 'false') ->
    {'error', Context}.

-spec save_doc_to_faxdb(cb_context:context(), 'create' | 'update') -> cb_context:context().
save_doc_to_faxdb(Context, Action) ->
    case maybe_save_faxbox_doc(prepare_faxes_doc(Context, Action)) of
        {'ok', Context1} -> Context1;
        {'error', Context1} ->
            _ = rollback_doc(Context, Action),
            Context1
    end.

-spec rollback_doc(cb_context:context(), 'create' | 'update') -> cb_context:context().
rollback_doc(Context, 'create') ->
    lager:error("failed to save doc to faxes db, rolling back"),
    crossbar_doc:delete(Context);
rollback_doc(Context, 'update') ->
    lager:debug("failed to save to faxdb, revert doc in account db"),
    crossbar_doc:save(cb_context:set_doc(Context, cb_context:fetch(Context, 'db_doc'))).

-spec prepare_faxes_doc(cb_context:context(), 'create' | 'update') -> cb_context:context().
prepare_faxes_doc(Context, Action) ->
    ToSave = kz_json:set_values([{kz_doc:path_account_db(), ?KZ_FAXES_DB}
                                ,{kz_doc:path_revision(), 'null'}
                                ]
                               ,cb_context:doc(Context)
                               ),
    Context1 = cb_context:setters(Context
                                 ,[{fun cb_context:set_db_name/2, ?KZ_FAXES_DB}
                                  ,{fun cb_context:set_doc/2, ToSave}
                                  ]),
    maybe_load_merge(Context1, Action).

-spec maybe_load_merge(cb_context:context(), 'create' | 'update') -> cb_context:context().
maybe_load_merge(Context, 'create') ->
    Context;
maybe_load_merge(Context, 'update') ->
    DocId = kz_doc:id(cb_context:doc(Context)),
    crossbar_doc:load_merge(DocId, Context, ?TYPE_CHECK_OPTION(kzd_fax_box:type())).

-spec faxbox_doc_delete(cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
faxbox_doc_delete(Context, Id) ->
    Ctx2 = crossbar_doc:delete(Context),
    _ = crossbar_doc:delete(
          read(Id, cb_context:set_db_name(Context, ?KZ_FAXES_DB))
         ),
    Ctx2.
