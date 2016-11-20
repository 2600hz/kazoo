%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, 2600Hz
%%% @doc
%%%
%%% Handle client requests for template documents
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_templates).

-export([init/0
        ,allowed_methods/0, allowed_methods/1
        ,resource_exists/0, resource_exists/1
        ,validate/1, validate/2
        ,put/2
        ,delete/2
        ,account_created/1
        ]).

-include("crossbar.hrl").

-define(DB_PREFIX, "template/").

%%%===================================================================
%%% API
%%%===================================================================

-spec init() -> ok.
init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.templates">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.templates">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.templates">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.post.templates">>, ?MODULE, 'post'),
    _ = crossbar_bindings:bind(<<"*.execute.delete.templates">>, ?MODULE, 'delete'),
    _ = crossbar_bindings:bind(<<"account.created">>, ?MODULE, 'account_created'),
    ok.

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
allowed_methods() -> [?HTTP_GET].

-spec allowed_methods(path_token()) -> http_methods().
allowed_methods(_TemplateName) -> [?HTTP_PUT, ?HTTP_DELETE].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines if the provided list of Nouns are valid.
%%
%% Failure here returns 404
%% @end
%%--------------------------------------------------------------------
-spec resource_exists() -> 'true'.
resource_exists() -> 'true'.

-spec resource_exists(path_token()) -> 'true'.
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
-spec validate_request(cb_context:context(), http_method(), req_nouns()) -> cb_context:context().
-spec validate_request(cb_context:context(), http_method(), req_nouns(), path_token()) ->
                              cb_context:context().

validate(Context) ->
    validate_request(Context, cb_context:req_verb(Context), cb_context:req_nouns(Context)).
validate(Context, TemplateName) ->
    validate_request(Context, cb_context:req_verb(Context), cb_context:req_nouns(Context), TemplateName).

validate_request(Context, ?HTTP_GET, [{<<"templates">>, _}]) ->
    summary(Context).

validate_request(Context, ?HTTP_PUT, [{<<"templates">>, _}], TemplateName) ->
    case cb_context:resp_status(load_template_db(TemplateName, Context)) of
        'success' -> cb_context:add_system_error('datastore_conflict', Context);
        _Else     -> crossbar_util:response(kz_json:new(), Context)
    end;
validate_request(Context, ?HTTP_DELETE, [{<<"templates">>, _}], TemplateName) ->
    load_template_db(TemplateName, Context);
validate_request(Context, _, _, TemplateName) ->
    load_template_db(TemplateName, Context).

-spec put(cb_context:context(), path_token()) -> cb_context:context().
put(Context, TemplateName) ->
    create_template_db(TemplateName, Context).

-spec delete(cb_context:context(), ne_binary()) -> cb_context:context().
delete(Context, TemplateName) ->
    DbName = format_template_name(TemplateName, 'encoded'),
    case kz_datamgr:db_delete(DbName) of
        'true' -> crossbar_util:response(kz_json:new(), Context);
        'false' -> cb_context:add_system_error('datastore_fault', Context)
    end.

-spec account_created(cb_context:context()) -> ok.
account_created(Context) ->
    JObj = cb_context:doc(Context),
    AccountId = cb_context:account_id(Context),
    AccountDb = cb_context:account_db(Context),
    import_template(kz_json:get_value(<<"template">>, JObj), AccountId, AccountDb).

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%--------------------------------------------------------------------
-spec summary(cb_context:context()) -> cb_context:context().
summary(Context) ->
    case kz_datamgr:db_info() of
        {'ok', Dbs} ->
            RespData = [format_template_name(Db, 'raw') || <<?DB_PREFIX, _/binary>>=Db <- Dbs],
            cb_context:set_resp_status(cb_context:set_resp_data(Context, RespData)
                                      ,'success'
                                      );
        _ -> cb_context:add_system_error('datastore_missing_view', Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will attempt to load the context with the db name of
%% for this account
%% @end
%%--------------------------------------------------------------------
-spec load_template_db(ne_binary(), cb_context:context()) -> cb_context:context().
load_template_db([TemplateName], Context) ->
    load_template_db(TemplateName, Context);
load_template_db(TemplateName, Context) ->
    DbName = format_template_name(TemplateName, 'encoded'),
    case kz_datamgr:db_exists(DbName) of
        'false' ->
            lager:debug("check failed for template db ~s", [DbName]),
            cb_context:add_system_error('datastore_missing', Context);
        'true' ->
            lager:debug("check succeeded for template db ~s", [DbName]),
            cb_context:setters(Context, [{fun cb_context:set_resp_status/2, 'success'}
                                        ,{fun cb_context:set_account_id/2, TemplateName}
                                        ,{fun cb_context:set_account_db/2, DbName}
                                        ])
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Format the template/db name into a raw, unencoded or encoded form.
%% @end
%%--------------------------------------------------------------------
-spec format_template_name(ne_binary(), 'encoded' | 'raw') -> ne_binary().
format_template_name(<<"template%2F", _/binary>> = TemplateName, 'encoded') ->
    TemplateName;
format_template_name(<<"template/", TemplateName/binary>>, 'encoded') ->
    <<"template%2F", TemplateName/binary>>;
format_template_name(TemplateName, 'encoded') ->
    <<"template%2F", TemplateName/binary>>;
format_template_name(<<"template%2F", TemplateName/binary>>, 'raw') ->
    TemplateName;
format_template_name(<<"template/", TemplateName/binary>>, 'raw') ->
    TemplateName;
format_template_name(TemplateName, 'raw') ->
    TemplateName.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Create a new template database and load it with views so it can be
%% used as an 'account'
%% @end
%%--------------------------------------------------------------------
-spec create_template_db(ne_binary(), cb_context:context()) -> cb_context:context().
create_template_db(TemplateName, Context) ->
    TemplateDb = format_template_name(TemplateName, 'encoded'),
    case kz_datamgr:db_create(TemplateDb) of
        'false' ->
            lager:debug("failed to create database: ~s", [TemplateDb]),
            cb_context:add_system_error('datastore_fault', Context);
        'true' ->
            lager:debug("created DB for template ~s", [TemplateName]),
            kz_datamgr:revise_docs_from_folder(TemplateDb, 'crossbar', "account", 'false'),
            _ = kz_datamgr:revise_doc_from_file(TemplateDb, 'crossbar', ?MAINTENANCE_VIEW_FILE),
            cb_context:set_resp_status(Context, 'success')
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% If a valid template database is provided import the non-design
%% documents into the account
%% @end
%%--------------------------------------------------------------------
-spec import_template(api_binary(), ne_binary(), ne_binary()) -> 'ok'.
import_template('undefined', _, _) -> 'ok';
import_template(TemplateName, AccountId, AccountDb) ->
    %%TODO: use couch replication...
    TemplateDb = format_template_name(TemplateName, 'encoded'),
    case kz_datamgr:all_docs(TemplateDb) of
        {'ok', Docs} ->
            Ids = [Id || Doc <- Docs,
                         begin
                             Id = kz_doc:id(Doc),
                             not is_design_doc_id(Id)
                         end
                  ],
            import_template_docs(Ids, TemplateDb, AccountId, AccountDb);
        _ -> 'ok'
    end.

-spec is_design_doc_id(ne_binary()) -> boolean().
is_design_doc_id(<<"_design/", _/binary>>) -> 'false';
is_design_doc_id(_) -> 'true'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Given a list of IDs in the template database, import them into the
%% account database, correcting the pvt fields.
%% @end
%%--------------------------------------------------------------------
-spec import_template_docs(ne_binaries(), ne_binary(), ne_binary(), ne_binary()) -> 'ok'.
import_template_docs([], _, _, _) -> 'ok';
import_template_docs([Id|Ids], TemplateDb, AccountId, AccountDb) ->
    case kz_datamgr:open_doc(TemplateDb, Id) of
        {'error', _} -> import_template_docs(Ids, TemplateDb, AccountId, AccountDb);
        {'ok', JObj} ->
            Routines = [fun(J) -> kz_doc:set_account_id(J, AccountId) end
                       ,fun(J) -> kz_doc:set_account_db(J, AccountDb) end
                       ,fun kz_doc:delete_revision/1
                       ,fun kz_doc:delete_attachments/1
                       ],
            _ = kz_datamgr:ensure_saved(AccountDb, lists:foldr(fun(F, J) -> F(J) end, JObj, Routines)),
            Attachments = kz_doc:attachment_names(JObj),
            _ = import_template_attachments(Attachments, JObj, TemplateDb, AccountDb, Id),
            import_template_docs(Ids, TemplateDb, AccountId, AccountDb)
    end.

-spec import_template_attachments(ne_binaries(), kz_json:object(), ne_binary(), ne_binary(), ne_binary()) -> 'ok'.
import_template_attachments([], _, _, _, _) -> 'ok';
import_template_attachments([Attachment|Attachments], JObj, TemplateDb, AccountDb, Id) ->
    {'ok', Bin} = kz_datamgr:fetch_attachment(TemplateDb, Id, Attachment),
    ContentType = kz_doc:attachment_content_type(JObj, Attachment),
    Opts = [{'content_type', ContentType}],
    _ = kz_datamgr:put_attachment(AccountDb, Id, Attachment, Bin, Opts),
    import_template_attachments(Attachments, JObj, TemplateDb, AccountDb, Id).
