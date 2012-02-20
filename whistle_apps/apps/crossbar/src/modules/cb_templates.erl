%%%-------------------------------------------------------------------
%%% @copyright (C) 2011, VoIP INC
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

-include("../../include/crossbar.hrl").

-define(DB_PREFIX, "template/").

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    _ = crossbar_bindings:bind(<<"v1_resource.allowed_methods.templates">>, ?MODULE, allowed_methods),
    _ = crossbar_bindings:bind(<<"v1_resource.resource_exists.templates">>, ?MODULE, resource_exists),
    _ = crossbar_bindings:bind(<<"v1_resource.validate.templates">>, ?MODULE, validate),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.post.templates">>, ?MODULE, post),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.delete.templates">>, ?MODULE, delete),
    crossbar_bindings:bind(<<"account.created">>, ?MODULE, account_created).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines the verbs that are appropriate for the
%% given Nouns.  IE: '/accounts/' can only accept GET and PUT
%%
%% Failure here returns 405
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods/0 :: () -> http_methods().
-spec allowed_methods/1 :: (path_token()) -> http_methods().
allowed_methods() ->
    ['GET'].
allowed_methods(_) ->
    ['PUT', 'DELETE'].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines if the provided list of Nouns are valid.
%%
%% Failure here returns 404
%% @end
%%--------------------------------------------------------------------
-spec resource_exists/0 :: () -> boolean().
-spec resource_exists/1 :: (path_token()) -> boolean().
resource_exists() ->
    true.
resource_exists(_) ->
    true.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400
%% @end
%%--------------------------------------------------------------------
-spec validate/1 :: (#cb_context{}) -> #cb_context{}.
-spec validate/2 :: (#cb_context{}, path_token()) -> #cb_context{}.
validate(#cb_context{req_nouns=[{<<"templates">>, _}], req_verb = <<"get">>}=Context) ->
    summary(Context).

validate(#cb_context{req_nouns=[{<<"templates">>, _}], req_verb = <<"put">>}=Context, TemplateName) ->
    case load_template_db(TemplateName, Context) of
        #cb_context{resp_status=success} ->
            crossbar_util:response_conflicting_docs(Context);
        _Else ->
            crossbar_util:response(wh_json:new(), Context)
    end;
validate(#cb_context{req_nouns=[{<<"templates">>, _}], req_verb = <<"delete">>}=Context, TemplateName) ->
    load_template_db(TemplateName, Context);
validate(Context, TemplateName) ->
    load_template_db(TemplateName, Context).

put(Context, TemplateName) ->
    create_template_db(TemplateName, Context).

delete(Context, TemplateName) ->
    DbName = format_template_name(TemplateName, encoded),
    case couch_mgr:db_delete(DbName) of
        true -> crossbar_util:response(wh_json:new(), Context);
        false -> crossbar_util:response_db_fatal(Context)
    end.

account_created(#cb_context{doc=JObj, account_id=AccountId, db_name=AccountDb}) ->
    import_template(wh_json:get_value(<<"role">>, JObj), AccountId, AccountDb).

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
-spec summary/1 :: (#cb_context{}) -> #cb_context{}.
summary(Context) ->
    case couch_mgr:db_info() of
        {ok, Dbs} ->
            Context#cb_context{resp_status=success
                               ,resp_data = [format_template_name(Db, raw) || Db <- Dbs,
                                            (fun(<<?DB_PREFIX, _/binary>>) -> true;
                                                (_) -> false end)(Db)]
                              };
        _ ->
            crossbar_util:response_missing_view(Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will attempt to load the context with the db name of
%% for this account
%% @end
%%--------------------------------------------------------------------
-spec load_template_db/2 :: (ne_binary(), #cb_context{}) -> #cb_context{}.
load_template_db([TemplateName], Context) ->
    load_template_db(TemplateName, Context);
load_template_db(TemplateName, Context) ->
    DbName = format_template_name(TemplateName, encoded),
    case couch_mgr:db_exists(DbName) of
        false ->
            ?LOG("check failed for template db ~s", [DbName]),
            crossbar_util:response_db_missing(Context);
        true ->
            ?LOG("check succeeded for template db ~s", [DbName]),
            Context#cb_context{resp_status=success
                               ,db_name = DbName
                               ,account_id = TemplateName
                              }
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Format the template/db name into a raw, unencoded or encoded form.
%% @end
%%--------------------------------------------------------------------
-spec format_template_name/2 :: (ne_binary(), unencoded | encoded | raw) -> ne_binary().
format_template_name(<<"template/", _/binary>> = TemplateName, unencoded) ->
    TemplateName;
format_template_name(<<"template%2F", TemplateName/binary>>, unencoded) ->
    <<"template/", TemplateName/binary>>;
format_template_name(TemplateName, unencoded) ->
    <<"template/", TemplateName/binary>>;
format_template_name(<<"template%2F", _/binary>> = TemplateName, encoded) ->
    TemplateName;
format_template_name(<<"template/", TemplateName/binary>>, encoded) ->
    <<"template%2F", TemplateName/binary>>;
format_template_name(TemplateName, encoded) ->
    <<"template%2F", TemplateName/binary>>;
format_template_name(<<"template%2F", TemplateName/binary>>, raw) ->
    TemplateName;
format_template_name(<<"template/", TemplateName/binary>>, raw) ->
    TemplateName; 
format_template_name(TemplateName, raw) ->
    TemplateName.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Create a new template database and load it with views so it can be
%% used as an 'account'
%% @end
%%--------------------------------------------------------------------
-spec create_template_db/2 :: (ne_binary(), #cb_context{}) -> #cb_context{}.
create_template_db(TemplateName, Context) ->
    TemplateDb = format_template_name(TemplateName, encoded),
    case couch_mgr:db_create(TemplateDb) of
        false ->
            ?LOG_SYS("failed to create database: ~s", [TemplateDb]),
            crossbar_util:response_db_fatal(Context);
        true ->
            ?LOG_SYS("created DB for template ~s", [TemplateName]),
            couch_mgr:revise_docs_from_folder(TemplateDb, crossbar, "account", false),
            couch_mgr:revise_doc_from_file(TemplateDb, crossbar, ?MAINTENANCE_VIEW_FILE),
            Context#cb_context{resp_status=success}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% If a valid template database is provided import the non-design
%% documents into the account
%% @end
%%--------------------------------------------------------------------
-spec import_template/3 :: (undefined | ne_binary(), ne_binary(), ne_binary()) -> ne_binary().
import_template(undefined, _, _) ->
    ok;
import_template(TemplateName, AccountId, AccountDb) ->
    TemplateDb = format_template_name(TemplateName, encoded),
    case couch_mgr:all_docs(TemplateDb) of
        {ok, Docs} ->
            Ids = [Id || Doc <- Docs,
                         begin
                             Id = wh_json:get_value(<<"id">>, Doc),
                             (fun(<<"_design/", _/binary>>) -> false;
                                 (_) -> true end)(Id)
                         end],
            import_template_docs(Ids, TemplateDb, AccountId, AccountDb);
        _ ->
            ok
    end.            

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Given a list of IDs in the template database, import them into the
%% account database, correcting the pvt fields.
%% @end
%%--------------------------------------------------------------------
-spec import_template_docs/4 :: ([] | [ne_binary(),...], ne_binary(), ne_binary(), ne_binary()) -> ok.
import_template_docs([], _, _, _) ->
    ok;
import_template_docs([Id|Ids], TemplateDb, AccountId, AccountDb) ->
    case couch_mgr:open_doc(TemplateDb, Id) of
        {ok, JObj} ->
            Correctors = [fun(J) -> wh_json:set_value(<<"pvt_account_id">>, AccountId, J) end
                          ,fun(J) -> wh_json:set_value(<<"pvt_account_db">>, AccountDb, J) end
                         ],
            couch_mgr:ensure_saved(AccountDb, lists:foldr(fun(F, J) -> F(J) end, JObj, Correctors)),
            import_template_docs(Ids, TemplateDb, AccountId, AccountDb);
        {error, _} ->
            import_template_docs(Ids, TemplateDb, AccountId, AccountDb)
    end.
