%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2012, VoIP INC
%%% @doc
%%% Provision template module
%%%
%%% Handle client requests for provisioner template documents
%%%
%%% Note regarding storing the template as an attachment:
%%% Since the template is a 300k json object it is more efficent to store it as
%%% an attachment, funky I know but necessary. Also since we already require
%%% two API calls for editing a template we will maintain backward compatiblity by
%%% not requiring an additional API call for the template and merge/unmerge it
%%% from requests.
%%%
%%% @end
%%% @contributors
%%%   Jon Blanton
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_global_provisioner_templates).

-export([init/0
         ,content_types_provided/3, content_types_accepted/3
         ,allowed_methods/0, allowed_methods/1, allowed_methods/2
         ,resource_exists/0, resource_exists/1, resource_exists/2
         ,validate/1, validate/2, validate/3
         ,put/1
         ,post/2
         ,delete/2
         ,put/3
         ,post/3
         ,delete/3
        ]).

-include("../crossbar.hrl").

-define(MOD_CONFIG_CAT, <<(?CONFIG_CAT)/binary, ".provisioner_templates">>).
-define(CB_LIST, <<"provisioner_templates/crossbar_listing">>).
-define(IMAGE_REQ, <<"image">>).
-define(TEMPLATE_ATTCH, <<"template">>).
-define(MIME_TYPES, [{<<"image">>, <<"*">>}
                     ,{<<"application">>, <<"base64">>}
                     ,{<<"application">>, <<"x-base64">>}
                     ,{<<"application">>, <<"octet-stream">>}
                    ]).

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    _ = crossbar_bindings:bind(<<"*.content_types_provided.global_provisioner_templates">>, ?MODULE, 'content_types_provided'),
    _ = crossbar_bindings:bind(<<"*.content_types_accepted.global_provisioner_templates">>, ?MODULE, 'content_types_accepted'),
    _ = crossbar_bindings:bind(<<"*.allowed_methods.global_provisioner_templates">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.global_provisioner_templates">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.global_provisioner_templates">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.put.global_provisioner_templates">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"*.execute.post.global_provisioner_templates">>, ?MODULE, 'post'),
    _ = crossbar_bindings:bind(<<"*.execute.delete.global_provisioner_templates">>, ?MODULE, 'delete'),
    _ = crossbar_bindings:bind(<<"*.finish_request.put.devices">>, ?MODULE, 'device_updated').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Add content types provided by this module
%% @end
%%--------------------------------------------------------------------
-spec content_types_provided(cb_context:context(), path_token(), path_token()) ->
                                    crossbar_content_handlers().
content_types_provided(Context, DocId, ?IMAGE_REQ) ->
    case cb_context:req_verb(Context) of
        ?HTTP_GET ->
            case couch_mgr:open_doc(?WH_PROVISIONER_DB, DocId) of
                {'error', _} -> Context;
                {'ok', JObj} ->
                    [Type, SubType] = binary:split(get_content_type(JObj), <<"/">>),
                    lager:debug("found attachement of content type: ~s/~s~n", [Type, SubType]),
                    cb_context:set_content_types_provided(Context, [{'to_binary', [{Type, SubType}]}])
            end;
        _ -> Context
    end;
content_types_provided(Context, _, _) ->
    Context.

%% @private
-spec get_content_type(wh_json:object()) -> ne_binary().
get_content_type(JObj) ->
    case wh_doc:attachment_content_type(JObj, ?IMAGE_REQ) of
        'undefined' -> <<"application/octet-stream">>;
        CT -> CT
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Add content types accepted by this module
%% @end
%%--------------------------------------------------------------------
-spec content_types_accepted(cb_context:context(), path_token(), path_token()) ->
                                    cb_context:context().
content_types_accepted(Context, _, ?IMAGE_REQ) ->
    case cb_context:req_verb(Context) of
        ?HTTP_PUT ->
            cb_context:set_content_types_accepted(Context, [{'from_binary', ?MIME_TYPES}]);
        ?HTTP_POST ->
            cb_context:set_content_types_accepted(Context, [{'from_binary', ?MIME_TYPES}]);
        _ ->
            Context
    end;
content_types_accepted(Context, _, _) ->
    Context.

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
-spec allowed_methods(path_token(), path_token()) -> http_methods().
allowed_methods() ->
    [?HTTP_GET, ?HTTP_PUT].
allowed_methods(_) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_DELETE].
allowed_methods(_, ?IMAGE_REQ) ->
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
-spec resource_exists(path_token(), path_token()) -> 'true'.
resource_exists() -> 'true'.
resource_exists(_) -> 'true'.
resource_exists(_, ?IMAGE_REQ) -> 'true'.

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
-spec validate_verb(cb_context:context(), http_method()) -> cb_context:context().
validate(Context) ->
    validate_verb(Context, cb_context:req_verb(Context)).
validate_verb(Context, ?HTTP_GET) ->
    load_provisioner_template_summary(cb_context:set_account_db(Context, ?WH_PROVISIONER_DB));
validate_verb(Context, ?HTTP_PUT) ->
    create_provisioner_template(cb_context:set_account_db(Context, ?WH_PROVISIONER_DB)).

-spec validate(cb_context:context(), path_token()) -> cb_context:context().
-spec validate_verb(cb_context:context(), http_method(), path_token()) -> cb_context:context().
validate(Context, DocId) ->
    validate_verb(Context, cb_context:req_verb(Context), DocId).
validate_verb(Context, ?HTTP_GET, DocId) ->
    load_provisioner_template(DocId, cb_context:set_account_db(Context, ?WH_PROVISIONER_DB));
validate_verb(Context, ?HTTP_POST, DocId) ->
    update_provisioner_template(DocId, cb_context:set_account_db(Context, ?WH_PROVISIONER_DB));
validate_verb(Context, ?HTTP_DELETE, DocId) ->
    load_provisioner_template(DocId, cb_context:set_account_db(Context, ?WH_PROVISIONER_DB)).

-spec validate(cb_context:context(), path_token(), ne_binary()) -> cb_context:context().
-spec validate_verb(cb_context:context(), http_method(), path_token(), ne_binary()) ->
                           cb_context:context().
validate(Context, DocId, Noun) ->
    validate_verb(Context, cb_context:req_verb(Context), DocId, Noun).
validate_verb(Context, ?HTTP_GET, DocId, ?IMAGE_REQ) ->
    load_template_image(DocId, cb_context:set_account_db(Context, ?WH_PROVISIONER_DB));
validate_verb(Context, ?HTTP_PUT, DocId, ?IMAGE_REQ) ->
    upload_template_image(DocId, cb_context:set_account_db(Context, ?WH_PROVISIONER_DB));
validate_verb(Context, ?HTTP_POST, DocId, ?IMAGE_REQ) ->
    upload_template_image(DocId, cb_context:set_account_db(Context, ?WH_PROVISIONER_DB));
validate_verb(Context, ?HTTP_DELETE, DocId, ?IMAGE_REQ) ->
    load_template_image(DocId, cb_context:set_account_db(Context, ?WH_PROVISIONER_DB)).

-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, DocId) ->
    %% see note at top of file
    JObj = cb_context:doc(Context),
    Template = wh_json:get_value(<<"template">>, JObj),
    Doc = wh_json:delete_key(<<"template">>, JObj),
    Context1 = crossbar_doc:save(cb_context:set_doc(Context, Doc)),
    case cb_context:resp_status(Context1) of
        'success' ->
            Opts = [{'headers', [{'content_type', "application/json"}]}],
            Context2 = crossbar_doc:save_attachment(DocId, ?TEMPLATE_ATTCH, wh_json:encode(Template), Context, Opts),
            case cb_context:resp_status(Context2) of
                'success' ->
                    SavedResp = cb_context:resp_data(Context1),
                    NewRespData = wh_json:set_value(<<"template">>, Template, SavedResp),
                    cb_context:set_resp_data(Context1, NewRespData);
                Else -> Else
            end;
        Else -> Else
    end.

-spec put(cb_context:context()) -> cb_context:context().
put(Context) ->
    %% see note at top of file
    JObj = cb_context:doc(Context),
    Template = wh_json:get_value(<<"template">>, JObj),
    Doc = wh_json:delete_key(<<"template">>, JObj),
    Context1 = crossbar_doc:save(cb_context:set_doc(Context, Doc)),
    case cb_context:resp_status(Context1) of
        'success' ->
            DocId = wh_doc:id(cb_context:doc(Context1)),
            Opts = [{'headers', [{'content_type', "application/json"}]}],
            Context2 = crossbar_doc:save_attachment(DocId, ?TEMPLATE_ATTCH, wh_json:encode(Template), Context, Opts),
            case cb_context:resp_status(Context2) of
                'success' ->
                    SavedResp = cb_context:resp_data(Context1),
                    cb_context:set_resp_data(Context1, wh_json:set_value(<<"template">>, Template, SavedResp));
                Else -> Else
            end;
        Else -> Else
    end.

-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, _) ->
    crossbar_doc:delete(Context).

-spec post(cb_context:context(), path_token(), path_token()) -> cb_context:context().
post(#cb_context{req_files=[{_, JObj}]}=Context, DocId, ?IMAGE_REQ) ->
    Contents = wh_json:get_value(<<"contents">>, JObj),
    CT = wh_json:get_value([<<"headers">>, <<"content_type">>], JObj, <<"application/octet-stream">>),
    Opts = [{'headers', [{'content_type', wh_util:to_list(CT)}]}],
    crossbar_doc:save_attachment(DocId, ?IMAGE_REQ, Contents, Context, Opts).

-spec put(cb_context:context(), path_token(), path_token()) -> cb_context:context().
put(#cb_context{req_files=[{_, JObj}]}=Context, DocId, ?IMAGE_REQ) ->
    Contents = wh_json:get_value(<<"contents">>, JObj),
    CT = wh_json:get_value([<<"headers">>, <<"content_type">>], JObj, <<"application/octet-stream">>),
    Opts = [{'headers', [{'content_type', wh_util:to_list(CT)}]}],
    crossbar_doc:save_attachment(DocId, ?IMAGE_REQ, Contents, Context, Opts).

-spec delete(cb_context:context(), path_token(), path_token()) -> cb_context:context().
delete(Context, DocId, ?IMAGE_REQ) ->
    crossbar_doc:delete_attachment(DocId, ?IMAGE_REQ, Context).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec load_template_image(path_token(), cb_context:context()) -> cb_context:context().
load_template_image(DocId, Context) ->
    crossbar_doc:load_attachment(DocId, ?IMAGE_REQ, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec upload_template_image(path_token(), cb_context:context()) -> cb_context:context().
upload_template_image(_, Context) ->
    case cb_context:req_files(Context) of
        [] ->
            Msg = wh_json:from_list([{<<"message">>, <<"Please provide an image file">>}
                                    ]),
            cb_context:add_validation_error(<<"file">>
                                            ,<<"required">>
                                            ,Msg
                                            ,Context
                                           );
        [{_, _}] ->
            crossbar_util:response(wh_json:new(), Context);
        [_|_] ->
            Msg = wh_json:from_list([{<<"message">>, <<"Please provide a single image file">>}
                                    ]),
            cb_context:add_validation_error(<<"file">>
                                            ,<<"maxItems">>
                                            ,Msg
                                            ,Context
                                           )
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load list of provision templates, each summarized.  Or a specific
%% provision template summary.
%% @end
%%--------------------------------------------------------------------
-spec load_provisioner_template_summary(cb_context:context()) -> cb_context:context().
load_provisioner_template_summary(Context) ->
    crossbar_doc:load_view(?CB_LIST, [], Context, fun normalize_view_results/2).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Create a new provision template document with the data provided, if it is valid
%% @end
%%--------------------------------------------------------------------
-spec create_provisioner_template(cb_context:context()) -> cb_context:context().
create_provisioner_template(#cb_context{}=Context) ->
    OnSuccess = fun(C) -> on_successful_validation('undefined', C) end,
    cb_context:validate_request_data(<<"provisioner_templates">>, Context, OnSuccess).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load a provision template document from the database
%% @end
%%--------------------------------------------------------------------
-spec load_provisioner_template(ne_binary(), cb_context:context()) -> cb_context:context().
load_provisioner_template(DocId, Context) ->
    %% see note at top of file
    Context1 = crossbar_doc:load(DocId, Context),
    case cb_context:resp_status(Context1) of
        'success' ->
            Context2 = crossbar_doc:load_attachment(DocId, ?TEMPLATE_ATTCH, Context),
            case cb_context:resp_status(Context2) of
                'success' ->
                    Template = wh_json:decode(cb_context:resp_data(Context2)),
                    RespJObj = cb_context:resp_data(Context1),
                    cb_context:set_resp_data(Context1, wh_json:set_value(<<"template">>, Template, RespJObj));
                Else -> Else
            end;
        Else -> Else
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update an existing provision template document with the data provided, if it is
%% valid
%% @end
%%--------------------------------------------------------------------
-spec update_provisioner_template(ne_binary(), cb_context:context()) -> cb_context:context().
update_provisioner_template(DocId, #cb_context{}=Context) ->
    OnSuccess = fun(C) -> on_successful_validation(DocId, C) end,
    cb_context:validate_request_data(<<"provisioner_templates">>, Context, OnSuccess).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec on_successful_validation(api_binary(), cb_context:context()) -> cb_context:context().
on_successful_validation('undefined', Context) ->
    C = cb_context:set_doc(Context, wh_json:set_values([{<<"pvt_type">>, <<"provisioner_template">>}
                                                        ,{<<"pvt_provider">>, <<"provisioner.net">>}
                                                        ,{<<"pvt_provisioner_type">>, <<"global">>}
                                                       ], cb_context:doc(Context))),
    provisioner_util:get_provision_defaults(C);
on_successful_validation(DocId, #cb_context{}=Context) ->
    crossbar_doc:load_merge(DocId, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the resuts of a view
%% @end
%%--------------------------------------------------------------------
-spec normalize_view_results(wh_json:object(), wh_json:objects()) -> wh_json:objects().
normalize_view_results(JObj, Acc) ->
    [wh_json:get_value(<<"value">>, JObj) | Acc].
