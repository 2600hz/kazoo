%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc Provision template module.
%%% Handle client requests for provisioner template documents
%%%
%%% <div class="notice">Regarding storing the template as an attachment:
%%% Since the template is a 300k JSON object it is more efficient to store it as
%%% an attachment, funky I know but necessary. Also since we already require
%%% two API calls for editing a template we will maintain backward compatibility by
%%% not requiring an additional API call for the template and merge/unmerge it
%%% from requests.</div>
%%%
%%%
%%% @author Jon Blanton
%%% @author Karl Anderson
%%% @author James Aimonetti
%%% @author Pierre Fenoll
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
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

        ,acceptable_content_types/0
        ]).

-include("crossbar.hrl").

-define(MOD_CONFIG_CAT, <<(?CONFIG_CAT)/binary, ".provisioner_templates">>).
-define(CB_LIST, <<"provisioner_templates/crossbar_listing">>).
-define(IMAGE_REQ, <<"image">>).
-define(TEMPLATE_ATTCH, <<"template">>).
-define(MIME_TYPES, [{<<"image">>, <<"*">>, '*'}
                    ,{<<"application">>, <<"octet-stream">>, '*'}
                     | ?BASE64_CONTENT_TYPES
                    ]).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    init_db(),
    _ = crossbar_bindings:bind(<<"*.content_types_provided.global_provisioner_templates">>, ?MODULE, 'content_types_provided'),
    _ = crossbar_bindings:bind(<<"*.content_types_accepted.global_provisioner_templates">>, ?MODULE, 'content_types_accepted'),
    _ = crossbar_bindings:bind(<<"*.allowed_methods.global_provisioner_templates">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.global_provisioner_templates">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.global_provisioner_templates">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.put.global_provisioner_templates">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"*.execute.post.global_provisioner_templates">>, ?MODULE, 'post'),
    _ = crossbar_bindings:bind(<<"*.execute.delete.global_provisioner_templates">>, ?MODULE, 'delete'),
    'ok'.

init_db() ->
    _ = kz_datamgr:db_create(?KZ_PROVISIONER_DB),
    _ = kapps_maintenance:refresh(?KZ_PROVISIONER_DB),
    'ok'.

%%------------------------------------------------------------------------------
%% @doc Add content types provided by this module.
%% @end
%%------------------------------------------------------------------------------
-spec acceptable_content_types() -> cowboy_content_types().
acceptable_content_types() -> ?MIME_TYPES.

-spec content_types_provided(cb_context:context(), path_token(), path_token()) ->
                                    cb_context:context().
content_types_provided(Context, PT1, PT2) ->
    content_types_provided_for_provisioner(Context, PT1, PT2, cb_context:req_verb(Context)).

-spec content_types_provided_for_provisioner(cb_context:context(), path_token(), path_token(), http_method()) ->
                                                    cb_context:context().
content_types_provided_for_provisioner(Context, DocId, ?IMAGE_REQ, ?HTTP_GET) ->
    case kz_datamgr:open_doc(?KZ_PROVISIONER_DB, DocId) of
        {'error', _} -> Context;
        {'ok', JObj} ->
            [Type, SubType] = binary:split(get_content_type(JObj), <<"/">>),
            lager:debug("found attachment of content type: ~s/~s~n", [Type, SubType]),
            cb_context:set_content_types_provided(Context, [{'to_binary', [{Type, SubType}]}])
    end;
content_types_provided_for_provisioner(Context, _, _, _) ->
    Context.

-spec get_content_type(kz_json:object()) -> kz_term:ne_binary().
get_content_type(JObj) ->
    kz_doc:attachment_content_type(JObj, ?IMAGE_REQ, <<"application/octet-stream">>).

%%------------------------------------------------------------------------------
%% @doc Add content types accepted by this module.
%% @end
%%------------------------------------------------------------------------------

-spec content_types_accepted(cb_context:context(), path_token(), path_token()) -> cb_context:context().
content_types_accepted(Context, PT1, PT2) ->
    content_types_accepted(Context, PT1, PT2, cb_context:req_verb(Context)).

-spec content_types_accepted(cb_context:context(), path_token(), path_token(), http_method()) -> cb_context:context().
content_types_accepted(Context, _, ?IMAGE_REQ, ?HTTP_PUT) ->
    cb_context:set_content_types_accepted(Context, [{'from_binary', ?MIME_TYPES}]);
content_types_accepted(Context, _, ?IMAGE_REQ, ?HTTP_POST) ->
    cb_context:set_content_types_accepted(Context, [{'from_binary', ?MIME_TYPES}]);
content_types_accepted(Context, _, ?IMAGE_REQ, _) ->
    Context.

%%------------------------------------------------------------------------------
%% @doc This function determines the verbs that are appropriate for the
%% given Nouns. For example `/accounts/' can only accept `GET' and `PUT'.
%%
%% Failure here returns `405'.
%% @end
%%------------------------------------------------------------------------------

-spec allowed_methods() -> http_methods().
allowed_methods() ->
    [?HTTP_GET, ?HTTP_PUT].

-spec allowed_methods(path_token()) -> http_methods().
allowed_methods(_TemplateId) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_DELETE].

-spec allowed_methods(path_token(), path_token()) -> http_methods().
allowed_methods(_TemplateId, ?IMAGE_REQ) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_DELETE].

%%------------------------------------------------------------------------------
%% @doc This function determines if the provided list of Nouns are valid.
%% Failure here returns `404 Not Found'.
%% @end
%%------------------------------------------------------------------------------

-spec resource_exists() -> 'true'.
resource_exists() -> 'true'.

-spec resource_exists(path_token()) -> 'true'.
resource_exists(_) -> 'true'.

-spec resource_exists(path_token(), path_token()) -> 'true'.
resource_exists(_, ?IMAGE_REQ) -> 'true'.

%%------------------------------------------------------------------------------
%% @doc This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400.
%% @end
%%------------------------------------------------------------------------------

-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    validate_verb(Context, cb_context:req_verb(Context)).

-spec validate_verb(cb_context:context(), http_method()) -> cb_context:context().
validate_verb(Context, ?HTTP_GET) ->
    load_provisioner_template_summary(cb_context:set_account_db(Context, ?KZ_PROVISIONER_DB));
validate_verb(Context, ?HTTP_PUT) ->
    create_provisioner_template(cb_context:set_account_db(Context, ?KZ_PROVISIONER_DB)).

-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context, DocId) ->
    validate_verb(Context, cb_context:req_verb(Context), DocId).

-spec validate_verb(cb_context:context(), http_method(), path_token()) -> cb_context:context().
validate_verb(Context, ?HTTP_GET, DocId) ->
    load_provisioner_template(DocId, cb_context:set_account_db(Context, ?KZ_PROVISIONER_DB));
validate_verb(Context, ?HTTP_POST, DocId) ->
    update_provisioner_template(DocId, cb_context:set_account_db(Context, ?KZ_PROVISIONER_DB));
validate_verb(Context, ?HTTP_DELETE, DocId) ->
    load_provisioner_template(DocId, cb_context:set_account_db(Context, ?KZ_PROVISIONER_DB)).

-spec validate(cb_context:context(), path_token(), kz_term:ne_binary()) -> cb_context:context().
validate(Context, DocId, Noun) ->
    validate_verb(Context, cb_context:req_verb(Context), DocId, Noun).

-spec validate_verb(cb_context:context(), http_method(), path_token(), kz_term:ne_binary()) ->
                           cb_context:context().
validate_verb(Context, ?HTTP_GET, DocId, ?IMAGE_REQ) ->
    load_template_image(DocId, cb_context:set_account_db(Context, ?KZ_PROVISIONER_DB));
validate_verb(Context, ?HTTP_PUT, _DocId, ?IMAGE_REQ) ->
    upload_template_image(cb_context:set_account_db(Context, ?KZ_PROVISIONER_DB));
validate_verb(Context, ?HTTP_POST, _DocId, ?IMAGE_REQ) ->
    upload_template_image(cb_context:set_account_db(Context, ?KZ_PROVISIONER_DB));
validate_verb(Context, ?HTTP_DELETE, DocId, ?IMAGE_REQ) ->
    load_template_image(DocId, cb_context:set_account_db(Context, ?KZ_PROVISIONER_DB)).

-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, DocId) ->
    %% see note at top of file
    JObj = cb_context:doc(Context),
    Template = kz_json:get_value(<<"template">>, JObj),
    Doc = kz_json:delete_key(<<"template">>, JObj),
    Context1 = crossbar_doc:save(cb_context:set_doc(Context, Doc)),
    case cb_context:resp_status(Context1) of
        'success' ->
            Opts = [{'content_type', <<"application/json">>} | ?TYPE_CHECK_OPTION(<<"provisioner_template">>)],
            Context2 = crossbar_doc:save_attachment(DocId, ?TEMPLATE_ATTCH, kz_json:encode(Template), Context, Opts),
            case cb_context:resp_status(Context2) of
                'success' ->
                    SavedResp = cb_context:resp_data(Context1),
                    NewRespData = kz_json:set_value(<<"template">>, Template, SavedResp),
                    cb_context:set_resp_data(Context1, NewRespData);
                Else -> Else
            end;
        Else -> Else
    end.

-spec put(cb_context:context()) -> cb_context:context().
put(Context) ->
    %% see note at top of file
    JObj = cb_context:doc(Context),
    Template = kz_json:get_value(<<"template">>, JObj),
    Doc = kz_json:delete_key(<<"template">>, JObj),
    Context1 = crossbar_doc:save(cb_context:set_doc(Context, Doc)),
    case cb_context:resp_status(Context1) of
        'success' ->
            DocId = kz_doc:id(cb_context:doc(Context1)),
            Opts = [{'content_type', <<"application/json">>} | ?TYPE_CHECK_OPTION(<<"provisioner_template">>)],
            Context2 = crossbar_doc:save_attachment(DocId, ?TEMPLATE_ATTCH, kz_json:encode(Template), Context, Opts),
            case cb_context:resp_status(Context2) of
                'success' ->
                    SavedResp = cb_context:resp_data(Context1),
                    cb_context:set_resp_data(Context1, kz_json:set_value(<<"template">>, Template, SavedResp));
                Else -> Else
            end;
        Else -> Else
    end.

-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, _) ->
    crossbar_doc:delete(Context).

-spec post(cb_context:context(), path_token(), path_token()) -> cb_context:context().
post(Context, DocId, ?IMAGE_REQ) ->
    [{_, JObj}] = cb_context:req_files(Context),
    Contents = kz_json:get_value(<<"contents">>, JObj),
    CT = kz_json:get_value([<<"headers">>, <<"content_type">>], JObj, <<"application/octet-stream">>),
    Opts = [{'content_type', CT} | ?TYPE_CHECK_OPTION(<<"provisioner_template">>)],
    crossbar_doc:save_attachment(DocId, ?IMAGE_REQ, Contents, Context, Opts).

-spec put(cb_context:context(), path_token(), path_token()) -> cb_context:context().
put(Context, DocId, ?IMAGE_REQ) ->
    [{_, JObj}] = cb_context:req_files(Context),
    Contents = kz_json:get_value(<<"contents">>, JObj),
    CT = kz_json:get_value([<<"headers">>, <<"content_type">>], JObj, <<"application/octet-stream">>),
    Opts = [{'content_type', CT} | ?TYPE_CHECK_OPTION(<<"provisioner_template">>)],
    crossbar_doc:save_attachment(DocId, ?IMAGE_REQ, Contents, Context, Opts).

-spec delete(cb_context:context(), path_token(), path_token()) -> cb_context:context().
delete(Context, DocId, ?IMAGE_REQ) ->
    crossbar_doc:delete_attachment(DocId, ?IMAGE_REQ, Context).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec load_template_image(path_token(), cb_context:context()) -> cb_context:context().
load_template_image(DocId, Context) ->
    crossbar_doc:load_attachment(DocId, ?IMAGE_REQ, ?TYPE_CHECK_OPTION(<<"provisioner_template">>), Context).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec upload_template_image(cb_context:context()) -> cb_context:context().
upload_template_image(Context) ->
    upload_template_image(Context, cb_context:req_files(Context)).
upload_template_image(Context, []) ->
    Msg = kz_json:from_list([{<<"message">>, <<"Please provide an image file">>}]),
    cb_context:add_validation_error(<<"file">>
                                   ,<<"required">>
                                   ,Msg
                                   ,Context
                                   );
upload_template_image(Context, [{_, _}]) ->
    crossbar_util:response(kz_json:new(), Context);
upload_template_image(Context, [_|_]) ->
    Msg = kz_json:from_list([{<<"message">>, <<"Please provide a single image file">>}]),
    cb_context:add_validation_error(<<"file">>, <<"maxItems">>, Msg, Context).

%%------------------------------------------------------------------------------
%% @doc Attempt to load list of provision templates, each summarized.  Or a specific
%% provision template summary.
%% @end
%%------------------------------------------------------------------------------
-spec load_provisioner_template_summary(cb_context:context()) -> cb_context:context().
load_provisioner_template_summary(Context) ->
    crossbar_doc:load_view(?CB_LIST, [], Context, fun normalize_view_results/2).

%%------------------------------------------------------------------------------
%% @doc Create a new provision template document with the data provided, if it is valid
%% @end
%%------------------------------------------------------------------------------
-spec create_provisioner_template(cb_context:context()) -> cb_context:context().
create_provisioner_template(Context) ->
    OnSuccess = fun(C) -> on_successful_validation('undefined', C) end,
    cb_context:validate_request_data(<<"provisioner_templates">>, Context, OnSuccess).

%%------------------------------------------------------------------------------
%% @doc Load a provision template document from the database
%% @end
%%------------------------------------------------------------------------------
-spec load_provisioner_template(kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
load_provisioner_template(DocId, Context) ->
    %% see note at top of file
    Context1 = crossbar_doc:load(DocId, Context, ?TYPE_CHECK_OPTION(<<"provisioner_template">>)),
    case cb_context:resp_status(Context1) of
        'success' ->
            Context2 = crossbar_doc:load_attachment(DocId, ?TEMPLATE_ATTCH, ?TYPE_CHECK_OPTION(<<"provisioner_template">>), Context),
            case cb_context:resp_status(Context2) of
                'success' ->
                    Template = kz_json:decode(cb_context:resp_data(Context2)),
                    RespJObj = cb_context:resp_data(Context1),
                    cb_context:set_resp_data(Context1, kz_json:set_value(<<"template">>, Template, RespJObj));
                Else -> Else
            end;
        Else -> Else
    end.

%%------------------------------------------------------------------------------
%% @doc Update an existing provision template document with the data provided, if it is
%% valid
%% @end
%%------------------------------------------------------------------------------
-spec update_provisioner_template(kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
update_provisioner_template(DocId, Context) ->
    OnSuccess = fun(C) -> on_successful_validation(DocId, C) end,
    cb_context:validate_request_data(<<"provisioner_templates">>, Context, OnSuccess).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec on_successful_validation(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
on_successful_validation('undefined', Context) ->
    Doc = kz_json:set_values([{<<"pvt_type">>, <<"provisioner_template">>}
                             ,{<<"pvt_provider">>, <<"provisioner.net">>}
                             ,{<<"pvt_provisioner_type">>, <<"global">>}
                             ]
                            ,cb_context:doc(Context)
                            ),

    case provisioner_util:get_provision_defaults(Doc) of
        {'ok', Defaults} ->
            cb_context:setters(Context
                              ,[{fun cb_context:set_doc/2, Defaults}
                               ,{fun cb_context:set_resp_status/2, 'success'}
                               ]);
        {'error', Msg} ->
            crossbar_util:response('error', Msg, 500, Context)
    end;
on_successful_validation(DocId, Context) ->
    crossbar_doc:load_merge(DocId, Context, ?TYPE_CHECK_OPTION(<<"provisioner_template">>)).

%%------------------------------------------------------------------------------
%% @doc Normalizes the results of a view.
%% @end
%%------------------------------------------------------------------------------
-spec normalize_view_results(kz_json:object(), kz_json:objects()) -> kz_json:objects().
normalize_view_results(JObj, Acc) ->
    [kz_json:get_value(<<"value">>, JObj) | Acc].
